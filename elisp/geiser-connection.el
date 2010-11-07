;;; geiser-connection.el -- talking to a scheme process

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Feb 07, 2009 21:11



;; Connection datatype and functions for managing request queues
;; between emacs and inferior guile processes.

(require 'geiser-log)
(require 'geiser-syntax)
(require 'geiser-base)

(require 'comint)
(require 'advice)


;;; Buffer connections:

(make-variable-buffer-local
 (defvar geiser-con--connection nil))

(defun geiser-con--get-connection (buffer/proc)
  (if (processp buffer/proc)
      (geiser-con--get-connection (process-buffer buffer/proc))
    (with-current-buffer buffer/proc geiser-con--connection)))


;;; Request datatype:

(defun geiser-con--make-request (con str cont &optional sender-buffer)
  (list :geiser-connection-request
        (cons :id (geiser-con--connection-inc-count con))
        (cons :string str)
        (cons :continuation cont)
        (cons :buffer (or sender-buffer (current-buffer)))
        (cons :connection con)))

(defsubst geiser-con--request-p (req)
  (and (listp req) (eq (car req) :geiser-connection-request)))

(defsubst geiser-con--request-id (req)
  (cdr (assoc :id req)))

(defsubst geiser-con--request-string (req)
  (cdr (assoc :string req)))

(defsubst geiser-con--request-continuation (req)
  (cdr (assoc :continuation req)))

(defsubst geiser-con--request-buffer (req)
  (cdr (assoc :buffer req)))

(defsubst geiser-con--request-connection (req)
  (cdr (assoc :connection req)))

(defsubst geiser-con--request-deactivate (req)
  (setcdr (assoc :continuation req) nil))

(defsubst geiser-con--request-deactivated-p (req)
  (null (cdr (assoc :continuation req))))


;;; Connection datatype:

(defsubst geiser-con--make-connection (buffer)
  (list :geiser-connection
        (cons :requests (list))
        (cons :current nil)
        (cons :count 0)
        (cons :completed (make-hash-table :weakness 'value))
        (cons :buffer buffer)
        (cons :timer nil)
        (cons :reply (geiser-con--make-reply-buffer (buffer-name buffer)))))

(defun geiser-con--make-reply-buffer (n)
  (let ((rb (generate-new-buffer (concat " geiser-con-reply: " n))))
    (buffer-disable-undo rb)
    rb))

(defsubst geiser-con--connection-p (c)
  (and (listp c) (eq (car c) :geiser-connection)))

(defsubst geiser-con--connection-buffer (c)
  (cdr (assoc :buffer c)))

(defsubst geiser-con--connection-process (c)
  (get-buffer-process (geiser-con--connection-buffer c)))

(defsubst geiser-con--connection-requests (c)
  (cdr (assoc :requests c)))

(defsubst geiser-con--connection-current-request (c)
  (cdr (assoc :current c)))

(defsubst geiser-con--connection-reply-buffer (c)
  (cdr (assoc :reply c)))

(defun geiser-con--connection-clean-current-request (c)
  (let* ((cell (assoc :current c))
         (req (cdr cell)))
    (when req
      (puthash (geiser-con--request-id req) req (cdr (assoc :completed c)))
      (setcdr cell nil))))

(defun geiser-con--connection-add-request (c r)
  (let ((reqs (assoc :requests c)))
    (setcdr reqs (append (cdr reqs) (list r)))))

(defsubst geiser-con--connection-completed-p (c id)
  (gethash id (cdr (assoc :completed c))))

(defun geiser-con--connection-pop-request (c)
  (let ((reqs (assoc :requests c))
        (current (assoc :current c)))
    (setcdr current (prog1 (cadr reqs) (setcdr reqs (cddr reqs))))
    (if (and (cdr current)
             (geiser-con--request-deactivated-p (cdr current)))
        (geiser-con--connection-pop-request c)
      (cdr current))))

(defun geiser-con--connection-inc-count (c)
  (let* ((cnt (assoc :count c))
         (new (1+ (cdr cnt))))
    (setcdr cnt new)
    new))


;;; Connection setup:
(make-variable-buffer-local
 (defvar geiser-con--debugging-prompt-regexp nil))

(make-variable-buffer-local
 (defvar geiser-con--debugging-inhibits-eval t))

(make-variable-buffer-local
 (defvar geiser-con--debugging-preamble-regexp nil))

(defun geiser-con--is-debugging ()
  (and geiser-con--debugging-prompt-regexp
       geiser-con--debugging-inhibits-eval
       comint-last-prompt-overlay
       (string-match-p geiser-con--debugging-prompt-regexp
                       (buffer-substring (overlay-start
                                          comint-last-prompt-overlay)
                                         (overlay-end
                                          comint-last-prompt-overlay)))))

(defsubst geiser-con--has-entered-debugger (con)
  (with-current-buffer (geiser-con--connection-buffer con)
    (and geiser-con--debugging-prompt-regexp
         (re-search-backward geiser-con--debugging-prompt-regexp nil t)
         (or (null geiser-con--debugging-preamble-regexp)
             (save-excursion
               (re-search-backward geiser-con--debugging-preamble-regexp
                                   nil t))))))

(defun geiser-con--connection-teardown ()
  (when geiser-con--connection
    (kill-buffer
     (geiser-con--connection-reply-buffer geiser-con--connection))))

(defun geiser-con--setup-connection (buffer
                                     prompt-regexp
                                     &optional debug-prompt-regexp
                                     debug-preamble-regexp)
  (with-current-buffer buffer
    (geiser-con--connection-teardown)
    (setq geiser-con--debugging-prompt-regexp debug-prompt-regexp)
    (setq geiser-con--debugging-preamble-regexp debug-preamble-regexp)
    (setq geiser-con--connection (geiser-con--make-connection buffer))
    (set (make-local-variable 'comint-redirect-insert-matching-regexp)
         (not (null debug-prompt-regexp)))
    (set (make-local-variable 'comint-redirect-finished-regexp)
         (if debug-prompt-regexp
             (format "\\(%s\\)\\|\\(%s\\)" prompt-regexp debug-prompt-regexp)
           prompt-regexp))
    (setq comint-prompt-regexp comint-redirect-finished-regexp)
    (add-hook 'comint-redirect-hook 'geiser-con--comint-redirect-hook nil t)))


;;; Requests handling:

(defun geiser-con--comint-buffer-form (con)
  (with-current-buffer (geiser-con--connection-reply-buffer con)
    (goto-char (point-max))
    (if (geiser-con--has-entered-debugger con)
        `((error (key . geiser-debugger))
          (output . ,(buffer-substring (point-min) (point))))
      (condition-case nil
          (progn
            (goto-char (point-min))
            (re-search-forward "((\\(result\\|error\\)\\>")
            (goto-char (match-beginning 0))
            (let ((form (read (current-buffer))))
              (if (listp form) form (error ""))))
        (error `((error (key . geiser-con-error))
                 (output . ,(buffer-string))))))))

(defun geiser-con--process-next (con)
  (when (not (geiser-con--connection-current-request con))
    (let* ((buffer (geiser-con--connection-buffer con))
           (debug-prompt (with-current-buffer buffer
                           geiser-con--debugging-prompt-regexp))
           (debug-preamble (with-current-buffer buffer
                             geiser-con--debugging-preamble-regexp))
           (req (geiser-con--connection-pop-request con))
           (str (and req (geiser-con--request-string req)))
           (rbuffer (geiser-con--connection-reply-buffer con)))
      (when (and buffer (buffer-live-p buffer) req str)
        (with-current-buffer rbuffer
          (delete-region (point-min) (point-max)))
        (set-buffer buffer)
        (if (geiser-con--is-debugging)
            (geiser-con--request-deactivate req)
          (geiser-log--info "<%s>: %s" (geiser-con--request-id req) str)
          (comint-redirect-send-command (format "%s" str) rbuffer nil t))))))

(defun geiser-con--process-completed-request (req)
  (let* ((cont (geiser-con--request-continuation req))
         (id (geiser-con--request-id req))
         (rstr (geiser-con--request-string req))
         (buffer (geiser-con--request-buffer req))
         (con (geiser-con--request-connection req))
         (form (geiser-con--comint-buffer-form con)))
    (if (not cont)
        (geiser-log--warn "<%s> Droping result for request %S: %s"
                          id rstr form)
      (condition-case cerr
          (with-current-buffer (or buffer (current-buffer))
            (funcall cont form)
            (geiser-log--info "<%s>: processed" id))
        (error (geiser-log--error
                "<%s>: continuation failed %S \n\t%s" id rstr cerr))))))

(defun geiser-con--comint-redirect-hook ()
  (if (not geiser-con--connection)
      (geiser-log--error "No connection in buffer")
    (let ((req (geiser-con--connection-current-request
                geiser-con--connection)))
      (if (not req) (geiser-log--error "No current request")
        (geiser-con--process-completed-request req)
        (geiser-con--connection-clean-current-request
         geiser-con--connection)))))

(defadvice comint-redirect-setup
  (after geiser-con--advice
         (output-buffer comint-buffer finished-regexp &optional echo))
  (with-current-buffer comint-buffer
    (when geiser-con--connection (setq mode-line-process nil))))
(ad-activate 'comint-redirect-setup)


;;; Message sending interface:

(defconst geiser-con--error-message "Geiser connection not active")

(defun geiser-con--send-string (buffer/proc str cont &optional sender-buffer)
  (save-current-buffer
    (let ((con (geiser-con--get-connection buffer/proc)))
      (unless con (error geiser-con--error-message))
      (let ((req (geiser-con--make-request con str cont sender-buffer)))
        (geiser-con--connection-add-request con req)
        (geiser-con--process-next con)
        req))))

(defvar geiser-connection-timeout 30000
  "Time limit, in msecs, blocking on synchronous evaluation requests")

(defun geiser-con--send-string/wait (buffer/proc str cont
                                     &optional timeout sbuf)
  (save-current-buffer
    (let* ((con (geiser-con--get-connection buffer/proc))
           (proc (and con (geiser-con--connection-process con))))
      (unless proc
        (error geiser-con--error-message))
      (with-current-buffer (geiser-con--connection-buffer con)
        (when (geiser-con--is-debugging)
          (error "Geiser REPL is in debug mode")))
      (let* ((req (geiser-con--send-string buffer/proc str cont sbuf))
             (id (and req (geiser-con--request-id req)))
             (time (or timeout geiser-connection-timeout))
             (step 100)
             (waitsecs (/ step 1000.0)))
        (when id
          (condition-case nil
              (while (and (> time 0)
                          (geiser-con--connection-process con)
                          (not (geiser-con--connection-completed-p con id)))
                (unless (accept-process-output nil waitsecs)
                  (setq time (- time step))))
            (error (setq time 0)))
          (or (> time 0)
              (geiser-con--request-deactivate req)
              nil))))))


(provide 'geiser-connection)
;;; geiser-connection.el ends here
