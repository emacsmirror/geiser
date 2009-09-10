;; geiser-autodoc.el -- autodoc mode

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Feb 08, 2009 19:44



(require 'geiser-eval)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)

(require 'eldoc)


;;; Customization:

(defgroup geiser-autodoc nil
  "Options for displaying autodoc strings in the echo area."
  :group 'geiser)

(geiser-custom--defface autodoc-current-arg
  'bold geiser-autodoc "highlighting current argument in autodoc messages")

(geiser-custom--defface autodoc-procedure-name
  'font-lock-function-name-face
  geiser-autodoc "highlighting procedure name in autodoc messages")

(defcustom geiser-autodoc-delay 0.3
  "Delay before autodoc messages are fetched and displayed, in seconds."
  :type 'number
  :group 'geiser-autodoc)

(defcustom geiser-autodoc-display-module-p t
  "Whether to display procedure module in autodoc strings."
  :type 'boolean
  :group 'geiser-autodoc)

(defcustom geiser-autodoc-procedure-name-format "%s:%s"
  "Format for displaying module and procedure name, in that order,
when `geiser-autodoc-display-module-p' is on."
  :type 'string
  :group 'geiser-autodoc)


;;; Procedure arguments:

(make-variable-buffer-local
 (defvar geiser-autodoc--cached-signatures nil))

(defun geiser-autodoc--get-signatures (funs &optional keep-cached)
  (when funs
    (let ((fs (assq (car funs) geiser-autodoc--cached-signatures)))
      (unless fs
        (let ((missing) (cached))
          (if (not geiser-autodoc--cached-signatures)
              (setq missing funs)
            (dolist (f funs)
              (let ((cf (assq f geiser-autodoc--cached-signatures)))
                (if cf (push cf cached)
                  (push f missing)))))
          (unless (or cached keep-cached)
            (setq geiser-autodoc--cached-signatures nil))
          (when missing
            (let ((res (geiser-eval--send/result `(:eval ((:ge autodoc)
                                                          (quote ,missing)))
                                                 500)))
              (when res
                (setq geiser-autodoc--cached-signatures
                      (append res (if keep-cached
                                      geiser-autodoc--cached-signatures
                                    cached))))))))
      geiser-autodoc--cached-signatures)))

(defun geiser-autodoc--insert-args (args current &optional pos)
  (dolist (a args)
    (let ((p (point)))
      (insert (format "%s" a))
      (when (or (and (numberp pos)
                     (numberp current)
                     (setq current (1+ current))
                     (= (1+ pos) current))
                (and (symbolp current)
                     (listp a)
                     (eq current (car a))))
        (put-text-property p (point) 'face 'geiser-font-lock-autodoc-current-arg)
        (setq pos nil current nil)))
    (insert " "))
  (when args (backward-char))
  current)

(defsubst geiser-autodoc--proc-name (proc module)
  (let ((str (if module
                 (format geiser-autodoc-procedure-name-format module proc)
               proc)))
    (propertize str 'face 'geiser-font-lock-autodoc-procedure-name)))

(defun geiser-autodoc--str (desc signature)
  (let ((proc (car desc))
        (args (cdr (assoc 'args signature)))
        (module (cdr (assoc 'module signature))))
    (if (not args) (geiser-autodoc--proc-name proc module)
      (let ((cpos 1)
            (pos (or (cadr desc) 0))
            (prev (caddr desc))
            (reqs (cdr (assoc 'required args)))
            (opts (cdr (assoc 'optional args)))
            (keys (cdr (assoc 'key args))))
        (save-current-buffer
          (set-buffer (geiser-syntax--font-lock-buffer))
          (erase-buffer)
          (insert (format "(%s" (geiser-autodoc--proc-name proc module)))
          (when reqs
            (insert " ")
            (setq cpos
                  (geiser-autodoc--insert-args reqs
                                               cpos
                                               (and (not (zerop pos)) pos))))
          (when opts
            (insert " [")
            (setq cpos (geiser-autodoc--insert-args opts cpos pos))
            (when keys
              (insert " [")
              (geiser-autodoc--insert-args keys prev nil)
              (insert "]"))
            (insert "]"))
          (insert ")")
          (buffer-string))))))

(defun geiser-autodoc--autodoc (path &optional keep-cached)
  (let ((signs (geiser-autodoc--get-signatures (mapcar 'car path) keep-cached))
        (p (car path))
        (s))
    (while (and path (not s))
      (unless (setq s (cdr (assq (car p) signs)))
        (setq p (car path))
        (setq path (cdr path))))
    (when s (geiser-autodoc--str p s))))


;;; Autodoc function:

(make-variable-buffer-local
 (defvar geiser-autodoc--inhibit-flag nil))

(defun geiser-autodoc--eldoc-function ()
  (condition-case e
      (and (not geiser-autodoc--inhibit-flag)
           (geiser-autodoc--autodoc (geiser-syntax--scan-sexps)))
    (error (format "Autodoc not available (%s)" (error-message-string e)))))


;;; Autodoc mode:

(make-variable-buffer-local
 (defvar geiser-autodoc-mode-string " A"
   "Modeline indicator for geiser-autodoc-mode"))

(define-minor-mode geiser-autodoc-mode
  "Toggle Geiser's Autodoc mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Autodoc mode is enabled, a synopsis of the word at point is
displayed in the minibuffer."
  :init-value nil
  :lighter geiser-autodoc-mode-string
  :group 'geiser-autodoc

  (set (make-local-variable 'eldoc-documentation-function)
       (when geiser-autodoc-mode 'geiser-autodoc--eldoc-function))
  (set (make-local-variable 'eldoc-minor-mode-string) nil)
  (set (make-local-variable 'eldoc-idle-delay) geiser-autodoc-delay)
  (eldoc-mode geiser-autodoc-mode)
  (message "Geiser Autodoc %s" (if geiser-autodoc-mode "enabled" "disabled")))


(provide 'geiser-autodoc)
;;; geiser-autodoc.el ends here
