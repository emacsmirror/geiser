;;; geiser-repl.el --- Geiser's REPL

;; Copyright (C) 2009  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode (comint-based) to interact with a Scheme REPL.

;;; Code:

(require 'geiser-autodoc)
(require 'geiser-edit)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-connection)
(require 'geiser-custom)
(require 'geiser-base)

(require 'comint)


;;; Customization:

(defgroup geiser-repl nil
  "Interacting with the Geiser REPL."
  :group 'geiser)

(defcustom geiser-repl-use-other-window t
  "Whether to Use a window other than the current buffer's when
switching to the Geiser REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(defcustom geiser-repl-window-allow-split t
  "Whether to allow window splitting when switching to the Geiser
REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(defcustom geiser-repl-history-filename (expand-file-name "~/.geiser_history")
  "File where REPL input history is saved, so that it persists between sessions.
This is actually the base name: the concrete Scheme
implementation name gets appended to it."
  :type 'filename
  :group 'geiser-repl)

(defcustom geiser-repl-history-size comint-input-ring-size
  "Maximum size of the saved REPL input history."
  :type 'integer
  :group 'geiser-repl)

(defcustom geiser-repl-autodoc-p t
  "Whether to enable `geiser-autodoc-mode' in the REPL by default."
  :type 'boolean
  :group 'geiser-repl)

(defcustom geiser-repl-read-only-prompt-p t
  "Whether the REPL's prompt should be read-only."
  :type 'boolean
  :group 'geiser-repl)


;;; Geiser REPL buffers and processes:

(defvar geiser-repl--repls nil)
(defvar geiser-repl--closed-repls nil)

(make-variable-buffer-local
 (defvar geiser-repl--repl nil))

(defsubst geiser-repl--this-buffer-repl ()
  geiser-repl--repl)

(defsubst geiser-repl--set-this-buffer-repl (r)
  (setq geiser-repl--repl r))

(defun geiser-repl--repl/impl (impl &optional repls)
  (catch 'repl
    (dolist (repl (or repls geiser-repl--repls))
      (with-current-buffer repl
        (when (eq geiser-impl--implementation impl)
          (throw 'repl repl))))))

(defun geiser-repl--get-repl (&optional impl)
  (or (and (not impl) geiser-repl--repl)
      (setq geiser-repl--repl
            (let ((impl (or impl
                            geiser-impl--implementation
                            (geiser-impl--guess))))
              (when impl (geiser-repl--repl/impl impl))))))

(defun geiser-repl--active-impls ()
  (let ((act))
    (dolist (repl geiser-repl--repls act)
      (with-current-buffer repl
        (add-to-list 'act geiser-impl--implementation)))))

(defsubst geiser-repl--repl-name (impl)
  (format "%s REPL" (geiser-impl--impl-str impl)))

(defun geiser-repl--to-repl-buffer (impl)
  (unless (and (eq major-mode 'geiser-repl-mode)
               (not (get-buffer-process (current-buffer))))
    (let* ((old (geiser-repl--repl/impl impl geiser-repl--closed-repls))
           (old (and (buffer-live-p old) old)))
      (pop-to-buffer
       (or old
           (generate-new-buffer (format "* %s *"
                                        (geiser-repl--repl-name impl)))))))
  (geiser-repl-mode)
  (geiser-impl--set-buffer-implementation impl))

(defun geiser-repl--start-repl (impl)
  (message "Starting Geiser REPL for %s ..." impl)
  (geiser-repl--to-repl-buffer impl)
  (let ((binary (geiser-impl--binary impl))
        (args (geiser-impl--parameters impl))
        (prompt-rx (geiser-impl--prompt-regexp impl))
        (cname (geiser-repl--repl-name impl)))
    (unless (and binary prompt-rx)
      (error "Sorry, I don't know how to start a REPL for %s" impl))
    (set (make-local-variable 'comint-prompt-regexp) prompt-rx)
    (apply 'make-comint-in-buffer `(,cname ,(current-buffer) ,binary nil ,@args))
    (geiser-repl--wait-for-prompt 10000)
    (geiser-repl--history-setup)
    (geiser-con--setup-connection (current-buffer) prompt-rx)
    (add-to-list 'geiser-repl--repls (current-buffer))
    (geiser-repl--set-this-buffer-repl (current-buffer))
    (geiser-impl--startup impl)))

(defun geiser-repl--process ()
  (let ((buffer (geiser-repl--get-repl geiser-impl--implementation)))
    (or (and (buffer-live-p buffer) (get-buffer-process buffer))
        (error "No Geiser REPL for this buffer (try M-x run-geiser)"))))

(setq geiser-eval--default-proc-function 'geiser-repl--process)

(defun geiser-repl--wait-for-prompt (timeout)
  (let ((p (point)) (seen) (buffer (current-buffer)))
    (while (and (not seen)
                (> timeout 0)
                (get-buffer-process buffer))
      (sleep-for 0.1)
      (setq timeout (- timeout 100))
      (goto-char p)
      (setq seen (re-search-forward comint-prompt-regexp nil t)))
    (goto-char (point-max))
    (unless seen (error "No prompt found!"))))


;;; Interface: starting and interacting with geiser REPL:

(defun geiser-repl--read-impl (prompt &optional active)
  (geiser-impl--read-impl prompt (and active (geiser-repl--active-impls))))

(defsubst geiser-repl--only-impl-p ()
  (and (null (cdr geiser-impl--impls))
       (car geiser-impl--impls)))

(defun run-geiser (impl)
  "Start a new Geiser REPL."
  (interactive
   (list (or (geiser-repl--only-impl-p)
             (and (eq major-mode 'geiser-repl-mode) geiser-impl--implementation)
             (geiser-repl--read-impl
              "Start Geiser for scheme implementation: "))))
   (geiser-repl--start-repl impl))

(defun switch-to-geiser (&optional ask impl)
  "Switch to running Geiser REPL.
With prefix argument, ask for which one if more than one is running.
If no REPL is running, execute `run-geiser' to start a fresh one."
  (interactive "P")
  (let* ((impl (or impl geiser-impl--implementation))
         (repl (cond ((and (not ask) (not impl)
                           (or (geiser-repl--this-buffer-repl)
                               (car geiser-repl--repls))))
                     ((and (not ask) impl (geiser-repl--repl/impl impl)))))
         (pop-up-windows geiser-repl-window-allow-split))
    (if repl
        (pop-to-buffer repl)
      (run-geiser (or impl
                      (and (not ask)
                           (geiser-repl--only-impl-p))
                      (geiser-repl--read-impl "Switch to scheme REPL: "))))))

(defalias 'geiser 'switch-to-geiser)

(defun geiser-repl-nuke ()
  "Try this command if the REPL becomes unresponsive."
  (interactive)
  (goto-char (point-max))
  (comint-kill-region comint-last-input-start (point))
  (comint-redirect-cleanup)
  (geiser-con--setup-connection (current-buffer)
                                comint-prompt-regexp))


;;; REPL history and clean-up:

(defsubst geiser-repl--history-file ()
  (format "%s.%s" geiser-repl-history-filename geiser-impl--implementation))

(defun geiser-repl--on-quit ()
  (comint-write-input-ring)
  (let ((cb (current-buffer))
        (impl geiser-impl--implementation)
        (comint-prompt-read-only nil))
    (setq geiser-repl--repls (remove cb geiser-repl--repls))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq geiser-impl--implementation impl)
                   (equal cb (geiser-repl--this-buffer-repl)))
          (geiser-repl--get-repl geiser-impl--implementation))))))

(defun geiser-repl--sentinel (proc event)
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer proc)
      (let ((comint-prompt-read-only nil)
            (comint-input-ring-file-name (geiser-repl--history-file)))
        (geiser-repl--on-quit)
        (push (current-buffer) geiser-repl--closed-repls)
        (when (buffer-name (current-buffer))
          (comint-kill-region comint-last-input-start (point))
          (insert "\nIt's been nice interacting with you!\n")
          (insert "Press C-cz to bring me back.\n" ))))))

(defun geiser-repl--on-kill ()
  (geiser-repl--on-quit)
  (setq geiser-repl--closed-repls
        (remove (current-buffer) geiser-repl--closed-repls)))

(defun geiser-repl--input-filter (str)
  (and (not (string-match "^\\s *$" str))
       (not (string-match "^,quit *$" str))))

(defun geiser-repl--history-setup ()
  (set (make-local-variable 'comint-input-ring-file-name)
       (geiser-repl--history-file))
  (set (make-local-variable 'comint-input-ring-size) geiser-repl-history-size)
  (set (make-local-variable 'comint-input-filter) 'geiser-repl--input-filter)
  (add-hook 'kill-buffer-hook 'geiser-repl--on-kill nil t)
  (comint-read-input-ring t)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'geiser-repl--sentinel))


;;; geiser-repl mode:

(defun geiser-repl--bol ()
  (interactive)
  (when (= (point) (comint-bol)) (beginning-of-line)))

(defun geiser-repl--beginning-of-defun ()
  (let ((p (point)))
    (comint-bol)
    (when (not (eq (char-after (point)) ?\())
      (skip-syntax-forward "^(" p))))

(defun geiser-repl--module-function (&optional ignore) :f)

(defun geiser-repl--doc-module ()
  (interactive)
  (let ((geiser-eval--get-module-function
         (geiser-impl--module-function geiser-impl--implementation)))
    (geiser-doc-module)))

(define-derived-mode geiser-repl-mode comint-mode "Geiser REPL"
  "Major mode for interacting with an inferior scheme repl process.
\\{geiser-repl-mode-map}"
  (set (make-local-variable 'mode-line-process) nil)
  (set (make-local-variable 'comint-use-prompt-regexp) t)
  (set (make-local-variable 'comint-prompt-read-only)
       geiser-repl-read-only-prompt-p)
  (set (make-local-variable 'beginning-of-defun-function)
       'geiser-repl--beginning-of-defun)
  (set-syntax-table scheme-mode-syntax-table)
  (setq geiser-eval--get-module-function 'geiser-repl--module-function)
  (when geiser-repl-autodoc-p (geiser-autodoc-mode 1))
  (compilation-shell-minor-mode 1))

(define-key geiser-repl-mode-map "\C-d" 'delete-char)

(define-key geiser-repl-mode-map "\C-cz" 'switch-to-geiser)
(define-key geiser-repl-mode-map "\C-c\C-z" 'switch-to-geiser)
(define-key geiser-repl-mode-map "\C-a" 'geiser-repl--bol)
(define-key geiser-repl-mode-map (kbd "<home>") 'geiser-repl--bol)
(define-key geiser-repl-mode-map "\C-ca" 'geiser-autodoc-mode)
(define-key geiser-repl-mode-map "\C-cd" 'geiser-doc-symbol-at-point)
(define-key geiser-repl-mode-map "\C-cm" 'geiser-repl--doc-module)
(define-key geiser-repl-mode-map "\C-ck" 'geiser-compile-file)
(define-key geiser-repl-mode-map "\C-cl" 'geiser-load-file)

(define-key geiser-repl-mode-map "\M-p" 'comint-previous-matching-input-from-input)
(define-key geiser-repl-mode-map "\M-n" 'comint-next-matching-input-from-input)
(define-key geiser-repl-mode-map "\C-c\M-p" 'comint-previous-input)
(define-key geiser-repl-mode-map "\C-c\M-n" 'comint-next-input)

(define-key geiser-repl-mode-map (kbd "TAB") 'geiser-completion--complete-symbol)
(define-key geiser-repl-mode-map (kbd "M-TAB") 'geiser-completion--complete-symbol)
(define-key geiser-repl-mode-map (kbd "M-`") 'geiser-completion--complete-module)
(define-key geiser-repl-mode-map (kbd "C-.") 'geiser-completion--complete-module)
(define-key geiser-repl-mode-map "\M-." 'geiser-edit-symbol-at-point)
(define-key geiser-repl-mode-map "\M-," 'geiser-edit-pop-edit-symbol-stack)


;;; Unload:

(defun geiser-repl--repl-list ()
  (let (lst)
    (dolist (repl geiser-repl--repls lst)
      (when (buffer-live-p repl)
        (with-current-buffer repl
          (push geiser-impl--implementation lst))))))

(defun geiser-repl--restore (impls)
  (dolist (impl impls)
    (when impl (geiser nil impl))))

(defun geiser-repl-unload-function ()
  (dolist (repl geiser-repl--repls)
    (when (buffer-live-p repl)
      (kill-buffer repl))))


(provide 'geiser-repl)
;;; geiser-repl.el ends here
