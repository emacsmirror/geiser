;; geiser-mode.el -- minor mode for scheme buffers

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Feb 08, 2009 15:13

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comentary:

;; Minor mode adding Geiser REPL/Emacs interaction commands to Scheme
;; buffers.

;;; Code:

(require 'geiser-autodoc)
(require 'geiser-eval)
(require 'geiser-popup)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-mode nil
  "Mode enabling Geiser abilities in Scheme buffers &co.."
  :group 'geiser)

(defcustom geiser-mode-autodoc-p t
  "Whether `geiser-autodoc-mode' gets enabled by default in factor buffers."
  :group 'geiser-mode
  :group 'geiser-autodoc
  :type 'boolean)



;;; Auxiliary functions:

(geiser-popup--define mode "*Geiser evaluation results*" scheme-mode)

(defun geiser-eval--display-error (err output)
  (if (not output)
      (message (geiser-eval--error-str err))
    (geiser-mode--with-buffer
      (erase-buffer)
      (insert ";; " (geiser-eval--error-str err))
      (newline 2)
      (insert output)
      (newline))
    (geiser-mode--pop-to-buffer)))


;;; Evaluation commands:

(defun geiser-send-region (start end &optional and-go)
  "Send the current region to the Geiser REPL.
With prefix, goes to the REPL buffer afterwards (as
`geiser-send-region-and-go')"
  (interactive "rP")
  (let* ((str (buffer-substring-no-properties start end))
         (code `(:gs (:scm ,str)))
         (ret (geiser-eval--send/wait code))
         (err (geiser-eval--retort-error ret)))
    (when and-go
      (switch-to-guile)
      (push-mark)
      (goto-char (point-max)))
    (if (not err)
        (message (format "=> %s" (geiser-eval--retort-result ret)))
      (geiser-eval--display-error err (geiser-eval--retort-output ret)))))

(defun geiser-send-region-and-go (start end)
  "Send the current region to the Geiser REPL and visit it afterwads."
  (interactive "r")
  (geiser-send-region start end t))

(defun geiser-send-definition (&optional and-go)
  "Send the current definition to the Geiser REPL.
With prefix, goes to the REPL buffer afterwards (as
`geiser-send-definition-and-go')"
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (geiser-send-region (point) end and-go))))

(defun geiser-send-definition-and-go ()
  "Send the current definition to the Geiser REPL and visit it afterwads."
  (interactive)
  (geiser-send-definition t))

(defun geiser-send-last-sexp ()
  "Send the previous sexp to the Geiser REPL."
  (interactive)
  (geiser-send-region (save-excursion (backward-sexp) (point)) (point)))


;;; Geiser mode:

(make-variable-buffer-local
 (defvar geiser-mode-string " Geiser"
   "Modeline indicator for geiser-mode"))

(defvar geiser-mode-map (make-sparse-keymap)
  "Key map for geiser-mode")

(define-minor-mode geiser-mode
  "Toggle Geiser's mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Geiser mode is enabled, a host of nice utilities for
interacting with the Geiser REPL is at your disposal.
\\{geiser-mode-map}"
  :init-value nil
  :lighter geiser-mode-string
  :group 'geiser
  :keymap geiser-mode-map
  (setq geiser-autodoc-mode-string "/A")
  (when geiser-mode-autodoc-p (geiser-autodoc-mode geiser-mode)))


;;; Keys:

(define-key geiser-mode-map "\M-\C-x" 'geiser-send-definition)
(define-key geiser-mode-map "\C-c\C-a" 'geiser-autodoc-mode)
(define-key geiser-mode-map "\C-x\C-e" 'geiser-send-last-sexp)
(define-key geiser-mode-map "\C-c\C-e" 'geiser-send-definition)
(define-key geiser-mode-map "\C-c\M-e" 'geiser-send-definition-and-go)
(define-key geiser-mode-map "\C-c\C-r" 'geiser-send-region)
(define-key geiser-mode-map "\C-c\M-r" 'geiser-send-region-and-go)
(define-key geiser-mode-map "\C-c\M-c" 'geiser-compile-definition)
(define-key geiser-mode-map "\C-c\C-c" 'geiser-compile-definition-and-go)
(define-key geiser-mode-map "\C-c\C-t" 'geiser-trace-procedure)
(define-key geiser-mode-map "\C-c\C-x" 'geiser-expand-current-form)
(define-key geiser-mode-map "\C-c\C-z" 'switch-to-guile)
(define-key geiser-mode-map "\C-c\C-l" 'geiser-load-file)
(define-key geiser-mode-map "\C-c\C-k" 'geiser-compile-file)


(provide 'geiser-mode)
;;; geiser-mode.el ends here
