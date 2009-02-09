;; geiser-autodoc.el -- autodoc mode

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Feb 08, 2009 19:44

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

;; A minor mode that echoes information about procedures and variables
;; near point at the minibuffer.

;;; Code:

(require 'geiser-eval)
(require 'geiser-syntax)
(require 'geiser-base)

(require 'eldoc)


;;; Customization:

(defgroup geiser-autodoc nil
  "Options for displaying autodoc strings in the echo area."
  :group 'geiser)

(defface geiser-font-lock-autodoc-current-arg
  '((t (:background unspecified :foreground "red" :bold t)))
  "Face for highlighting current argument in autodoc messages."
  :group 'faces
  :group 'geiser-faces
  :group 'geiser-autodoc)

(defcustom geiser-autodoc-delay 0.2
  "Delay before autodoc messages are fetched and displayed, in seconds."
  :type 'number
  :group 'geiser-autodoc)


;;; Procedure arguments:

(make-variable-buffer-local
 (defvar geiser-autodoc--last nil))

(defun geiser-autodoc--function-args (fun)
  (if (eq fun (car geiser-autodoc--last))
      (cdr geiser-autodoc--last)
    (let* ((cmd `(:gs ((:ge proc-args) ',fun)))
           (result (geiser-eval--retort-result (geiser-eval--send/wait cmd))))
      (when (and (not (eq result :f)) (listp result))
        (setq geiser-autodoc--last (cons fun result))
        result))))

(defun geiser-autodoc--insert (sym current pos)
  (let ((str (format "%s" sym)))
    (when (= current pos)
      (put-text-property 0 (length str) 'face 'geiser-font-lock-autodoc-current-arg str))
    (insert str)))

(defun geiser-autodoc--fun-args-str (fun args pos)
  (save-current-buffer
    (set-buffer (geiser-syntax--font-lock-buffer))
    (erase-buffer)
    (let ((current 0))
      (insert "(")
      (geiser-autodoc--insert fun current pos)
      (dolist (arg (cdr (assoc 'required args)))
        (setq current (1+ current))
        (insert " ")
        (geiser-autodoc--insert arg current pos))
      (setq current (1+ current))
      (when (cdr (assoc 'optional args))
        (when (> pos current) (setq current pos))
        (insert " . ")
        (geiser-autodoc--insert (cdr (assoc 'optional args)) current pos))
      (insert ")")
      (buffer-string))))


;;; Autodoc function:

(defun geiser-autodoc--eldoc-function ()
  (let* ((f/a (geiser-syntax--enclosing-form-data))
         (fun (car f/a))
         (arg-no (cdr f/a)))
    (when fun
      (let ((args (geiser-autodoc--function-args fun)))
        (geiser-autodoc--fun-args-str fun args arg-no)))))


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
