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
(require 'geiser-custom)
(require 'geiser-base)

(require 'eldoc)


;;; Customization:

(defgroup geiser-autodoc nil
  "Options for displaying autodoc strings in the echo area."
  :group 'geiser)

(geiser-custom--defface fuel-font-lock-markup-title
  'bold geiser-autodoc "highlighting current argument in autodoc messages")

(defcustom geiser-autodoc-delay 0.2
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
 (defvar geiser-autodoc--last nil))

(make-variable-buffer-local
 (defvar geiser-autodoc--last-funs nil))

(defun geiser-autodoc--function-args (funs)
  (when funs
    (let ((pr (and (eq (car geiser-autodoc--last) (caar funs)) (car funs))))
      (if pr (geiser-autodoc--fun-args-str (car pr)
                                           (cdr geiser-autodoc--last)
                                           (cdr pr))
        (setq geiser-autodoc--last-funs funs)
        (geiser-eval--send `(:gs ((:ge arguments) ,@(mapcar (lambda (f) (list 'quote (car f)))
                                                            funs)))
                           'geiser-autodoc--function-args-cont)
        ""))))

(defun geiser-autodoc--function-args-cont (ret)
  (let ((result (geiser-eval--retort-result ret)))
    (when (and result (listp result))
      (setq geiser-autodoc--last result)
      (eldoc-message
       (geiser-autodoc--fun-args-str (car result)
                                     (cdr result)
                                     (or (cdr (assoc (car result)
                                                     geiser-autodoc--last-funs))
                                         0))))))

(defun geiser-autodoc--insert (sym current pos)
  (let ((str (format "%s" sym)))
    (when (= current pos)
      (put-text-property 0 (length str)
                         'face 'geiser-font-lock-autodoc-current-arg
                         str))
    (insert str)))

(defun geiser-autodoc--fun-args-str (fun args pos)
  (save-current-buffer
    (set-buffer (geiser-syntax--font-lock-buffer))
    (erase-buffer)
    (let* ((current 0)
           (module (and geiser-autodoc-display-module-p
                        (cdr (assoc 'module args))))
           (fun (if module
                    (format geiser-autodoc-procedure-name-format module fun)
                  fun)))
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
  (geiser-autodoc--function-args (geiser-syntax--enclosing-form-data)))


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
