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

(geiser-custom--defface autodoc-current-arg
  'bold geiser-autodoc "highlighting current argument in autodoc messages")

(geiser-custom--defface autodoc-procedure-name
  'font-lock-function-name-face
  geiser-autodoc "highlighting procedure name in autodoc messages")

(geiser-custom--defface autodoc-optional-arg-marker
  'font-lock-keyword-face
  geiser-autodoc "highlighting #:opt marker in autodoc messages")

(geiser-custom--defface autodoc-key-arg-marker
  'font-lock-keyword-face
  geiser-autodoc "highlighting #:key marker in autodoc messages")

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
 (defvar geiser-autodoc--last nil))

(make-variable-buffer-local
 (defvar geiser-autodoc--last-result nil))

(defun geiser-autodoc--function-args (form)
  (if (equal (car geiser-autodoc--last) form) (cdr geiser-autodoc--last)
    (when form
      (let ((res (geiser-eval--send/result
                  `(:eval ((:ge autodoc) (quote (:scm ,form))))
                  500)))
        (when (and res (listp res))
          (unless (equalp res geiser-autodoc--last-result)
            (setq geiser-autodoc--last-result res)
            (setq geiser-autodoc--last
                  (cons form
                        (geiser-autodoc--str (cdr (assoc 'signature res))
                                             (or (cdr (assoc 'position res)) 0)
                                             (cdr (assoc 'module res))))))
          (cdr geiser-autodoc--last))))))

(defun geiser-autodoc--insert-arg (arg current pos)
  (let ((p (point))
        (str (format "%s" (if (eq arg 'geiser-rest_marker) "." arg)))
        (face (cond ((eq 'geiser-opt_marker arg)
                     'geiser-font-lock-autodoc-optional-arg-marker)
                    ((eq 'geiser-key_marker arg)
                     'geiser-font-lock-autodoc-key-arg-marker)
                    ((= current pos)
                     'geiser-font-lock-autodoc-current-arg)
                    (t nil))))
    (insert str)
    (when (listp arg)
      (save-excursion
        (replace-regexp "(quote \\(.*\\))" "'\\1" nil p (point))
        (replace-string "nil" "()" t p (point))))
    (when face (put-text-property p (point) 'face face))))

(defsubst geiser-autodoc--proc-name (proc module)
  (let ((str (if module
                 (format geiser-autodoc-procedure-name-format module proc)
               proc)))
    (put-text-property 0 (length str)
                       'face 'geiser-font-lock-autodoc-procedure-name
                       str)
    str))

(defun geiser-autodoc--str (signature pos module)
  (when (consp signature)
    (let* ((proc (car signature))
           (args (cdr signature))
           (len (if (listp args) (length args) 0))
           (current 1)
           (pos (if (> pos len) len pos)))
      (if (eq args 'variable)
          (geiser-autodoc--proc-name proc module)
        (save-current-buffer
          (set-buffer (geiser-syntax--font-lock-buffer))
          (erase-buffer)
          (insert (format "(%s" (geiser-autodoc--proc-name proc module)))
          (dolist (a args)
            (insert " ")
            (geiser-autodoc--insert-arg a current pos)
            (setq current (1+ current)))
          (insert ")")
          (buffer-string))))))


;;; Autodoc function:

(defun geiser-autodoc--eldoc-function ()
  (condition-case e
      (geiser-autodoc--function-args (geiser-syntax--get-partial-sexp))
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
