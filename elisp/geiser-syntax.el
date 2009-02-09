;; geiser-syntax.el -- guile-specific scheme syntax

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Feb 08, 2009 15:03

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

;; Utilities for parsing Guile-specific Scheme syntax.

;;; Code:

(require 'geiser-base)

(require 'scheme)


;;; Modules:

(defconst geiser-syntax--module-definition-re
  "(define-module +\\(([^)]+)\\)")

(defun geiser-syntax--buffer-module (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward geiser-syntax--module-definition-re nil t)
            (match-string-no-properties 1)
          "#f")))))

;;; Indentation:

(defun geiser-syntax--setup-scheme-indent ()
  (let ((defuns '(catch)))
    (mapc (lambda (d) (put d 'scheme-indent-function 'defun)) defuns)))


;;; Code parsing:

(defun geiser-syntax--enclosing-form-data ()
  (save-excursion
    (let ((p (progn (ignore-errors
                      (unless (zerop (car (syntax-after (point))))
                        (forward-sexp)
                        (when (= 7 (car (syntax-after (point))))
                          (forward-char))))
                    (point)))
          (arg-no 0)
          (proc))
      (condition-case nil
          (progn (backward-up-list)
                 (forward-char)
                 (setq proc (symbol-at-point))
                 (while (< (point) p)
                   (forward-sexp)
                   (when (< (point) p) (setq arg-no (1+ arg-no))))
                 (cons proc arg-no))
        (error nil)))))


;;; Fontify strings as Scheme code:

(defun geiser-syntax--font-lock-buffer ()
  (let ((name " *geiser font lock*"))
    (or (get-buffer name)
        (let ((buffer (get-buffer-create name)))
          (set-buffer buffer)
          (scheme-mode)
          buffer))))

(defun geiser-syntax--scheme-str (str)
  (save-current-buffer
    (set-buffer (geiser-syntax--font-lock-buffer))
    (erase-buffer)
    (insert str)
    (let ((font-lock-verbose nil)) (font-lock-fontify-buffer))
    (buffer-string)))


(provide 'geiser-syntax)
;;; geiser-syntax.el ends here
