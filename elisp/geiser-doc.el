;; geiser-doc.el -- accessing documentation

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Feb 14, 2009 14:09

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

;; Utilities for accessing docstrings and texinfo documentation.

;;; Code:

(require 'geiser-completion)
(require 'geiser-eval)
(require 'geiser-syntax)
(require 'geiser-popup)
(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-doc nil
  "Options for documentation buffers."
  :group 'geiser)

(geiser-custom--defface doc-title
  'bold geiser-doc "article titles in documentation buffers")

(geiser-custom--defface doc-link
  'link geiser-doc "links in documentation buffers")


;;; Documentation buffer:

(geiser-popup--define doc "*Geiser documentation*" fundamental-mode)


;;; Docstrings:

(defun geiser-doc--get-docstring (symbol)
  (geiser-eval--send/result `(:eval ((:ge docstring) ',symbol))))

(defun geiser-doc--get-module-children (module)
  (geiser-eval--send/result `(:eval ((:ge module-children) (quote (:scm ,module))))))


;;; Auxiliary functions:

(defun geiser-doc--insert-title (title)
  (let ((p (point)))
    (insert title)
    (put-text-property p (point) 'face 'geiser-font-lock-doc-title))
  (newline))

(defun geiser-doc--insert-list (title lst)
  (when lst
    (geiser-doc--insert-title title)
    (newline)
    (dolist (w lst)
      (insert (format "\t- %s\n" w)))
    (newline)))


;;; Commands:

(defun geiser-doc-symbol-at-point (&optional arg)
  "Get docstring for symbol at point.
With prefix argument, ask for symbol (with completion)."
  (interactive "P")
  (let ((symbol (or (and (not arg) (symbol-at-point))
                    (geiser-completion--read-symbol "Symbol: " (symbol-at-point)))))
    (when symbol
      (let ((ds (geiser-doc--get-docstring symbol)))
        (if (or (not (stringp ds)) (zerop (length ds)))
            (message "No documentation available for '%s'" symbol)
          (geiser-doc--with-buffer
            (erase-buffer)
            (insert ds))
          (geiser-doc--pop-to-buffer))))))

(defun geiser-doc-module (module)
  "Display information about a given module."
  (interactive (list (geiser-completion--read-module)))
  (let ((children (geiser-doc--get-module-children module)))
    (if (not children)
        (message "No info available for %s" module)
      (geiser-doc--with-buffer
        (erase-buffer)
        (geiser-doc--insert-title (format "%s" module))
        (newline)
        (geiser-doc--insert-list "Procedures:" (cdr (assoc 'procs children)))
        (geiser-doc--insert-list "Variables:" (cdr (assoc 'vars children)))
        (geiser-doc--insert-list "Submodules:" (cdr (assoc 'modules children)))
        (goto-char (point-min)))
      (geiser-doc--pop-to-buffer))))



(provide 'geiser-doc)
;;; geiser-doc.el ends here
