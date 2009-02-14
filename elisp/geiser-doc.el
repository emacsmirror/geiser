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
(require 'geiser-popup)
(require 'geiser-base)


;;; Documentation buffer:

(geiser-popup--define doc "*Geiser documentation*" fundamental-mode)


;;; Docstrings:

(defun geiser-doc--get-docstring (symbol)
  (geiser-eval--send/result `(:gs ((:ge docstring) ',symbol))))


;;; Commands:

(defun geiser-doc-symbol-at-point (&optional arg)
  "Get docstring for symbol at point.
With prefix argument, ask for symbol (with completion)."
  (interactive "P")
  (let ((symbol (or (and (not arg) (symbol-at-point))
                    (geiser-completion--read-symbol "Symbol: " (symbol-at-point)))))
    (when symbol
      (let ((ds (geiser-doc--get-docstring symbol)))
        (if (not (stringp ds))
            (message "No docstring available for '%s'" symbol)
          (geiser-doc--with-buffer
            (erase-buffer)
            (insert ds))
          (geiser-doc--pop-to-buffer))))))


(provide 'geiser-doc)
;;; geiser-doc.el ends here
