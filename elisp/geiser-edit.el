;; geiser-edit.el -- visiting source files

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Wed Feb 11, 2009 21:07

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

;; Functions to access Scheme files and spots.

;;; Code:

(require 'geiser-completion)
(require 'geiser-eval)
(require 'geiser-custom)
(require 'geiser-base)

(require 'etags)


;;; Customization

(defmacro geiser-edit--define-custom-visit (var group doc)
  `(defcustom ,var nil
     ,doc
     :group ',group
     :type '(choice (const :tag "Other window" window)
                    (const :tag "Other frame" frame)
                    (const :tag "Current window" nil))))

(geiser-edit--define-custom-visit
 geiser-edit-symbol-method geiser-mode
 "How the new buffer is opened when invoking \\[geiser-edit-symbol-at-point]")


;;; Auxiliar functions:

(defun geiser-edit--visit-file (file method)
  (cond ((eq method 'window) (find-file-other-window file))
        ((eq method 'frame) (find-file-other-frame file))
        (t (find-file file))))

(defsubst geiser-edit--location-file (loc)
  (cdr (assoc 'file loc)))

(defsubst geiser-edit--location-line (loc)
  (cdr (assoc 'line loc)))

(defconst geiser-edit--def-re
  (regexp-opt '("define"
                "defmacro"
                "define-macro"
                "define-syntax"
                "define*"
                "define-method"
                "define-class")))

(defsubst geiser-edit--def-re (thing)
  (format "(%s +(?%s\\_>" geiser-edit--def-re (regexp-quote (format "%s" thing))))

(defsubst geiser-edit--symbol-re (thing)
  (format "\\_<%s\\_>" (regexp-quote (format "%s" thing))))

(defun geiser-edit--goto-line (symbol line)
  (if (numberp line)
      (goto-line line)
    (goto-char (point-min))
    (when (or (re-search-forward (geiser-edit--def-re symbol) nil t)
              (re-search-forward (geiser-edit--symbol-re symbol) nil t))
      (goto-char (match-beginning 0)))))

(defun geiser-edit--try-edit (symbol ret)
  (let* ((loc (geiser-eval--retort-result ret))
         (file (geiser-edit--location-file loc))
         (line (geiser-edit--location-line loc)))
    (unless file (error "Couldn't find edit location for '%s'" symbol))
    (unless (file-readable-p file) (error "Couldn't open '%s' for read" file))
    (geiser-edit--visit-file file geiser-edit-symbol-method)
    (geiser-edit--goto-line symbol line)))


;;; Commands:

(defun geiser-edit-symbol ()
  "Asks for a symbol to edit, with completion."
  (interactive)
  (let* ((symbol (geiser-completion--read-symbol "Edit symbol: "
                                                nil
                                                geiser-edit--symbol-history))
         (cmd `(:eval ((:ge symbol-location) ',symbol))))
    (geiser-edit--try-edit symbol (geiser-eval--send/wait cmd))))

(defun geiser-edit-symbol-at-point (&optional arg)
  "Opens a new window visiting the definition of the symbol at point.
With prefix, asks for the symbol to edit."
  (interactive "P")
  (let* ((symbol (or (and (not arg) (symbol-at-point))
                     (geiser-completion--read-symbol "Edit symbol: ")))
         (cmd `(:eval ((:ge symbol-location) ',symbol)))
         (marker (point-marker)))
    (geiser-edit--try-edit symbol (geiser-eval--send/wait cmd))
    (when marker (ring-insert find-tag-marker-ring marker))))

(defun geiser-edit-pop-edit-symbol-stack ()
  "Pop back to where \\[geiser-edit-symbol-at-point] was last invoked."
  (interactive)
  (condition-case nil
      (pop-tag-mark)
    (error "No previous location for find symbol invocation")))

(defun geiser-edit-module (module)
  "Asks for a module and opens it in a new buffer."
  (interactive (list (geiser-completion--read-module)))
  (let ((cmd `(:eval ((:ge module-location) (quote (:scm ,module))))))
    (geiser-edit--try-edit module (geiser-eval--send/wait cmd))))


(provide 'geiser-edit)
;;; geiser-edit.el ends here
