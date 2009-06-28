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
 "How the new buffer is opened when invoking \\[geiser-edit-symbol-at-point]
or following links in error buffers.")

(geiser-custom--defface error-link
  'link geiser-debug "links in error buffers")


;;; Auxiliar functions:

(defun geiser-edit--visit-file (file method)
  (cond ((eq method 'window) (find-file-other-window file))
        ((eq method 'frame) (find-file-other-frame file))
        (t (find-file file))))

(defsubst geiser-edit--location-name (loc)
  (cdr (assoc 'name loc)))

(defsubst geiser-edit--location-file (loc)
  (cdr (assoc 'file loc)))

(defsubst geiser-edit--to-number (x)
  (cond ((numberp x) x)
        ((stringp x) (string-to-number x))))

(defsubst geiser-edit--location-line (loc)
  (geiser-edit--to-number (cdr (assoc 'line loc))))

(defsubst geiser-edit--location-column (loc)
  (geiser-edit--to-number (cdr (assoc 'column loc))))

(defsubst geiser-edit--make-location (name file line column)
  `((name . ,name) (file . ,file) (line . ,line) (column . ,column)))

(defconst geiser-edit--def-re
  (regexp-opt '("define"
                "defmacro"
                "define-macro"
                "define-syntax"
                "-define-syntax"
                "-define"
                "define*"
                "define-method"
                "define-class"
                "define-struct")))

(defconst geiser-edit--def-re*
  (regexp-opt '("define-syntaxes" "define-values")))

(defsubst geiser-edit--def-re (thing)
  (format "(%s +(?%s\\_>"
          geiser-edit--def-re
          (regexp-quote (format "%s" thing))))

(defsubst geiser-edit--def-re* (thing)
  (format "(%s +([^)]*?\\_<%s\\_>"
          geiser-edit--def-re*
          (regexp-quote (format "%s" thing))))

(defsubst geiser-edit--symbol-re (thing)
  (format "\\_<%s\\_>" (regexp-quote (format "%s" thing))))

(defun geiser-edit--goto-line (symbol line)
  (if (numberp line)
      (goto-line line)
    (goto-char (point-min))
    (when (or (re-search-forward (geiser-edit--def-re symbol) nil t)
              (re-search-forward (geiser-edit--def-re* symbol) nil t)
              (re-search-forward (geiser-edit--symbol-re symbol) nil t))
      (goto-char (match-beginning 0)))))

(defun geiser-edit--try-edit-location (symbol loc &optional method)
  (let ((symbol (or (geiser-edit--location-name loc) symbol))
        (file (geiser-edit--location-file loc))
        (line (geiser-edit--location-line loc))
        (col (geiser-edit--location-column loc)))
    (unless file (error "Couldn't find edit location for '%s'" symbol))
    (unless (file-readable-p file) (error "Couldn't open '%s' for read" file))
    (geiser-edit--visit-file file (or method geiser-edit-symbol-method))
    (geiser-edit--goto-line symbol line)
    (when col
      (beginning-of-line)
      (forward-char col))))

(defsubst geiser-edit--try-edit (symbol ret)
  (geiser-edit--try-edit-location symbol (geiser-eval--retort-result ret)))


;;; Links

(define-button-type 'geiser-edit--button
  'action 'geiser-edit--button-action
  'face 'geiser-font-lock-error-link
  'follow-link t)

(defun geiser-edit--button-action (button)
  (let ((loc (button-get button 'geiser-location)))
    (when loc (geiser-edit--try-edit-location nil loc))))

(defun geiser-edit--make-link (beg end file line col)
  (make-button beg end
               :type 'geiser-edit--button
               'geiser-location
               (geiser-edit--make-location 'error file line col)
               'help-echo "Go to error location"))


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
  (let ((cmd `(:eval ((:ge module-location) (:module ,module)))))
    (geiser-edit--try-edit module (geiser-eval--send/wait cmd))))


(provide 'geiser-edit)
;;; geiser-edit.el ends here
