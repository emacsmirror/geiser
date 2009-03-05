;; geiser-xref.el -- utilities for cross-referencing

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Thu Mar 05, 2009 23:03

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

;; Obtaining cross-reference information (generic's methods, callers, etc.)

;;; Code:

(require' geiser-edit)
(require 'geiser-eval)
(require 'geiser-popup)
(require 'geiser-custom)
(require 'geiser-base)

(require 'button)


;;; Customization:
(defgroup geiser-xref nil
  "Options for cross-referencing commands."
  :group 'geiser)

(geiser-edit--define-custom-visit
 geiser-xref-follow-link-method geiser-xref
 "How to visit buffers when following xrefs.")

(geiser-custom--defface xref-link
  'link geiser-xref "links in cross-reference buffers")

(geiser-custom--defface xref-header
  'bold geiser-xref "headers in cross-reference buffers")


;;; Ref button:

(define-button-type 'geiser-xref--button
  'action 'geiser-xref--button-action
  'face 'geiser-font-lock-xref-link
  'follow-link t)

(defun geiser-xref--button-action (button)
  (let ((location (button-get button 'location))
        (name (button-get button 'name)))
    (when location
      (geiser-edit--try-edit-location name location geiser-xref-follow-link-method))))

(defun geiser-xref--insert-button (xref)
  (let ((location (cdr (assoc 'location xref)))
        (signature (cdr (assoc 'signature xref))))
    (when signature
      (insert "\t")
      (if location
          (insert-text-button (format "%s" signature)
                              :type 'geiser-xref--button
                              'location location
                              'name (car signature)
                              'help-echo (format "%s in %s"
                                                 (car signature)
                                                 (geiser-edit--location-file location)))
        (insert (format "%s" signature)))
      (newline))))


;;; Buffer and mode:

(geiser-popup--define xref "*Geiser xref*" geiser-xref-mode)

(defvar geiser-xref-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map button-buffer-map)
    map))

(defun geiser-xref-mode ()
  "Major mode for displaying cross-references.
\\{geiser-xref-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map geiser-xref-mode-map)
  (set-syntax-table scheme-mode-syntax-table)
  (setq mode-name "Geiser Xref")
  (setq major-mode 'geiser-xref-mode)
  (setq buffer-read-only t))


;;; Generic's methods:

(defun geiser-xref--display-generic-methods (generic res)
  (geiser-xref--with-buffer
   (erase-buffer)
   (geiser--insert-with-face (format "Methods for generic '%s'" generic)
                             'geiser-font-lock-xref-header)
   (newline 2)
   (mapc 'geiser-xref--insert-button res))
  (geiser-xref--pop-to-buffer))

(defun geiser-xref-generic-methods (&optional arg)
  "Display information about known methods of a given generic.
With prefix, ask for the name of the generic."
  (interactive "P")
  (let* ((name (or (and (not arg) (symbol-at-point))
                   (read-string "Generic: " nil nil (symbol-at-point))))
         (name (and name (format "%s" name)))
         (res (geiser-eval--send/result
               `(:eval ((:ge generic-methods) (quote (:scm ,name)))))))
    (if (or (not res) (not (listp res)))
        (message "No methods found for '%s'" name)
      (geiser-xref--display-generic-methods name res))))



(provide 'geiser-xref)
;;; geiser-xref.el ends here
