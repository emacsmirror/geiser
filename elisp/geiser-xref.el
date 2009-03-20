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
  (let* ((location (cdr (assoc 'location xref)))
         (file (geiser-edit--location-file location))
         (signature (cdr (assoc 'signature xref)))
         (module (cdr (assoc 'module xref))))
    (when signature
      (insert "\t")
      (if (stringp file)
          (insert-text-button (format "%s" signature)
                              :type 'geiser-xref--button
                              'location location
                              'name (car signature)
                              'help-echo (format "%s in %s" (car signature) file))
        (insert (format "%s" signature)))
      (when (and (not (null module)) (not (eq '\#f module)))
        (insert (format " in module %s" module)))
      (newline))))

(defun geiser-xref--display-xrefs (header xrefs)
  (geiser-xref--with-buffer
   (erase-buffer)
   (geiser--insert-with-face header 'geiser-font-lock-xref-header)
   (newline 2)
   (mapc 'geiser-xref--insert-button xrefs))
  (geiser-xref--pop-to-buffer))

(defun geiser-xref--read-name (ask prompt)
  (let ((name (or (and (not prompt) (symbol-at-point))
                  (read-string prompt nil nil (symbol-at-point)))))
    (and name (format "%s" name))))

(defun geiser-xref--fetch-xrefs (ask kind rkind proc)
  (let* ((name (geiser-xref--read-name (format "%s: " (capitalize kind)) ask))
         (res (and name (geiser-eval--send/result
                         `(:eval ((:ge ,proc) (quote (:scm ,name))))))))
    (message "Retrieving %ss list for '%s'..." rkind name)
    (if (or (not res) (not (listp res)))
        (message "No %ss found for '%s'" rkind name)
      (message "")
      (geiser-xref--display-xrefs (format "%ss for %s" (capitalize rkind) name) res))))


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


;;; Commands:

(defun geiser-xref-generic-methods (&optional arg)
  "Display information about known methods of a given generic.
With prefix, ask for the name of the generic."
  (interactive "P")
  (geiser-xref--fetch-xrefs arg "generic" "method" 'generic-methods))

(defun geiser-xref-callers (&optional arg)
  "Display list of callers for procedure at point.
With prefix, ask for the procedure."
  (interactive "P")
  (geiser-xref--fetch-xrefs arg "procedure" "caller" 'callers))

(defun geiser-xref-callees (&optional arg)
  "Display list of callees for procedure at point.
With prefix, ask for the procedure."
  (interactive "P")
  (geiser-xref--fetch-xrefs arg "procedure" "callee" 'callees))


(provide 'geiser-xref)
;;; geiser-xref.el ends here
