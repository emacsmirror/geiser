;; geiser-debug.el -- displaying debug information

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Feb 23, 2009 22:34

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

;; Buffer and associated mode for displaying results of evaluations
;; and compilations.

;;; Code:

(require 'geiser-eval)
(require 'geiser-popup)
(require 'geiser-base)


;;; Debug buffer mode:

(defvar geiser-debug-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "g" 'geiser-debug-goto-error)
    (define-key map "\C-c\C-c" 'geiser-debug-goto-error)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map))

(define-derived-mode geiser-debug-mode compilation-mode "Geiser Dbg"
  "A major mode for displaying Scheme compilation and evaluation results.
\\{geiser-debug-mode-map}")


;;; Buffer for displaying evaluation results:

(geiser-popup--define debug "*Geiser dbg*" geiser-debug-mode)


;;; Displaying retorts

(defun geiser-debug--display-retort (what ret)
  (let* ((err (geiser-eval--retort-error ret))
         (output (geiser-eval--retort-output ret))
         (stack (geiser-eval--retort-stack ret))
         (step 2)
         (indent step))
    (when err
      (geiser-debug--with-buffer
        (erase-buffer)
        (insert what)
        (newline 2)
        (insert (geiser-eval--error-str err) "\n\n")
        (when output (insert output "\n\n"))
        (dolist (f (reverse (cdr stack)))
          (geiser-debug--display-stack-frame f indent)
          (setq indent (+ step indent)))
        (goto-char (point-min)))
      (geiser-debug--pop-to-buffer))))

(defsubst geiser-debug--frame-proc (frame) (cdr (assoc 'procedure frame)))
(defsubst geiser-debug--frame-desc (frame) (cdr (assoc 'description frame)))
(defsubst geiser-debug--frame-source (frame) (cdr (assoc 'source frame)))
(defsubst geiser-debug--frame-source-file (src) (car src))
(defsubst geiser-debug--frame-source-line (src) (or (cadr src) 1))
(defsubst geiser-debug--frame-source-column (src) (or (caddr src) 0))

(defun geiser-debug--display-stack-frame (frame offset)
  (let ((procedure (geiser-debug--frame-proc frame))
        (source (geiser-debug--frame-source frame))
        (description (geiser-debug--frame-desc frame)))
    (if source
        (insert (format "%s:%s:%s\n"
                        (geiser-debug--frame-source-file source)
                        (geiser-debug--frame-source-line source)
                        (geiser-debug--frame-source-column source)))
      (insert "In expression:\n"))
    (insert (format "%s%s\n" (make-string offset ?\ ) description))))


(provide 'geiser-debug)
;;; geiser-debug.el ends here
