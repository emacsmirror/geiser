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

(defconst geiser-debug--error-alist
  '(("^\\(In file +\\| +\\)\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4)
    ("^Error.+$" nil nil nil 0)))

(define-derived-mode geiser-debug-mode compilation-mode "Geiser Dbg"
  "A major mode for displaying Scheme compilation and evaluation results.
\\{geiser-debug-mode-map}"
  (set (make-local-variable 'compilation-error-regexp-alist)
       geiser-debug--error-alist))


;;; Buffer for displaying evaluation results:

(geiser-popup--define debug "*Geiser dbg*" geiser-debug-mode)


;;; Displaying retorts

(defun geiser-debug--display-retort (what ret)
  (let* ((err (geiser-eval--retort-error ret))
         (output (geiser-eval--retort-output ret))
         (stack (geiser-eval--retort-stack ret)))
    (geiser-debug--with-buffer
      (erase-buffer)
      (insert what)
      (newline 2)
      (when err (insert (geiser-eval--error-str err) "\n\n"))
      (when output (insert output "\n\n"))
      (when stack (geiser-debug--display-stack stack))
      (goto-char (point-min)))
    (when err (geiser-debug--pop-to-buffer))))

(defsubst geiser-debug--frame-proc (frame) (cdr (assoc 'procedure frame)))
(defsubst geiser-debug--frame-desc (frame) (cdr (assoc 'description frame)))
(defsubst geiser-debug--frame-source (frame) (cdr (assoc 'source frame)))
(defsubst geiser-debug--frame-source-file (src) (car src))
(defsubst geiser-debug--frame-source-line (src) (or (cadr src) 1))
(defsubst geiser-debug--frame-source-column (src) (or (caddr src) 0))

(defun geiser-debug--display-stack (stack)
  (let* ((frames (cdr stack))
         (step 2)
         (indent (* (length frames) step)))
    (dolist (f frames)
      (geiser-debug--display-stack-frame f indent)
      (setq indent (- indent step)))))

(defun geiser-debug--display-stack-frame (frame offset)
  (let ((procedure (geiser-debug--frame-proc frame))
        (source (geiser-debug--frame-source frame))
        (description (geiser-debug--frame-desc frame)))
    (if source
        (insert (format "In file %s:%s:%s\n"
                        (geiser-debug--frame-source-file source)
                        (geiser-debug--frame-source-line source)
                        (1+ (geiser-debug--frame-source-column source))))
      (insert "In expression:\n"))
    (insert (format "%s%s\n" (make-string offset ?\ ) description))))

(defsubst geiser-debug--wrap-region (str)
  (format "(begin %s)" str))

(defun geiser-debug--unwrap (str)
  (if (string-match "(begin[ \t\n\v\r]+\\(.+\\)*)" str)
      (match-string 1 str)
    str))

(defun geiser-debug--send-region (compile start end and-go wrap)
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code `(,(if compile :comp :eval) (:scm ,wrapped)))
         (ret (geiser-eval--send/wait code))
         (err (geiser-eval--retort-error ret)))
    (when and-go (funcall and-go))
    (when (not err) (message (format "=> %s" (geiser-eval--retort-result ret))))
    (geiser-debug--display-retort str ret)))

(defun geiser-debug--expand-region (start end all wrap)
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code `(:eval ((:ge macroexpand) (quote (:scm ,wrapped)) ,(if all :t :f))))
         (ret (geiser-eval--send/wait code))
         (err (geiser-eval--retort-error ret))
         (result (geiser-eval--retort-result ret)))
    (if err
        (geiser-debug--display-retort str ret)
      (geiser-debug--with-buffer
        (erase-buffer)
        (insert (format "%s" (if wrap (geiser-debug--unwrap result) result)))
        (goto-char (point-min)))
      (geiser-debug--pop-to-buffer))))


(provide 'geiser-debug)
;;; geiser-debug.el ends here
