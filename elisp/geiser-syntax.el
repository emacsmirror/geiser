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
        (when (re-search-forward geiser-syntax--module-definition-re nil t)
          (car (read-from-string (match-string-no-properties 1))))))))

;;; Indentation:

(defun geiser-syntax--setup-scheme-indent ()
  (let ((defuns '(catch)))
    (mapc (lambda (d) (put d 'scheme-indent-function 'defun)) defuns)))

(geiser-syntax--setup-scheme-indent)


;;; Code parsing:

(geiser-popup--define syntax " *geiser syntax analyst*" scheme-mode)

(defun geiser-syntax--complete-partial-sexp (buffer begin end)
  (set-buffer buffer)
  (let ((inhibit-read-only t))
    (copy-to-buffer (geiser-syntax--buffer) begin end))
  (geiser-syntax--with-buffer
    (goto-char (point-max))
    (skip-syntax-backward "-")
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (cond ((nth 8 pps) ;; inside a comment or string
             (delete-region (nth 8 pps) (point-max)))
            ((nth 5 pps) (delete-char -1))) ;; after a quote
      (let ((depth (nth 0 pps)))
        (unless (zerop depth) (insert (make-string depth ?\)))))
      (geiser-syntax--prepare-scheme-for-elisp-reader)
      (read (current-buffer)))))

(defsubst geiser-syntax--get-partial-sexp ()
  (save-excursion
    (let* ((begin (point))
           (end (progn (beginning-of-defun) (point))))
      (geiser-syntax--complete-partial-sexp (current-buffer) begin end))))

(defun geiser-syntax--collect-local-symbols (sexp acc)
  (cond ((or (null sexp) (not (listp sexp))) acc)
        ((listp (car sexp))
         (geiser-syntax--collect-local-symbols
          (cdr sexp)
          (geiser-syntax--collect-local-symbols (car sexp) acc)))
        ((memq (car sexp) '(define define*))
         (let* ((name (cadr sexp))
                (name (if (symbolp name) name (car name)))
                (acc (if (symbolp name) (cons name acc) acc)))
           (geiser-syntax--collect-local-symbols (cddr sexp) acc)))
        ((memq (car sexp) '(let let* letrec))
         (let* ((n (if (listp (nth 1 sexp)) 1 2))
                (syms (mapcar 'car (nth n sexp)))
                (rest (if (= 1 n) (cddr sexp) (cdr (cddr sexp)))))
           (geiser-syntax--collect-local-symbols rest (append syms acc))))
        (t (geiser-syntax--collect-local-symbols (cdr sexp) acc))))

(defsubst geiser-syntax--local-bindings ()
  (ignore-errors
    (mapcar 'symbol-name
            (geiser-syntax--collect-local-symbols (geiser-syntax--get-partial-sexp) '()))))

(defsubst geiser-syntax--end-of-thing ()
  (let ((sc (or (syntax-class (syntax-after (point))) 0)))
    (when (= sc 7) (forward-char))
    (cond ((nth 3 (syntax-ppss))
           (skip-syntax-forward "^\"")
           (forward-char))
          ((and (= sc 5) (eq ?\( (char-before))) (forward-char))
          ((not (or (= sc 0) (= sc 12))) ;; comment, whitespace
           (ignore-errors (forward-sexp))))
    (point)))

(defun geiser-syntax--enclosing-form-data ()
  (save-excursion
    (let* ((p (geiser-syntax--end-of-thing))
           (current (cons (symbol-at-point) 0))
           (data))
      (ignore-errors
        (while (not (bobp))
          (backward-up-list)
          (save-excursion
            (forward-char)
            (let ((proc (symbol-at-point))
                  (arg-no 0))
              (when proc
                (while (< (point) p)
                  (forward-sexp)
                  (when (< (point) p) (setq arg-no (1+ arg-no))))
                (push (cons proc arg-no) data))))))
      (reverse (if (car current) (push current data) data)))))

(defun geiser-syntax--prepare-scheme-for-elisp-reader ()
  (goto-char (point-min))
  (while (re-search-forward "#\<\\([^>]*?\\)\>" nil t)
    (let ((from (match-beginning 1))
          (to (match-end 1)))
      (goto-char from)
      (while (re-search-forward "\\([() ;'`]\\)" to t)
        (replace-match "\\\\\\1"))
      (goto-char to)))
  (goto-char (point-min))
  (while (re-search-forward "#(" nil t) (replace-match "(vector "))
  (goto-char (point-min))
  (while (re-search-forward "#" nil t) (replace-match "\\\\#"))
  (goto-char (point-min))
  (skip-syntax-forward "^("))


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
