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

(require 'geiser-popup)
(require 'geiser-base)

(require 'scheme)


;;; Indentation:

(defmacro geiser-syntax--scheme-indent (&rest pairs)
  `(progn ,@(mapcar (lambda (p)
                      `(put ',(car p) 'scheme-indent-function ',(cadr p)))
                    pairs)))

(geiser-syntax--scheme-indent
 (begin0 1)
 (c-declare 0)
 (c-lambda 2)
 (case-lambda 0)
 (catch defun)
 (class defun)
 (class* defun)
 (compound-unit/sig 0)
 (dynamic-wind 0)
 (for/fold 2)
 (instantiate 2)
 (interface 1)
 (lambda/kw 1)
 (let*-values 1)
 (let+ 1)
 (let-values 1)
 (let/ec 1)
 (match defun)
 (mixin 2)
 (module defun)
 (opt-lambda 1)
 (parameterize 1)
 (parameterize-break 1)
 (parameterize* 1)
 (pmatch defun)
 (quasisyntax/loc 1)
 (receive 2)
 (send* 1)
 (sigaction 1)
 (syntax-case 2)
 (syntax/loc 1)
 (type-case defun)
 (unit defun)
 (unit/sig 2)
 (unless 1)
 (when 1)
 (while 1)
 (with-handlers 1)
 (with-method 1)
 (with-syntax 1))


;;; Code parsing:

(geiser-popup--define syntax " *geiser syntax analyst*" scheme-mode)

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

(defsubst geiser-syntax--del-sexp (arg)
  (let ((p (point)))
    (forward-sexp arg)
    (delete-region p (point))))

(defconst geiser-syntax--placeholder (format "___%s___" (random 100)))

(defsubst geiser-syntax--beginning-of-form ()
  (memq (char-after (point)) '(?\" ?\()))

(defun geiser-syntax--complete-partial-sexp (buffer begin end)
  (geiser-syntax--with-buffer
    (erase-buffer)
    (insert-buffer-substring-no-properties buffer begin end)
    (when (not (geiser-syntax--beginning-of-form))
      (skip-syntax-backward "-<>")
      (delete-region (point) (point-max)))
    (let ((p (nth 8 (syntax-ppss))))
      (when p ;; inside a comment or string
        (delete-region p (point-max))
        (insert geiser-syntax--placeholder)))
    (when (cond ((eq (char-after (1- (point))) ?\)) (geiser-syntax--del-sexp -1) t)
                ((geiser-syntax--beginning-of-form)
                 (delete-region (point) (point-max)) t)
                ((memq (char-after (1- (point))) (list ?. ?@ ?, ?\' ?\` ?\# ?\\))
                 (skip-syntax-backward "^-(")
                 (delete-region (point) (point-max))
                 t))
      (insert geiser-syntax--placeholder))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[.@,'`#\\\\]" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "\\[" nil t)
        (replace-match "(" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "\\]" nil t)
        (replace-match ")" nil nil)))
    (let ((depth (nth 0 (parse-partial-sexp (point-min) (point)))))
      (unless (zerop depth) (insert (make-string depth ?\)))))
    (when (< (point-min) (point)) (buffer-substring (point-min) (point)))))

(defsubst geiser-syntax--get-partial-sexp ()
  (unless (zerop (nth 0 (syntax-ppss)))
    (let* ((end (if (geiser-syntax--beginning-of-form) (1+ (point))
                  (save-excursion (skip-syntax-forward "^-\"<>()") (point))))
           (begin (save-excursion (beginning-of-defun) (point))))
      (geiser-syntax--complete-partial-sexp (current-buffer) begin end))))


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
