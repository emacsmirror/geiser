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

(defsubst geiser-syntax--skip-comment/string ()
  (goto-char (or (nth 8 (syntax-ppss)) (point))))

(defsubst geiser-syntax--nesting-level ()
  (or (nth 0 (syntax-ppss)) 0))

(defun geiser-syntax--scan-sexp ()
  (let ((p (point))
        (n -1)
        prev head)
    (ignore-errors
      (backward-up-list)
      (save-excursion
        (forward-char)
        (skip-syntax-forward "^_w(" p)
        (when (setq head (symbol-at-point))
          (while (< (point) p)
            (setq n (1+ n))
            (setq prev (symbol-at-point))
            (forward-sexp))))
      (if head (list head n (and (> n 1) prev)) 'skip))))

(defun geiser-syntax--scan-sexps ()
  (save-excursion
    (geiser-syntax--skip-comment/string)
    (let* ((sap (symbol-at-point))
           (fst (and sap (geiser-syntax--scan-sexp)))
           (path (and fst
                      (cond ((not (listp fst)) `((,sap 0)))
                             ((eq sap (car fst)) (list fst))
                             (t (list fst (list sap 0)))))))
      (while (setq fst (geiser-syntax--scan-sexp))
        (when (listp fst) (push fst path)))
      (nreverse path))))

(defun geiser-syntax--read-list (p)
  (let ((list (ignore-errors (read (current-buffer)))))
    (if (and list (< (point) p))
        list
      (goto-char p)
      nil)))

(defconst geiser-syntax--delim-regexp "\\(?:[\s-\s<\s>$\n]+\\)")

(defconst geiser-syntax--ident-regexp
  (format "\\(?:%s\\([^ (]+?\\)\\)" geiser-syntax--delim-regexp))

(defconst geiser-syntax--let-regexp
  (format "\\=(let\\(?:\\*\\|rec\\|%s\\|%s\\)%s*("
          geiser-syntax--ident-regexp
          geiser-syntax--delim-regexp
          geiser-syntax--delim-regexp))

(defconst geiser-syntax--ldefine-regexp
  (format "\\=(define%s%s" geiser-syntax--ident-regexp geiser-syntax--delim-regexp))

(defconst geiser-syntax--define-regexp
  (format "\\=(\\(?:define\\|lambda\\)%s(" geiser-syntax--delim-regexp))

(defun geiser-syntax--locals-around-point ()
  (when (eq major-mode 'scheme-mode)
    (save-excursion
      (geiser-syntax--skip-comment/string)
      (let ((ids))
        (while (not (zerop (geiser-syntax--nesting-level)))
          (let ((p (point)))
            (backward-up-list)
            (save-excursion
              (while (< (point) p)
                (cond ((re-search-forward geiser-syntax--let-regexp p t)
                       (when (match-string 1) (push (intern (match-string 1)) ids))
                       (backward-char 1)
                       (dolist (l (nreverse (geiser-syntax--read-list p)))
                         (when (and (listp l) (symbolp (car l)))
                           (push (car l) ids))))
                      ((re-search-forward geiser-syntax--ldefine-regexp p t)
                       (when (match-string 1) (push (intern (match-string 1)) ids)))
                      ((re-search-forward geiser-syntax--define-regexp p t)
                       (backward-char 1)
                       (dolist (s (nreverse (geiser-syntax--read-list p)))
                         (let ((sn (if (listp s) (car s) s)))
                           (when (symbolp sn) (push sn ids)))))
                      (t (goto-char (1+ p))))))))
        (nreverse ids)))))


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
