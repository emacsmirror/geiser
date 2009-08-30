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


;;; A simple scheme reader

(defvar geiser-syntax--read/buffer-limit nil)

(defsubst geiser-syntax--read/eos ()
  (or (eobp)
      (and (numberp geiser-syntax--read/buffer-limit)
           (<= geiser-syntax--read/buffer-limit (point)))))

(defsubst geiser-syntax--read/next-char ()
  (unless (geiser-syntax--read/eos)
    (forward-char)
    (char-after)))

(defsubst geiser-syntax--read/token (token)
  (geiser-syntax--read/next-char)
  (if (listp token) token (list token)))

(defsubst geiser-syntax--read/elisp ()
  (ignore-errors (read (current-buffer))))

(defun geiser-syntax--read/next-token ()
  (skip-syntax-forward "->")
  (if (geiser-syntax--read/eos) '(eob)
    (let ((c (char-after)))
      (cond ((not c) '(eob))
            ((eq c '\;)
             (skip-syntax-forward "^>")
             (geiser-syntax--read/next-token))
            ((memq c '(?\( ?\[)) (geiser-syntax--read/token 'lparen))
            ((memq c '(?\) ?\])) (geiser-syntax--read/token 'rparen))
            ((eq c ?.)
             (if (memq (syntax-after (1+ (point))) '(0 11 12))
                 (geiser-syntax--read/token 'dot)
               (cons 'atom (geiser-syntax--read/elisp))))
            ((eq c ?\#)
             (let ((c (geiser-syntax--read/next-char)))
               (cond ((not c) '(eob))
                     ((eq c ?\\) (cons 'char (geiser-syntax--read/elisp)))
                     ((eq c ?\() (geiser-syntax--read/token 'vectorb))
                     (t (geiser-syntax--read/next-token)))))
            ((eq c ?\') (geiser-syntax--read/token '(quote . quote)))
            ((eq c ?\`) (geiser-syntax--read/token
                         `(backquote . ,backquote-backquote-symbol)))
            ((eq c ?,) (if (eq (geiser-syntax--read/next-char) ?@)
                           (geiser-syntax--read/token
                            `(splice . ,backquote-splice-symbol))
                         `(unquote . ,backquote-unquote-symbol)))
            ((eq c ?\") (cons 'string (geiser-syntax--read/elisp)))
            (t (cons 'atom (geiser-syntax--read/elisp)))))))

(defsubst geiser-syntax--read/match (&rest tks)
  (let ((token (geiser-syntax--read/next-token)))
    (if (memq (car token) tks) token
      (error "Unexpected token: %s" token))))

(defsubst geiser-syntax--read/try (&rest tks)
  (let ((p (point))
        (tk (ignore-errors (apply 'geiser-syntax--read/match tks))))
    (unless tk (goto-char p))
    tk))

(defun geiser-syntax--read/list ()
  (cond ((geiser-syntax--read/try 'dot)
         (let ((tail (geiser-syntax--read)))
           (geiser-syntax--read/match 'eob 'rparen)
           tail))
        ((geiser-syntax--read/try 'rparen 'eob) nil)
        (t (cons (geiser-syntax--read)
                 (geiser-syntax--read/list)))))

(defun geiser-syntax--read ()
  (let ((token (geiser-syntax--read/next-token)))
    (case (car token)
      (eob nil)
      (lparen (geiser-syntax--read/list))
      (vectorb (apply 'vector (geiser-syntax--read/list)))
      ((quote backquote unquote splice) (list (cdr token) (geiser-syntax--read)))
      ((char string atom) (cdr token))
      (t (error "Reading scheme syntax: unexpected token: %s" token)))))


;;; Code parsing:

(defsubst geiser-syntax--skip-comment/string ()
  (goto-char (or (nth 8 (syntax-ppss)) (point))))

(defsubst geiser-syntax--nesting-level ()
  (or (nth 0 (syntax-ppss)) 0))

(defun geiser-syntax--scan-sexps ()
  (save-excursion
    (let* ((fst (symbol-at-point))
           (path (and fst (list (list fst 0)))))
      (while (not (zerop (geiser-syntax--nesting-level)))
        (let ((geiser-syntax--read/buffer-limit (1+ (point))))
          (backward-up-list)
          (let ((form (save-excursion (geiser-syntax--read))))
            (when (and (listp form) (car form) (symbolp (car form)))
              (let* ((len-1 (1- (length form)))
                     (prev (and (> len-1 1) (nth (1- len-1) form))))
                (push `(,(car form) ,len-1 ,@(and prev (symbolp prev) (list prev)))
                      path))))))
      (nreverse path))))

(defun geiser-syntax--locals-around-point ()
  (when (eq major-mode 'scheme-mode)
    (save-excursion
      (geiser-syntax--skip-comment/string)
      (let* ((ids)
             (push-id (lambda (n) (when (symbolp n) (push n ids))))
             (get-arg (lambda (n) (if (listp n) (car n) n)))
             (push-ids (lambda (is) (mapc push-id (nreverse (mapcar get-arg is))))))
        (while (not (zerop (geiser-syntax--nesting-level)))
          (let ((geiser-syntax--read/buffer-limit (point)))
            (backward-up-list)
            (let* ((form (save-excursion (geiser-syntax--read)))
                   (head (and (listp form) (car form)))
                   (snd (and head (cadr form)))
                   (third (and head (caddr form)))
                   (is (case head
                         ((define define*) (if (listp snd) snd (list snd)))
                         ((let* letrec lambda let)
                          (if (listp snd) snd
                            (cons snd (and (eq head 'let)
                                           (listp third)
                                           third))))))
                   (body (and is (case head
                                   ((define define*) (and (listp snd) (cddr form)))
                                   ((let let* letrec lambda)
                                    (if (listp snd) (cddr form)
                                      (cdddr form)))))))
              (when is
                (funcall push-ids
                         (mapcar 'cdr
                                 (remove-if (lambda (f) (or (not (listp f))
                                                       (not (eq (car f) 'define))))
                                            body)))
                (funcall push-ids is)))))
        (nreverse ids)))))


;;; Fontify strings as Scheme code:

(geiser-popup--define syntax " *geiser syntax analyst*" scheme-mode)

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
