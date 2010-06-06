;;; geiser-syntax.el -- utilities for parsing scheme syntax

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Feb 08, 2009 15:03



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
      (and geiser-syntax--read/buffer-limit
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

(defun geiser-syntax--read/matching (open close)
  (let ((count 1)
        (p (1+ (point))))
    (while (and (> count 0)
                (geiser-syntax--read/next-char))
      (cond ((looking-at-p open) (setq count (1+ count)))
            ((looking-at-p close) (setq count (1- count)))))
    (buffer-substring-no-properties p (point))))

(defsubst geiser-syntax--read/unprintable ()
  (geiser-syntax--read/token
   (cons 'unprintable (geiser-syntax--read/matching "<" ">"))))

(defun geiser-syntax--read/skip-comment ()
  (while (and (geiser-syntax--read/next-char)
              (nth 8 (syntax-ppss))))
  (geiser-syntax--read/next-token))

(defun geiser-syntax--read/next-token ()
  (skip-syntax-forward "->")
  (if (geiser-syntax--read/eos) '(eob)
    (case (char-after)
      (?\; (geiser-syntax--read/skip-comment))
      ((?\( ?\[) (geiser-syntax--read/token 'lparen))
      ((?\) ?\]) (geiser-syntax--read/token 'rparen))
      (?. (if (memq (car (syntax-after (1+ (point)))) '(0 11 12))
              (geiser-syntax--read/token 'dot)
            (cons 'atom (geiser-syntax--read/elisp))))
      (?\# (case (geiser-syntax--read/next-char)
             ('nil '(eob))
             (?| (geiser-syntax--read/skip-comment))
             (?: (if (geiser-syntax--read/next-char)
                     (cons 'kwd (geiser-syntax--read/elisp))
                   '(eob)))
             (?\\ (cons 'char (geiser-syntax--read/elisp)))
             (?\( (geiser-syntax--read/token 'vectorb))
             (?\< (geiser-syntax--read/unprintable))
             ((?' ?` ?,) (geiser-syntax--read/next-token))
             (t (let ((tok (geiser-syntax--read/elisp)))
                  (if tok (cons 'atom (intern (format "#%s" tok)))
                    (geiser-syntax--read/next-token))))))
      (?\' (geiser-syntax--read/token '(quote . quote)))
      (?\` (geiser-syntax--read/token
            `(backquote . ,backquote-backquote-symbol)))
      (?, (if (eq (geiser-syntax--read/next-char) ?@)
              (geiser-syntax--read/token
               `(splice . ,backquote-splice-symbol))
            `(unquote . ,backquote-unquote-symbol)))
      (?\" (cons 'string (geiser-syntax--read/elisp)))
      (t (cons 'atom (geiser-syntax--read/elisp))))))

(defsubst geiser-syntax--read/match (&rest tks)
  (let ((token (geiser-syntax--read/next-token)))
    (if (memq (car token) tks) token
      (error "Unexpected token: %s" token))))

(defsubst geiser-syntax--read/skip-until (&rest tks)
  (let (token)
    (while (and (not (memq (car token) tks))
                (not (eq (car token) 'eob)))
      (setq token (geiser-syntax--read/next-token)))
    token))

(defsubst geiser-syntax--read/try (&rest tks)
  (let ((p (point))
        (tk (ignore-errors (apply 'geiser-syntax--read/match tks))))
    (unless tk (goto-char p))
    tk))

(defun geiser-syntax--read/list ()
  (cond ((geiser-syntax--read/try 'dot)
         (let ((tail (geiser-syntax--read)))
           (geiser-syntax--read/skip-until 'eob 'rparen)
           tail))
        ((geiser-syntax--read/try 'rparen 'eob) nil)
        (t (cons (geiser-syntax--read)
                 (geiser-syntax--read/list)))))

(defun geiser-syntax--read ()
  (let ((token (geiser-syntax--read/next-token))
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000)))
    (case (car token)
      (eob nil)
      (lparen (geiser-syntax--read/list))
      (vectorb (apply 'vector (geiser-syntax--read/list)))
      ((quote backquote unquote splice) (list (cdr token)
                                              (geiser-syntax--read)))
      (kwd (intern (format ":%s" (cdr token))))
      (unprintable (format "#<%s>" (cdr token)))
      ((char string atom) (cdr token))
      (t (error "Reading scheme syntax: unexpected token: %s" token)))))

(defun geiser-syntax--read-from-string (string &optional start end)
  (when (stringp string)
    (let* ((start (or start 0))
           (end (or end (length string)))
           (max-lisp-eval-depth (max max-lisp-eval-depth (- end start))))
      (with-temp-buffer
        (save-excursion (insert string))
        (cons (ignore-errors (geiser-syntax--read)) (point))))))

(defsubst geiser-syntax--form-after-point (&optional boundary)
  (let ((geiser-syntax--read/buffer-limit (and (numberp boundary) boundary)))
    (save-excursion (values (geiser-syntax--read) (point)))))


;;; Code parsing:

(defsubst geiser-syntax--skip-comment/string ()
  (goto-char (or (nth 8 (syntax-ppss)) (point))))

(defsubst geiser-syntax--nesting-level ()
  (or (nth 0 (syntax-ppss)) 0))

(defsubst geiser-syntax--pair-length (p)
  (if (cdr (last p)) (1+ (safe-length p)) (length p)))

(defun geiser-syntax--scan-sexps ()
  (let ((path))
    (save-excursion
      (geiser-syntax--skip-comment/string)
      (while (not (zerop (geiser-syntax--nesting-level)))
        (let ((boundary (1+ (point))))
          (backward-up-list)
          (let ((form
                 (nth-value 0 (geiser-syntax--form-after-point boundary))))
            (when (and (listp form) (car form) (symbolp (car form)))
              (let* ((len-1 (1- (geiser-syntax--pair-length form)))
                     (prev (and (> len-1 1) (nth (1- len-1) form)))
                     (prev (and (keywordp prev) (list prev))))
                (push `(,(car form) ,len-1 ,@prev) path)))))))
    (if path (nreverse path)
      (let ((fst (symbol-at-point)))
        (and fst `((,fst 0)))))))

(defsubst geiser-syntax--binding-form-p (bfs sbfs f)
  (or (memq f '(define define* lambda let let* letrec))
      (memq f bfs)
      (memq f sbfs)))

(defsubst geiser-syntax--binding-form*-p (sbfs f)
  (or (eq 'let* f) (memq f sbfs)))

(defun geiser-syntax--scan-locals (bfs sbfs form partial locals)
  (flet ((if-symbol (x) (and (symbolp x) x))
         (if-list (x) (and (listp x) x))
         (normalize (vars) (mapcar (lambda (i) (if (listp i) (car i) i)) vars)))
    (cond ((or (null form) (not (listp form))) (normalize locals))
          ((not (geiser-syntax--binding-form-p bfs sbfs (car form)))
           (geiser-syntax--scan-locals bfs sbfs
                                       (car (last form)) partial locals))
          (t
           (let* ((head (car form))
                  (name (if-symbol (cadr form)))
                  (names (if name (if-list (caddr form))
                           (if-list (cadr form))))
                  (rest (if name (cdddr form) (cddr form)))
                  (use-names (or rest
                                 (not partial)
                                 (geiser-syntax--binding-form*-p sbfs head))))
             (when name (push name locals))
             (when use-names (dolist (n names) (push n locals)))
             (dolist (f (butlast rest))
               (when (eq (car f) 'define) (push (cadr f) locals)))
             (geiser-syntax--scan-locals bfs sbfs
                                         (car (last (or rest names)))
                                         partial
                                         locals))))))

(defun geiser-syntax--locals-around-point (bfs sbfs)
  (when (eq major-mode 'scheme-mode)
    (save-excursion
      (geiser-syntax--skip-comment/string)
      (let ((boundary (point)))
        (while (not (zerop (geiser-syntax--nesting-level)))
          (backward-up-list))
        (multiple-value-bind (form end)
            (geiser-syntax--form-after-point boundary)
          (geiser-syntax--scan-locals bfs sbfs form (> end boundary) '()))))))


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
