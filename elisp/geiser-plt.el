;; geiser-plt.el -- geiser support for PLT scheme

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Apr 25, 2009 21:13



(require 'geiser-edit)
(require 'geiser-doc)
(require 'geiser-eval)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-plt nil
  "Customization for Geiser's PLT flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-plt-binary
  (cond ((eq system-type 'windows-nt) "MzScheme.exe")
        ((eq system-type 'darwin) "mzscheme")
        (t "mzscheme"))
  "Name to use to call the mzscheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-plt)

(geiser-custom--defcustom geiser-plt-collects nil
  "A list of paths to be added to mzscheme's collection directories."
  :type '(repeat file)
  :group 'geiser-plt)

(geiser-custom--defcustom geiser-plt-init-file "~/.plt-geiser"
  "Initialization file with user code for the mzscheme REPL."
  :type 'string
  :group 'geiser-plt)



;;; REPL support:

(defun geiser-plt--binary ()
  (if (listp geiser-plt-binary) (car geiser-plt-binary) geiser-plt-binary))

(defun geiser-plt--parameters ()
  "Return a list with all parameters needed to start mzscheme.
This function uses `geiser-plt-init-file' if it exists."
  (let ((init-file (and (stringp geiser-plt-init-file)
                        (expand-file-name geiser-plt-init-file))))
    `("-i" "-q"
      "-S" ,(expand-file-name "plt/" geiser-scheme-dir)
      ,@(apply 'append (mapcar (lambda (p) (list "-S" p)) geiser-plt-collects))
      ,@(and (listp geiser-plt-binary) (cdr geiser-plt-binary))
      ,@(and init-file (file-readable-p init-file) (list "-f" init-file))
      "-f" ,(expand-file-name "plt/geiser.ss" geiser-scheme-dir))))

(defconst geiser-plt--prompt-regexp "^=?mzscheme@[^ ]*?> ")


;;; Evaluation support:

(defun geiser-plt--language ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         "^\\(?:#lang\\|(module +[^ ]+?\\) +\\([^ ]+?\\|([^)]+)\\) *$" nil t)
        (car (read-from-string (match-string-no-properties 1)))
      :f)))

(defun geiser-plt--geiser-procedure (proc)
  (if (memq proc '(eval compile))
      `((dynamic-require ''geiser 'geiser:eval) ',(geiser-plt--language))
    `(dynamic-require ''geiser ',(intern (format "geiser:%s" proc)))))

(defconst geiser-plt--module-re
  "^(module +\\([^ ]+\\)")

(defun geiser-plt--explicit-module ()
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward geiser-plt--module-re nil t)
         (ignore-errors
           (car (read-from-string (match-string-no-properties 1)))))))

(defsubst geiser-plt--implicit-module ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#lang " nil t)
        (buffer-file-name)
      :f)))

(defun geiser-plt--get-module (&optional module)
  (cond ((and (null module) (buffer-file-name)))
        ;; (geiser-plt--explicit-module)
        ((null module) (geiser-plt--implicit-module))
        ((symbolp module) module)
        ((and (stringp module) (file-name-absolute-p module)) module)
        ((stringp module) (intern module))
        (t nil)))

(defun geiser-plt--symbol-begin (module)
  (save-excursion (skip-syntax-backward "^-()>") (point)))


;;; External help

(defsubst geiser-plt--get-help (symbol module)
  (geiser-eval--send/wait
   `(:eval (get-help ',symbol (:module ,module)) geiser/autodoc)))

(defun geiser-plt--external-help (id module)
  (message "Requesting help for '%s'..." id)
  (let ((out (geiser-eval--retort-output (geiser-plt--get-help id module))))
    (when (and out (string-match " but provided by:\n +\\(.+\\)\n" out))
      (geiser-plt--get-help symbol (match-string 1 out))))
  (minibuffer-message "%s done" (current-message))
  t)


;;; Error display

(defconst geiser-plt--file-rxs '("^\\([^:\n\"]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                                 "path:\"?\\([^>\"\n]+\\)\"?>"
                                 "module: \"\\([^>\"\n]+\\)\""))

(defun geiser-plt--find-files (rx)
  (save-excursion
    (while (re-search-forward rx nil t)
      (geiser-edit--make-link (match-beginning 1)
                              (match-end 1)
                              (match-string 1)
                              (match-string 2)
                              (match-string 3)))))

(defun geiser-plt--display-error (module key msg)
  (when key
    (insert "Error: ")
    (geiser-doc--insert-button key nil 'plt)
    (newline 2))
  (when msg
    (let ((p (point)))
      (insert msg)
      (let ((end (point)))
        (goto-char p)
        (mapc 'geiser-plt--find-files geiser-plt--file-rxs)
        (goto-char end)
        (fill-region p end)
        (newline))))
  t)


;;; Trying to ascertain whether a buffer is mzscheme scheme:

(defun geiser-plt--guess ()
  (or (save-excursion
        (goto-char (point-min))
        (re-search-forward "#lang " nil t))
      (geiser-plt--explicit-module)))


;;; Implementation definition:

(define-geiser-implementation plt
  (binary geiser-plt--binary)
  (arglist geiser-plt--parameters)
  (startup)
  (unsupported-procedures '(callers callees generic-methods))
  (prompt-regexp geiser-plt--prompt-regexp)
  (marshall-procedure geiser-plt--geiser-procedure)
  (find-module geiser-plt--get-module)
  (find-symbol-begin geiser-plt--symbol-begin)
  (display-error geiser-plt--display-error)
  (display-help geiser-plt--external-help)
  (check-buffer geiser-plt--guess))

(geiser-impl--add-to-alist 'regexp "\\.mzscheme\\.sl?s$" 'plt t)
(geiser-impl--add-to-alist 'regexp "\\.ss$" 'plt t)


(provide 'geiser-plt)
;;; geiser-plt.el ends here
