;; geiser-impl.el -- generic support for scheme implementations

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Mar 07, 2009 23:32



(require 'geiser-eval)
(require 'geiser-base)
(require 'geiser-completion)


;;; Customization:

(defgroup geiser-impl nil
  "Generic support for multiple Scheme implementations."
  :group 'geiser)

(defcustom geiser-impl-default-implementation nil
  "Symbol naming the default Scheme implementation."
  :type 'symbol
  :group 'geiser-impl)

(defcustom geiser-impl-installed-implementations nil
  "Initial list of installed Scheme implementations."
  :type '(repeat symbol)
  :group 'geiser-impl)

(defcustom geiser-impl-implementations-alist nil
  "A map from regular expressions or directories to implementations.
When opening a new file, its full path will be matched against
each one of the regular expressions or directories in this map in order to
determine its scheme flavour."
  :type '(repeat (list (choice (group :tag "Regular expression"
                                      (const regexp) regexp)
                               (group :tag "Directory"
                                      (const dir) directory))
                       symbol))
  :group 'geiser-impl)


;;; Auxiliary functions:
(defsubst geiser-impl--sym (imp name)
  (intern (format "geiser-%s-%s" imp name)))

(defsubst geiser-impl--boundp (imp name)
  (boundp (geiser-impl--sym imp name)))

(defsubst geiser-impl--fboundp (imp name)
  (fboundp (geiser-impl--sym imp name)))

(defsubst geiser-impl--impl-feature (impl)
  (intern (format "geiser-%s" impl)))

(defun geiser-impl--value (imp name &optional fun)
  (let ((sym (geiser-impl--sym imp name)))
    (unless (or (and (not fun) (boundp sym))
                (and fun (fboundp sym)))
      (error "Unbound %s '%s' in Geiser Scheme implementation %s"
             (if fun "function" "variable") sym imp))
    (if fun (symbol-function sym) (symbol-value sym))))

(defsubst geiser-impl--call-if-bound (imp name &rest args)
  (when (geiser-impl--fboundp imp name)
    (apply (geiser-impl--value imp name t) args)))


;;; Registering implementations:

(defvar geiser-impl--impls nil)

(make-variable-buffer-local
 (defvar geiser-impl--implementation nil))

(defun geiser-impl--register (impl)
  (when (and (not (memq impl geiser-impl--impls))
             (require (geiser-impl--impl-feature impl) nil t))
    (add-to-list 'geiser-impl--impls impl)))

(defun geiser-impl--unregister (impl)
  (setq geiser-impl--impls (remove impl geiser-impl--impls))
  (ignore-errors (unload-feature (geiser-impl--impl-feature impl))))

(defun geiser-impl--add-to-alist (kind what impl)
  (add-to-list 'geiser-impl-implementations-alist (list (list kind what) impl)))

(defvar geiser-impl--default-implementation
  geiser-impl-default-implementation)

(defun geiser-impl--default-implementation (&optional new)
  (when new (setq geiser-impl--default-implementation new))
  (or geiser-impl--default-implementation
      geiser-impl-default-implementation
      (car geiser-impl--impls)))

(defsubst geiser-impl--impl-str (&optional impl)
  (let ((impl (or impl geiser-impl--implementation)))
    (and impl (capitalize (format "%s" impl)))))


;;; Installing Scheme implementations:

(defvar geiser-impl--impl-prompt-history nil)

(defun geiser-impl--read-impl (&optional prompt impls non-req)
  (let* ((impls (or impls geiser-impl--impls))
         (impls (mapcar (lambda (s) (format "%s" s)) impls))
         (prompt (or prompt "Scheme implementation: ")))
    (intern (completing-read prompt impls nil (not non-req) nil
                             geiser-impl--impl-prompt-history
                             (and (car geiser-impl--impls)
                                  (symbol-name (car geiser-impl--impls)))))))

(defun geiser-impl--set-buffer-implementation (&optional impl)
  (let ((impl (or impl
                  (geiser-impl--guess)
                  (geiser-impl--read-impl nil nil t))))
    (require (geiser-impl--impl-feature impl))
    (setq geiser-impl--implementation impl)
    (geiser-impl--install-vars impl)
    (geiser-impl--register impl)))

(defsubst geiser-impl--module-function (impl)
  (geiser-impl--sym impl "get-module"))

(defsubst geiser-impl--geiser-procedure-function (impl)
  (geiser-impl--sym impl "geiser-procedure"))

(defsubst geiser-impl--symbol-begin (impl)
  (geiser-impl--sym impl "symbol-begin"))

(defun geiser-impl--install-vars (impl)
  (setq geiser-eval--get-module-function
        (geiser-impl--module-function impl))
  (setq geiser-eval--geiser-procedure-function
        (geiser-impl--geiser-procedure-function impl))
  (setq geiser-completion--symbol-begin-function
        (geiser-impl--symbol-begin impl)))


;;; Evaluating Elisp in a given implementation context:

(defun with--geiser-implementation (imp thunk)
  (let ((geiser-impl--implementation imp)
        (geiser-eval--get-module-function
         (geiser-impl--module-function imp))
        (geiser-eval--geiser-procedure-function
         (geiser-impl--geiser-procedure-function imp))
        (geiser-completion--symbol-begin-function
         (geiser-impl--symbol-begin imp)))
    (funcall thunk)))

(put 'with--geiser-implementation 'lisp-indent-function 1)


;;; Default evaluation environment:

(defun geiser-impl-module (&optional module)
  (geiser-impl--call-if-bound (geiser-impl--default-implementation)
                              "get-module"
                              module))
(set-default 'geiser-eval--get-module-function 'geiser-impl-module)

(defun geiser-impl-geiser-procedure (proc)
  (geiser-impl--call-if-bound (geiser-impl--default-implementation)
                              "geiser-procedure"
                              proc))
(set-default 'geiser-eval--geiser-procedure-function
             'geiser-impl-geiser-procedure)


;;; Access to implementation specific execution parameters:

(defsubst geiser-impl--binary (impl)
  (or (geiser-impl--call-if-bound impl "binary")
      (geiser-impl--value impl "binary")))

(defsubst geiser-impl--parameters (impl)
  (or (geiser-impl--call-if-bound impl "parameters")
      (ignore-errors (geiser-impl--value impl "parameters"))))

(defsubst geiser-impl--prompt-regexp (impl)
  (or (geiser-impl--call-if-bound impl "prompt-regexp")
      (geiser-impl--value impl "prompt-regexp")))

(defsubst geiser-impl--startup (impl)
  (geiser-impl--call-if-bound impl "startup"))

(defsubst geiser-impl--external-help (impl symbol module)
  (geiser-impl--call-if-bound impl "external-help" symbol module))

(defsubst geiser-impl--display-error (impl module key msg)
  (geiser-impl--call-if-bound impl "display-error" module key msg))


;;; Access to implementation guessing function:

(make-variable-buffer-local
 (defvar geiser-scheme-implementation nil
   "Set this buffer local variable to specify the Scheme
implementation to be used by Geiser."))

(defun geiser-impl--match-impl (desc bn)
  (let ((rx (if (eq (car desc) 'regexp)
                (cadr desc)
              (format "^%s" (regexp-quote (cadr desc))))))
    (and rx (string-match-p rx bn))))

(defun geiser-impl--guess ()
  (or geiser-impl--implementation
      geiser-scheme-implementation
      (catch 'impl
        (let ((bn (buffer-file-name)))
          (when bn
            (dolist (x geiser-impl-implementations-alist)
              (when (geiser-impl--match-impl (car x) bn)
                (throw 'impl (cadr x))))))
        (dolist (impl geiser-impl--impls)
          (when (geiser-impl--call-if-bound impl "guess")
            (throw 'impl impl))))
      (geiser-impl--default-implementation)))


;;; User commands

(defun geiser-register-implementation ()
  "Register a new Scheme implementation."
  (interactive)
  (let ((current geiser-impl-installed-implementations)
        (impl (geiser-impl--read-impl "New Scheme implementation: " nil t)))
    (unless (geiser-impl--register impl)
      (error "geiser-%s.el not found in load-path" impl))
    (when (and (not (memq impl current))
               (y-or-n-p "Remember this implementation using customize? "))
      (customize-save-variable
       'geiser-impl-installed-implementations (cons impl current)))))

(defun geiser-unregister-implementation ()
  "Unregister an installed Scheme implementation."
  (interactive)
  (let* ((current geiser-impl-installed-implementations)
         (impl (geiser-impl--read-impl "Forget implementation: " current)))
    (geiser-impl--unregister impl)
    (when (and impl
               (y-or-n-p "Forget permanently using customize? "))
      (customize-save-variable
       'geiser-impl-installed-implementations (remove impl current)))))


;;; Unload support

(defun geiser-impl-unload-function ()
  (dolist (imp (mapcar 'geiser-impl--impl-feature geiser-impl--impls))
    (when (featurep imp) (unload-feature imp t))))

(defun geiser-impl--reload-implementations (impls)
  (dolist (impl impls)
    (load-library (format "geiser-%s" impl))))


;;; Initialization:

(eval-after-load 'geiser-impl
  '(mapc 'geiser-impl--register
         (or geiser-impl-installed-implementations '(guile plt))))


(provide 'geiser-impl)

;;; geiser-impl.el ends here
