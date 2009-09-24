;; geiser-impl.el -- generic support for scheme implementations

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Mar 07, 2009 23:32



(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-implementation nil
  "Generic support for multiple Scheme implementations."
  :group 'geiser)

(geiser-custom--defcustom geiser-default-implementation nil
  "Symbol naming the default Scheme implementation."
  :type 'symbol
  :group 'geiser-implementation)

(geiser-custom--defcustom geiser-active-implementations '(guile plt)
  "List of active installed Scheme implementations."
  :type '(repeat symbol)
  :group 'geiser-implementation)

(geiser-custom--defcustom geiser-implementations-alist '(((regexp "\\.ss$") plt))
  "A map from regular expressions or directories to implementations.
When opening a new file, its full path will be matched against
each one of the regular expressions or directories in this map in order to
determine its scheme flavour."
  :type '(repeat (list (choice (group :tag "Regular expression"
                                      (const regexp) regexp)
                               (group :tag "Directory"
                                      (const dir) directory))
                       symbol))
  :group 'geiser-implementation)


;;; Implementation registry:

(defvar geiser-impl--registry nil)
(defvar geiser-impl--load-files nil)
(defvar geiser-impl--method-docs nil)
(defvar geiser-impl--local-methods nil)
(defvar geiser-impl--local-variables nil)

(geiser-custom--memoize 'geiser-impl--load-files)

(make-variable-buffer-local
 (defvar geiser-impl--implementation nil))

(defsubst geiser-impl--impl-str (&optional impl)
  (let ((impl (or impl geiser-impl--implementation)))
    (and impl (capitalize (format "%s" impl)))))

(defsubst geiser-impl--feature (impl)
  (intern (format "geiser-%s" impl)))

(defsubst geiser-impl--load-impl (impl)
  (require (geiser-impl--feature impl)
           (cdr (assq impl geiser-impl--load-files))
           t))

(defsubst geiser-impl--methods (impl)
  (cdr (assq impl geiser-impl--registry)))

(defun geiser-impl--method (method &optional impl)
  (let ((impl (or impl
                  geiser-impl--implementation
                  geiser-default-implementation)))
    (cadr (assq method (geiser-impl--methods impl)))))

(defun geiser-impl--call-method (method impl &rest args)
  (let ((fun (geiser-impl--method method impl)))
    (when (functionp fun) (apply fun args))))

(defun geiser-impl--method-doc (method doc)
  (push (cons method doc) geiser-impl--method-docs))

(defun geiser-impl--register-local-method (var-name method fallback doc)
  (add-to-list 'geiser-impl--local-methods (list var-name method fallback))
  (geiser-impl--method-doc method doc))

(defun geiser-impl--register-local-variable (var-name method fallback doc)
  (add-to-list 'geiser-impl--local-variables (list var-name method fallback))
  (geiser-impl--method-doc method doc))

(defmacro geiser-impl--define-caller (fun-name method arglist doc)
  (let ((m (make-symbol "method-candidate"))
        (impl (make-symbol "implementation-name")))
    `(progn
       (defun ,fun-name ,(cons impl arglist) ,doc
         (geiser-impl--call-method ',method ,impl ,@arglist))
       (geiser-impl--method-doc ',method ,doc))))
(put 'geiser-impl--define-caller 'lisp-indent-function 3)

(defun geiser-impl--register (file impl methods)
  (let ((current (assq impl geiser-impl--registry)))
    (if current (setcdr current methods)
      (push (cons impl methods) geiser-impl--registry))
    (push (cons impl file) geiser-impl--load-files)))

(defsubst geiser-activate-implementation (impl)
  (add-to-list 'geiser-active-implementations impl))

(defsubst geiser-deactivate-implementation (impl)
  (setq geiser-active-implementations (delq impl geiser-active-implementations)))


;;; Defining implementations:

(defun geiser-impl--normalize-method (m)
  (when (and (listp m)
             (= 2 (length m))
             (symbolp (car m))
             (symbolp (cadr m)))
    (if (functionp (cadr m)) m
      `(,(car m) (lambda (&rest) ,(cadr m))))))

(defun geiser-impl--define (file name parent methods)
  (let* ((methods (mapcar 'geiser-impl--normalize-method methods))
         (methods (delq nil methods))
         (inherited-methods (and parent (geiser-impl--methods parent)))
         (methods (append methods
                          (dolist (m methods inherited-methods)
                            (setq inherited-methods
                                  (assq-delete-all m inherited-methods))))))
    (geiser-impl--register file name methods)))

(defmacro define-geiser-implementation (name &rest methods)
  (let ((name (if (listp name) (car name) name))
        (parent (and (listp name) (cadr name))))
    (unless (symbolp name)
      (error "Malformed implementation name: %s" name))
    (let ((runner (intern (format "run-%s" name)))
          (switcher (intern (format "switch-to-%s" name)))
          (runner-doc (format "Start a new %s REPL." name))
          (switcher-doc (format "Switch to a running %s REPL, or start one." name))
          (ask (make-symbol "ask")))
      `(progn
         (geiser-impl--define ,load-file-name ',name ',parent ',methods)
         (require 'geiser-repl)
         (defun ,runner ()
           ,runner-doc
           (interactive)
           (run-geiser ',name))
         (defun ,switcher (&optional ,ask)
           ,switcher-doc
           (interactive "P")
           (switch-to-geiser ,ask ',name))
         (provide ',(geiser-impl--feature name))))))

(defun geiser-impl--add-to-alist (kind what impl)
  (add-to-list 'geiser-implementations-alist (list (list kind what) impl)))


;;; Trying to guess the scheme implementation:

(make-variable-buffer-local
 (defvar geiser-scheme-implementation nil
   "Set this buffer local variable to specify the Scheme
implementation to be used by Geiser."))

(defun geiser-impl--match-impl (desc bn)
  (let ((rx (if (eq (car desc) 'regexp)
                (cadr desc)
              (format "^%s" (regexp-quote (cadr desc))))))
    (and rx (string-match-p rx bn))))

(defvar geiser-impl--impl-prompt-history nil)

(defun geiser-impl--read-impl (&optional prompt impls non-req)
  (let* ((impls (or impls geiser-active-implementations))
         (impls (mapcar 'symbol-name impls))
         (prompt (or prompt "Scheme implementation: ")))
    (intern (completing-read prompt impls nil (not non-req) nil
                             geiser-impl--impl-prompt-history
                             (and (car impls) (car impls))))))

(geiser-impl--define-caller geiser-impl--check-buffer check-buffer ()
  "Method called without arguments that should check whether the current
buffer contains Scheme code of the given implementation.")

(defun geiser-impl--guess (&optional prompt)
  (or geiser-impl--implementation
      geiser-scheme-implementation
      (catch 'impl
        (let ((bn (buffer-file-name)))
          (when bn
            (dolist (x geiser-implementations-alist)
              (when (and (memq (cadr x) geiser-active-implementations)
                         (geiser-impl--match-impl (car x) bn))
                (throw 'impl (cadr x))))))
        (dolist (impl geiser-active-implementations)
          (when (geiser-impl--check-buffer impl)
            (throw 'impl impl))))
      geiser-default-implementation
      (and (null (cdr geiser-active-implementations))
           (car geiser-active-implementations))
      (and prompt (geiser-impl--read-impl))))


;;; Using implementations:

(defsubst geiser-impl--registered-method (impl method fallback)
  (let ((m (geiser-impl--method method impl)))
    (if (fboundp m) m
      (or fallback (error "%s not defined for %s" method impl)))))

(defsubst geiser-impl--registered-value (impl method fallback)
  (let ((m (geiser-impl--method method impl)))
    (if (fboundp m) (funcall m)
      (or fallback (error "%s not defined for %s" method impl)))))

(defun geiser-impl--set-buffer-implementation (&optional impl)
  (let ((impl (or impl (geiser-impl--guess))))
    (when impl
      (unless (geiser-impl--load-impl impl)
        (error "Cannot find %s implementation" impl))
      (setq geiser-impl--implementation impl)
      (dolist (m geiser-impl--local-methods)
        (set (make-local-variable (nth 0 m))
             (geiser-impl--registered-method impl (nth 1 m) (nth 2 m))))
      (dolist (m geiser-impl--local-variables)
        (set (make-local-variable (nth 0 m))
             (geiser-impl--registered-value impl (nth 1 m) (nth 2 m)))))))

(defmacro with--geiser-implementation (impl &rest body)
  (let* ((mbindings (mapcar (lambda (m)
                              `(,(nth 0 m)
                                (geiser-impl--registered-method ',impl
                                                                ',(nth 1 m)
                                                                ',(nth 2 m))))
                            geiser-impl--local-methods))
         (vbindings (mapcar (lambda (m)
                              `(,(nth 0 m)
                                (geiser-impl--registered-value ',impl
                                                               ',(nth 1 m)
                                                               ',(nth 2 m))))
                            geiser-impl--local-variables))
         (bindings (append mbindings vbindings)))
    `(let* ,bindings ,@body)))
(put 'with--geiser-implementation 'lisp-indent-function 1)


;;; Reload support:

(defun geiser-impl-unload-function ()
  (dolist (imp (mapcar (lambda (i)
                         (geiser-impl--feature (car i)))
                       geiser-impl--registry))
    (when (featurep imp) (unload-feature imp t))))


(provide 'geiser-impl)


;;; Initialization:
;; After providing 'geiser-impl, so that impls can use us.
(mapc 'geiser-impl--load-impl geiser-active-implementations)

;;; geiser-impl.el ends here
