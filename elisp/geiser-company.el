;; geiser-company.el -- integration with company-mode

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Aug 24, 2009 12:44



(require 'geiser-autodoc)
(require 'geiser-completion)
(require 'geiser-edit)
(require 'geiser-base)


;;; Helpers:

(make-variable-buffer-local
 (defvar geiser-company--enabled-flag nil))

(make-variable-buffer-local
 (defvar geiser-company--autodoc-flag nil))

(defsubst geiser-company--candidates (prefix module)
  (geiser-completion--complete prefix module))

(defsubst geiser-company--doc (id module)
  (ignore-errors
    (if module
	(format "%s [module]" id)
      (or (geiser-autodoc--autodoc (list (list (format "%s" id) 0)))
          (format "%s [local id]" id)))))

(defsubst geiser-company--doc-buffer (id module)
  nil)

(defun geiser-company--location (id module)
  (ignore-errors
    (let ((id (make-symbol id)))
      (save-excursion
        (if module
            (geiser-edit-module id 'noselect)
          (geiser-edit-symbol id 'noselect))))))

(defun geiser-company--prefix-at-point (module)
  (when geiser-company--enabled-flag
    (cond ((nth 8 (syntax-ppss)) 'stop)
          ((looking-at-p "\\_>") (geiser-completion--prefix module))
          (module 'stop)
          (t nil))))


;;; Activation

(defun geiser-company--setup (enable)
  (setq geiser-company--enabled-flag enable)
  (when (fboundp 'geiser-company--setup-company)
    (geiser-company--setup-company enable)))

(defun geiser-company--inhibit-autodoc (ignored)
  (when (setq geiser-company--autodoc-flag geiser-autodoc-mode)
    (geiser-autodoc-mode -1)))

(defun geiser-company--restore-autodoc (&optional ignored)
  (when geiser-company--autodoc-flag
    (geiser-autodoc-mode 1)))


;;; Backends:
(defmacro geiser-company--make-backend (name mod)
  `(defun ,name (command &optional arg &rest ignored)
     "A `company-mode' completion back-end for `geiser-mode'."
     (interactive (list 'interactive))
     (case command
       ('interactive (company-begin-backend ',name))
       ('prefix (geiser-company--prefix-at-point ,mod))
       ('candidates (geiser-company--candidates arg ,mod))
       ('meta (geiser-company--doc arg ,mod))
       ('doc-buffer (geiser-company--doc-buffer arg ,mod))
       ('location (geiser-company--location arg ,mod))
       ('sorted t))))

(defvar geiser-company--backend '(company-geiser-ids company-geiser-modules))

(eval-after-load "company"
  '(progn
     (defun geiser-company--setup-company (enable)
       (set (make-local-variable 'company-default-lighter) "/C")
       (set (make-local-variable 'company-echo-delay) 0.01)
       (company-mode nil)
       (when enable (company-mode enable)))
     (geiser-company--make-backend company-geiser-ids nil)
     (geiser-company--make-backend company-geiser-modules t)
     (add-to-list 'company-backends geiser-company--backend)
     (add-hook 'company-completion-finished-hook
               'geiser-company--restore-autodoc)
     (add-hook 'company-completion-cancelled-hook
               'geiser-company--restore-autodoc)
     (add-hook 'company-completion-started-hook
               'geiser-company--inhibit-autodoc)))


;;; Reload support:

(defun geiser-company-unload-function ()
  (when (boundp 'company-backends)
    (setq company-backends (remove geiser-company--backend company-backends))))


(provide 'geiser-company)
;;; geiser-company.el ends here
