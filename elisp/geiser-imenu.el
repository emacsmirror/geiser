;;; geiser-imenu.el -- Tweaks to imenu configuration  -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Oct 12, 2022 01:43


(require 'geiser-impl)

(require 'imenu)
(require 'scheme)

(defvar geiser-imenu-generic-expression
  (mapcar (lambda (e)
            `(,(car e)
              ,(concat "^\\(?: *\\)" (substring (cadr e) 1))
              ,@(cddr e)))
          scheme-imenu-generic-expression))

(defvar geiser-imenu--nested-defs nil)
(geiser-impl--register-local-variable
 'geiser-imenu--nested-defs 'nested-definitions nil
 "A flag indicating whether this implementation accepts nested definitions.
For instance, R6%S library forms will contain them.")

(defun geiser-imenu-declare-nested-definitions (impl)
  "Declare IMPL as one that accepts nested definitions."
  (add-to-list 'geiser-imenu--nested-definition-impls impl))

(defun geiser-imenu-setup (activate)
  "Adjust imenu for the current implementation."
  (setq-local imenu-generic-expression
              (if (and activate geiser-imenu--nested-defs)
                  geiser-imenu-generic-expression
                scheme-imenu-generic-expression)))

(provide 'geiser-imenu)
;;; geiser-imenu.el ends here
