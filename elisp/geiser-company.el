;; geiser-company.el -- integration with company-mode

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Aug 24, 2009 12:44

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

;;; Code:

(require 'geiser-autodoc)
(require 'geiser-completion)
(require 'geiser-edit)
(require 'geiser-base)


;;; Helpers:

(make-variable-buffer-local
 (defvar geiser-company--enabled-flag nil))

(defsubst geiser-company--candidates (prefix module)
  (car (geiser-completion--complete prefix module)))

(defsubst geiser-company--doc (id module)
  (ignore-errors
    (if module
	(format "%s [module]" id)
      (geiser-autodoc--autodoc (list (list (intern id) 0)) t))))

(defsubst geiser-company--doc-buffer (id module)
  nil)

(defun geiser-company--location (id module)
  (ignore-errors
    (let ((id (intern id)))
      (save-current-buffer
	(if module (geiser-edit-module id) (geiser-edit-symbol id))
	(cons (current-buffer) (point))))))

(defsubst geiser-company--prefix-at-point (module)
  (and geiser-company--enabled-flag
       (looking-at-p "\\_>")
       (not (nth 8 (syntax-ppss)))
       (geiser-completion--prefix module)))


;;; Activation

(defun geiser-company--setup (enable)
  (setq geiser-company--enabled-flag enable)
  (when (boundp 'company-lighter)
    (setq company-lighter "/C"))
  (when (fboundp 'company-mode)
    (company-mode nil)
    (when enable (company-mode enable))))

(defun geiser-company--inhibit-autodoc (ignored)
  (setq geiser-autodoc--inhibit-flag t))

(defun geiser-company--restore-autodoc (&optional ignored)
  (setq geiser-autodoc--inhibit-flag nil))


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
     (geiser-company--make-backend company-geiser-ids nil)
     (geiser-company--make-backend company-geiser-modules t)
     (add-to-list 'company-backends geiser-company--backend)
     (add-hook 'company-completion-finished-hook 'geiser-company--restore-autodoc)
     (add-hook 'company-completion-cancelled-hook 'geiser-company--restore-autodoc)
     (add-hook 'company-completion-started-hook 'geiser-company--inhibit-autodoc)))


;;; Reload support:

(defun geiser-company-unload-function ()
  (when (boundp 'company-backends)
    (setq company-backends (remove geiser-company--backend company-backends))))


(provide 'geiser-company)
;;; geiser-company.el ends here
