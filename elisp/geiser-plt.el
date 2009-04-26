;; geiser-plt.el -- geiser support for PLT scheme

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Apr 25, 2009 21:13

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

;; Implementation of Geiser's protocols for MzScheme.

;;; Code:

(require 'geiser-impl)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-plt nil
  "Customization for Geiser's PLT flavour."
  :group 'geiser)

(defcustom geiser-plt-binary
  (cond ((eq system-type 'windows-nt) "MzScheme.exe")
        ((eq system-type 'darwin) "mzscheme")
        (t "mzscheme"))
  "Name to use to call the mzscheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-plt)

(defcustom geiser-plt-init-file "~/.plt-geiser"
  "Initialization file with user code for the mzscheme REPL."
  :type 'string
  :group 'geiser-plt)



;;; REPL support:

(defun geiser-plt-binary ()
  (if (listp geiser-plt-binary) (car geiser-plt-binary) geiser-plt-binary))

(defun geiser-plt-parameters ()
  "Return a list with all parameters needed to start mzscheme.
This function uses `geiser-plt-init-file' if it exists."
  (let ((init-file (and (stringp geiser-plt-init-file)
                        (expand-file-name geiser-plt-init-file))))
    `("-i" "-q"
      "-S" ,(expand-file-name "plt/" geiser-scheme-dir)
      "-f" ,(expand-file-name "plt/geiser.ss" geiser-scheme-dir)
      ,@(and (listp geiser-plt-binary) (cdr geiser-plt-binary))
      ,@(and init-file (file-readable-p init-file) (list "-f" init-file)))))

(defconst geiser-plt-prompt-regexp "^mzscheme@([^)]*?)> ")

(defun switch-to-plt (&optional ask)
  (interactive "P")
  (switch-to-geiser ask 'plt))

(defun run-plt ()
  "Run Geiser using mzscheme."
  (interactive)
  (run-geiser 'plt))


;;; Evaluation support:

(defun geiser-plt-geiser-procedure (proc)
  (let ((proc (intern (format "geiser/%s" proc))))
    `(dynamic-require ''geiser ',proc)))

(defconst geiser-plt--module-re
  "^(module +\\(([^)]+)\\)")

(defun geiser-plt--explicit-module ()
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward geiser-plt--module-re nil t)
         (ignore-errors
           (car (read-from-string (match-string-no-properties 1)))))))

(defun geiser-plt-get-module (&optional module)
  (cond ((and (null module) (geiser-plt--explicit-module)))
        ((null module) (buffer-file-name))
        (t module)))


;;; Trying to ascertain whether a buffer is mzscheme scheme:

(defun geiser-plt-guess ()
  (or (save-excursion
        (goto-char (point-min))
        (re-search-forward "#lang " nil t))
      (geiser-plt--explicit-module)
      (string-equal (file-name-extension (buffer-file-name)) "ss")))


;;; Emacs tweaks for PLT scheme code:

(put 'begin0             'scheme-indent-function 1)
(put 'c-declare          'scheme-indent-function 0)
(put 'c-lambda           'scheme-indent-function 2)
(put 'case-lambda        'scheme-indent-function 0)
(put 'catch              'scheme-indent-function 1)
(put 'chicken-setup      'scheme-indent-function 1)
(put 'class              'scheme-indent-function 'defun)
(put 'class*             'scheme-indent-function 'defun)
(put 'compound-unit/sig  'scheme-indent-function 0)
(put 'dynamic-wind       'scheme-indent-function 0)
(put 'for/fold           'scheme-indent-function 2)
(put 'instantiate        'scheme-indent-function 2)
(put 'interface          'scheme-indent-function 1)
(put 'lambda/kw          'scheme-indent-function 1)
(put 'let*-values        'scheme-indent-function 1)
(put 'let+               'scheme-indent-function 1)
(put 'let-values         'scheme-indent-function 1)
(put 'let/ec             'scheme-indent-function 1)
(put 'mixin              'scheme-indent-function 2)
(put 'module             'scheme-indent-function 'defun)
(put 'opt-lambda         'scheme-indent-function 1)
(put 'parameterize       'scheme-indent-function 1)
(put 'parameterize-break 'scheme-indent-function 1)
(put 'parameterize*      'scheme-indent-function 1)
(put 'quasisyntax/loc    'scheme-indent-function 1)
(put 'receive            'scheme-indent-function 2)
(put 'send*              'scheme-indent-function 1)
(put 'sigaction          'scheme-indent-function 1)
(put 'syntax-case        'scheme-indent-function 2)
(put 'syntax/loc         'scheme-indent-function 1)
(put 'unit               'scheme-indent-function 'defun)
(put 'unit/sig           'scheme-indent-function 2)
(put 'unless             'scheme-indent-function 1)
(put 'when               'scheme-indent-function 1)
(put 'while              'scheme-indent-function 1)
(put 'with-handlers      'scheme-indent-function 1)
(put 'with-method        'scheme-indent-function 1)
(put 'with-syntax        'scheme-indent-function 1)


;;; Register this implementation:

(geiser-impl--register 'plt)


(provide 'geiser-plt)
;;; geiser-plt.el ends here
