;; geiser-larceny.el -- larceny's implementation of the geiser protocols

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Mar 15, 2009 03:08

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

;; Implementation of all required Elisp Geiser protocols for Larceny.

;;; Code:

(require 'geiser-eval)
(require 'geiser-impl)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-larceny nil
  "Customization for Geiser's Larceny flavour."
  :group 'geiser)

(defcustom geiser-larceny-binary
  (cond ((eq system-type 'windows-nt) "larceny.exe")
        ((eq system-type 'darwin) "larceny")
        (t "larceny"))
  "Name to use to call the Larceny executable when starting a REPL."
  :type 'string
  :group 'geiser-larceny)

(defcustom geiser-larceny-mode 'err5rs
  "Mode to use when starting the Larceny REPL."
  :type '(choice (const :tag "ERR5RS" err5rs)
                 (const :tag "R5RS" 'r5rs))
  :group 'geiser-larceny)


;;; REPL support:

(defun geiser-larceny-parameters ()
  "Return a list with all parameters needed to start Larceny."
  `(,(if (eq 'r5rs geiser-larceny-mode) "-r5rs" "-err5rs")
    "-path" ,(expand-file-name "larceny/" geiser-scheme-dir)))

(defconst geiser-larceny-prompt-regexp "^\\(debug\\)?> ")

(defun geiser-larceny--startup ()
  (when (eq 'err5rs geiser-larceny-mode)
    (geiser-eval--send/wait '(import (rnrs))))
  (geiser-eval--send/wait '(require (geiser))))

(defun switch-to-larceny (&optional ask)
  (interactive "P")
  (switch-to-geiser ask 'larceny))

(defun run-larceny ()
  "Run Geiser using Larceny."
  (interactive)
  (run-geiser 'larceny))


;;; Evaluation support:

(defun geiser-larceny-geiser-procedure (proc)
  "Translate a bare procedure symbol to one executable in Larceny's
context. Return NULL for unsupported ones; at the very least,
EVAL, COMPILE, LOAD-FILE and COMPILE-FILE should be supported."
  (intern (format "ge:%s" proc)))

(defconst geiser-larceny--module-re
  "(library +\\(([^)]+)\\)")

(defun geiser-larceny-get-module (&optional module)
  "Return a scheme datum representing the current module.
If MODULE is provided, transform it to such a datum."
  (cond ((null module)
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward geiser-larceny--module-re nil t)
               (geiser-larceny-get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module) (or (ignore-errors (car (read-from-string module))) :f))
        (t :f)))


;;; Trying to ascertain whether a buffer is Larceny Scheme:

(defun geiser-larceny-guess ()
  "Return `t' if the current buffer looks like a Larceny file."
  (listp (geiser-larceny-get-module)) t)


;;; Register this implementation:

(geiser-impl--register 'larceny)



(provide 'geiser-larceny)
;;; geiser-larceny.el ends here
