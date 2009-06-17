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

(defcustom geiser-plt-binary
  (cond ((eq system-type 'windows-nt) "MzScheme.exe")
        ((eq system-type 'darwin) "mzscheme")
        (t "mzscheme"))
  "Name to use to call the mzscheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-plt)

(defcustom geiser-plt-collects nil
  "A list of paths to be added to mzscheme's collection directories."
  :type '(repeat file)
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
      ,@(apply 'append (mapcar (lambda (p) (list "-S" p)) geiser-plt-collects))
      ,@(and (listp geiser-plt-binary) (cdr geiser-plt-binary))
      ,@(and init-file (file-readable-p init-file) (list "-f" init-file))
      "-f" ,(expand-file-name "plt/geiser.ss" geiser-scheme-dir))))

(defconst geiser-plt-prompt-regexp "^mzscheme@[^ ]*?> ")

(defun switch-to-plt (&optional ask)
  (interactive "P")
  (switch-to-geiser ask 'plt))

(defun run-plt ()
  "Run Geiser using mzscheme."
  (interactive)
  (run-geiser 'plt))


;;; Evaluation support:

(defun geiser-plt-geiser-procedure (proc)
  (let ((proc (intern (format "geiser:%s" proc))))
    `(dynamic-require ''geiser ',proc)))

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

(defun geiser-plt-get-module (&optional module)
  (cond ((and (null module) (geiser-plt--explicit-module)))
        ((null module) (geiser-plt--implicit-module))
        ((symbolp module) module)
        ((and (stringp module) (file-name-absolute-p module)) module)
        ((stringp module) (intern module))
        (t nil)))

(defun geiser-plt-symbol-begin (module)
  (save-excursion (skip-syntax-backward "^-()>") (point)))


;;; External help

(defun geiser-plt-external-help (symbol module)
  (message "Requesting help for '%s'..." symbol)
  (geiser-eval--send/wait
   `(:eval (get-help ',symbol (:module ,module)) geiser/autodoc))
  (minibuffer-message "%s done" (current-message))
  t)


;;; Error display

(defconst geiser-plt--file-rx-0 "^\\([^:\n\"]+\\):\\([0-9]+\\):\\([0-9]+\\)")
(defconst geiser-plt--file-rx-1 "path:\"?\\([^>\"\n]+\\)\"?>")
(defconst geiser-plt--file-rx-2 "module: \"\\([^>\"\n]+\\)\"")

(defun geiser-plt--find-files (rx)
  (save-excursion
    (while (re-search-forward rx nil t)
      (geiser-edit--make-link (match-beginning 1)
                              (match-end 1)
                              (match-string 1)
                              (match-string 2)
                              (match-string 3)))))

(defun geiser-plt-display-error (module key msg)
  (insert "Error: ")
  (when key (geiser-doc--insert-button key nil 'plt))
  (newline 2)
  (when msg
    (let ((p (point)))
      (insert msg)
      (let ((end (point)))
        (goto-char p)
        (geiser-plt--find-files geiser-plt--file-rx-0)
        (geiser-plt--find-files geiser-plt--file-rx-1)
        (geiser-plt--find-files geiser-plt--file-rx-2)
        (goto-char end)
        (fill-region p end)
        (newline))))
  t)


;;; Trying to ascertain whether a buffer is mzscheme scheme:

(defun geiser-plt-guess ()
  (or (save-excursion
        (goto-char (point-min))
        (re-search-forward "#lang " nil t))
      (geiser-plt--explicit-module)
      (string-equal (file-name-extension (buffer-file-name)) "ss")))


(provide 'geiser-plt)
;;; geiser-plt.el ends here
