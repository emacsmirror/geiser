;;; geiser.el --- main geiser file

;; Copyright (C) 2009  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Autoloads and basic setup for geiser.

;;; Code:


;;; Locations:

(defvar geiser-root-dir nil
  "Geiser's root directory.")

(defvar geiser-elisp-dir nil
  "Directory containing Geiser's Elisp files.")

(defvar geiser-scheme-dir nil
  "Directory containing Geiser's Scheme files.")

(setq geiser-elisp-dir (file-name-directory load-file-name))
(setq geiser-scheme-dir (expand-file-name "../scheme/" geiser-elisp-dir))
(setq geiser-root-dir (expand-file-name "../" geiser-elisp-dir))

(add-to-list 'load-path geiser-elisp-dir)


;;; Autoloads:

(autoload 'run-guile "geiser-repl.el"
  "Start a Geiser Guile REPL, or switch to a running one." t)

(autoload 'switch-to-guile "geiser-repl.el"
  "Start a Geiser Guile REPL, or switch to a running one." t)

(autoload 'geiser-mode "geiser-mode.el"
  "Minor mode adding Geiser REPL interaction to Scheme buffers." t)


;;; Scheme mode setup:

(defun geiser-setup-scheme-mode ()
  (eval-after-load "scheme"
    '(add-hook 'scheme-mode-hook (lambda () (interactive) (geiser-mode 1)))))

(defun geiser-setup ()
  (geiser-setup-scheme-mode))


(provide 'geiser)
;;; geiser.el ends here
