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

(defvar geiser-elisp-dir nil
  "Directory containing Geiser's Elisp files.")

(defvar geiser-scheme-dir nil
  "Directory containing Geiser's Scheme files.")

(setq geiser-elisp-dir (file-name-directory load-file-name))
(add-to-list 'load-path geiser-elisp-dir)

(setq geiser-scheme-dir (expand-file-name "../scheme/" geiser-elisp-dir))


;;; Autoloads:

(autoload 'geiser-version "geiser-version.el" "Echo Geiser's version." t)

(autoload 'geiser-unload "geiser-reload.el" "Unload all Geiser code." t)

(autoload 'geiser-reload "geiser-reload.el" "Reload Geiser code." t)

(autoload 'geiser "geiser-repl.el"
  "Start a Geiser REPL, or switch to a running one." t)

(autoload 'run-geiser "geiser-repl.el"
  "Start a Geiser REPL." t)

(autoload 'switch-to-geiser "geiser-guile.el"
  "Switch to a running one Geiser REPL." t)

(autoload 'run-guile "geiser-guile.el"
  "Start a Geiser Guile REPL, or switch to a running one." t)

(autoload 'switch-to-guile "geiser-guile.el"
  "Start a Geiser Guile REPL, or switch to a running one." t)

(autoload 'run-plt "geiser-plt.el"
  "Start a Geiser MzScheme REPL, or switch to a running one." t)

(autoload 'switch-to-plt "geiser-guile.el"
  "Start a Geiser MzScheme REPL, or switch to a running one." t)

(autoload 'geiser-mode "geiser-mode.el"
  "Minor mode adding Geiser REPL interaction to Scheme buffers." t)

(autoload 'turn-on-geiser-mode "geiser-mode.el"
  "Enable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'turn-off-geiser-mode "geiser-mode.el"
  "Disable Geiser's mode (useful in Scheme buffers)." t)

(mapc (lambda (group)
        (custom-add-load group (symbol-name group))
        (custom-add-load 'geiser (symbol-name group)))
      '(geiser
        geiser-repl
        geiser-autodoc
        geiser-doc
        geiser-faces
        geiser-mode
        geiser-guile
        geiser-plt
        geiser-impl
        geiser-xref))


;;; Setup:

(eval-after-load "scheme"
  '(add-hook 'scheme-mode-hook 'turn-on-geiser-mode))


(provide 'geiser)
