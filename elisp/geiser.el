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


;;; Scheme mode setup:

(defsubst geiser-impl--impl-feature (impl)
  (intern (format "geiser-%s" impl)))

(defun geiser-setup ()
  (eval-after-load "scheme"
    '(add-hook 'scheme-mode-hook 'turn-on-geiser-mode))
  (mapc (lambda (impl)
          (require (geiser-impl--impl-feature impl) nil t))
        (or geiser-impl-installed-implementations
            '(guile plt))))


;;; Reload:

(defmacro geiser--features-list ()
  (quote '(
           geiser-mode
           geiser-repl
           geiser-xref
           geiser-edit
           geiser-doc
           geiser-debug
           geiser-impl
           geiser-completion
           geiser-autodoc
           geiser-compile
           geiser-eval
           geiser-connection
           geiser-syntax
           geiser-log
           geiser-custom
           geiser-base
           geiser-popup
           )))

(defun geiser-unload-function ()
  (dolist (feature (geiser--features-list))
    (when (featurep feature) (unload-feature feature t)))
  t)

(defun geiser-unload ()
  (interactive)
  (when (featurep 'geiser) (unload-feature 'geiser)))

(defun geiser-reload (&optional arg)
  "Reload Geiser.
With prefix arg, prompts for the DIRECTORY in which Geiser should be
loaded."
  (interactive "P")
  (let* ((dir (or (and arg (read-directory-name "New Geiser root dir: "
                                                geiser-root-dir
                                                geiser-root-dir
                                                t
                                                geiser-root-dir))
                  geiser-root-dir))
         (geiser-main-file (expand-file-name "elisp/geiser.el" dir))
         (installed-impls geiser-impl-installed-implementations)
         (impls (and (featurep 'geiser-impl) geiser-impl--impls))
         (repls (and (featurep 'geiser-repl) (geiser-repl--repl-list)))
         (buffers (and (featurep 'geiser-mode) (geiser-mode--buffers))))
    (unless (file-exists-p geiser-main-file)
      (error "%s does not contain Geiser!" dir))
    (geiser-unload)
    (setq load-path (remove geiser-elisp-dir load-path))
    (setq geiser-impl-installed-implementations installed-impls)
    (load-file geiser-main-file)
    (dolist (feature (geiser--features-list))
      (load-library (format "%s" feature)))
    (geiser-impl--reload-implementations impls)
    (geiser-repl--restore repls)
    (geiser-mode--restore buffers)
    (message "Geiser reloaded!")))


;; Initialization:

(geiser-setup)


(provide 'geiser)
;;; geiser.el ends here
