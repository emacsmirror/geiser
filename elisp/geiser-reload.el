;; geiser-reload.el -- unload/load geiser packages

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Aug 22, 2009 23:04

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

(require 'geiser-impl)
(require 'geiser-repl)
(require 'geiser-mode)
(require 'geiser-base)
(require 'geiser)


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
           geiser-install
           geiser
           geiser-version
           )))

(defun geiser-unload ()
  "Unload all Geiser modules."
  (interactive)
  (let ((fs (geiser--features-list)))
    (unload-feature 'geiser-reload t)
    (dolist (f fs)
      (when (featurep f) (unload-feature f t)))))

(defun geiser-reload (&optional arg)
  "Reload Geiser.
With prefix arg, prompts for the DIRECTORY from which Geiser should be
loaded again."
  (interactive "P")
  (let* ((old-dir geiser-elisp-dir)
         (dir (or (and arg (read-directory-name "New Geiser elisp dir: "
                                                old-dir old-dir t old-dir))
                  old-dir)))
    (unless (or (file-exists-p (expand-file-name "geiser-reload.el" dir))
                (file-exists-p (expand-file-name "geiser-reload.elc" dir)))
      (error "%s does not contain Geiser!" dir))
    (let ((installed (featurep 'geiser-install))
          (installed-impls geiser-impl-installed-implementations)
          (impls geiser-impl--impls)
          (repls (geiser-repl--repl-list))
          (buffers (geiser-mode--buffers)))
      (geiser-unload)
      (setq load-path (remove old-dir load-path))
      (add-to-list 'load-path dir)
      (setq geiser-impl-installed-implementations installed-impls)
      (require 'geiser-reload)
      (when installed (require 'geiser-install nil t))
      (geiser-impl--reload-implementations impls)
      (geiser-repl--restore repls)
      (geiser-mode--restore buffers)
      (message "Geiser reloaded!"))))


(provide 'geiser-reload)
;;; geiser-reload.el ends here
