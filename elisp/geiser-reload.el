;; geiser-reload.el -- unload/load geiser packages

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Aug 22, 2009 23:04



(require 'geiser-repl)
(require 'geiser-mode)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser)


;;; Reload:

(defmacro geiser--features-list ()
  (quote '(
           geiser-mode
           geiser-repl
           geiser-xref
           geiser-doc
           geiser-compile
           geiser-debug
           geiser-impl
           geiser-company
           geiser-edit
           geiser-completion
           geiser-autodoc
           geiser-eval
           geiser-connection
           geiser-syntax
           geiser-log
           geiser-menu
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
          (memo (geiser-custom--memoized-state))
          (repls (geiser-repl--repl-list))
          (buffers (geiser-mode--buffers)))
      (geiser-unload)
      (setq load-path (remove old-dir load-path))
      (add-to-list 'load-path dir)
      (mapc (lambda (x) (set (car x) (cdr x))) memo)
      (require 'geiser-reload)
      (when installed (require 'geiser-install nil t))
      (geiser-repl--restore repls)
      (geiser-mode--restore buffers)
      (message "Geiser reloaded!"))))


(provide 'geiser-reload)
;;; geiser-reload.el ends here
