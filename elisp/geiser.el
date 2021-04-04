;;; geiser.el --- GNU Emacs and Scheme talk to each other

;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2018, 2020, 2021 Jose Antonio Ortega Ruiz
;; URL: http://www.nongnu.org/geiser/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;; Commentary:

;; Autoloads and basic setup for geiser.


;;; Code:
;;; Locations:

;;;###autoload
(defconst geiser-elisp-dir (file-name-directory load-file-name)
  "Directory containing Geiser's Elisp files.")

;;;###autoload
(add-to-list 'load-path (directory-file-name geiser-elisp-dir))


;;; Autoloads:

;;;###autoload
(autoload 'geiser-version "geiser-version" "Echo Geiser's version." t)

;;;###autoload
(autoload 'geiser-unload "geiser-reload" "Unload all Geiser code." t)

;;;###autoload
(autoload 'geiser-reload "geiser-reload" "Reload Geiser code." t)

;;;###autoload
(autoload 'geiser "geiser-repl"
  "Start a Geiser REPL, or switch to a running one." t)

;;;###autoload
(autoload 'run-geiser "geiser-repl" "Start a Geiser REPL." t)

;;;###autoload
(autoload 'geiser-connect "geiser-repl"
  "Start a Geiser REPL connected to a remote server." t)

;;;###autoload
(autoload 'geiser-connect-local "geiser-repl"
  "Start a Geiser REPL connected to a remote server over a Unix-domain socket."
  t)

;;;###autoload
(autoload 'switch-to-geiser "geiser-repl"
  "Switch to a running one Geiser REPL." t)

;;;###autoload
(autoload 'geiser-mode "geiser-mode"
  "Minor mode adding Geiser REPL interaction to Scheme buffers." t)

;;;###autoload
(autoload 'turn-on-geiser-mode "geiser-mode"
  "Enable Geiser's mode (useful in Scheme buffers)." t)

;;;###autoload
(autoload 'turn-off-geiser-mode "geiser-mode"
  "Disable Geiser's mode (useful in Scheme buffers)." t)

;;;###autoload
(autoload 'geiser-mode--maybe-activate "geiser-mode")

;;;###autoload
(mapc (lambda (group)
        (custom-add-load group (symbol-name group))
        (custom-add-load 'geiser (symbol-name group)))
      '(geiser
        geiser-repl
        geiser-autodoc
        geiser-doc
        geiser-debug
        geiser-faces
        geiser-mode
        geiser-image
        geiser-implementation
        geiser-xref))


;;; Setup:

;;;###autoload
(add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)


(provide 'geiser)

;;; geiser.el ends here
