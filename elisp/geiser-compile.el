;; geiser-compile.el -- compile/load scheme files

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Wed Feb 11, 2009 00:16

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

;; Functions to compile and load Scheme files.

;;; Code:

(require 'geiser-eval)
(require 'geiser-popup)
(require 'geiser-base)


;;; Compilation buffer:

(define-derived-mode geiser-compile-mode compilation-mode "Geiser Compilation"
  "Major mode showing the results of compiling or loading scheme files.
\{geiser-compile-mode-keymap}")

(geiser-popup--define compile "*Geiser compilation*" geiser-compile-mode)


;;; Compile file command:

(defun geiser-compile-file (&optional path)
  "Compile and load Scheme file."
  (interactive (or path (read-file-name "Scheme file: " nil nil t)))
  (let ((buffer (find-file-noselect path))
        (msg (format "Compiling %s ..." path)))
    (when (and (buffer-modified-p buffer)
               (y-or-n-p "Save buffer? "))
      (save-buffer buffer))
    (message msg)
    (let* ((ret (geiser-eval--send/wait `(:gs ((:ge compile-file) ,path))))
           (err (geiser-eval--retort-error ret))
           (output (geiser-eval--retort-output ret)))
      (geiser-compile--with-buffer
        (erase-buffer)
        (insert msg)
        (newline)
        (when output
          (insert output)
          (newline))
        (when err
          (insert "\n" (geiser-eval--error-str  err) "\n")))
      (if (not err)
          (message "%s %s" msg (if output (geiser--chomp output) "OK!"))
        (message "")
        (geiser-compile--pop-to-buffer)))))

(defun geiser-compile-current-buffer ()
  "Compile and load current Scheme file."
  (interactive)
  (geiser-compile-file (buffer-file-name (current-buffer))))



(provide 'geiser-compile)
;;; geiser-compile.el ends here
