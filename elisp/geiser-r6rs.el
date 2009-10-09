;;; geiser-r6rs.el -- Generic support for R6RS implementations

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Thu Sep 24, 2009 15:06


(require 'geiser-impl)
(require 'geiser-base)



(defun geiser-r6rs--marshall-procedure (proc)
  `(eval ,proc '(geiser)))

(defun geiser-r6rs--find-library (&optional name)
  (cond ((null name)
         (save-excursion
           (while (not (zerop (geiser-syntax--nesting-level)))
             (backward-up-list))
           (when (re-search-forward "\\=(library +(" nil t)
             (backward-char)
             (geiser-syntax--read/list))))
        ((listp name) name)
        ((stringp name)
         (let ((name (car (geiser-syntax--read-from-string name))))
           (and (listp name) name)))))

(defun geiser-r6rs--check ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^(library[ \t]" nil t)))


;;; Base class for R6RS implementations:

(geiser-impl--define load-file-name 'r6rs nil
                     '(marshall-procedure geiser-r6rs--marshall-procedure)
                     '(find-module geiser-r6rs--find-library)
                     '(check-buffer geiser-r6rs--check))


(provide 'geiser-r6rs)
;;; geiser-r6rs.el ends here

