;;; geiser-capf.el -- Setup for Geiser's CAPFs

;; Copyright (c) 2022  Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Apr 23, 2022 18:39



(require 'geiser-autodoc)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-doc)
(require 'geiser-completion)
(require 'geiser-edit)

(defun geiser-capf--company-docsig (id)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (let ((help (geiser-autodoc--autodoc `((,id 0)) nil)))
        (and help (substring-no-properties help))))))

(defun geiser-capf--company-doc-buffer (id)
  (let* ((impl geiser-impl--implementation)
         (module (geiser-eval--get-module))
         (symbol (make-symbol id))
         (ds (geiser-doc--get-docstring symbol module)))
    (when (consp ds)
      (with-current-buffer (get-buffer-create "*company-documentation*")
        (geiser-doc--render-docstring ds symbol module impl)
        (current-buffer)))))

(defun geiser-capf--company-location (id)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (let ((id (make-symbol id)))
        (condition-case nil
            (geiser-edit-module id 'noselect)
          (error (geiser-edit-symbol id 'noselect)))))))

(defun geiser-capf--thing-at-point (module &optional predicate)
  (with-syntax-table scheme-mode-syntax-table
    (let* ((beg (geiser-completion--symbol-begin module))
           (end (or (geiser-completion--prefix-end beg module) beg))
           (prefix (and (> end beg) (buffer-substring-no-properties beg end)))
           (prefix (and prefix
                        (if (string-match "\\([^-]+\\)-" prefix)
                            (match-string 1 prefix)
                          prefix)))
           (cmps (and prefix (geiser-completion--complete prefix module))))
      (when cmps
        (list beg end cmps
              :company-docsig #'geiser-capf--company-docsig
              :company-doc-buffer #'geiser-capf--company-doc-buffer
              :company-location #'geiser-capf--company-location)))))


(defun geiser-capf--for-symbol (&optional predicate)
  (geiser-capf--thing-at-point nil predicate))

(defun geiser-capf--for-module (&optional predicate)
  (geiser-capf--thing-at-point t predicate))

(defun geiser-capf--for-filename ()
  (when (geiser-syntax--in-string-p)
    (let ((comint-completion-addsuffix "\""))
      (ignore-errors (comint-filename-completion)))))

(defun geiser-capf-setup (enable)
  (set (make-local-variable 'completion-at-point-functions)
       (if enable
           '(geiser-capf--for-symbol
             geiser-capf--for-module
             geiser-capf--for-filename)
         (default-value 'completion-at-point-functions))))

(defun geiser-capf-complete-module ()
  "Complete module name at point."
  (interactive)
  (let ((completion-at-point-functions '(geiser-completion--for-module)))
    (call-interactively 'completion-at-point)))



(provide 'geiser-capf)
;;; geiser-capf.el ends here
