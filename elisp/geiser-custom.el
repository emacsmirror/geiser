;;; geiser-custom.el -- customization utilities

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Feb 14, 2009 21:49



(require 'font-lock)
(require 'geiser-base)


;;; Customization group:

(defgroup geiser nil
  "Geiser framework for Scheme-Emacs interaction."
  :group 'languages)


;;; Faces:

(defgroup geiser-faces nil
  "Faces used by Geiser."
  :group 'geiser
  :group 'faces)

(defmacro geiser-custom--defface (face def group doc)
  (let ((face (intern (format "geiser-font-lock-%s" face))))
    `(defface ,face (face-default-spec ,def)
       ,(format "Face for %s." doc)
       :group ',group
       :group 'geiser-faces
       :group 'faces)))

(put 'geiser-custom--defface 'lisp-indent-function 1)



(provide 'geiser-custom)
;;; geiser-custom.el ends here
