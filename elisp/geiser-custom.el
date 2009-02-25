;; geiser-custom.el -- customization

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Feb 14, 2009 21:49

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

;; Utilities for handling custom variables and faces.

;;; Code:

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
