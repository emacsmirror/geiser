;;; geiser-custom.el -- customization utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2012, 2024 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Feb 14, 2009 21:49


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
  "Utility macro to define geiser.
This is a very simple macro ensuring all geiser faces follow a common
naming pattern and are added to a common group besides the one provided
in the macro arguments."
  (declare (doc-string 4) (indent 1))
  (let ((face (intern (format "geiser-font-lock-%s" face))))
    `(defface ,face (face-default-spec ,def)
       ,(format "Face for %s." doc)
       :group ',group
       :group 'geiser-faces)))

;;; Reload support:

(defvar geiser-custom--memoized-vars nil)

(defun geiser-custom--memoize (name)
  ;; Why not build this list with mapatoms, filtering on a "\\`'geiser-"
  ;; prefix and checking that it's a `defcustom', so we don't need
  ;; `geiser-custom--defcustom'? Because implementation-specific modules using
  ;; geiser may use similarly named custom variables that are, for some reason
  ;; or another, not meant to partake in the geiser-reload mechanism, and we
  ;; don't know their names.
  (add-to-list 'geiser-custom--memoized-vars name))

(defmacro geiser-custom--defcustom (name &rest body)
  "Like `defcustom' but also put NAME on an internal list.
That list is used by `geiser-reload' to preserve the values of the
listed variables.  It is not used for anything else.

Note that, even though defcustom preserves the existing value of a
variable if it's already defined, geiser-reload unloads all geiser
features and therefore undefines all defined variables: we keep this
list of values around to be able to restore them after loading
a (presumably) new version of geiser after that unloading."
  (declare (doc-string 3) (debug (name body)) (indent 2))
  `(progn
     (geiser-custom--memoize ',name)
     (defcustom ,name ,@body)))

(defun geiser-custom--memoized-state ()
  (let ((result))
    (dolist (name geiser-custom--memoized-vars result)
      (when (boundp name)
        (push (cons name (symbol-value name)) result)))))


(defconst geiser-custom-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(geiser-custom--\\(?:defcustom\\|defface\\)\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode geiser-custom-font-lock-keywords)

(provide 'geiser-custom)
