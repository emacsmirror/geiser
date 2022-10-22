;;; geiser-edit.el -- scheme edit locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2012, 2013, 2019-2022 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Feb 11, 2009 21:07


;;; Code:

(require 'geiser-completion)
(require 'geiser-eval)
(require 'geiser-custom)
(require 'geiser-base)

(require 'etags)


;;; Customization:

(defmacro geiser-edit--define-custom-visit (var group doc)
  `(geiser-custom--defcustom ,var nil
     ,doc
     :group ',group
     :type '(choice (const :tag "Other window" window)
                    (const :tag "Other frame" frame)
                    (const :tag "Current window" nil))))

(geiser-edit--define-custom-visit
 geiser-edit-symbol-method geiser-mode
 "How the new buffer is opened when invoking \\[geiser-edit-symbol-at-point]
or following links in error buffers.")

(geiser-custom--defface error-link
  'link geiser-debug "links in error buffers")


;;; Auxiliary functions:

(defun geiser-edit--visit-file (file method)
  (cond ((eq method 'window) (pop-to-buffer (find-file-noselect file t)))
        ((eq method 'frame) (find-file-other-frame file))
        ((eq method 'noselect) (find-file-noselect file t))
        (t (find-file file))))

(defsubst geiser-edit--location-name (loc)
  (cdr (assoc "name" loc)))

(defsubst geiser-edit--location-file (loc)
  (when-let ((file-name (cdr (assoc "file" loc))))
    (concat (or (file-remote-p default-directory) "")
            file-name)))

(defsubst geiser-edit--to-number (x)
  (cond ((numberp x) x)
        ((stringp x) (string-to-number x))))

(defsubst geiser-edit--location-line (loc)
  (geiser-edit--to-number (cdr (assoc "line" loc))))

(defsubst geiser-edit--location-column (loc)
  (geiser-edit--to-number (cdr (assoc "column" loc))))

(defsubst geiser-edit--location-char (loc)
  (geiser-edit--to-number (cdr (assoc "char" loc))))

(defsubst geiser-edit--make-location (name file line column)
  (if (equal line "")
      `(("name" . ,name) ("file" . ,file) ("char" . ,column))
    `(("name" . ,name) ("file" . ,file) ("line" . ,line) ("column" . ,column))))

(defconst geiser-edit--def-re
  (regexp-opt '("define"
                "defmacro"
                "define-macro"
                "define-syntax"
                "define-syntax-rule"
                "-define-syntax"
                "-define"
                "define*"
                "define-method"
                "define-class"
                "define-struct")))

(defconst geiser-edit--def-re*
  (regexp-opt '("define-syntaxes" "define-values")))

(defsubst geiser-edit--def-re (thing)
  (let ((sx (regexp-quote (format "%s" thing))))
    (format (concat "(%s[[:space:]]+\\("
                    "(%s\\_>[^)]*)\\|"
                    "\\(\\_<%s\\_>\\) *\\([^\n]*?\\)[)\n]"
                    "\\)")
            geiser-edit--def-re sx sx)))

(defsubst geiser-edit--def-re* (thing)
  (format "(%s +([^)]*?\\_<%s\\_>"
          geiser-edit--def-re*
          (regexp-quote (format "%s" thing))))

(defun geiser-edit--find-def (symbol &optional args)
  (save-excursion
    (goto-char (point-min))
    (when (or (re-search-forward (geiser-edit--def-re symbol) nil t)
              (re-search-forward (geiser-edit--def-re* symbol) nil t))
      (cons (match-beginning 0)
            (and args
                 (if (match-string 2)
                     (let* ((v (or (match-string 3) ""))
                            (v (and (not (string-blank-p v)) v)))
                       (concat (match-string 2)
                               (and v " => ")
                               v
                               (and v (string-prefix-p "(" v) " ...")))
                   (match-string 1)))))))

(defsubst geiser-edit--symbol-re (thing)
  (format "\\_<%s\\_>" (regexp-quote (format "%s" thing))))

(defun geiser-edit--goto-location (symbol line col pos)
  (cond ((numberp line)
         (goto-char (point-min))
         (forward-line (max 0 (1- line))))
        ((numberp pos) (goto-char pos)))
  (if (not col)
      (when-let (pos (car (geiser-edit--find-def symbol)))
        (goto-char pos))
    (beginning-of-line)
    (forward-char col)
    (cons (current-buffer) (point))))

(defun geiser-edit--try-edit-location (symbol loc &optional method no-error)
  (let ((symbol (or (geiser-edit--location-name loc) symbol))
        (file (geiser-edit--location-file loc))
        (line (geiser-edit--location-line loc))
        (col (geiser-edit--location-column loc))
        (pos (geiser-edit--location-char loc)))
    (when file
      (geiser-edit--visit-file file (or method geiser-edit-symbol-method)))
    (or (geiser-edit--goto-location symbol line col pos)
        file
        (unless no-error
          (error "Couldn't find location for '%s'" symbol)))))

(defsubst geiser-edit--try-edit (symbol ret &optional method no-error)
  (geiser-edit--try-edit-location symbol
                                  (geiser-eval--retort-result ret)
                                  method
                                  no-error))


;;; Links

(define-button-type 'geiser-edit--button
  'action 'geiser-edit--button-action
  'face 'geiser-font-lock-error-link
  'follow-link t)

(defun geiser-edit--button-action (button)
  (let ((loc (button-get button 'geiser-location))
        (method (button-get button 'geiser-method)))
    (when loc (geiser-edit--try-edit-location nil loc method))))

(defun geiser-edit--make-link (beg end file line col &optional method)
  (make-button beg end
               :type 'geiser-edit--button
               'geiser-method method
               'geiser-location
               (geiser-edit--make-location 'error file line col)
               'help-echo "Go to error location"))

(defconst geiser-edit--default-file-rx
  "^[ \t]*\\([^<>:\n\"]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?")

(defun geiser-edit--buttonize-files (&optional rx no-fill)
  (let ((rx (or rx geiser-edit--default-file-rx))
        (fill-column (- (window-width) 2)))
    (save-excursion
      (while (re-search-forward rx nil t)
        (geiser-edit--make-link (match-beginning 1)
                                (match-end 1)
                                (match-string 1)
                                (match-string 2)
                                (or (match-string 3) 0)
                                'window)
        (unless no-fill (fill-region (match-end 0) (point-at-eol)))))))

(defun geiser-edit--open-next (&optional n reset)
  (interactive)
  (let* ((n (or n 1))
         (nxt (if (< n 0) 'backward-button 'forward-button))
         (msg (if (< n 0) "previous" "next"))
         (n (abs n))
         (p (point))
         (found nil))
    (when reset (goto-char (point-min)))
    (while (> n 0)
      (let ((b (ignore-errors (funcall nxt 1))))
        (unless b (setq n 0))
        (when (and b (eq (button-type b) 'geiser-edit--button))
          (setq n (- n 1))
          (when (<= n 0)
            (setq found t)
            (push-button (point))))))
    (unless found
      (goto-char p)
      (error "No %s error" msg))))


;;; Visibility
(defun geiser-edit--cloak (form)
  (intern (format "geiser-edit-cloak-%s" form)))

(defun geiser-edit--hide (form)
  (geiser-edit--show form)
  (let ((cloak (geiser-edit--cloak form)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "(%s\\b" form) nil t)
        (let* ((beg (match-beginning 0))
               (end (progn (ignore-errors (goto-char beg) (forward-sexp))
                           (point))))
          (when (> end beg)
            (overlay-put (make-overlay beg end) 'invisible cloak)))))
    (add-to-invisibility-spec (cons cloak t))))

(defun geiser-edit--show (form)
  (let ((cloak (geiser-edit--cloak form)))
    (remove-overlays nil nil 'invisible cloak)
    (remove-from-invisibility-spec (cons cloak t))))

(defun geiser-edit--show-all ()
  (remove-overlays)
  (setq buffer-invisibility-spec '(t)))

(defun geiser-edit--toggle-visibility (form)
  (if (and (listp buffer-invisibility-spec)
           (assoc (geiser-edit--cloak form) buffer-invisibility-spec))
      (geiser-edit--show form)
    (geiser-edit--hide form)))


;;; Commands:

(defvar geiser-edit--symbol-history nil)

(defun geiser-edit-symbol (symbol &optional method marker)
  "Asks for a symbol to edit, with completion."
  (interactive
   (list (geiser-completion--read-symbol "Edit symbol: "
                                         nil
                                         geiser-edit--symbol-history)))
  (let ((cmd `(:eval (:ge symbol-location ',symbol))))
    (geiser-edit--try-edit symbol (geiser-eval--send/wait cmd) method)
    (when marker (xref-push-marker-stack))))

(defun geiser-edit-symbol-at-point (&optional arg)
  "Visit the definition of the symbol at point.
With prefix, asks for the symbol to locate."
  (interactive "P")
  (let* ((symbol (or (and (not arg) (geiser--symbol-at-point))
                     (geiser-completion--read-symbol "Edit symbol: ")))
         (cmd `(:eval (:ge symbol-location ',symbol)))
         (marker (point-marker))
         (ret (ignore-errors (geiser-eval--send/wait cmd))))
    (if (geiser-edit--try-edit symbol ret nil t)
        (when marker (xref-push-marker-stack marker))
      (unless (geiser-edit-module-at-point t)
        (error "Couldn't find location for '%s'" symbol)))
    t))

(defun geiser-pop-symbol-stack ()
  "Pop back to where \\[geiser-edit-symbol-at-point] was last invoked."
  (interactive)
  (xref-pop-marker-stack))

(defun geiser-edit-module (module &optional method no-error)
  "Asks for a module and opens it in a new buffer."
  (interactive (list (geiser-completion--read-module)))
  (let ((cmd `(:eval (:ge module-location '(:module ,module)))))
    (geiser-edit--try-edit module (geiser-eval--send/wait cmd) method no-error)))

(defun geiser-edit-module-at-point (&optional no-error)
  "Opens a new window visiting the module at point."
  (interactive)
  (let ((marker (point-marker)))
    (geiser-edit-module (or (geiser-completion--module-at-point)
                            (geiser-completion--read-module))
                        nil no-error)
    (when marker (xref-push-marker-stack marker))
    t))

(defun geiser-insert-lambda (&optional full)
  "Insert λ at point.  With prefix, inserts (λ ())."
  (interactive "P")
  (if (not full)
      (insert (make-char 'greek-iso8859-7 107))
    (insert "(" (make-char 'greek-iso8859-7 107) " ())")
    (backward-char 2)))

(defun geiser-squarify (n)
  "Toggle between () and [] for current form.

With numeric prefix, perform that many toggles, forward for
positive values and backward for negative."
  (interactive "p")
  (let ((pared (and (boundp 'paredit-mode) paredit-mode))
        (fwd (> n 0))
        (steps (abs n)))
    (when (and pared (fboundp 'paredit-mode)) (paredit-mode -1))
    (unwind-protect
        (save-excursion
          (unless (looking-at-p "\\s(") (backward-up-list))
          (while (> steps 0)
            (let ((p (point))
                  (round (looking-at-p "(")))
              (forward-sexp)
              (backward-delete-char 1)
              (insert (if round "]" ")"))
              (goto-char p)
              (delete-char 1)
              (insert (if round "[" "("))
              (setq steps (1- steps))
              (backward-char)
              (condition-case nil
                  (progn (when fwd (forward-sexp 2))
                         (backward-sexp))
                (error (setq steps 0))))))
      (when (and pared (fboundp 'paredit-mode)) (paredit-mode 1)))))



(provide 'geiser-edit)
