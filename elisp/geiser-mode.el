;; geiser-mode.el -- minor mode for scheme buffers

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sun Feb 08, 2009 15:13



(require 'geiser-doc)
(require 'geiser-compile)
(require 'geiser-completion)
(require 'geiser-company)
(require 'geiser-xref)
(require 'geiser-edit)
(require 'geiser-autodoc)
(require 'geiser-debug)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-repl)
(require 'geiser-popup)
(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-mode nil
  "Mode enabling Geiser abilities in Scheme buffers &co.."
  :group 'geiser)

(geiser-custom--defcustom geiser-mode-autodoc-p t
  "Whether `geiser-autodoc-mode' gets enabled by default in Scheme buffers."
  :group 'geiser-mode
  :group 'geiser-autodoc
  :type 'boolean)

(geiser-custom--defcustom geiser-mode-company-p t
  "Whether to use company-mode for completion, if available."
  :group 'geiser-mode
  :type 'boolean)

(geiser-custom--defcustom geiser-mode-smart-tab-p nil
  "Whether `geiser-smart-tab-mode' gets enabled by default in Scheme buffers."
  :group 'geiser-mode
  :type 'boolean)



;;; Evaluation commands:

(defun geiser--go-to-repl ()
  (switch-to-geiser)
  (push-mark)
  (goto-char (point-max)))

(defun geiser-eval-region (start end &optional and-go raw)
  "Eval the current region in the Geiser REPL.
With prefix, goes to the REPL buffer afterwards (as
`geiser-eval-region-and-go')"
  (interactive "rP")
  (geiser-debug--send-region nil
                             start
                             end
                             (and and-go 'geiser--go-to-repl)
                             (not raw)))

(defun geiser-eval-region-and-go (start end)
  "Eval the current region in the Geiser REPL and visit it afterwads."
  (interactive "r")
  (geiser-eval-region start end t))

(defun geiser-eval-definition (&optional and-go)
  "Eval the current definition in the Geiser REPL.
With prefix, goes to the REPL buffer afterwards (as
`geiser-eval-definition-and-go')"
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (geiser-eval-region (point) end and-go t))))

(defun geiser-eval-definition-and-go ()
  "Eval the current definition in the Geiser REPL and visit it afterwads."
  (interactive)
  (geiser-eval-definition t))

(defun geiser-eval-last-sexp ()
  "Eval the previous sexp in the Geiser REPL."
  (interactive)
  (geiser-eval-region (save-excursion (backward-sexp) (point))
                      (point)
                      nil
                      t))

(defun geiser-compile-definition (&optional and-go)
  "Compile the current definition in the Geiser REPL.
With prefix, goes to the REPL buffer afterwards (as
`geiser-eval-definition-and-go')"
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (geiser-debug--send-region t
                                 (point)
                                 end
                                 (and and-go 'geiser--go-to-repl)
                                 t))))

(defun geiser-compile-definition-and-go ()
  "Compile the current definition in the Geiser REPL and visit it afterwads."
  (interactive)
  (geiser-compile-definition t))

(defun geiser-expand-region (start end &optional all raw)
  "Macro-expand the current region and display it in a buffer.
With prefix, recursively macro-expand the resulting expression."
  (interactive "rP")
  (geiser-debug--expand-region start end all (not raw)))

(defun geiser-expand-definition (&optional all)
  "Macro-expand the current definition.
With prefix, recursively macro-expand the resulting expression."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (geiser-expand-region (point) end all t))))

(defun geiser-expand-last-sexp (&optional all)
  "Macro-expand the previous sexp.
With prefix, recursively macro-expand the resulting expression."
  (interactive "P")
  (geiser-expand-region (save-excursion (backward-sexp) (point))
                        (point)
                        all
                        t))

(defun geiser-set-scheme ()
  "Associates current buffer with a given Scheme implementation."
  (interactive)
  (let ((impl (geiser-impl--read-impl)))
    (geiser-impl--set-buffer-implementation impl)
    (geiser-repl--get-repl impl)))


;;; Geiser mode:

(make-variable-buffer-local
 (defvar geiser-mode-string nil
   "Modeline indicator for geiser-mode"))

(defun geiser-mode--lighter ()
  (or geiser-mode-string
      (format " %s" (or (geiser-impl--impl-str) "G"))))

(defvar geiser-mode-map (make-sparse-keymap)
  "Key map for geiser-mode")

(define-minor-mode geiser-mode
  "Toggle Geiser's mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Geiser mode is enabled, a host of nice utilities for
interacting with the Geiser REPL is at your disposal.
\\{geiser-mode-map}"
  :init-value nil
  :lighter (:eval (geiser-mode--lighter))
  :group 'geiser-mode
  :keymap geiser-mode-map
  (when geiser-mode (geiser-impl--set-buffer-implementation))
  (setq geiser-autodoc-mode-string "/A")
  (setq geiser-smart-tab-mode-string "/T")
  (geiser-company--setup (and geiser-mode geiser-mode-company-p))
  (when geiser-mode-autodoc-p (geiser-autodoc-mode geiser-mode))
  (when geiser-mode-smart-tab-p (geiser-smart-tab-mode geiser-mode)))

(defun turn-on-geiser-mode ()
  "Enable `geiser-mode' (in a Scheme buffer)."
  (interactive)
  (geiser-mode 1))

(defun turn-off-geiser-mode ()
  "Disable `geiser-mode' (in a Scheme buffer)."
  (interactive)
  (geiser-mode -1))


;;; Keys:

(defun geiser-mode--triple-chord (p k c)
  (define-key geiser-mode-map (vector '(control ?c) `(control ,p) k) c)
  (define-key geiser-mode-map (vector '(control ?c) `(control ,p) `(control ,k)) c))

(define-key geiser-mode-map "\C-c\C-z" 'switch-to-geiser)
(define-key geiser-mode-map "\C-c\C-s" 'geiser-set-scheme)

(define-key geiser-mode-map "\C-c\C-l" 'geiser-load-current-buffer)
(define-key geiser-mode-map "\C-c\C-k" 'geiser-compile-current-buffer)

(define-key geiser-mode-map (kbd "M-TAB") 'geiser-completion--complete-symbol)
(define-key geiser-mode-map (kbd "M-`") 'geiser-completion--complete-module)
(define-key geiser-mode-map (kbd "C-.") 'geiser-completion--complete-module)
(define-key geiser-mode-map "\M-." 'geiser-edit-symbol-at-point)
(define-key geiser-mode-map "\M-," 'geiser-edit-pop-edit-symbol-stack)
(define-key geiser-mode-map (kbd "C-c <") 'geiser-xref-callers)
(define-key geiser-mode-map (kbd "C-c >") 'geiser-xref-callees)

(define-key geiser-mode-map "\M-\C-x" 'geiser-eval-definition)
(define-key geiser-mode-map "\C-x\C-e" 'geiser-eval-last-sexp)
(define-key geiser-mode-map "\C-c\M-e" 'geiser-eval-definition-and-go)
(define-key geiser-mode-map "\C-c\C-r" 'geiser-eval-region)
(define-key geiser-mode-map "\C-c\M-r" 'geiser-eval-region-and-go)
(define-key geiser-mode-map "\C-c\M-c" 'geiser-compile-definition)
(define-key geiser-mode-map "\C-c\C-c" 'geiser-compile-definition-and-go)

(geiser-mode--triple-chord ?d ?a 'geiser-autodoc-mode)
(geiser-mode--triple-chord ?d ?d 'geiser-doc-symbol-at-point)
(geiser-mode--triple-chord ?d ?m 'geiser-doc-module)

(geiser-mode--triple-chord ?e ?m 'geiser-edit-module)

(geiser-mode--triple-chord ?m ?e 'geiser-expand-last-sexp)
(geiser-mode--triple-chord ?m ?r 'geiser-expand-region)
(geiser-mode--triple-chord ?m ?x 'geiser-expand-definition)

(geiser-mode--triple-chord ?x ?m 'geiser-xref-generic-methods)


;;; Reload support:

(defun geiser-mode--buffers ()
  (let ((buffers))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (set-buffer buffer)
        (when geiser-mode
          (push (cons buffer geiser-impl--implementation) buffers))))
    buffers))

(defun geiser-mode--restore (buffers)
  (dolist (b buffers)
    (when (buffer-live-p (car b))
      (set-buffer (car b))
      (geiser-mode 1)
      (when (cdr b) (geiser-impl--set-buffer-implementation (cdr b))))))

(defun geiser-mode-unload-function ()
  (dolist (b (geiser-mode--buffers))
    (with-current-buffer (car b) (geiser-mode nil))))


(provide 'geiser-mode)
;;; geiser-mode.el ends here
