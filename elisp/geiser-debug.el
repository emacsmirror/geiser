;;; geiser-debug.el -- displaying debug and eval info  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2016, 2020-2022 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Feb 23, 2009 22:34


;;; Code:

(eval-when-compile (require 'cl-macs))

(require 'geiser-edit)
(require 'geiser-autodoc)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-menu)
(require 'geiser-popup)
(require 'geiser-base)
(require 'geiser-image)

(require 'ansi-color)
(require 'compile)

(declare-function geiser-repl--switch-to-buffer "geiser-repl" (buffer))


;;; Customization:

(defgroup geiser-debug nil
  "Debugging and error display options."
  :group 'geiser)

(define-obsolete-variable-alias 'geiser-debug-always-display-sexp-after-p
  'geiser-debug-always-display-sexp-after "0.26.2")

(geiser-custom--defcustom geiser-debug-always-display-sexp-after nil
  "Whether to always display the sexp whose evaluation caused an
error after the error message in the debug pop-up.

If nil, expressions shorter than `geiser-debug-long-sexp-lines`
lines are shown before the error message."
  :type 'boolean)

(geiser-custom--defcustom geiser-debug-long-sexp-lines 6
  "Length of an expression in order to be relegated to the bottom
of the debug pop-up (after the error message).

If `geiser-debug-always-display-sexp-after` is t, this variable
has no effect."
  :type 'int)

(define-obsolete-variable-alias 'geiser-debug-jump-to-debug-p
  'geiser-debug-jump-to-debug "0.26.2")

(geiser-custom--defcustom geiser-debug-jump-to-debug t
  "When set to t (the default), jump to the debug pop-up buffer
in case of evaluation errors.

See also `geiser-debug-show-debug`. "
  :type 'boolean)

(define-obsolete-variable-alias 'geiser-debug-show-debug-p
  'geiser-debug-show-debug "0.26.2")

(geiser-custom--defcustom geiser-debug-auto-next-error-p nil
  "When set, automatically invoke `next-error' on of evaluation errors.

This will make point jump to the location of an error if the output
of the evaluation contains any."
  :type 'boolean)

(geiser-custom--defcustom geiser-debug-show-debug t
  "When set to t (the default), show the debug pop-up buffer in
case of evaluation errors.

This option takes effect even if `geiser-debug-jump-to-debug`
is set."
  :type 'boolean)

(define-obsolete-variable-alias 'geiser-debug-auto-display-images-p
  'geiser-debug-auto-display-images "0.26.2")

(geiser-custom--defcustom geiser-debug-auto-display-images t
  "Whether to automatically invoke the external viewer to display
images when they're evaluated.

See also `geiser-repl-auto-display-images-p'."
  :type 'boolean)

(geiser-custom--defcustom geiser-debug-treat-ansi-colors nil
  "Colorize ANSI escape sequences produced by the scheme process.

Some schemes are able to colorize their evaluation or error
results using ANSI color sequences (e.g. when using the the
colorized module in Guile).

If set to `nil', no special treatment is applied to output.  The
symbol colors indicates colorizing the display of the Geiser debug
buffer using any color escape, and the symbol remove to remove
all ANSI sequences."
  :type '(choice (const :tag "No special treatment" nil)
                 (const :tag "Use font lock for colors" colors)
                 (const :tag "Remove all ANSI codes" remove)))


;;; Debug buffer mode:

(defvar geiser-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map)
  "Keymap for `geiser-debug-mode'.")

(define-derived-mode geiser-debug-mode nil "Geiser Debug"
  "A major mode for displaying Scheme compilation and evaluation results.
\\{geiser-debug-mode-map}"
  (buffer-disable-undo)
  (set-syntax-table scheme-mode-syntax-table)
  (setq next-error-function 'geiser-edit--open-next)
  (compilation-setup nil)
  (setq buffer-read-only t))

(defvar-local geiser-debug--debugger-active nil)
(defvar-local geiser-debug--sender-buffer nil)

(defun geiser-debug-active-p ()
  "Check whether debugger has been entered by a scheme buffer operation."
  (and geiser-debug--debugger-active geiser-debug--sender-buffer))

(defun geiser-debug-switch-to-buffer ()
  "Return to the scheme buffer that pooped this debug window."
  (interactive)
  (when geiser-debug--sender-buffer
    (geiser-repl--switch-to-buffer geiser-debug--sender-buffer)))

(geiser-menu--defmenu debug geiser-debug-mode-map
  ("Next error" ("n" [?\t]) compilation-next-error)
  ("Previous error" ("p" "\e\t" [backtab]) compilation-previous-error)
  ("Next error location" ((kbd "M-n")) next-error)
  ("Previous error location" ((kbd "M-p")) previous-error)
  ("Source buffer" ("z" (kbd "C-c C-z")) geiser-debug-switch-to-buffer)
  --
  ("Quit" nil View-quit))


;;; Implementation-dependent functionality
(geiser-impl--define-caller geiser-debug--clean-up-output clean-up-output (output)
  "Clean up output from an evaluation for display.")


;;; Buffer for displaying evaluation results:

(geiser-popup--define debug "*Geiser Debug*" geiser-debug-mode)


;;; Displaying retorts

(geiser-impl--define-caller geiser-debug--display-error
    display-error (module key message)
  "This method takes 3 parameters (a module name, the error key,
and the accompanying error message) and should display
(in the current buffer) a formatted version of the error. If the
error was successfully displayed, the call should evaluate to a
non-null value.")

(geiser-impl--define-caller geiser-debug--enter-debugger
    enter-debugger ()
  "This method is called upon entering the debugger, in the REPL
buffer.")

(defun geiser-debug--display-after (what)
  (or geiser-debug-always-display-sexp-after
      (>= (with-temp-buffer
            (insert what)
            (count-lines (point-min) (point-max)))
          geiser-debug-long-sexp-lines)))

(defun geiser-debug--insert-res (res)
  (let ((begin (point)))
    (insert res)
    (let ((end (point)))
      (goto-char begin)
      (let ((no (geiser-image--replace-images t
                                              geiser-debug-auto-display-images)))
        (goto-char end)
        (newline 2)
        (and no (> no 0))))))

(defun geiser-debug--default-display-error (key msg)
  (insert "\n"
          (if key (format "Error: %s\n" key) "")
          (format "%s" (or msg "")) "\n"))

(defun geiser-debug--display-retort (what ret &optional res _auto-p)
  (let* ((err (geiser-eval--retort-error ret))
         (key (geiser-eval--error-key err))
         (debug (alist-get 'debug ret))
         (impl geiser-impl--implementation)
         (output (geiser-eval--retort-output ret))
         (output (and (stringp output)
                      (not (string= output ""))
                      (or (geiser-debug--clean-up-output impl output) output)))
         (module (geiser-eval--get-module))
         (img nil)
         (dir default-directory)
         (buffer (current-buffer))
         (debug-entered (when debug (geiser-debug--enter-debugger impl)))
         (after (geiser-debug--display-after what)))
    (unless debug-entered
      (geiser-debug--with-buffer
        (when (and (not debug) geiser-debug--debugger-active)
          (message "Debugger exited"))
        (setq geiser-debug--debugger-active debug
              geiser-debug--sender-buffer buffer
              geiser-impl--implementation impl)
        (erase-buffer)
        (when dir (setq default-directory dir))
        (unless after (insert what "\n\n"))
        (setq img (when (and res (not err) (not debug))
                    (geiser-debug--insert-res res)))
        (when (or err key output)
          (when (fboundp 'next-error-select-buffer)
            (next-error-select-buffer (current-buffer)))
          (let ((msg (or (geiser-eval--error-msg err) output "")))
            (or (geiser-debug--display-error impl module key msg)
                (geiser-debug--default-display-error key msg))
            (unless err (geiser-edit--buttonize-files))))
        (when after
          (goto-char (point-max))
          (insert "\nExpression evaluated was:\n\n")
          (insert what "\n"))
        (cl-case geiser-debug-treat-ansi-colors
          (colors (ansi-color-apply-on-region (point-min) (point-max)))
          (remove (ansi-color-filter-region (point-min) (point-max))))
        (goto-char (point-min)))
      (when (or img err output)
        (cond (geiser-debug-jump-to-debug
               (geiser-debug--pop-to-buffer))
              (geiser-debug-show-debug
               (display-buffer (geiser-debug--buffer))))
        (when (and err geiser-debug-auto-next-error-p)
          (ignore-errors (next-error))
          (message "=> %s" output))))))

(defsubst geiser-debug--wrap-region (str)
  (format "(begin %s\n)" str))

(defun geiser-debug--unwrap (str)
  (if (string-match "(begin[ \t\n\v\r]+\\(.+\\)*)" str)
      (match-string 1 str)
    str))

(defun geiser-debug--send-region (compile start end and-go wrap &optional nomsg)
  "Evaluate (or COMPILE) the region delimited by START and END.
The result of the evaluation is reported asynchronously, so this
call is not blocking. If AND-GO is t, also jump to the repl
buffer.  If WRAP is t, the region's content is wrapped in a begin
form.  The flag NOMSG can be used to avoid reporting of the
result in the minibuffer."
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code `(,(if compile :comp :eval) (:scm ,wrapped)))
         (cont (lambda (ret)
                 (let ((res (geiser-eval--retort-result-str ret nil))
                       (scstr (geiser-syntax--scheme-str str)))
                   (when and-go (funcall and-go))
                   (unless (geiser-eval--retort-error ret)
                     (save-excursion
                       (goto-char (/ (+ end start) 2))
                       (geiser-autodoc--clean-cache))
                     (unless nomsg
                       (save-match-data
                         (when (string-match "\\(?:[ \t\n\r]+\\)\\'" res)
                           (setq res (replace-match "" t t res))))
                       (message "%s" res)))
                   (geiser-debug--display-retort scstr ret res)))))
    (geiser-eval--send code cont (current-buffer))))

(defun geiser-debug--send-region/wait (compile start end timeout)
  "Synchronous version of `geiser-debug--send-region', returning its result."
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (geiser-debug--wrap-region str))
         (code `(,(if compile :comp :eval) (:scm ,wrapped))))
    (message "evaluating: %s" code)
    (geiser-eval--send/wait code timeout)))

(defun geiser-debug--expand-region (start end all wrap)
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code
          `(:eval (:ge macroexpand (quote (:scm ,wrapped)) ,(if all :t :f))))
         (cont (lambda (ret)
                 (let ((err (geiser-eval--retort-error ret))
                       (result (geiser-eval--retort-result ret)))
                   (if err
                       (geiser-debug--display-retort str ret)
                     (geiser-debug--with-buffer
                       (erase-buffer)
                       (insert (format "%s"
                                       (if wrap
                                           (geiser-debug--unwrap result)
                                         result)))
                       (goto-char (point-min)))
                     (geiser-debug--pop-to-buffer))))))
    (geiser-eval--send code cont (current-buffer))))


(provide 'geiser-debug)
