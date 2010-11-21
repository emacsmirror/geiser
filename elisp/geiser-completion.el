;;; geiser-completion.el -- tab completion

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Feb 09, 2009 22:21



(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-log)
(require 'geiser-syntax)
(require 'geiser-base)


;;; Completions window handling, heavily inspired in slime's:

(defvar geiser-completion--comp-buffer "*Geiser Completions*")

(make-variable-buffer-local
 (defvar geiser-completion--window-cfg nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar geiser-completion--completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `geiser-completion--window-cfg'."))

(defun geiser-completion--save-window-cfg ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or geiser-completion--window-cfg
              (get-buffer-window geiser-completion--comp-buffer))
    (setq geiser-completion--window-cfg
          (current-window-configuration))
    t))

(defun geiser-completion--delay-restoration ()
  (add-hook 'pre-command-hook
            'geiser-completion--maybe-restore-window-cfg
            nil t))

(defun geiser-completion--forget-window-cfg ()
  (setq geiser-completion--window-cfg nil)
  (setq geiser-completion--completions-window nil))

(defun geiser-completion--restore-window-cfg ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'geiser-completion--maybe-restore-window-cfg)
  (when (and geiser-completion--window-cfg
             (geiser-completion--window-active-p))
    (save-excursion
      (set-window-configuration geiser-completion--window-cfg))
    (setq geiser-completion--window-cfg nil)
    (when (buffer-live-p geiser-completion--comp-buffer)
      (kill-buffer geiser-completion--comp-buffer))))

(defun geiser-completion--maybe-restore-window-cfg ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'geiser-completion--maybe-restore-window-cfg)
  (condition-case err
      (cond ((memq last-command-event '(?( ?) ?\" ?' ?` ?, ?# ? ?\r ?\n ?:))
             (geiser-completion--restore-window-cfg))
            ((not (geiser-completion--window-active-p))
             (geiser-completion--forget-window-cfg))
            (t (geiser-completion--delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in geiser-completion--restore-window-cfg: %S" err))))

(defun geiser-completion--window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p geiser-completion--completions-window)
       (equal (buffer-name
               (window-buffer geiser-completion--completions-window))
              geiser-completion--comp-buffer)))

(defun geiser-completion--display-comp-list (completions base)
  (let ((savedp (geiser-completion--save-window-cfg)))
    (with-output-to-temp-buffer geiser-completion--comp-buffer
      (display-completion-list completions base)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-position (list offset nil))
          (set-syntax-table scheme-mode-syntax-table))))
    (when savedp
      (setq geiser-completion--completions-window
            (get-buffer-window geiser-completion--comp-buffer)))))

(defun geiser-completion--display-or-scroll (completions base)
  (if (and (eq last-command this-command)
           (geiser-completion--window-active-p))
      (geiser-completion--scroll-completions)
    (geiser-completion--display-comp-list completions base))
  (geiser-completion--delay-restoration))

(defun geiser-completion--scroll-completions ()
  (let ((window geiser-completion--completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))


;;; Minibuffer maps:

(defvar geiser-completion--minibuffer-map
  (let ((map (make-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map "?" 'self-insert-command)
    map))

(defvar geiser-completion--module-minibuffer-map
  (let ((map (make-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " 'self-insert-command)
    (define-key map "?" 'self-insert-command)
    map))


;;; Completion functionality:

(defvar geiser-completion--binding-forms nil)
(geiser-impl--register-local-variable
 'geiser-completion--binding-forms 'binding-forms nil
 "A list of forms introducing local bindings, a la let or lambda.")

(defvar geiser-completion--binding-forms* nil)
(geiser-impl--register-local-variable
 'geiser-completion--binding-forms* 'binding-forms* nil
 "A list of forms introducing nested local bindings, a la let*.")

(defsubst geiser-completion--locals ()
  (mapcar (lambda (s) (and (symbolp s) (symbol-name s)))
          (geiser-syntax--locals-around-point
           geiser-completion--binding-forms
           geiser-completion--binding-forms*)))

(defun geiser-completion--symbol-list (prefix)
  (geiser--del-dups
   (append (all-completions prefix (geiser-completion--locals))
           (geiser-eval--send/result `(:eval (:ge completions ,prefix))))))

(defsubst geiser-completion--module-list (prefix)
  (geiser-eval--send/result `(:eval (:ge module-completions ,prefix))))

(defvar geiser-completion--symbol-list-func
  (completion-table-dynamic 'geiser-completion--symbol-list))

(defvar geiser-completion--module-list-func
  (completion-table-dynamic 'geiser-completion--module-list))

(defun geiser-completion--complete (prefix modules)
  (let* ((completions (if modules (geiser-completion--module-list prefix)
                        (geiser-completion--symbol-list prefix)))
         (partial (try-completion prefix completions))
         (partial (if (eq partial t) prefix partial)))
    (cons completions partial)))

(defvar geiser-completion--symbol-history nil)

(defun geiser-completion--read-symbol (prompt &optional default history)
  (let ((minibuffer-local-completion-map geiser-completion--minibuffer-map))
    (completing-read prompt
                     geiser-completion--symbol-list-func
                     nil nil nil
                     (or history geiser-completion--symbol-history)
                     (or default (symbol-at-point)))))

(defvar geiser-completion--module-history nil)

(defun geiser-completion--read-module (&optional prompt default history)
  (let ((minibuffer-local-completion-map
         geiser-completion--module-minibuffer-map))
    (completing-read (or prompt "Module name: ")
                     geiser-completion--module-list-func
                     nil nil nil
                     (or history geiser-completion--module-history)
                     default)))

(defun geiser--respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))

(defvar geiser-completion--symbol-begin-function nil)

(defsubst geiser-completion--def-symbol-begin (module)
  (save-excursion (skip-syntax-backward "^-()>") (point)))

(geiser-impl--register-local-method
 'geiser-completion--symbol-begin-function 'find-symbol-begin
 'geiser-completion--def-symbol-begin
 "An optional function finding the position of the beginning of
the identifier around point. Takes a boolean, indicating whether
we're looking for a module name.")

(defsubst geiser-completion--symbol-begin (module)
  (funcall geiser-completion--symbol-begin-function module))

(defun geiser-completion--module-at-point ()
  (save-excursion
    (goto-char (geiser-completion--symbol-begin t))
    (ignore-errors (thing-at-point 'sexp))))

(defsubst geiser-completion--prefix (module)
  (buffer-substring-no-properties (geiser-completion--symbol-begin module)
                                  (point)))

(defun geiser-completion--complete-symbol (&optional arg previous)
  "Complete the symbol at point.
Perform completion similar to Emacs' complete-symbol.
With prefix, complete module name."
  (interactive "P")
  (unless (geiser-syntax--symbol-at-point)
    (error "No symbol at point"))
  (geiser--respecting-message "Retrieving completions...")
  (let* ((prefix (geiser-completion--prefix arg))
         (result (and prefix (geiser-completion--complete prefix arg)))
         (completions (car result))
         (partial (cdr result)))
    (cond ((null completions)
           (if (not arg)
               (geiser-completion--complete-symbol t prefix)
             (geiser--respecting-message "Can't find completion for %s"
                                         (if (and previous
                                                  (not (equalp previous
                                                               prefix)))
                                             (format "%S or %S"
                                                     previous prefix)
                                           prefix))
             (geiser-completion--restore-window-cfg)))
          (t (insert-and-inherit (substring partial (length prefix)))
             (cond ((= (length completions) 1)
                    (geiser--respecting-message "Sole completion")
                    (geiser-completion--restore-window-cfg))
                   (t (geiser--respecting-message "Complete but not unique")
                      (geiser-completion--display-or-scroll completions
                                                            partial)))))))

(defun geiser-completion--complete-module ()
  (interactive)
  (geiser-completion--complete-symbol t))


;;; Smart tab mode:

(make-variable-buffer-local
 (defvar geiser-smart-tab-mode-string " SmartTab"
   "Modeline indicator for geiser-smart-tab-mode"))

(defun geiser-completion--maybe-complete ()
  "Indent if at beginning of line or after a white space or
closing parenthesis, try completion otherwise."
  (interactive)
  (let ((indent (or (bolp) (memq (syntax-class (syntax-after (1- (point))))
                                 '(0 5)))))
    (if indent (indent-according-to-mode)
      (geiser-completion--complete-symbol))))

(define-minor-mode geiser-smart-tab-mode
  "Toggle smart tab mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this mode is enable, TAB will indent if at point is at
beginning of line or after a white space or closing parenthesis,
and will try completing symbol at point otherwise."
  :init-value nil
  :lighter geiser-smart-tab-mode-string
  :group 'geiser-mode
  :keymap `((,(kbd "TAB") . geiser-completion--maybe-complete)))


(provide 'geiser-completion)
