;; geiser-completion.el -- tab completion

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Feb 09, 2009 22:21

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

;; Utilities for completing symbol at point in Guile buffers.

;;; Code:

(require 'geiser-eval)
(require 'geiser-log)
(require 'geiser-syntax)
(require 'geiser-base)


;;; Completions window handling, heavily inspired in slime's:

(defvar geiser-completion--comp-buffer "*Completions*")

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
      (cond ((find last-command-char "()\"'`,# \r\n:")
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
       (equal (buffer-name (window-buffer geiser-completion--completions-window))
              geiser-completion--comp-buffer)))

(defun geiser-completion--display-comp-list (completions base)
  (let ((savedp (geiser-completion--save-window-cfg)))
    (with-output-to-temp-buffer geiser-completion--comp-buffer
      (display-completion-list completions base)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-size offset)
          (set-syntax-table scheme-mode-syntax-table))))
    (when savedp
      (setq geiser-completion--completions-window
            (get-buffer-window geiser-completion--comp-buffer)))))

(defun geiser-completion--display-or-scroll (completions base)
  (cond ((and (eq last-command this-command) (geiser-completion--window-active-p))
         (geiser-completion--scroll-completions))
        (t (geiser-completion--display-comp-list completions base)))
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

(defsubst geiser-completion--symbol-list (prefix)
  (geiser-eval--send/result `(:eval ((:ge completions) ,prefix))))

(defsubst geiser-completion--module-list ()
  (geiser-eval--send/result '(:eval ((:ge all-modules)))))

(defvar geiser-completion--symbol-list-func
  (completion-table-dynamic 'geiser-completion--symbol-list))

(defun geiser-completion--complete (prefix modules)
  (let* ((symbols (if modules (geiser-completion--module-list)
                    (geiser-completion--symbol-list prefix)))
         (completions (all-completions prefix symbols))
         (partial (try-completion prefix symbols))
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
  (let ((minibuffer-local-completion-map geiser-completion--module-minibuffer-map))
    (completing-read (or prompt "Module name: ")
                     (geiser-completion--module-list)
                     nil nil
                     (or default (format "%s" (or (geiser-syntax--buffer-module) "(")))
                     (or history geiser-completion--module-history))))

(defun geiser--respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))

(defsubst geiser-completion--beg-pos (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(") (1- (point))))
    (save-excursion (skip-syntax-backward "^-()") (point))))

(defun geiser-completion--complete-symbol (&optional arg)
  "Complete the symbol at point.
Perform completion similar to Emacs' complete-symbol.
With prefix, complete module name."
  (interactive "P")
  (let* ((end (point))
         (beg (geiser-completion--beg-pos arg))
         (prefix (buffer-substring-no-properties beg end))
         (result (geiser-completion--complete prefix arg))
         (completions (car result))
         (partial (cdr result)))
    (cond ((null completions)
           (geiser--respecting-message "Can't find completion for %S" prefix)
           (geiser-completion--restore-window-cfg))
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
    (if indent (indent-according-to-mode) (geiser-completion--complete-symbol))))

(define-minor-mode geiser-smart-tab-mode
  "Toggle smart tab mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this mode is enable, TAB will indent if at point is at
beginning of line or after a white space or closing parenthesis,
and will try completing symbol at point otherwise. In addition,
M-TAB will try module name completion."
  :init-value nil
  :lighter geiser-smart-tab-mode-string
  :group 'geiser-mode
  :keymap `((,(kbd "TAB") . geiser-completion--maybe-complete)
            (,(kbd "M-TAB") . geiser-completion--complete-module)))




(provide 'geiser-completion)
;;; geiser-completion.el ends here
