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
(require 'geiser-base)


;;; Minibuffer map:

(defvar geiser-completion--minibuffer-map
  (let ((map (make-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map "?" 'self-insert-command)
    map))


;;; Modules dictionary:

;; (defvar geiser-completion--modules nil)

;; (defun geiser-completion--modules (&optional reload)
;;   (when (or reload (not geiser-completion--modules))
;;     (geiser--respecting-message "Retrieving modules list")
;;     (let ((geiser-log--inhibit-p t))
;;       (setq geiser-completion--modules
;;             (geiser-eval--retort-result
;;              (geiser-eval--send/wait '(:gs (:ge (module-list :t)))))))
;;   geiser-completion--modules)

;; (defun geiser-completion--read-module (&optional reload init-input history)
;;   (let ((minibuffer-local-completion-map geiser-completion--minibuffer-map)
;;         (modules (geiser-completion--modules reload)))
;;     (completing-read "Module name: " modules nil nil init-input history)))

;; (defsubst geiser-completion--module-list (prefix)
;;   (geiser-eval--retort-result
;;    (geiser-eval--send/wait `(:gs (:ge (module-list ,prefix))))))

;; (defvar geiser-completion--module-history nil)

;; (defun geiser-completion--read-module (refresh)
;;   (let ((minibuffer-local-completion-map geiser-completion--minibuffer-map)
;;         (modules (geiser-completion--modules refresh))
;;         (prompt "Module name: "))
;;     (if modules
;;         (completing-read prompt modules nil nil nil geiser-completion--module-history)
;;       (read-string prompt nil geiser-completion--module-history))))


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


;;; Completion functionality:

(defsubst geiser-completion--symbol-list (prefix)
  (geiser-eval--send/result `(:gs ((:ge completions) ,prefix))))

(defvar geiser-completion--symbol-list-func
  (completion-table-dynamic 'geiser-completion--symbol-list))

(defun geiser-completion--complete (prefix modules)
  (let* ((symbols (if modules nil ;; (geiser-completion--modules)
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

(defun geiser--respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))

(defun geiser-completion--complete-symbol ()
  "Complete the symbol at point.
Perform completion similar to Emacs' complete-symbol."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (skip-syntax-backward "^-()") (point)))
         (prefix (buffer-substring-no-properties beg end))
         (result (geiser-completion--complete prefix nil))
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


(provide 'geiser-completion)
;;; geiser-completion.el ends here
