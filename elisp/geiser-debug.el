;;; geiser-debug.el -- displaying debug information and evaluation results

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Mon Feb 23, 2009 22:34



(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-popup)
(require 'geiser-base)


;;; Debug buffer mode:

(defvar geiser-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map button-buffer-map)
    map))

(defun geiser-debug-mode ()
  "A major mode for displaying Scheme compilation and evaluation results.
\\{geiser-debug-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map geiser-debug-mode-map)
  (set-syntax-table scheme-mode-syntax-table)
  (setq mode-name "Geiser DBG")
  (setq major-mode 'geiser-debug-mode)
  (setq buffer-read-only t))


;;; Buffer for displaying evaluation results:

(geiser-popup--define debug "*Geiser dbg*" geiser-debug-mode)


;;; Displaying retorts

(geiser-impl--define-caller geiser-debug--display-error
    display-error (module key message)
  "This method takes 3 parameters (a module name, the error key,
and the accompanying error message) and should display
(in the current buffer) a formatted version of the error. If the
error was successfully displayed, the call should evaluate to a
non-null value.")

(defun geiser-debug--display-retort (what ret &optional res)
  (let* ((err (geiser-eval--retort-error ret))
         (key (geiser-eval--error-key err))
         (output (geiser-eval--retort-output ret))
         (impl geiser-impl--implementation)
         (module (geiser-eval--get-module)))
    (if (eq key 'geiser-debugger)
        (progn
          (switch-to-geiser)
          (geiser-debug--display-error impl module key output))
      (geiser-debug--with-buffer
        (erase-buffer)
        (insert what)
        (newline 2)
        (when res
          (insert res)
          (newline 2))
        (unless (geiser-debug--display-error impl module key output)
          (when err (insert (geiser-eval--error-str err) "\n\n"))
          (when output (insert output "\n\n")))
        (goto-char (point-min)))
      (when err (geiser-debug--pop-to-buffer)))))

(defsubst geiser-debug--wrap-region (str)
  (format "(begin %s)" str))

(defun geiser-debug--unwrap (str)
  (if (string-match "(begin[ \t\n\v\r]+\\(.+\\)*)" str)
      (match-string 1 str)
    str))

(defun geiser-debug--send-region (compile start end and-go wrap)
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code `(,(if compile :comp :eval) (:scm ,wrapped)))
         (ret (geiser-eval--send/wait code))
         (res (geiser-eval--retort-result-str ret))
         (err (geiser-eval--retort-error ret)))
    (when and-go (funcall and-go))
    (when (not err) (message "%s" res))
    (geiser-debug--display-retort str ret res)))

(defun geiser-debug--expand-region (start end all wrap)
  (let* ((str (buffer-substring-no-properties start end))
         (wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code `(:eval ((:ge macroexpand) (quote (:scm ,wrapped))
                        ,(if all :t :f))))
         (ret (geiser-eval--send/wait code))
         (err (geiser-eval--retort-error ret))
         (result (geiser-eval--retort-result ret)))
    (if err
        (geiser-debug--display-retort str ret)
      (geiser-debug--with-buffer
        (erase-buffer)
        (insert (format "%s" (if wrap (geiser-debug--unwrap result) result)))
        (goto-char (point-min)))
      (geiser-debug--pop-to-buffer))))


(provide 'geiser-debug)
;;; geiser-debug.el ends here
