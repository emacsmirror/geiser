;;; geiser-repl.el --- Geiser's REPL

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.



(require 'geiser-company)
(require 'geiser-autodoc)
(require 'geiser-edit)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-connection)
(require 'geiser-menu)
(require 'geiser-custom)
(require 'geiser-base)

(require 'comint)
(require 'compile)
(require 'scheme)


;;; Customization:

(defgroup geiser-repl nil
  "Interacting with the Geiser REPL."
  :group 'geiser)

(geiser-custom--defcustom geiser-repl-use-other-window t
  "Whether to Use a window other than the current buffer's when
switching to the Geiser REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-window-allow-split t
  "Whether to allow window splitting when switching to the Geiser
REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-history-filename (expand-file-name "~/.geiser_history")
  "File where REPL input history is saved, so that it persists between sessions.
This is actually the base name: the concrete Scheme
implementation name gets appended to it."
  :type 'filename
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-history-size comint-input-ring-size
  "Maximum size of the saved REPL input history."
  :type 'integer
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-history-no-dups-p t
   "Whether to skip duplicates when recording history."
   :type 'boolean
   :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-autodoc-p t
  "Whether to enable `geiser-autodoc-mode' in the REPL by default."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-company-p t
  "Whether to use company-mode for completion, if available."
  :group 'geiser-mode
  :type 'boolean)

(geiser-custom--defcustom geiser-repl-read-only-prompt-p t
  "Whether the REPL's prompt should be read-only."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-auto-indent-p t
  "Whether newlines for incomplete sexps are autoindented."
  :type 'boolean
  :group 'geiser-repl)


;;; Geiser REPL buffers and processes:

(defvar geiser-repl--repls nil)
(defvar geiser-repl--closed-repls nil)

(make-variable-buffer-local
 (defvar geiser-repl--repl nil))

(defsubst geiser-repl--set-this-buffer-repl (r)
  (setq geiser-repl--repl r))

(defun geiser-repl--live-p ()
  (and geiser-repl--repl
       (get-buffer-process geiser-repl--repl)))

(defun geiser-repl--repl/impl (impl &optional repls)
  (catch 'repl
    (dolist (repl (or repls geiser-repl--repls))
      (with-current-buffer repl
        (when (eq geiser-impl--implementation impl)
          (throw 'repl repl))))))

(defun geiser-repl--set-up-repl (impl)
  (or (and (not impl) geiser-repl--repl)
      (setq geiser-repl--repl
            (let ((impl (or impl
                            geiser-impl--implementation
                            (geiser-impl--guess))))
              (when impl (geiser-repl--repl/impl impl))))))

(defun geiser-repl--active-impls ()
  (let ((act))
    (dolist (repl geiser-repl--repls act)
      (with-current-buffer repl
        (add-to-list 'act geiser-impl--implementation)))))

(defsubst geiser-repl--repl-name (impl)
  (format "%s REPL" (geiser-impl--impl-str impl)))

(defun geiser-repl--to-repl-buffer (impl)
  (unless (and (eq major-mode 'geiser-repl-mode)
               (not (get-buffer-process (current-buffer))))
    (let* ((old (geiser-repl--repl/impl impl geiser-repl--closed-repls))
           (old (and (buffer-live-p old)
                     (not (get-buffer-process old))
                     old)))
      (pop-to-buffer
       (or old
           (generate-new-buffer (format "* %s *"
                                        (geiser-repl--repl-name impl)))))))
  (geiser-repl-mode)
  (geiser-impl--set-buffer-implementation impl))

(geiser-impl--define-caller geiser-repl--binary binary ()
  "A variable or function returning the path to the scheme binary
for this implementation.")

(geiser-impl--define-caller geiser-repl--arglist arglist ()
  "A function taking no arguments and returning a list of
arguments to be used when invoking the scheme binary.")

(geiser-impl--define-caller geiser-repl--prompt-regexp prompt-regexp ()
  "A variable (or thunk returning a value) giving the regular
expression for this implementation's scheme prompt.")

(geiser-impl--define-caller
    geiser-repl--debugger-prompt-regexp debugger-prompt-regexp ()
  "A variable (or thunk returning a value) giving the regular
expression for this implementation's debugging prompt.")

(geiser-impl--define-caller
    geiser-repl--debugger-preamble-regexp debugger-preamble-regexp ()
  "A variable (or thunk returning a value) used to determine whether
the REPL has entered debugging mode.")

(geiser-impl--define-caller geiser-repl--startup startup ()
  "Function taking no parameters that is called after the REPL
has been initialised. All Geiser functionality is available to
you at that point.")

(defun geiser-repl--start-repl (impl)
  (message "Starting Geiser REPL for %s ..." impl)
  (geiser-repl--to-repl-buffer impl)
  (let ((binary (geiser-repl--binary impl))
        (args (geiser-repl--arglist impl))
        (prompt-rx (geiser-repl--prompt-regexp impl))
        (deb-prompt-rx (geiser-repl--debugger-prompt-regexp impl))
        (deb-preamble-rx (geiser-repl--debugger-preamble-regexp impl))
        (cname (geiser-repl--repl-name impl)))
    (unless (and binary prompt-rx)
      (error "Sorry, I don't know how to start a REPL for %s" impl))
    (set (make-local-variable 'comint-prompt-regexp) prompt-rx)
    (apply 'make-comint-in-buffer
           `(,cname ,(current-buffer) ,binary nil ,@args))
    (geiser-repl--wait-for-prompt 10000)
    (geiser-repl--history-setup)
    (geiser-con--setup-connection (current-buffer)
                                  prompt-rx
                                  deb-prompt-rx
                                  deb-preamble-rx)
    (add-to-list 'geiser-repl--repls (current-buffer))
    (geiser-repl--set-this-buffer-repl (current-buffer))
    (geiser-repl--startup impl)))

(defun geiser-repl--process ()
  (let ((buffer (geiser-repl--set-up-repl geiser-impl--implementation)))
    (or (and (buffer-live-p buffer) (get-buffer-process buffer))
        (error "No Geiser REPL for this buffer (try M-x run-geiser)"))))

(setq geiser-eval--default-proc-function 'geiser-repl--process)

(defun geiser-repl--wait-for-prompt (timeout)
  (let ((p (point)) (seen) (buffer (current-buffer)))
    (while (and (not seen)
                (> timeout 0)
                (get-buffer-process buffer))
      (sleep-for 0.1)
      (setq timeout (- timeout 100))
      (goto-char p)
      (setq seen (re-search-forward comint-prompt-regexp nil t)))
    (goto-char (point-max))
    (unless seen (error "No prompt found!"))))


;;; Interface: starting and interacting with geiser REPL:

(defun geiser-repl--read-impl (prompt &optional active)
  (geiser-impl--read-impl prompt (and active (geiser-repl--active-impls))))

(defsubst geiser-repl--only-impl-p ()
  (and (null (cdr geiser-active-implementations))
       (car geiser-active-implementations)))

(defun run-geiser (impl)
  "Start a new Geiser REPL."
  (interactive
   (list (or (geiser-repl--only-impl-p)
             (and (eq major-mode 'geiser-repl-mode)
                  geiser-impl--implementation)
             (geiser-repl--read-impl
              "Start Geiser for scheme implementation: "))))
   (geiser-repl--start-repl impl))

(make-variable-buffer-local
 (defvar geiser-repl--last-scm-buffer nil))

(defun switch-to-geiser (&optional ask impl buffer)
  "Switch to running Geiser REPL.
With prefix argument, ask for which one if more than one is running.
If no REPL is running, execute `run-geiser' to start a fresh one."
  (interactive "P")
  (let* ((impl (or impl geiser-impl--implementation))
         (repl (cond ((and (not ask) (not impl)
                           (or geiser-repl--repl (car geiser-repl--repls))))
                     ((and (not ask) impl (geiser-repl--repl/impl impl)))))
         (pop-up-windows geiser-repl-window-allow-split))
    (cond ((and (eq (current-buffer) repl)
                (buffer-live-p geiser-repl--last-scm-buffer))
           (pop-to-buffer geiser-repl--last-scm-buffer))
          (repl (pop-to-buffer repl))
          (t (run-geiser (or impl
                             (and (not ask)
                                  (geiser-repl--only-impl-p))
                             (geiser-repl--read-impl
                              "Switch to scheme REPL: ")))))
    (when (and buffer (eq major-mode 'geiser-repl-mode))
      (setq geiser-repl--last-scm-buffer buffer))))

(defalias 'geiser 'switch-to-geiser)

(defun geiser-repl--send (cmd)
  (when (and cmd (eq major-mode 'geiser-repl-mode))
    (goto-char (point-max))
    (comint-kill-input)
    (insert cmd)
    (let ((comint-input-filter (lambda (x) nil)))
      (comint-send-input nil t))))

(geiser-impl--define-caller geiser-repl--enter-cmd enter-command (module)
  "Function taking a module designator and returning a REPL enter
module command as a string")

(defun switch-to-geiser-module (&optional module buffer)
  "Switch to running Geiser REPL and try to enter a given module."
  (interactive)
  (let* ((module (or module
                     (geiser-completion--read-module "Switch to module: ")))
         (cmd (and module
                   (geiser-repl--enter-cmd geiser-impl--implementation
                                           module))))
    (switch-to-geiser nil nil buffer)
    (geiser-repl--send cmd)))

(geiser-impl--define-caller geiser-repl--import-cmd import-command (module)
  "Function taking a module designator and returning a REPL import
module command as a string")

(defun geiser-repl-import-module (&optional module)
  "Import a given module in the current namespace."
  (interactive)
  (let* ((module (or module
                     (geiser-completion--read-module "Import module: ")))
         (cmd (and module
                   (geiser-repl--import-cmd geiser-impl--implementation
                                            module))))
    (switch-to-geiser)
    (geiser-repl--send cmd)))

(defun geiser-repl-nuke ()
  "Try this command if the REPL becomes unresponsive."
  (interactive)
  (goto-char (point-max))
  (comint-kill-region comint-last-input-start (point))
  (comint-redirect-cleanup)
  (geiser-con--setup-connection (current-buffer)
                                comint-prompt-regexp
                                geiser-con--debugging-prompt-regexp
                                geiser-con--debugging-preamble-regexp))


;;; REPL history and clean-up:

(defsubst geiser-repl--history-file ()
  (format "%s.%s" geiser-repl-history-filename geiser-impl--implementation))

(defun geiser-repl--on-quit ()
  (comint-write-input-ring)
  (let ((cb (current-buffer))
        (impl geiser-impl--implementation)
        (comint-prompt-read-only nil))
    (setq geiser-repl--repls (remove cb geiser-repl--repls))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (eq geiser-impl--implementation impl)
                     (equal cb geiser-repl--repl))
            (geiser-repl--set-up-repl geiser-impl--implementation)))))))

(defun geiser-repl--sentinel (proc event)
  (let ((pb (process-buffer proc)))
    (when (buffer-live-p pb)
      (with-current-buffer pb
        (let ((comint-prompt-read-only nil)
              (comint-input-ring-file-name (geiser-repl--history-file)))
          (geiser-repl--on-quit)
          (push pb geiser-repl--closed-repls)
          (when (buffer-name (current-buffer))
            (comint-kill-region comint-last-input-start (point))
            (insert "\nIt's been nice interacting with you!\n")
            (insert "Press C-c C-z to bring me back.\n" )))))))

(defun geiser-repl--on-kill ()
  (geiser-repl--on-quit)
  (setq geiser-repl--closed-repls
        (remove (current-buffer) geiser-repl--closed-repls)))

(defun geiser-repl--input-filter (str)
  (not (or (geiser-con--is-debugging)
           (string-match "^\\s *$" str)
           (string-match "^,quit *$" str))))

(defun geiser-repl--old-input ()
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun geiser-repl--history-setup ()
  (set (make-local-variable 'comint-input-ring-file-name)
       (geiser-repl--history-file))
  (set (make-local-variable 'comint-input-ring-size) geiser-repl-history-size)
  (set (make-local-variable 'comint-input-filter) 'geiser-repl--input-filter)
  (set (make-local-variable 'comint-get-old-input) 'geiser-repl--old-input)
  (add-hook 'kill-buffer-hook 'geiser-repl--on-kill nil t)
  (comint-read-input-ring t)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'geiser-repl--sentinel))


;;; geiser-repl mode:

(defun geiser-repl--bol ()
  (interactive)
  (when (= (point) (comint-bol)) (beginning-of-line)))

(defun geiser-repl--beginning-of-defun ()
  (save-restriction
    (when comint-last-prompt-overlay
      (narrow-to-region (overlay-end comint-last-prompt-overlay) (point)))
    (let ((beginning-of-defun-function nil))
      (beginning-of-defun))))

(defun geiser-repl--module-function (&optional ignore) :f)

(defun geiser-repl--doc-module ()
  (interactive)
  (let ((geiser-eval--get-module-function
         (geiser-impl--method 'find-module geiser-impl--implementation)))
    (geiser-doc-module)))

(defun geiser-repl--newline-and-indent ()
  (interactive)
  (save-restriction
    (narrow-to-region comint-last-input-start (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun geiser-repl--nesting-level ()
  (let ((begin (if comint-last-prompt-overlay
                   (overlay-end comint-last-prompt-overlay)
                 (save-excursion (geiser-repl--bol) (point)))))
    (save-restriction
      (narrow-to-region begin (point-max))
      (geiser-syntax--nesting-level))))

(defun geiser-repl--send-input ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc)))
         (intxt (and pmark (buffer-substring pmark (point)))))
    (when intxt
      (comint-send-input)
      (when (string-match "^\\s-*$" intxt)
        (comint-send-string proc
                            (geiser-eval--scheme-str '((:ge no-values))))
        (comint-send-string proc "\n")))))

(defun geiser-repl--maybe-send ()
  (interactive)
  (let ((p (point)))
    (end-of-line)
    (if (<= (geiser-repl--nesting-level) 0)
        (geiser-repl--send-input)
      (goto-char p)
      (if geiser-repl-auto-indent-p
          (geiser-repl--newline-and-indent)
        (insert "\n")))))

(define-derived-mode geiser-repl-mode comint-mode "REPL"
  "Major mode for interacting with an inferior scheme repl process.
\\{geiser-repl-mode-map}"
  (scheme-mode-variables)
  (set (make-local-variable 'mode-line-process) nil)
  (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'comint-prompt-read-only)
       geiser-repl-read-only-prompt-p)
  (set (make-local-variable 'beginning-of-defun-function)
       'geiser-repl--beginning-of-defun)
  (set (make-local-variable 'comint-input-ignoredups)
       geiser-repl-history-no-dups-p)
  (setq geiser-eval--get-module-function 'geiser-repl--module-function)
  (when geiser-repl-autodoc-p (geiser-autodoc-mode 1))
  (setq geiser-autodoc--inhibit-function 'geiser-con--is-debugging)
  (geiser-company--setup geiser-repl-company-p)
  (setq geiser-smart-tab-mode-string "")
  ;; enabling compilation-shell-minor-mode without the annoying highlighter
  (compilation-setup t))

(define-key geiser-repl-mode-map "\C-d" 'delete-char)
(define-key geiser-repl-mode-map "\C-m" 'geiser-repl--maybe-send)
(define-key geiser-repl-mode-map [return] 'geiser-repl--maybe-send)
(define-key geiser-repl-mode-map "\C-j" 'geiser-repl--newline-and-indent)

(define-key geiser-repl-mode-map "\C-a" 'geiser-repl--bol)
(define-key geiser-repl-mode-map (kbd "<home>") 'geiser-repl--bol)

(geiser-menu--defmenu repl geiser-repl-mode-map
  ("Complete symbol" ((kbd "TAB") (kbd "M-TAB"))
   geiser-completion--complete-symbol :enable (symbol-at-point))
  ("Complete module name" ((kbd "C-.") (kbd "M-`"))
   geiser-completion--complete-module :enable (symbol-at-point))
  ("Edit symbol" "\M-." geiser-edit-symbol-at-point
   :enable (symbol-at-point))
  --
  ("Switch to module..." "\C-c\C-m" switch-to-geiser-module)
  ("Import module..." "\C-c\C-i" geiser-repl-import-module)
  --
  ("Previous matching input" "\M-p" comint-previous-matching-input-from-input
   "Previous input matching current")
  ("Next matching input" "\M-n" comint-next-matching-input-from-input
   "Next input matching current")
  ("Previous input" "\C-c\M-p" comint-previous-input)
  ("Next input" "\C-c\M-n" comint-next-input)
  --
  (mode "Autodoc mode" ("\C-c\C-da" "\C-c\C-d\C-a") geiser-autodoc-mode)
  ("Symbol documentation" ("\C-c\C-dd" "\C-c\C-d\C-d")
   geiser-doc-symbol-at-point
   "Documentation for symbol at point" :enable (symbol-at-point))
  ("Module documentation" ("\C-c\C-dm" "\C-c\C-d\C-m") geiser-repl--doc-module
   "Documentation for module at point" :enable (symbol-at-point))
  --
  ("Kill Scheme interpreter" "\C-c\C-q" comint-kill-subjob
   :enable (geiser-repl--live-p))
  ("Restart" "\C-c\C-z" switch-to-geiser :enable (not (geiser-repl--live-p)))
  ("Revive REPL" "\C-c\C-k" geiser-repl-nuke
   "Use this command if the REPL becomes irresponsive"
   :enable (geiser-repl--live-p))
  --
  (custom "REPL options" geiser-repl))

(define-key geiser-repl-mode-map [menu-bar completion] 'undefined)


;;; Unload:

(defun geiser-repl--repl-list ()
  (let (lst)
    (dolist (repl geiser-repl--repls lst)
      (when (buffer-live-p repl)
        (with-current-buffer repl
          (push geiser-impl--implementation lst))))))

(defun geiser-repl--restore (impls)
  (dolist (impl impls)
    (when impl (run-geiser impl))))

(defun geiser-repl-unload-function ()
  (dolist (repl geiser-repl--repls)
    (when (buffer-live-p repl)
      (kill-buffer repl))))


(provide 'geiser-repl)
;;; geiser-repl.el ends here
