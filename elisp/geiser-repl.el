;;; geiser-repl.el --- Geiser's Guile REPL

;; Copyright (C) 2009  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode (comint-based) to interact with an inferior Guile
;; process using Geiser's modules.

;;; Code:

(require 'geiser-autodoc)
(require 'geiser-edit)
(require 'geiser-eval)
(require 'geiser-connection)
(require 'geiser-custom)
(require 'geiser-base)

(require 'comint)


;;; Customization:

(defgroup geiser-repl nil
  "Interacting with the Geiser REPL."
  :group 'geiser)

(defcustom geiser-repl-guile-binary
  (cond ((eq system-type 'windows-nt) "guile.exe")
        ((eq system-type 'darwin) "guile")
        (t "guile"))
  "Name to use to call the Guile executable when starting a REPL."
  :type 'string
  :group 'geiser-repl)

(defcustom geiser-repl-guile-init-file "~/.guile-geiser"
  "Initialization file with user code for the Guile REPL."
  :type 'string
  :group 'geiser-repl)

(defcustom geiser-repl-use-other-window t
  "Whether to Use a window other than the current buffer's when
switching to the Geiser REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(defcustom geiser-repl-window-allow-split t
  "Whether to allow window splitting when switching to the Geiser
REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(defcustom geiser-repl-history-filename (expand-file-name "~/.geiser_history")
  "File where REPL input history is saved, so that it persists between sessions."
  :type 'filename
  :group 'geiser-repl)

(defcustom geiser-repl-history-size comint-input-ring-size
  "Maximum size of the saved REPL input history."
  :type 'integer
  :group 'geiser-repl)

(defcustom geiser-repl-autodoc-p t
  "Whether to enable `geiser-autodoc-mode' in the REPL by default."
  :type 'boolean
  :group 'geiser-repl)


;;; REPL history:

(defun geiser-repl--sentinel (proc event)
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer proc)
      (let ((comint-input-ring-file-name geiser-repl-history-filename))
        (comint-write-input-ring)
        (when (buffer-name (current-buffer))
          (insert "\nIt's been nice interacting with you!\n")
          (insert "Press C-cz to bring me back.\n" ))))))

(defun geiser-repl--input-filter (str)
  (and (not (string-match "^\\s *$" str))
       (not (string-match "^,quit *$" str))))

(defun geiser-repl--history-setup ()
  (set (make-local-variable 'comint-input-ring-file-name) geiser-repl-history-filename)
  (set (make-local-variable 'comint-input-ring-size) geiser-repl-history-size)
  (set (make-local-variable 'comint-input-filter) 'geiser-repl--input-filter)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring nil t)
  (comint-read-input-ring t)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'geiser-repl--sentinel))


;;; Geiser REPL buffer/process:

(defvar geiser-repl--buffer nil
  "The buffer in which the Guile REPL is running.")

(defconst geiser-repl--prompt-regex "^[^() \n]+@([^)]*?)> ")

(defun geiser-repl--buffer ()
  (if (buffer-live-p geiser-repl--buffer) geiser-repl--buffer
    (with-current-buffer (get-buffer-create "*Geiser REPL*")
      (geiser-repl-mode)
      (setq geiser-repl--buffer (current-buffer)))))

(defun geiser-repl--start-process ()
  (let* ((guile geiser-repl-guile-binary)
         (args `("-q" "-L" ,(concat geiser-scheme-dir "/guile/")))
         (init-file (and geiser-repl-guile-init-file
                         (expand-file-name geiser-repl-guile-init-file)))
         (args (if (and init-file (file-readable-p init-file))
                   `(,@args "-l" ,init-file)
                 args)))
    (message "Starting Geiser REPL ...")
    (pop-to-buffer (geiser-repl--buffer))
    (apply 'make-comint-in-buffer `("Geiser REPL" ,(current-buffer) ,guile nil ,@args))
    (geiser-repl--wait-for-prompt 10000)
    (geiser-repl--history-setup)
    (geiser-con--setup-connection (current-buffer) geiser-repl--prompt-regex)))

(defun geiser-repl--process (&optional start)
  (or (and (buffer-live-p (geiser-repl--buffer))
           (get-buffer-process (geiser-repl--buffer)))
      (if (not start)
          (error "No running Guile REPL (try M-x run-guile)")
        (geiser-repl--start-process)
        (geiser-repl--process))))

(setq geiser-eval--default-proc-function 'geiser-repl--process)

(defun geiser-repl--wait-for-prompt (timeout)
  (let ((p (point)) (seen))
    (while (and (not seen) (> timeout 0))
      (sleep-for 0.1)
      (setq timeout (- timeout 100))
      (goto-char p)
      (setq seen (re-search-forward comint-prompt-regexp nil t)))
    (goto-char (point-max))
    (unless seen (error "No prompt found!"))))


;;; Interface: starting and interacting with geiser REPL:

(defalias 'switch-to-guile 'run-guile)
(defalias 'switch-to-geiser-repl 'run-guile)

(defun run-guile (&optional arg)
  "Show the geiser-repl buffer, starting the process if needed."
  (interactive)
  (let ((buf (process-buffer (geiser-repl--process t)))
        (pop-up-windows geiser-repl-window-allow-split))
    (if geiser-repl-use-other-window
        (pop-to-buffer buf)
      (switch-to-buffer buf))))

(defun geiser-repl-nuke ()
  "Try this command if the REPL becomes unresponsive."
  (interactive)
  (goto-char (point-max))
  (comint-kill-region comint-last-input-start (point))
  (comint-redirect-cleanup)
  (geiser-con--setup-connection geiser-repl--buffer))


;;; geiser-repl mode:

(defun geiser-repl--bol ()
  (interactive)
  (when (= (point) (comint-bol)) (beginning-of-line)))

(defun geiser-repl--beginning-of-defun ()
  (let ((p (point)))
    (comint-bol)
    (when (not (eq (char-after (point)) ?\())
      (skip-syntax-forward "^(" p))))

(define-derived-mode geiser-repl-mode comint-mode "Geiser REPL"
  "Major mode for interacting with an inferior Guile repl process.
\\{geiser-repl-mode-map}"
  (set (make-local-variable 'mode-line-process) nil)
  (set (make-local-variable 'comint-prompt-regexp) geiser-repl--prompt-regex)
  (set (make-local-variable 'comint-use-prompt-regexp) t)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'beginning-of-defun-function)
       'geiser-repl--beginning-of-defun)
  (set-syntax-table scheme-mode-syntax-table)
  (when geiser-repl-autodoc-p (geiser-autodoc-mode 1)))

(define-key geiser-repl-mode-map "\C-cz" 'run-guile)
(define-key geiser-repl-mode-map "\C-c\C-z" 'run-guile)
(define-key geiser-repl-mode-map "\C-a" 'geiser-repl--bol)
(define-key geiser-repl-mode-map "\C-ca" 'geiser-autodoc-mode)
(define-key geiser-repl-mode-map "\C-cd" 'geiser-doc-symbol-at-point)
(define-key geiser-repl-mode-map "\C-cm" 'geiser-doc-module)
(define-key geiser-repl-mode-map "\C-ck" 'geiser-compile-file)
(define-key geiser-repl-mode-map "\C-cl" 'geiser-load-file)

(define-key geiser-repl-mode-map "\M-p" 'comint-previous-matching-input-from-input)
(define-key geiser-repl-mode-map "\M-n" 'comint-next-matching-input-from-input)
(define-key geiser-repl-mode-map "\C-c\M-p" 'comint-previous-input)
(define-key geiser-repl-mode-map "\C-c\M-n" 'comint-next-input)

(define-key geiser-repl-mode-map (kbd "TAB") 'geiser-completion--complete-symbol)
(define-key geiser-repl-mode-map (kbd "M-TAB") 'geiser-completion--complete-symbol)
(define-key geiser-repl-mode-map (kbd "C-.") 'geiser-completion--complete-module)
(define-key geiser-repl-mode-map "\M-." 'geiser-edit-symbol-at-point)
(define-key geiser-repl-mode-map "\M-," 'geiser-edit-pop-edit-symbol-stack)



(provide 'geiser-repl)
;;; geiser-repl.el ends here
