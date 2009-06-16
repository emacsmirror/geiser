;; geiser-doc.el -- accessing documentation

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Feb 14, 2009 14:09

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

;; Utilities for accessing docstrings and texinfo documentation.

;;; Code:

(require 'geiser-impl)
(require 'geiser-completion)
(require 'geiser-eval)
(require 'geiser-syntax)
(require 'geiser-popup)
(require 'geiser-custom)
(require 'geiser-base)

(require 'button)


;;; Customization:

(defgroup geiser-doc nil
  "Options for documentation buffers."
  :group 'geiser)

(geiser-custom--defface doc-title
  'bold geiser-doc "article titles in documentation buffers")

(geiser-custom--defface doc-link
  'link geiser-doc "links in documentation buffers")


;;; Documentation browser history:

(defvar geiser-doc-history-size 50)

(defun geiser-doc--make-history ()
  (list nil                                   ; current
        (make-ring geiser-doc-history-size)   ; previous
        (make-ring geiser-doc-history-size))) ; next

(defsubst geiser-doc--history-current ()
  (car geiser-doc--history))

(defun geiser-doc--history-push (link)
  (unless (or (null link) (equal link (car geiser-doc--history)))
    (let ((next (geiser-doc--history-next)))
      (unless (equal link next)
        (when next (geiser-doc--history-previous))
        (ring-insert (nth 1 geiser-doc--history) (car geiser-doc--history))
        (setcar geiser-doc--history link))))
  link)

(defun geiser-doc--history-next (&optional forget-current)
  (when (not (ring-empty-p (nth 2 geiser-doc--history)))
    (when (and (car geiser-doc--history) (not forget-current))
      (ring-insert (nth 1 geiser-doc--history) (car geiser-doc--history)))
    (setcar geiser-doc--history (ring-remove (nth 2 geiser-doc--history) 0))))

(defun geiser-doc--history-previous (&optional forget-current)
  (when (not (ring-empty-p (nth 1 geiser-doc--history)))
    (when (and (car geiser-doc--history) (not forget-current))
      (ring-insert (nth 2 geiser-doc--history) (car geiser-doc--history)))
    (setcar geiser-doc--history (ring-remove (nth 1 geiser-doc--history) 0))))

(defvar geiser-doc--history nil)
(setq geiser-doc--history (geiser-doc--make-history))


;;; Links

(defsubst geiser-doc--make-link (target module impl)
  (list target module impl))

(defsubst geiser-doc--link-target (link)
  (nth 0 link))

(defsubst geiser-doc--link-module (link)
  (nth 1 link))

(defsubst geiser-doc--link-impl (link)
  (nth 2 link))

(defun geiser-doc--follow-link (link)
  (let ((target (geiser-doc--link-target link))
        (module (geiser-doc--link-module link))
        (impl (or (geiser-doc--link-impl link)
                  (geiser-impl--default-implementation))))
    (when (and (or target module) impl)
      (with--geiser-implementation impl
        `(lambda () (if (null ',target)
                   (geiser-doc-module ',module ',impl)
                 (geiser-doc-symbol ',target ',module ',impl)))))))

(defun geiser-doc--button-action (button)
  (let ((link (button-get button 'geiser-link)))
    (when link (geiser-doc--follow-link link))))

(define-button-type 'geiser-doc--button
  'action 'geiser-doc--button-action
  'face 'geiser-font-lock-doc-link
  'follow-link t)

(defun geiser-doc--insert-button (target module impl)
  (let ((link (geiser-doc--make-link target module impl))
        (text (format "%s" target))
        (help (if module (format "%s in module %s" target module) "")))
    (insert-text-button text
                        :type 'geiser-doc--button
                        'geiser-link link
                        'help-echo help)))


;;; Auxiliary functions:

(defun geiser-doc--insert-title (title)
  (let ((p (point)))
    (if (not (listp title))
        (insert (format "%s" title))
      (insert "(" (format "%s" (car title)))
      (dolist (a (cdr title))
        (insert " " (if (eq a '\#:rest) "." (format "%s" a))))
      (insert ")"))
    (put-text-property p (point) 'face 'geiser-font-lock-doc-title)
    (newline)))

(defun geiser-doc--insert-list (title lst module impl)
  (when lst
    (geiser-doc--insert-title title)
    (newline)
    (dolist (w lst)
      (insert (format "\t- "))
      (geiser-doc--insert-button w module impl)
      (newline))
    (newline)))

(make-variable-buffer-local
 (defvar geiser-doc--buffer-link nil))


;;; Commands:

(defun geiser-doc--get-docstring (symbol module)
  (geiser-eval--send/result
   `(:eval ((:ge symbol-documentation) ',symbol) ,module)))

(defun geiser-doc--get-module-exports (module)
  (geiser-eval--send/result `(:eval ((:ge module-exports) (:module ,module)))))

(defun geiser-doc-symbol (symbol &optional module impl)
  (let ((module (or module (geiser-eval--get-module)))
        (impl (or impl geiser-impl--implementation)))
    (unless (geiser-impl--external-help impl symbol module)
      (let ((ds (geiser-doc--get-docstring symbol module)))
        (if (or (not ds) (not (listp ds)))
            (message "No documentation available for '%s'" symbol)
          (geiser-doc--with-buffer
            (erase-buffer)
            (geiser-doc--insert-title (cdr (assoc 'signature ds)))
            (newline)
            (insert (or (cdr (assoc 'docstring ds)) ""))
            (goto-line (point-min))
            (setq geiser-doc--buffer-link
                  (geiser-doc--history-push
                   (geiser-doc--make-link symbol module impl))))
          (geiser-doc--pop-to-buffer))))))

(defun geiser-doc-symbol-at-point (&optional arg)
  "Get docstring for symbol at point.
With prefix argument, ask for symbol (with completion)."
  (interactive "P")
  (let ((symbol (or (and (not arg) (symbol-at-point))
                    (geiser-completion--read-symbol "Symbol: "
                                                    (symbol-at-point)))))
    (when symbol (geiser-doc-symbol symbol))))


(defun geiser-doc-module (&optional module impl)
  "Display information about a given module."
  (interactive)
  (let* ((module (or module (geiser-completion--read-module)))
         (exports (geiser-doc--get-module-exports module))
         (impl (or impl geiser-impl--implementation)))
    (if (not exports)
        (message "No information available for %s" module)
      (geiser-doc--with-buffer
        (erase-buffer)
        (geiser-doc--insert-title (format "%s" module))
        (newline)
        (dolist (g '(("Procedures:" . procs)
                     ("Variables:" . vars)
                     ("Syntax:" . syntax)
                     ("Submodules:" . modules)))
          (geiser-doc--insert-list (car g)
                                   (cdr (assoc (cdr g) exports))
                                   module
                                   impl))
        (goto-char (point-min))
        (setq geiser-doc--buffer-link
              (geiser-doc--history-push
               (geiser-doc--make-link nil module impl))))
      (geiser-doc--pop-to-buffer))))

(defun geiser-doc-next (&optional forget-current)
  "Go to next page in documentation browser.
With prefix, the current page is deleted from history."
  (interactive "P")
  (let ((link (geiser-doc--history-next forget-current)))
    (unless link (error "No next page"))
    (geiser-doc--follow-link link)))

(defun geiser-doc-previous (&optional forget-current)
  "Go to previous page in documentation browser.
With prefix, the current page is deleted from history."
  (interactive "P")
  (let ((link (geiser-doc--history-previous forget-current)))
    (unless link (error "No previous page"))
    (geiser-doc--follow-link link)))

(defun geiser-doc-kill-page ()
  "Kill current page if a previous or next one exists."
  (interactive)
  (condition-case nil
      (geiser-doc-previous t)
    (error (geiser-doc-next t))))

(defun geiser-doc-refresh ()
  "Refresh the contents of current page."
  (interactive)
  (when geiser-doc--buffer-link
    (geiser-doc--follow-link geiser-doc--buffer-link)))

(defun geiser-doc-clean-history ()
  "Clean up the document browser history."
  (interactive)
  (when (y-or-n-p "Clean browsing history? ")
    (setq geiser-doc--history (geiser-doc--make-history))
    (geiser-doc-refresh))
  (message ""))


;;; Documentation browser and mode:

(defvar geiser-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map button-buffer-map)
    (define-key map "a" 'geiser-apropos)
    (define-key map "c" 'geiser-doc-clean-history)
    (define-key map "k" 'geiser-doc-kill-page)
    (define-key map "n" 'geiser-doc-next)
    (define-key map "l" 'geiser-doc-previous)
    (define-key map "p" 'geiser-doc-previous)
    (define-key map "r" 'geiser-doc-refresh)
    (define-key map (kbd "SPC")  'scroll-up)
    (define-key map (kbd "S-SPC") 'scroll-down)
    (define-key map "\M-." 'geiser-edit-symbol-at-point)
    (define-key map "\C-cz" 'run-geiser)
    (define-key map "\C-c\C-z" 'run-geiser)
    map))

(defun geiser-doc-mode ()
  "Major mode for browsing scheme documentation.
\\{geiser-doc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map geiser-doc-mode-map)
  (set-syntax-table scheme-mode-syntax-table)
  (setq mode-name "Geiser Doc")
  (setq major-mode 'geiser-doc-mode)
  (setq buffer-read-only t))

(geiser-popup--define doc "*Geiser documentation*" geiser-doc-mode)


(provide 'geiser-doc)
;;; geiser-doc.el ends here
