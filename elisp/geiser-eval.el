;; geiser-eval.el -- sending scheme code for evaluation

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Feb 07, 2009 22:35

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

;; Functions, building on top of geiser-connection, to evaluate scheme
;; code.

;;; Code:

(require 'geiser-connection)
(require 'geiser-log)
(require 'geiser-base)


;;; Code formatting:

(defun geiser-eval--scheme-str (code)
  (cond ((null code) "'()")
        ((eq code :f) "#f")
        ((eq code :t) "#t")
        ((listp code)
         (cond ((eq (car code) :gs) (geiser-eval--gs (cdr code)))
               ((eq (car code) :scm) (cadr code))
               (t (concat "(" (mapconcat 'geiser-eval--scheme-str code " ") ")"))))
        (t (format "%S" code))))

(defsubst geiser-eval--gs (code)
  (concat "((@ (geiser eval) eval-in) (quote "
          (geiser-eval--scheme-str (nth 0 code))
          ") (quote "
          (or (and (nth 1 code)
                   (geiser-eval--scheme-str (nth 1 code)))
              (geiser-eval--buffer-module))
          "))"))

;;; Current module:

(defun geiser-eval--buffer-module (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "(define-module +\\(([^)]+)\\)" nil t)
            (match-string-no-properties 1)
          "#f")))))


;;; Code sending:

(defvar geiser-eval--default-proc-function nil)

(defsubst geiser-eval--default-proc ()
  (and geiser-eval--default-proc-function
       (funcall geiser-eval--default-proc-function)))

(defvar geiser-eval--proc nil)

(defsubst geiser-eval--proc ()
  (or geiser-eval--proc (geiser-eval--default-proc)))

(defsubst geiser-eval--log (s)
  (geiser-log--info "RETORT: %S" s)
  s)

(defsubst geiser-eval--code-str (code)
  (if (stringp code) code (geiser-eval--scheme-str code)))

(defvar geiser-eval--sync-retort nil)
(defun geiser-eval--set-sync-retort (s)
  (setq geiser-eval--sync-retort (geiser-eval--log s)))

(defun geiser-eval--send/wait (code &optional timeout buffer)
  (setq geiser-eval--sync-retort nil)
  (geiser-con--send-string/wait (geiser-eval--proc)
                                (geiser-eval--code-str code)
                                'geiser-eval--set-sync-retort
                                timeout
                                buffer)
  geiser-eval--sync-retort)

(defsubst geiser-eval--send (code cont &optional buffer)
  (geiser-con--send-string (geiser-eval--proc)
                           (geiser-eval--code-str code)
                           `(lambda (s) (,cont (geiser-eval--log s)))
                           buffer))


;;; Retort parsing:

(defsubst geiser-eval--retort-p (ret)
  (and (listp ret) (or (assoc 'error ret) (assoc 'result ret))))
(defsubst geiser-eval--retort-result (ret) (cdr (assoc 'result ret)))
(defsubst geiser-eval--retort-output (ret) (cdr (assoc 'output ret)))
(defsubst geiser-eval--retort-error (ret) (cdr (assoc 'error ret)))

(defsubst geiser-eval--error-key (err) (cdr (assoc 'key err)))
(defsubst geiser-eval--error-subr (err) (cdr (assoc 'subr err)))
(defsubst geiser-eval--error-msg (err) (cdr (assoc 'msg err)))
(defsubst geiser-eval--error-rest (err) (cdr (assoc 'rest err)))


(provide 'geiser-eval)
;;; geiser-eval.el ends here
