;;; geiser-eval.el --- Sending scheme code for evaluation  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2021, 2023 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Feb 07, 2009 22:35

;;; Commentary:

;; Functions, building on top of geiser-connection, to evaluate scheme
;; code.


;;; Code:

(require 'geiser-impl)
(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-log)
(require 'geiser-base)


;;; Plug-able functions:

(defvar-local geiser-eval--get-module-function nil)

(defvar geiser-eval--get-impl-module nil)
(geiser-impl--register-local-method
 'geiser-eval--get-impl-module 'find-module '(lambda (&rest args) nil)
 "Function used to obtain the module for current buffer. It takes
an optional argument, for cases where we want to force its
value.")

(defun geiser-eval--get-module (&optional module)
  (cond (geiser-eval--get-module-function
         (funcall geiser-eval--get-module-function module))
        (geiser-eval--get-impl-module
         (funcall geiser-eval--get-impl-module module))
        (t module)))

(defvar geiser-eval--geiser-procedure-function nil)
(geiser-impl--register-local-method
 'geiser-eval--geiser-procedure-function 'marshall-procedure 'identity
 "Function to translate a bare procedure symbol to one executable
in the Scheme context. Return NULL for unsupported ones; at the
very least, EVAL, COMPILE, LOAD-FILE and COMPILE-FILE should be
supported.  Geiser will also invoke, if defined, the following
procedures, always wrapped in EVAL (with the current module as
its context): AUTODOC, SYMBOL-DOCUMENTATION, MODULE-EXPORTS,
SYMBOL-LOCATION, MODULE-LOCATION, COMPLETIONS,
MODULE-COMPLETIONS, MACRO-EXPAND ADD-TO-LOAD-PATH, METHOD,
CALLER, CALLEE and NO-VALUES.")

(defvar geiser-eval--unsupported nil)
(geiser-impl--register-local-variable
 'geiser-eval--unsupported 'unsupported-procedures nil
 "A list, or function returning a list, of the Geiser procedures
not implemented by this Scheme implementation. Possible values
include macroexpand, completions, module-completions, find-file,
symbol-location, module-location, symbol-documentation,
module-exports, autodoc, callers, callees and generic-methods.")

(defun geiser-eval--supported-p (feat)
  (or (not geiser-eval--unsupported)
      (not (memq feat geiser-eval--unsupported))))

(defsubst geiser-eval--form (&rest args)
  (when (not (geiser-eval--supported-p (car args)))
    (error "Sorry, the %s scheme implementation does not support Geiser's %s"
           geiser-impl--implementation (car args)))
  (apply (or geiser-eval--geiser-procedure-function 'ignore) args))


;;; Code formatting:

(defsubst geiser-eval--load-file (file)
  (geiser-eval--form 'load-file (geiser-eval--scheme-str file)))

(defsubst geiser-eval--comp-file (file)
  (geiser-eval--form 'compile-file (geiser-eval--scheme-str file)))

(defsubst geiser-eval--module (code)
  (geiser-eval--scheme-str
   (cond ((or (null code) (eq code :t) (eq code :buffer))
          (geiser-eval--get-module))
         ((or (eq code :repl) (eq code :f)) :f)
         (t (geiser-eval--get-module code)))))

(defsubst geiser-eval--eval (code)
  (geiser-eval--form 'eval
                     (geiser-eval--module (nth 1 code))
                     (geiser-eval--scheme-str (nth 0 code))))

(defsubst geiser-eval--comp (code)
  (geiser-eval--form 'compile
                     (geiser-eval--module (nth 1 code))
                     (geiser-eval--scheme-str (nth 0 code))))

(defsubst geiser-eval--ge (proc args)
  (apply 'geiser-eval--form (cons proc (mapcar 'geiser-eval--scheme-str args))))

(defsubst geiser-eval--debug (args)
  (geiser-eval--ge 'debug args))

(defun geiser-eval--scheme-str (code)
  (cond ((null code) "'()")
        ((eq code :f) "#f")
        ((eq code :t) "#t")
        ((listp code)
         (cond ((eq (car code) :debug) (geiser-eval--debug (cdr code)))
               ((eq (car code) :eval) (geiser-eval--eval (cdr code)))
               ((eq (car code) :comp) (geiser-eval--comp (cdr code)))
               ((eq (car code) :load-file)
                (geiser-eval--load-file (cadr code)))
               ((eq (car code) :comp-file)
                (geiser-eval--comp-file (cadr code)))
               ((eq (car code) :module) (geiser-eval--module (cadr code)))
               ((eq (car code) :ge) (geiser-eval--ge (cadr code)
                                                     (cddr code)))
               ((eq (car code) :scm) (cadr code))
               (t (concat "("
                          (mapconcat 'geiser-eval--scheme-str code " ")
                          ")"))))
        ((symbolp code) (substring-no-properties (format "%s" code)))
        (t (substring-no-properties (format "%S" code)))))


;;; Code sending:

(defvar geiser-eval--default-connection-function nil)

(defsubst geiser-eval--connection ()
  (and geiser-eval--default-connection-function
       (funcall geiser-eval--default-connection-function)))

(defun geiser-eval--log (s)
  (geiser-log--info "RETORT: %S" s)
  s)

(defsubst geiser-eval--code-str (code)
  (if (stringp code) code (geiser-eval--scheme-str code)))

(defvar geiser-eval--async-retort nil)
(defsubst geiser-eval--send (code cont &optional buffer)
  (setq geiser-eval--async-retort nil)
  (geiser-con--send-string (geiser-eval--connection)
                           (geiser-eval--code-str code)
                           (lambda (s)
                             (setq geiser-eval--async-retort (geiser-eval--log s))
                             (funcall cont s))
                           buffer))

(defun geiser-eval--wait (req timeout)
  (or (geiser-con--wait req timeout) geiser-eval--async-retort))

(defun geiser-eval--send/wait (code &optional timeout buffer)
  (let ((sync-retort nil))
    (geiser-con--send-string/wait (geiser-eval--connection)
                                  (geiser-eval--code-str code)
                                  (lambda (s)
                                    (setq sync-retort (geiser-eval--log s)))
                                  timeout
                                  buffer)
    sync-retort))

(defun geiser-eval-interrupt ()
  "Interrupt on-going evaluation, if any."
  (interactive)
  (geiser-con--interrupt (geiser-eval--connection)))


;;; Retort parsing:

(defsubst geiser-eval--retort-p (ret)
  (and (listp ret) (or (assoc 'error ret) (assoc 'result ret))))

(defsubst geiser-eval--retort-result (ret)
  (let ((values (cdr (assoc 'result ret))))
    (car (geiser-syntax--read-from-string (car values)))))

(defsubst geiser-eval--send/result (code &optional timeout buffer)
  (geiser-eval--retort-result (geiser-eval--send/wait code timeout buffer)))

(defun geiser-eval--retort-result-str (ret prefix)
  (let* ((prefix (or prefix "=> "))
         (nlprefix (concat "\n" prefix))
         (values (cdr (assoc 'result ret))))
    (if values
        (concat prefix (mapconcat 'identity values nlprefix))
      (or prefix "(No value)"))))

(defsubst geiser-eval--retort-output (ret)
  (cdr (assq 'output ret)))

(defsubst geiser-eval--retort-error (ret)
  (cdr (assq 'error ret)))

(defsubst geiser-eval--error-key (err)
  (cdr (assq 'key err)))

(defsubst geiser-eval--error-subr (err)
  (cdr (assq 'subr err)))

(defsubst geiser-eval--error-msg (err)
  (cdr (assq 'msg err)))

(defsubst geiser-eval--error-rest (err)
  (cdr (assq 'rest err)))

(defun geiser-eval--error-str (err)
  (let* ((key (geiser-eval--error-key err))
         (key-str (if key (format ": %s" key) ":"))
         (subr (geiser-eval--error-subr err))
         (subr-str (if subr (format " (%s):" subr) ""))
         (msg (geiser-eval--error-msg err))
         (msg-str (if msg (format "\n  %s" msg) ""))
         (rest (geiser-eval--error-rest err))
         (rest-str (if rest (format "\n  %s" rest) "")))
    (format "Error%s%s%s%s" subr-str key-str msg-str rest-str)))



(provide 'geiser-eval)
