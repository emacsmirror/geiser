;;; geiser-inf.el -- inferior scheme processes

;; Copyright (c) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Thu Nov 11, 2010 01:04


(require 'geiser-impl)
(require 'geiser-base)

(require 'cmuscheme)


;; Implementation-dependent parameters

(geiser-impl--define-caller geiser-inf--binary binary ()
  "A variable or function returning the path to the scheme binary
for this implementation.")

(geiser-impl--define-caller geiser-inf--arglist arglist ()
  "A function taking no arguments and returning a list of
arguments to be used when invoking the scheme binary.")

(geiser-impl--define-caller geiser-inf--prompt-regexp prompt-regexp ()
  "A variable (or thunk returning a value) giving the regular
expression for this implementation's scheme prompt.")

(geiser-impl--define-caller geiser-inf--init-server-cmd init-server-cmd ()
  "A variable (or thunk returning a value) giving the REPL server
initialization command for local processes. The command must return a
list of the form (server PORT).")


;; Auxiliary functions

(defun geiser-inf--wait-for-prompt (timeout)
  (let ((p (point)) (seen) (buffer (current-buffer)))
    (while (and (not seen)
                (> timeout 0)
                (get-buffer-process buffer))
      (sleep-for 0.1)
      (setq timeout (- timeout 100))
      (goto-char p)
      (setq seen (re-search-forward comint-prompt-regexp nil t)))
    (goto-char (point-max))
    (unless seen (error "%s" "No prompt found!"))))

(defun geiser-inf--make-buffer (impl)
  (with-current-buffer (generate-new-buffer (format "* inferior %s *" impl))
    (inferior-scheme-mode)
    (current-buffer)))


;; Starting an inferior REPL

(defun geiser-inf--run-scheme (impl)
  (let ((bin (geiser-inf--binary impl))
        (args (geiser-inf--arglist impl))
        (prompt-rx (geiser-inf--prompt-regexp impl)))
    (unless (and bin args prompt-rx)
      (error "Sorry, I don't know how to start %s" impl))
    (with-current-buffer (geiser-inf--make-buffer impl)
      (setq comint-prompt-regexp prompt-rx)
      (condition-case err
          (apply 'make-comint-in-buffer
             `(,(buffer-name) ,(current-buffer) ,bin nil ,@args))
        (error (error "Unable to start REPL: %s" (error-message-string err))))
      (geiser-inf--wait-for-prompt 10000)
      (cons (current-buffer)
            (comint-redirect-results-list (geiser-inf--init-server-cmd impl)
                                          "(server-port \\([0-9]\\)+)"
                                          1)))))




(provide 'geiser-inf)

