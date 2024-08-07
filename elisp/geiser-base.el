;;; geiser-base.el --- Shared bits  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2010, 2012, 2013, 2015, 2016, 2019, 2024  Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;; Commentary:

;; Settings and variables shared by all geiser modules, including
;; little utilities and emacsen compatibility bits.


;;; Code:
;;; Emacs compatibility:

(require 'ring)

(unless (fboundp 'ring-member)
  (defun ring-member (ring item)
    (catch 'found
      (dotimes (ind (ring-length ring))
        (when (equal item (ring-ref ring ind))
          (throw 'found ind))))))

(unless (fboundp 'looking-at-p)
  (defsubst looking-at-p (regexp)
    (with-no-warnings
      (let ((inhibit-changing-match-data t))
        (looking-at regexp)))))

(defalias 'geiser--font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      #'font-lock-ensure
    (with-no-warnings
      (lambda (&optional _beg _end)
        (when font-lock-mode
          (font-lock-fontify-buffer))))))

;;; Utilities:

(defsubst geiser--chomp (str)
  (if (string-match-p ".*\n$" str) (substring str 0 -1) str))

(defun geiser--shorten-str (str len &optional sep)
  (let ((str-len (length str)))
    (if (<= str-len len)
        str
      (let* ((sep (or sep " ... "))
             (sep-len (length sep))
             (prefix-len (/ (- str-len sep-len) 2))
             (prefix (substring str 0 prefix-len))
             (suffix (substring str (- str-len prefix-len))))
        (format "%s%s%s" prefix sep suffix)))))

(defun geiser--region-to-string (begin &optional end)
  (let ((end (or end (point))))
    (when (< begin end)
      (let* ((str (buffer-substring-no-properties begin end))
             (pieces (split-string str nil t)))
        (mapconcat 'identity pieces " ")))))

(defun geiser--insert-with-face (str face)
  (let ((p (point)))
    (insert str)
    (put-text-property p (point) 'face face)))

(defmacro geiser--save-msg (&rest body)
  (let ((msg (make-symbol "msg")))
    `(let ((,msg (current-message)))
       ,@body
       (message "%s" ,msg))))

(put 'geiser--save-msg 'lisp-indent-function 0)

(defun geiser--del-dups (lst)
  (let (result)
    (dolist (e lst (nreverse result))
      (unless (member e result) (push e result)))))

(defsubst geiser--symbol-at-point ()
  (let ((thing (thing-at-point 'symbol)))
    (and thing (make-symbol thing))))

(defun geiser--cut-version (v)
  (when (string-match "\\([0-9]+\\(?:\\.[0-9]+\\)*\\).*" v)
    (match-string 1 v)))

(defun geiser--version< (v1 v2)
  (let ((v1 (geiser--cut-version v1))
        (v2 (geiser--cut-version v2)))
    (and v1 v2 (version< v1 v2))))

(provide 'geiser-base)
