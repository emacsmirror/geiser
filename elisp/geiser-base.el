;;; geiser-base.el --- shared bits

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

;; Settings and vars shared by all geiser modules, including little
;; utilities and emacsen compatibility bits.

;;; Code:

;;; Emacs compatibility:

(eval-when-compile (require 'cl))

(eval-after-load "ring"
  '(when (not (fboundp 'ring-member))
     (defun ring-member (ring item)
       (catch 'found
         (dotimes (ind (ring-length ring) nil)
           (when (equal item (ring-ref ring ind))
             (throw 'found ind)))))))

(when (not (fboundp 'completion-table-dynamic))
  (defun completion-table-dynamic (fun)
    (require 'cl)
    (lexical-let ((fun fun))
      (lambda (string pred action)
        (with-current-buffer (let ((win (minibuffer-selected-window)))
                               (if (window-live-p win) (window-buffer win)
                                 (current-buffer)))
          (complete-with-action action (funcall fun string) string pred))))))

(when (not (fboundp 'looking-at-p))
  (defsubst looking-at-p (regexp)
    (let ((inhibit-changing-match-data t))
      (looking-at regexp))))


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


(provide 'geiser-base)
;;; geiser-base.el ends here
