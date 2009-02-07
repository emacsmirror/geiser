;; geiser-popup.el -- popup windows

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Feb 07, 2009 14:05

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

;; Utilities for defining pop-up windows that get easily out of the
;; way.

;;; Code:

(make-variable-buffer-local
 (defvar geiser-popup--created-window nil))

(make-variable-buffer-local
 (defvar geiser-popup--selected-window nil))

(defun geiser-popup--display (&optional buffer)
  (when buffer (set-buffer buffer))
  (let ((selected-window (selected-window))
        (buffer (current-buffer)))
    (unless (eq selected-window (get-buffer-window buffer))
      (let ((windows))
        (walk-windows (lambda (w) (push w windows)) nil t)
        (prog1 (pop-to-buffer buffer)
          (set (make-local-variable 'geiser-popup--created-window)
               (unless (memq (selected-window) windows) (selected-window)))
          (set (make-local-variable 'geiser-popup--selected-window)
               selected-window))))))

(defun geiser-popup--quit ()
  (interactive)
  (let ((selected geiser-popup--selected-window)
        (created geiser-popup--created-window))
    (bury-buffer)
    (when (eq created (selected-window)) (delete-window created))
    (when (window-live-p selected) (select-window selected))))

(define-minor-mode geiser-popup-mode
  "Mode for displaying read only stuff"
  nil nil
  '(("q" . geiser-popup--quit))
  (setq buffer-read-only t))

(defmacro geiser-popup--define (base name mode)
  (let ((get-buff (intern (format "geiser-%s--buffer" base)))
        (pop-buff (intern (format "geiser-%s--pop-to-buffer" base)))
        (with-macro (intern (format "geiser-%s--with-buffer" base)))
        (method (make-symbol "method")))
  `(progn
     (defun ,get-buff ()
       (or (get-buffer ,name)
           (with-current-buffer (get-buffer-create ,name)
             (,mode)
             (geiser-popup-mode)
             (current-buffer))))
     (defun ,pop-buff (&optional ,method)
       (cond ((eq ,method 'buffer) (switch-to-buffer (,get-buff)))
             ((eq ,method 'frame) (switch-to-buffer-other-frame (,get-buff)))
             (t (geiser-popup--display (,get-buff)))))
     (defmacro ,with-macro (&rest body)
       (let ((buff ',get-buff))
         `(with-current-buffer (funcall ',buff)
            (let ((inhibit-read-only t))
              ,@body))))
     (put ',with-macro 'lisp-indent-function 'defun))))

(put 'geiser-popup--define 'lisp-indent-function 1)


(provide 'geiser-popup)
;;; geiser-popup.el ends here
