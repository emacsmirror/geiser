;; geiser-popup.el -- popup windows

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Feb 07, 2009 14:05



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


;;; Support for defining popup buffers and accessors:

(defvar geiser-popup--registry nil)

(defmacro geiser-popup--define (base name mode)
  (let ((get-buff (intern (format "geiser-%s--buffer" base)))
        (pop-buff (intern (format "geiser-%s--pop-to-buffer" base)))
        (with-macro (intern (format "geiser-%s--with-buffer" base)))
        (method (make-symbol "method")))
  `(progn
     (add-to-list 'geiser-popup--registry ,name)
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


;;; Reload support:

(defun geiser-popup-unload-function ()
  (dolist (name geiser-popup--registry)
    (when (buffer-live-p (get-buffer name))
      (kill-buffer name))))


(provide 'geiser-popup)
;;; geiser-popup.el ends here
