;;; geiser-menu.el -- menu and keymaps definition

;; Copyright (c) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Jun 12, 2010 03:01


(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(geiser-custom--defcustom geiser-global-menu-always-on-p nil
   "Whether the Geiser menu is always visible."
   :type 'boolean
   :group 'geiser)


;;; Top-level menu

(defmacro geiser-menu--add-items-1 (keymap map keys)
  `(progn ,@(mapcar (lambda (kd)
                      (let* ((title (nth 0 kd))
                             (binding (nth 1 kd))
                             (cmd (nth 2 kd))
                             (hlp (nth 3 kd))
                             (item (make-symbol title))
                             (hlp (and (stringp hlp) (list :help hlp)))
                             (rest (or (and hlp (nthcdr 4 kd))
                                       (nthcdr 3 kd)))
                             (binding (if (listp binding)
                                          binding
                                        (list binding))))
                        `(progn (define-key ,map [,item]
                                  '(menu-item ,title ,cmd ,@hlp ,@rest))
                                ,@(and binding
                                       `((put ',cmd
                                              :advertised-binding
                                              ,(car binding))))
                                ,@(mapcar (lambda (b)
                                            `(define-key ,keymap ,b ',cmd))
                                          binding))))
                    keys)))

(defmacro geiser-menu--add-items (keymap map keys)
  `(geiser-menu--add-items-1 ,keymap ,map ,(reverse keys)))

(defmacro geiser-menu--add-submenu (name keymap map keys)
  (let ((ev (make-symbol name))
        (map2 (make-symbol "map2")))
    `(progn
       (let ((,map2 (make-sparse-keymap ,name)))
         (define-key ,map [,ev] (cons ,name ,map2))
         (geiser-menu--add-items-1 ,keymap ,map2 ,keys)))))

(put 'geiser-menu--add-submenu 'lisp-indent-function 1)
(put 'geiser-menu--add-items 'lisp-indent-function 1)

(defvar geiser-menu--line-counter 0)

(defun geiser-menu--add-line (&optional keymap)
  (let ((line (make-symbol (format "line%s"
                                   (setq geiser-menu--line-counter
                                         (1+ geiser-menu--line-counter))))))
    (define-key (or keymap global-map) `[,line]
      `(menu-item "--single-line"))))

(defmacro geiser-menu--add-custom (title group keymap map)
  `(geiser-menu--add-items ,keymap ,map
     ((,title nil (lambda () (interactive) (customize-group ',group))))))

(defmacro geiser-menu--mode-toggle (title bindings mode keymap map)
  `(geiser-menu--add-items ,keymap ,map
     ((,title ,bindings ,mode
              :button (:toggle . (and (boundp ',mode) ,mode))))))

(defmacro geiser-menu--defmenu (e keymap &rest keys)
  (let* ((fs)
         (name (format "Geiser %s" e))
         (mmap (make-symbol "mmap")))
    (dolist (kd keys)
      (setq fs
            (cons (cond ((eq 'line kd) `(geiser-menu--add-line ,mmap))
                        ((stringp (car kd))
                         `(geiser-menu--add-items ,keymap ,mmap ,(list kd)))
                        ((eq 'menu (car kd))
                         `(geiser-menu--add-submenu ,(cadr kd)
                            ,keymap ,mmap ,(cddr kd)))
                        ((eq 'custom (car kd))
                         `(geiser-menu--add-custom ,(nth 1 kd)
                                                   ,(nth 2 kd)
                                                   ,keymap
                                                   ,mmap))
                        ((eq 'mode (car kd))
                         `(geiser-menu--mode-toggle ,(nth 1 kd)
                                                    ,(nth 2 kd)
                                                    ,(nth 3 kd)
                                                    ,keymap
                                                    ,mmap))
                        (t (error "Bad form: %s" kd)))
                  fs)))
    `(progn
       (let ((,mmap (make-sparse-keymap ,name)))
         (define-key ,keymap [menu-bar ,e] (cons ,name ,mmap))
         (define-key ,mmap [customize]
           (cons "Customize" geiser-menu--custom-customize))
         (define-key ,mmap [switch]
           (cons "Switch to" geiser-menu--custom-switch))
         (define-key ,mmap [Run] (cons "Run" geiser-menu--custom-run))
         (geiser-menu--add-line ,mmap)
         ,@fs))))

(put 'geiser-menu--defmenu 'lisp-indent-function 2)


;;; Shared entries

(defvar geiser-menu--custom-map (make-sparse-keymap "Geiser"))
(defvar geiser-menu--custom-run (make-sparse-keymap "Run"))
(defvar geiser-menu--custom-switch (make-sparse-keymap "Switch"))
(defvar geiser-menu--custom-customize (make-sparse-keymap "Customize"))

(define-key geiser-menu--custom-map [customize]
  (cons "Customize" geiser-menu--custom-customize))
(define-key geiser-menu--custom-map [switch]
  (cons "Switch to" geiser-menu--custom-switch))
(define-key geiser-menu--custom-map [run]
  (cons "Run" geiser-menu--custom-run))

(defun geiser-menu--add-global-custom (title group)
  (define-key geiser-menu--custom-customize `[,(make-symbol title)]
    (cons title `(lambda () (interactive) (customize-group ',group)))))

(defun geiser-menu--add-impl (name runner switcher)
  (let ((title (capitalize (format "%s" name)))
        (group (intern (format "geiser-%s" name))))
    (define-key geiser-menu--custom-run `[,name] (cons title runner))
    (define-key geiser-menu--custom-switch `[,name] (cons title switcher))
    (geiser-menu--add-global-custom title group)))

(geiser-menu--add-global-custom "Geiser" 'geiser)



(provide 'geiser-menu)
;;; geiser-menu.el ends here

