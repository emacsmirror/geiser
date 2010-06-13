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


;;; Visibility:

(make-variable-buffer-local
 (defvar geiser-menu--geiser-component-p nil))

(defun geiser-menu--visible-p ()
  (or geiser-global-menu-always-on-p geiser-menu--geiser-component-p))

(defun geiser-menu--provide ()
  (setq geiser-menu--geiser-component-p t))


;;; Top-level menu

(define-key global-map [menu-bar geiser]
  `(menu-item "Geiser" ,(make-sparse-keymap "Geiser")
              :visible (geiser-menu--visible-p)))

(add-to-list 'menu-bar-final-items 'geiser)

(defmacro geiser-menu--add-items-1 (ev keymap keys)
  `(progn ,@(mapcar (lambda (kd)
                      (let* ((ev (if (listp ev) ev (list ev)))
                             (title (nth 0 kd))
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
                        `(progn (define-key global-map
                                  [menu-bar geiser ,@ev ,item]
                                  '(menu-item ,title ,cmd ,@hlp ,@rest))
                                ,@(and binding
                                       `((put ',cmd
                                              :advertised-binding
                                              ,(car binding))))
                                ,@(mapcar (lambda (b)
                                            `(define-key ,keymap ,b ',cmd))
                                          binding))))
                    keys)))

(defmacro geiser-menu--add-items (keymap keys &optional visible)
  (let ((keys (if visible
                  (mapcar (lambda (k) (append k `(:visible ,visible))) keys)
                keys)))
    `(geiser-menu--add-items-1 nil ,keymap ,(reverse keys))))

(defmacro geiser-menu--add-submenu (name keymap visible keys)
  (let ((ev (make-symbol name)))
    `(progn
       (define-key global-map [menu-bar geiser ,ev]
         (list 'menu-item ,name ,keymap :visible ',visible))
       (geiser-menu--add-items-1 ,ev ,keymap ,keys))))

(put 'geiser-menu--add-submenu 'lisp-indent-function 3)
(put 'geiser-menu--add-items 'lisp-indent-function 1)

(defvar geiser-menu--line-counter 0)

(defun geiser-menu--add-line ()
  (let ((line (make-symbol (format "line%s"
                                   (setq geiser-menu--line-counter
                                         (1+ geiser-menu--line-counter))))))
    (define-key global-map `[menu-bar geiser ,line]
      `(menu-item "--single-line"))))

(defmacro geiser-menu--add-custom (title group keymap visible)
  `(geiser-menu--add-items ,keymap
     ((,title nil (lambda () (interactive) (customize-group ',group))))
     ,visible))

(defmacro geiser-menu--mode-toggle (title bindings mode keymap visible)
  `(geiser-menu--add-items ,keymap
     ((,title ,bindings ,mode :button (:toggle . (and (boundp ',mode) ,mode))))
     ,visible))

(defmacro geiser-menu--defmenu (keymap visible &rest keys)
  (let ((fs))
    (dolist (kd keys)
      (setq fs
            (cons (cond ((eq 'line kd) '(geiser-menu--add-line))
                        ((stringp (car kd))
                         `(geiser-menu--add-items ,keymap
                            ,(list kd) ,visible))
                        ((eq 'menu (car kd))
                         `(geiser-menu--add-submenu ,(cadr kd) ,keymap
                                                    ,visible ,(cddr kd)))
                        ((eq 'custom (car kd))
                         `(geiser-menu--add-custom ,(nth 1 kd)
                                                   ,(nth 2 kd)
                                                   ,keymap
                                                   ,visible))
                        ((eq 'mode (car kd))
                         `(geiser-menu--mode-toggle ,(nth 1 kd)
                                                    ,(nth 2 kd)
                                                    ,(nth 3 kd)
                                                    ,keymap
                                                    ,visible))
                        (t (error "Bad form: %s" kd)))
                  fs)))
    `(progn ,@fs)))

(put 'geiser-menu--defmenu 'lisp-indent-function 2)


;;; Implementation support

(define-key global-map [menu-bar geiser custom]
  (cons "Customize" (make-sparse-keymap "Customize")))

(defun geiser-menu--add-global-custom (title group)
  (define-key global-map `[menu-bar geiser custom ,(make-symbol title)]
    (cons title `(lambda () (interactive) (customize-group ',group)))))

(defun geiser-menu--add-impl (name runner switcher)
  (let ((title (capitalize (format "%s" name)))
        (group (intern (format "geiser-%s" name))))
    (define-key global-map `[menu-bar geiser run ,name] (cons title runner))
    (define-key global-map `[menu-bar geiser switch ,name]
      (cons title switcher))
    (geiser-menu--add-global-custom title group)))


;;; Permanent entries

(geiser-menu--add-global-custom "Geiser" 'geiser)

(define-key global-map [menu-bar geiser switch]
  (cons "Switch to" (make-sparse-keymap "Switch to")))
(define-key global-map [menu-bar geiser run]
  (cons "Run" (make-sparse-keymap "Run")))

(geiser-menu--add-line)



(provide 'geiser-menu)
;;; geiser-menu.el ends here

