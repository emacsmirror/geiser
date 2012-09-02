;; geiser-image.el -- support for image display

;; Copyright (c) 2012 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Authors: Michael Wilber, Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Sep 02, 2012 00:00



(require 'geiser-custom)
(require 'geiser-base)


;;; Customization:

(defgroup geiser-image nil
  "Options for image displaying."
  :group 'geiser)

(geiser-custom--defcustom geiser-system-image-viewer "display"
  "Which system image viewer program to invoke upon M-x
`geiser-view-last-image'."
  :type 'string
  :group 'geiser-image)

(geiser-custom--defcustom geiser-image-cache-keep-last 10
  "How many images to keep in geiser's image cache."
  :type 'integer
  :group 'geiser-image)

(geiser-custom--defcustom geiser-image-cache-dir nil
  ;; Currently, this variable is updated, if needed, by racket during
  ;; initialization.  If/when we add image support for other
  ;; implementations, we'll have to work with implementation-specific
  ;; caches.
  "Directory where generated images are stored.  If nil, the
system wide tmp dir will be used."
  :type 'path
  :group 'geiser-image)

(geiser-custom--defface image-button
  'button geiser-image "image buttons in terminal buffers")



(defun geiser-image--list-cache ()
  "List all the images in the image cache."
  (and geiser-image-cache-dir
       (file-directory-p geiser-image-cache-dir)
       (let ((files (directory-files-and-attributes
                     geiser-image-cache-dir t "geiser-img-[0-9]*.png")))
         (mapcar 'car
                 (sort files (lambda (a b)
                               (< (float-time (nth 6 a))
                                  (float-time (nth 6 b)))))))))

(defun geiser-image--clean-cache ()
  "Clean all except for the last `geiser-image-cache-keep-last'
images in `geiser-image-cache-dir'."
  (interactive)
  (dolist (f (butlast (geiser-image--list-cache) geiser-image-cache-keep-last))
    (delete-file f)))

(defun geiser-image--display (file)
  (start-process "Geiser image view" nil geiser-system-image-viewer file))

(defun geiser-image--button-action (button)
  (let ((file (button-get button 'geiser-image-file)))
    (when (file-exists-p file) (geiser-image--display file))))

(define-button-type 'geiser-image--button
  'action 'geiser-image--button-action
  'follow-link t)

(defun geiser-image--replace-images (inline-images-p auto-p)
  "Replace all image patterns with actual images"
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#<Image: \\([-+./_0-9a-zA-Z]+\\)>" nil t)
        ;; can't pass a filename to create-image because emacs might
        ;; not display it before it gets deleted (race condition)
        (let* ((file (match-string 1))
               (begin (match-beginning 0))
               (end (match-end 0)))
          (delete-region begin end)
          (if (and inline-images-p (display-images-p))
              (put-image (create-image file) begin "[image]")
            (goto-char begin)
            (insert-text-button "[image]"
                                :type 'geiser-image--button
                                'face 'geiser-font-lock-image-button
                                'geiser-image-file file
                                'help-echo "Click to display image")
            (when auto-p (geiser-image--display file)))
          (setq geiser-image-cache-dir (file-name-directory file))
          (geiser-image--clean-cache))))))

(defun geiser-view-last-image (n)
  "Open the last displayed image in the system's image viewer.

With prefix arg, open the N-th last shown image in the system's
image viewer."
  (interactive "p")
  (let ((images (reverse (geiser-image--list-cache))))
    (if (>= (length images) n)
        (geiser-image--display (nth (- n 1) images))
      (error "There aren't %d recent images" n))))


(provide 'geiser-image)
