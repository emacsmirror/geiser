;; geiser-log.el -- logging utilities

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Feb 07, 2009 12:07

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

;; Some utilities for maintaining a simple log buffer, mainly for
;; debugging purposes.

;;; Code:

(require 'geiser-popup)
(require 'geiser-base)

(require 'comint)


;;; Customization:

(defvar geiser-log--buffer-name "*geiser messages*"
  "Name of the Geiser log buffer.")

(defvar geiser-log--max-buffer-size 32000
  "Maximum size of the Geiser messages log.")

(defvar geiser-log--max-message-size 512
  "Maximum size of individual Geiser log messages.")

(defvar geiser-log--verbose-p t
  "Log level for Geiser messages")

(defvar geiser-log--inhibit-p nil
  "Set this to t to inhibit all log messages")


;;; Log buffer and mode:

(define-derived-mode geiser-messages-mode fundamental-mode "Geiser Messages"
  "Simple mode for Geiser log messages buffer."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (set (make-local-variable 'comint-redirect-subvert-readonly) t)
  (add-hook 'after-change-functions
            '(lambda (b e len)
               (let ((inhibit-read-only t))
                 (when (> b geiser-log--max-buffer-size)
                   (delete-region (point-min) b))))
            nil t)
  (setq buffer-read-only t))

(geiser-popup--define log geiser-log--buffer-name geiser-messages-mode)


;;; Logging functions:

(defun geiser-log--msg (type &rest args)
  (unless geiser-log--inhibit-p
    (geiser-log--with-buffer
      (insert (geiser--shorten-str (format "\n%s: %s\n" type (apply 'format args))
                                   geiser-log--max-message-size)))))

(defsubst geiser-log--warn (&rest args)
  (apply 'geiser-log--msg 'WARNING args))

(defsubst geiser-log--error (&rest args)
  (apply 'geiser-log--msg 'ERROR args))

(defsubst geiser-log--info (&rest args)
  (when geiser-log--verbose-p
    (apply 'geiser-log--msg 'INFO args) ""))


;;; User commands:

(defun geiser-show-logs ()
  "Show Geiser log messages."
  (interactive)
  (geiser-log--pop-to-buffer))


(provide 'geiser-log)
;;; geiser-log.el ends here
