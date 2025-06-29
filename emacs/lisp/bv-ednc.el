;;; bv-ednc.el --- Desktop notifications configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for EDNC desktop notification handling with custom
;; functions for managing notifications and keybindings.

;;; Code:


(autoload 'ednc-notifications "ednc")
(autoload 'ednc-format-notification "ednc")
(autoload 'ednc--close-notification "ednc")
(autoload 'ednc-dismiss-notification "ednc")
(autoload 'ednc--update-log-buffer "ednc")

(defvar bv-ednc-map nil
  "Keymap for EDNC notification commands.")
(define-prefix-command 'bv-ednc-map)

(defun bv-ednc--notify ()
  "Format and return the most recent notification."
  (when (ednc-notifications)
    (ednc-format-notification (car (ednc-notifications)))))

(defun bv-ednc-show-notification-log ()
  "Switch to the EDNC notification log buffer."
  (interactive)
  (when (and (boundp 'ednc-log-name) ednc-log-name (bufferp (get-buffer ednc-log-name)))
    (switch-to-buffer ednc-log-name)))

(defun bv-ednc-close-last-notification ()
  "Close the most recent notification."
  (interactive)
  (when-let* ((notification (car (ednc-notifications))))
    (ednc--close-notification notification 2)))

(defun bv-ednc-close-all-notifications ()
  "Close all pending notifications."
  (interactive)
  (when (and (boundp 'ednc--state) ednc--state)
    (mapc 'ednc-dismiss-notification (cdr ednc--state))))

(defun bv-ednc-update-notifications (&rest _)
  "Update mode line to reflect notification change."
  (interactive)
  (force-mode-line-update t))

(when (boundp 'after-init-hook)
  (add-hook 'after-init-hook 'ednc-mode))
(when (boundp 'ednc-notification-presentation-functions)
  (add-hook 'ednc-notification-presentation-functions 'bv-ednc-update-notifications)
  (add-hook 'ednc-notification-presentation-functions 'ednc--update-log-buffer))

(with-eval-after-load 'notifications
  (when (boundp 'notifications-application-name)
    (setq notifications-application-name "Notification")))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "n") 'bv-ednc-map)))

(let ((map bv-ednc-map))
  (define-key map "c" 'bv-ednc-close-last-notification)
  (define-key map "l" 'bv-ednc-show-notification-log)
  (define-key map "d" 'bv-ednc-close-all-notifications))

(provide 'bv-ednc)
;;; bv-ednc.el ends here