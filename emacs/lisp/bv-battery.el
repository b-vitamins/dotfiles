;;; bv-battery.el --- Battery configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Battery status display in mode line.

;;; Code:

(require 'battery nil t)

(defun bv-battery-format (data)
  "Format battery status from DATA with text indicator."
  (let* ((bat-str (cdr (assq ?p data)))
         (status (cdr (assq ?B data)))
         (bat (if (and bat-str (not (string= bat-str "N/A")))
                  (string-to-number bat-str)
                nil)))
    (when (numberp bat)
      (let ((indicator (cond
                        ((string= status "Charging") "+")
                        ((string= status "Discharging") "-")
                        (t ""))))
        (propertize (format "BAT%s%d%%" indicator bat)
                    'face (cond
                           ((< bat 20) 'error)
                           ((< bat 50) 'warning)
                           (t 'shadow)))))))

(when (and (fboundp 'battery-status-function)
           battery-status-function)
  (when (boundp 'battery-mode-line-format)
    (setq battery-mode-line-format " %b "))
  (when (boundp 'battery-mode-line-limit)
    (setq battery-mode-line-limit 90))
  (when (boundp 'battery-update-interval)
    (setq battery-update-interval 60))
  (when (boundp 'battery-echo-area-format)
    (setq battery-echo-area-format "Battery: %B %p%%, %t"))
  (when (boundp 'battery-mode-line-string-creator)
    (setq battery-mode-line-string-creator 'bv-battery-format))
  (when (fboundp 'display-battery-mode)
    (display-battery-mode 1)))

(provide 'bv-battery)
;;; bv-battery.el ends here