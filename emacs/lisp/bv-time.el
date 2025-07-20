;;; bv-time.el --- Time display configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Time display in mode line and world clock.

;;; Code:

(require 'time)

(when (boundp 'display-time-format)
  (setq display-time-format "%H:%M"))
(when (boundp 'display-time-default-load-average)
  (setq display-time-default-load-average nil))
(when (boundp 'display-time-load-average-threshold)
  (setq display-time-load-average-threshold 10.0))
(when (boundp 'display-time-day-and-date)
  (setq display-time-day-and-date nil))
(when (boundp 'display-time-24hr-format)
  (setq display-time-24hr-format t))
(when (boundp 'display-time-mail-string)
  (setq display-time-mail-string ""))
(when (boundp 'display-time-interval)
  (setq display-time-interval 60))

(when (boundp 'world-clock-list)
  (setq world-clock-list
        '(("Asia/Kolkata" "Bangalore")
          ("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("UTC" "UTC")
          ("Europe/London" "London")
          ("Europe/Berlin" "Berlin")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Sydney" "Sydney"))))

(when (boundp 'world-clock-time-format)
  (setq world-clock-time-format "%a %d %b %H:%M %Z"))
(when (boundp 'world-clock-buffer-name)
  (setq world-clock-buffer-name "*World Clock*"))


(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "C") 'world-clock)))

(when (fboundp 'display-time-mode)
  (display-time-mode 1))

(provide 'bv-time)
;;; bv-time.el ends here