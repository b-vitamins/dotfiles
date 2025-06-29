;;; bv-time.el --- Time display configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for time display and world clock.

;;; Code:

(eval-when-compile (require 'time))


(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "C") 'world-clock)))

(when (boundp 'world-clock-list)
  (setq world-clock-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Boise" "Boise")
          ("America/New_York" "New York")
          ("UTC" "UTC")
          ("Europe/London" "London")
          ("Europe/Paris" "Paris")
          ("Europe/Helsinki" "Helsinki")
          ("Europe/Moscow" "Moscow")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Tokyo" "Tokyo"))))

(when (boundp 'world-clock-time-format)
  (setq world-clock-time-format "%A %d %B %R %Z"))
(when (boundp 'display-time-default-load-average)
  (setq display-time-default-load-average nil))
(when (boundp 'display-time-load-average-threshold)
  (setq display-time-load-average-threshold 0))
(when (boundp 'display-time-day-and-date)
  (setq display-time-day-and-date t))
(when (boundp 'display-time-24hr-format)
  (setq display-time-24hr-format t))

(display-time-mode)

(provide 'bv-time)
;;; bv-time.el ends here