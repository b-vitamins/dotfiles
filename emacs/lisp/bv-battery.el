;;; bv-battery.el --- Battery configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Battery status display in mode line.

;;; Code:

(require 'battery nil t)

(defvar battery-mode-line-format)
(defvar battery-mode-line-limit)
(defvar battery-update-interval)
(defvar battery-echo-area-format)

(defun bv-battery--present-p ()
  "Return non-nil when a battery reports a numeric charge level."
  (when-let* ((status-fn (bound-and-true-p battery-status-function))
              (percent (cdr (assq ?p (ignore-errors (funcall status-fn))))))
    (string-match-p "\\`[0-9.]+\\'" percent)))

(when (bv-battery--present-p)
  ;; Renders as e.g. "BAT+42%": %b is the charge-state sign, %p the percentage.
  (setq battery-mode-line-format "BAT%b%p%%"
        battery-mode-line-limit 90
        battery-update-interval 60
        battery-echo-area-format "Battery: %B %p%%, %t")
  (display-battery-mode 1))

(provide 'bv-battery)
;;; bv-battery.el ends here
