;;; bv-circadian.el --- Circadian rhythm configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Automatic theme switching based on time of day using built-in solar.el

;;; Code:

(require 'solar)
(require 'bv-themes)

(defcustom bv-circadian-latitude 12.9716
  "Latitude for sunrise/sunset calculations (default: Bangalore)."
  :type 'number
  :group 'bv)

(defcustom bv-circadian-longitude 77.5946
  "Longitude for sunrise/sunset calculations (default: Bangalore)."
  :type 'number
  :group 'bv)

(defvar bv-circadian--timer nil
  "Timer for periodic theme updates.")

(defun bv-circadian--get-sunrise-sunset ()
  "Get today's sunrise and sunset times."
  (let* ((calendar-latitude bv-circadian-latitude)
         (calendar-longitude bv-circadian-longitude)
         (date (calendar-current-date))
         (sunrise-sunset (solar-sunrise-sunset date)))
    ;; Extract times from the solar calculation
    (when sunrise-sunset
      (let* ((sunrise-time (caar sunrise-sunset))
             (sunset-time (caadr sunrise-sunset)))
        (list :sunrise sunrise-time :sunset sunset-time)))))

(defun bv-circadian--time-to-minutes (time)
  "Convert TIME (hours as float) to minutes since midnight."
  (round (* time 60)))

(defun bv-circadian--current-minutes ()
  "Get current time as minutes since midnight."
  (let ((now (decode-time)))
    (+ (* (nth 2 now) 60) (nth 1 now))))

(defun bv-circadian--should-use-dark-theme-p ()
  "Return t if dark theme should be used based on current time."
  (let ((times (bv-circadian--get-sunrise-sunset)))
    (if times
        (let* ((sunrise-min (bv-circadian--time-to-minutes (plist-get times :sunrise)))
               (sunset-min (bv-circadian--time-to-minutes (plist-get times :sunset)))
               (current-min (bv-circadian--current-minutes)))
          (or (< current-min sunrise-min)
              (>= current-min sunset-min)))
      ;; Fallback: use dark theme between 6 PM and 6 AM
      (let ((hour (nth 2 (decode-time))))
        (or (>= hour 18) (< hour 6))))))

(defun bv-circadian-update-theme ()
  "Update theme based on current time."
  (interactive)
  (let ((use-dark (bv-circadian--should-use-dark-theme-p))
        (current-variant (bv-themes-variant)))
    (cond
     ((and use-dark (not (string= current-variant "dark")))
      (bv-themes-load-theme 'bv-dark)
      (message "Switched to dark theme"))
     ((and (not use-dark) (not (string= current-variant "light")))
      (bv-themes-load-theme 'bv-light)
      (message "Switched to light theme")))))

(defun bv-circadian-setup ()
  "Setup automatic theme switching based on sunrise/sunset."
  ;; Cancel any existing timer
  (when bv-circadian--timer
    (cancel-timer bv-circadian--timer))

  ;; Set initial theme
  (bv-circadian-update-theme)

  ;; Setup timer to check every 5 minutes
  (setq bv-circadian--timer
        (run-at-time "5 min" 300 #'bv-circadian-update-theme)))

(defun bv-circadian-stop ()
  "Stop automatic theme switching."
  (interactive)
  (when bv-circadian--timer
    (cancel-timer bv-circadian--timer)
    (setq bv-circadian--timer nil)))

(provide 'bv-circadian)
;;; bv-circadian.el ends here