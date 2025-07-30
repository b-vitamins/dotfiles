;;; bv-circadian.el --- Circadian rhythm configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Automatic theme switching based on time of day using built-in solar.el
;; Enhanced to work with theme system.

;;; Code:

(require 'solar)
(require 'bv-themes)

(defgroup bv-circadian nil
  "Circadian rhythm theme switching."
  :group 'bv-themes
  :prefix "bv-circadian-")

(defcustom bv-circadian-latitude 12.9716
  "Latitude for sunrise/sunset calculations (default: Bangalore)."
  :type 'number
  :group 'bv-circadian)

(defcustom bv-circadian-longitude 77.5946
  "Longitude for sunrise/sunset calculations (default: Bangalore)."
  :type 'number
  :group 'bv-circadian)

(defcustom bv-circadian-update-interval 300
  "Seconds between automatic theme updates (default: 5 minutes)."
  :type 'integer
  :group 'bv-circadian)

(defcustom bv-circadian-transition-time 30
  "Minutes before/after sunrise/sunset to start transition.
This creates a transition period around sunrise and sunset."
  :type 'integer
  :group 'bv-circadian)

(defcustom bv-circadian-enabled-p t
  "Whether circadian theme switching is enabled."
  :type 'boolean
  :group 'bv-circadian)

(defvar bv-circadian--timer nil
  "Timer for periodic theme updates.")

(defvar bv-circadian--last-theme nil
  "Last theme that was set by circadian.")

;;; Enhanced time calculations

(defun bv-circadian--get-sunrise-sunset ()
  "Get today's sunrise and sunset times with transition periods."
  (let* ((calendar-latitude bv-circadian-latitude)
         (calendar-longitude bv-circadian-longitude)
         (date (calendar-current-date))
         (sunrise-sunset (solar-sunrise-sunset date)))
    ;; Extract times from the solar calculation
    (when sunrise-sunset
      (let* ((sunrise-time (caar sunrise-sunset))
             (sunset-time (caadr sunrise-sunset))
             (transition-hours (/ bv-circadian-transition-time 60.0)))
        (list :sunrise-start (- sunrise-time transition-hours)
              :sunrise sunrise-time
              :sunrise-end (+ sunrise-time transition-hours)
              :sunset-start (- sunset-time transition-hours)
              :sunset sunset-time
              :sunset-end (+ sunset-time transition-hours))))))

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
        (let* ((sunrise-min (bv-circadian--time-to-minutes
                             (plist-get times :sunrise)))
               (sunset-min (bv-circadian--time-to-minutes
                            (plist-get times :sunset)))
               (current-min (bv-circadian--current-minutes)))
          (or (< current-min sunrise-min)
              (>= current-min sunset-min)))
      ;; Fallback: use dark theme between 6 PM and 6 AM
      (let ((hour (nth 2 (decode-time))))
        (or (>= hour 18) (< hour 6))))))

(defun bv-circadian--get-transition-state ()
  "Get current transition state (:day :night :sunrise :sunset nil)."
  (let ((times (bv-circadian--get-sunrise-sunset)))
    (when times
      (let* ((current-min (bv-circadian--current-minutes))
             (sunrise-start (bv-circadian--time-to-minutes
                             (plist-get times :sunrise-start)))
             (sunrise-end (bv-circadian--time-to-minutes
                           (plist-get times :sunrise-end)))
             (sunset-start (bv-circadian--time-to-minutes
                            (plist-get times :sunset-start)))
             (sunset-end (bv-circadian--time-to-minutes
                          (plist-get times :sunset-end))))
        (cond
         ((and (>= current-min sunrise-start)
               (<= current-min sunrise-end))
          :sunrise)
         ((and (>= current-min sunset-start)
               (<= current-min sunset-end))
          :sunset)
         ((and (> current-min sunrise-end)
               (< current-min sunset-start))
          :day)
         (t :night))))))

;;; Theme management

(defun bv-circadian-update-theme (&optional force)
  "Update theme based on current time.
With FORCE, update even if the theme shouldn't change."
  (interactive "P")
  (when (or force bv-circadian-enabled-p)
    (let* ((use-dark (bv-circadian--should-use-dark-theme-p))
           (target-theme (if use-dark 'bv-dark 'bv-light))
           (current-theme (bv-themes-current))
           (transition-state (bv-circadian--get-transition-state)))
      (when (or force (not (eq current-theme target-theme)))
        (bv-themes-load-theme target-theme)
        (setq bv-circadian--last-theme target-theme)
        (let ((time-desc (pcase transition-state
                           (:sunrise "sunrise transition")
                           (:sunset "sunset transition")
                           (:day "daytime")
                           (:night "nighttime")
                           (_ "current time"))))
          (message "Circadian: Switched to %s theme for %s"
                   (if use-dark "dark" "light")
                   time-desc))))))

(defun bv-circadian-status ()
  "Display current circadian status."
  (interactive)
  (let* ((times (bv-circadian--get-sunrise-sunset))
         (state (bv-circadian--get-transition-state))
         (current-theme (bv-themes-variant)))
    (if times
        (message "Circadian: %s | Sunrise: %s | Sunset: %s | Theme: %s | State: %s"
                 (if bv-circadian-enabled-p "Enabled" "Disabled")
                 (format-time-string "%H:%M"
                                     (seconds-to-time
                                      (* 60 (plist-get times :sunrise))))
                 (format-time-string "%H:%M"
                                     (seconds-to-time
                                      (* 60 (plist-get times :sunset))))
                 (or current-theme "none")
                 (or state "unknown"))
      (message "Circadian: Unable to calculate sunrise/sunset times"))))

;;; Timer management

(defun bv-circadian-setup ()
  "Setup automatic theme switching based on sunrise/sunset."
  (interactive)
  ;; Cancel any existing timer
  (bv-circadian-stop)

  ;; Enable circadian switching
  (setq bv-circadian-enabled-p t)

  ;; Set initial theme
  (bv-circadian-update-theme)

  ;; Setup timer
  (setq bv-circadian--timer
        (run-at-time bv-circadian-update-interval
                     bv-circadian-update-interval
                     #'bv-circadian-update-theme))

  (message "Circadian theme switching enabled"))

(defun bv-circadian-stop ()
  "Stop automatic theme switching."
  (interactive)
  (when bv-circadian--timer
    (cancel-timer bv-circadian--timer)
    (setq bv-circadian--timer nil))
  (setq bv-circadian-enabled-p nil)
  (message "Circadian theme switching disabled"))

(defun bv-circadian-toggle ()
  "Toggle circadian theme switching."
  (interactive)
  (if bv-circadian-enabled-p
      (bv-circadian-stop)
    (bv-circadian-setup)))

;;; Location management

(defun bv-circadian-set-location (latitude longitude)
  "Set location for sunrise/sunset calculations.
LATITUDE and LONGITUDE should be decimal degrees."
  (interactive
   (list (read-number "Latitude: " bv-circadian-latitude)
         (read-number "Longitude: " bv-circadian-longitude)))
  (setq bv-circadian-latitude latitude
        bv-circadian-longitude longitude)
  (when bv-circadian-enabled-p
    (bv-circadian-update-theme t))
  (message "Circadian location set to %.4f, %.4f" latitude longitude))

(defun bv-circadian-use-system-location ()
  "Try to determine location from system.
This is a placeholder - you would need to implement actual detection."
  (interactive)
  (message "System location detection not yet implemented"))

;;; Integration with theme system

(defun bv-circadian-exempt-p ()
  "Return t if current buffer should be exempt from theme change."
  ;; You can customize this to exempt certain modes or buffers
  nil)

;;; Hooks for sunrise/sunset events

(defvar bv-circadian-sunrise-hook nil
  "Hook run at sunrise.")

(defvar bv-circadian-sunset-hook nil
  "Hook run at sunset.")

(defun bv-circadian--check-transitions ()
  "Check for sunrise/sunset transitions and run hooks."
  (let ((state (bv-circadian--get-transition-state)))
    (when (and (eq state :sunrise)
               (not (eq bv-circadian--last-theme 'bv-light)))
      (run-hooks 'bv-circadian-sunrise-hook))
    (when (and (eq state :sunset)
               (not (eq bv-circadian--last-theme 'bv-dark)))
      (run-hooks 'bv-circadian-sunset-hook))))

;;; Add transition checking to update function

(advice-add 'bv-circadian-update-theme :after
            (lambda (&rest _) (bv-circadian--check-transitions)))

(provide 'bv-circadian)
;;; bv-circadian.el ends here
