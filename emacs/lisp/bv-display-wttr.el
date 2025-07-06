;;; bv-display-wttr.el --- Weather display configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Weather information display with wttr.in.

;;; Code:


(declare-function display-wttr-mode "display-wttr")
(declare-function display-wttr-update "display-wttr")

(defgroup bv-weather nil
  "Weather display settings."
  :group 'bv)

(defcustom bv-weather-idle-delay 2.0
  "Idle time before loading weather display."
  :type 'number
  :group 'bv-weather)

(defcustom bv-weather-location "Bangalore"
  "Default weather location."
  :type 'string
  :group 'bv-weather)

(defcustom bv-weather-format "%c %t"
  "Weather display format."
  :type 'string
  :group 'bv-weather)

(defcustom bv-weather-update-interval (* 60 60)
  "Weather update interval in seconds."
  :type 'integer
  :group 'bv-weather)

;; Load display-wttr after idle delay
(run-with-idle-timer bv-weather-idle-delay t
                     (lambda ()
                       (require 'display-wttr nil t)))

(setq display-wttr-format bv-weather-format
      display-wttr-locations (list bv-weather-location)
      display-wttr-interval bv-weather-update-interval)

(defun bv-weather-show ()
  "Show current weather."
  (interactive)
  (let ((url (format "https://wttr.in/%s?format=%s"
                     (url-encode-url bv-weather-location)
                     (url-encode-url bv-weather-format))))
    (url-retrieve url
                  (lambda (status)
                    (goto-char (point-min))
                    (re-search-forward "\n\n")
                    (message "Weather: %s"
                             (buffer-substring (point) (point-max)))
                    (kill-buffer)))))

(defun bv-weather-set-location ()
  "Set weather location."
  (interactive)
  (let ((location (read-string "Location: " bv-weather-location)))
    (setq bv-weather-location location
          display-wttr-locations (list location))
    (when (fboundp 'display-wttr-update)
      (display-wttr-update))))

(defun bv-weather-toggle ()
  "Toggle weather display."
  (interactive)
  (if display-wttr-mode
      (display-wttr-mode -1)
    (display-wttr-mode 1)))

(with-eval-after-load 'display-wttr
  (display-wttr-mode 1))

(global-set-key (kbd "C-c W") 'bv-weather-show)

(provide 'bv-display-wttr)
;;; bv-display-wttr.el ends here