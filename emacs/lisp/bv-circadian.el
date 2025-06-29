;;; bv-circadian.el --- Circadian rhythm configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Automatic theme switching based on time of day and geographic location.

;;; Code:

(require 'json)
(require 'url)

(declare-function circadian-setup "circadian" ())
(declare-function bv-modus-themes-set-custom-faces "bv-modus-themes" ())


(defun bv-circadian--get-geolocation ()
  "Get geographic location from web service for automatic day/night calculation."
  (let ((response (ignore-errors
                    (url-retrieve-synchronously
                     "https://position.xyz/v1/geolocate" t))))
    (when response
      (with-current-buffer response
        (goto-char (point-min))
        (re-search-forward "^\n" nil t)
        (delete-region (point) (point-min))
        (condition-case nil
            (let* ((json-data (json-parse-string
                               (buffer-string)
                               :object-type 'plist))
                   (location (plist-get json-data :location))
                   (latitude (plist-get location :lat))
                   (longitude (plist-get location :lng)))
              (cons longitude latitude))
          (json-parse-error nil))))))

(with-eval-after-load 'solar
  (let ((coordinates (bv-circadian--get-geolocation)))
    (when (boundp 'calendar-longitude)
      (setq calendar-longitude (if coordinates (car coordinates) 0)))
    (when (boundp 'calendar-latitude)
      (setq calendar-latitude (if coordinates (cdr coordinates) 0)))))

(when (boundp 'circadian-after-load-theme-hook)
  (add-hook 'circadian-after-load-theme-hook 'bv-modus-themes-set-custom-faces))

(with-eval-after-load 'circadian-autoloads
  (when (boundp 'circadian-themes)
    (setq circadian-themes
          '((:sunrise . modus-operandi-deuteranopia)
            (:sunset . modus-vivendi-deuteranopia))))
  (circadian-setup))

(provide 'bv-circadian)
;;; bv-circadian.el ends here