;;; bv-calendar.el --- Calendar configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Calendar and appointment configuration with holidays and time display.

;;; Code:


(define-prefix-command 'bv-calendar-appt-map)

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "c") 'calendar)
    (define-key bv-app-map (kbd "A") 'bv-calendar-appt-map))
  (when (boundp 'bv-calendar-appt-map)
    (let ((map bv-calendar-appt-map))
      (define-key map "a" 'appt-add)
      (define-key map "d" 'appt-delete))))

(with-eval-after-load 'calendar
  (when (boundp 'diary-file)
    (setq diary-file "~/documents/diary"))
  (when (boundp 'calendar-week-start-day)
    (setq calendar-week-start-day 1))
  (when (boundp 'calendar-view-diary-initially-flag)
    (setq calendar-view-diary-initially-flag t))
  (when (boundp 'calendar-date-style)
    (setq calendar-date-style 'iso))
  (when (boundp 'calendar-mark-diary-entries-flag)
    (setq calendar-mark-diary-entries-flag t))
  (when (boundp 'calendar-intermonth-header)
    (setq calendar-intermonth-header
          (propertize "WK" 'font-lock-face 'font-lock-function-name-face)))
  (when (boundp 'calendar-intermonth-text)
    (setq calendar-intermonth-text
          '(propertize
            (format "%2d"
                    (car (calendar-iso-from-absolute
                          (calendar-absolute-from-gregorian
                           (list month day year)))))
            'font-lock-face
            'font-lock-function-name-face))))

(appt-activate 1)

(with-eval-after-load 'appt
  (when (boundp 'appt-display-format)
    (setq appt-display-format 'echo))
  (when (boundp 'appt-audible)
    (setq appt-audible nil))
  (when (boundp 'appt-message-warning-time)
    (setq appt-message-warning-time 10))
  (when (boundp 'appt-display-interval)
    (setq appt-display-interval 2))
  (when (boundp 'appt-display-diary)
    (setq appt-display-diary nil)))

(provide 'bv-calendar)
;;; bv-calendar.el ends here