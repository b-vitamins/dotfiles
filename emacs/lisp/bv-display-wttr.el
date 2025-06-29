;;; bv-display-wttr.el --- Weather display configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for displaying weather information using wttr.in service.

;;; Code:


(autoload 'display-wttr-mode "display-wttr")

;; Set variables if the display-wttr package defines them
(when (boundp 'display-wttr-format)
  (setq display-wttr-format "%c %t"))
(when (boundp 'display-wttr-locations)
  (setq display-wttr-locations '()))
(when (boundp 'display-wttr-interval)
  (setq display-wttr-interval 3600))

(display-wttr-mode)

(provide 'bv-display-wttr)
;;; bv-display-wttr.el ends here