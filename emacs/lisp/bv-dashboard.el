;;; bv-dashboard.el --- Dashboard configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Minimal startup screen with recent files and projects.

;;; Code:

(require 'dashboard)

(when (boundp 'dashboard-banner-logo-title)
  (setq dashboard-banner-logo-title "Welcome to Emacs"))
(when (boundp 'dashboard-startup-banner)
  (setq dashboard-startup-banner 'logo))
(when (boundp 'dashboard-center-content)
  (setq dashboard-center-content t))
(when (boundp 'dashboard-set-init-info)
  (setq dashboard-set-init-info nil))
(when (boundp 'dashboard-set-footer)
  (setq dashboard-set-footer nil))
(when (boundp 'dashboard-show-shortcuts)
  (setq dashboard-show-shortcuts nil))
(when (boundp 'dashboard-set-heading-icons)
  (setq dashboard-set-heading-icons nil))
(when (boundp 'dashboard-set-file-icons)
  (setq dashboard-set-file-icons nil)) ;; Temporarily disabled due to font issues
(when (boundp 'dashboard-icon-type)
  (setq dashboard-icon-type (if (fboundp 'nerd-icons-octicon)
                                'nerd-icons
                              'all-the-icons)))
(when (boundp 'dashboard-display-icons-p)
  (setq dashboard-display-icons-p #'display-graphic-p))
(when (boundp 'dashboard-heading-icons)
  (setq dashboard-heading-icons '((recents   . "history")
                                   (bookmarks . "bookmark")
                                   (agenda    . "calendar")
                                   (projects  . "briefcase")
                                   (registers . "database"))))
(when (boundp 'dashboard-items)
  (setq dashboard-items '((recents . 10)
                          (projects . 5))))
(when (boundp 'dashboard-path-max-length)
  (setq dashboard-path-max-length 60))
(when (boundp 'dashboard-path-style)
  (setq dashboard-path-style 'truncate-beginning))
(when (boundp 'dashboard-projects-backend)
  (setq dashboard-projects-backend 'project-el))
(when (boundp 'dashboard-heading-shorcut-format)
  (setq dashboard-heading-shorcut-format " [%s]"))
(when (boundp 'dashboard-image-banner-max-height)
  (setq dashboard-image-banner-max-height 200))
(when (boundp 'dashboard-image-banner-max-width)
  (setq dashboard-image-banner-max-width 350))


(defun bv-dashboard-open ()
  "Open dashboard buffer."
  (interactive)
  (when (fboundp 'dashboard-refresh)
    (dashboard-refresh)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "h") 'bv-dashboard-open)))

(when (fboundp 'dashboard-setup-startup-hook)
  (dashboard-setup-startup-hook))

(provide 'bv-dashboard)
;;; bv-dashboard.el ends here