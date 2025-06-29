;;; bv-dashboard.el --- Dashboard configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for the dashboard package, providing a startup screen
;; with recent files, projects, and agenda items.

;;; Code:

(require 'dashboard)

(defun bv-dashboard-open ()
  "Jump to a dashboard buffer, creating one if it doesn't exist."
  (interactive)
  (when (boundp 'dashboard-buffer-name)
    (when (get-buffer-create dashboard-buffer-name)
      (switch-to-buffer dashboard-buffer-name)
      (dashboard-mode)
      (dashboard-insert-startupify-lists)
      (dashboard-refresh-buffer))))

(when (boundp 'after-init-hook)
  (add-hook 'after-init-hook 'bv-dashboard-open))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "h") 'bv-dashboard-open)))

(with-eval-after-load 'dashboard
  (when (boundp 'dashboard-center-content)
    (setq dashboard-center-content t)))

(with-eval-after-load 'dashboard-widgets
  (when (boundp 'dashboard-bookmarks-show-base)
    (setq dashboard-bookmarks-show-base nil))
  (when (boundp 'dashboard-projects-backend)
    (setq dashboard-projects-backend 'project-el))
  (when (boundp 'dashboard-path-max-length)
    (setq dashboard-path-max-length 70))
  (when (boundp 'dashboard-path-style)
    (setq dashboard-path-style 'truncate-beginning))
  (when (boundp 'dashboard-set-init-info)
    (setq dashboard-set-init-info nil))
  (when (boundp 'dashboard-set-heading-icons)
    (setq dashboard-set-heading-icons nil))
  (when (boundp 'dashboard-set-file-icons)
    (setq dashboard-set-file-icons nil))
  (when (boundp 'dashboard-set-footer)
    (setq dashboard-set-footer nil))
  (when (boundp 'dashboard-items)
    (setq dashboard-items '((recents . 5) (projects . 5) (agenda . 10))))
  (when (boundp 'dashboard-startup-banner)
    (setq dashboard-startup-banner 'logo))
  (when (boundp 'dashboard-banner-logo-title)
    (setq dashboard-banner-logo-title ""))
  (when (boundp 'dashboard-image-banner-max-height)
    (setq dashboard-image-banner-max-height 0))
  (when (boundp 'dashboard-image-banner-max-width)
    (setq dashboard-image-banner-max-width 0))
  (when (boundp 'dashboard-show-shortcuts)
    (setq dashboard-show-shortcuts nil))
  (when (boundp 'dashboard-week-agenda)
    (setq dashboard-week-agenda t))
  (when (boundp 'dashboard-agenda-release-buffers)
    (setq dashboard-agenda-release-buffers t)))

(provide 'bv-dashboard)
;;; bv-dashboard.el ends here