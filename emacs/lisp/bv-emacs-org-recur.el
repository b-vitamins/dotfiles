;;; bv-emacs-org-recur.el --- Org recurring tasks configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module configures org-recur for managing recurring tasks in Org mode.
;; It sets up keybindings and customizes behavior for completing and
;; scheduling recurring tasks.

;;; Code:


(when (boundp 'org-mode-hook)
  (add-hook 'org-mode-hook 'org-recur-mode))
(when (boundp 'org-agenda-mode-hook)
  (add-hook 'org-agenda-mode-hook 'org-recur-agenda-mode))

(with-eval-after-load 'org-recur
  (when (boundp 'org-recur-mode-map)
    (let ((map org-recur-mode-map))
      (define-key map (kbd "C-c d") 'org-recur-finish)
      (define-key map (kbd "C-c 0") 'org-recur-schedule-today)))
  (when (boundp 'org-recur-agenda-mode-map)
    (let ((map org-recur-agenda-mode-map))
      (define-key map (kbd "d") 'org-recur-finish)
      (define-key map (kbd "C-c d") 'org-recur-finish)))
  (when (boundp 'org-recur-finish-done)
    (setq org-recur-finish-done t))
  (when (boundp 'org-recur-finish-archive)
    (setq org-recur-finish-archive t)))

(provide 'bv-emacs-org-recur)
;;; bv-emacs-org-recur.el ends here