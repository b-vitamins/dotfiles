;;; bv-window.el --- Window management configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for window management with ace-window.

;;; Code:


(when (boundp 'global-map)
  (define-key global-map (kbd "M-o") 'ace-window))

(with-eval-after-load 'ace-window
  (when (boundp 'aw-keys)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  (when (boundp 'aw-background)
    (setq aw-background nil))
  (when (boundp 'aw-scope)
    (setq aw-scope 'frame))
  (when (boundp 'aw-ignore-current)
    (setq aw-ignore-current nil))
  (when (boundp 'aw-display-mode-overlay)
    (setq aw-display-mode-overlay nil)))

(provide 'bv-window)
;;; bv-window.el ends here