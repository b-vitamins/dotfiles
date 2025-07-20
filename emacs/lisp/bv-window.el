;;; bv-window.el --- Window management configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Window navigation with ace-window.

;;; Code:

(require 'ace-window nil t)

(when (boundp 'aw-keys)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(when (boundp 'aw-background)
  (setq aw-background nil))
(when (boundp 'aw-scope)
  (setq aw-scope 'frame))
(when (boundp 'aw-ignore-current)
  (setq aw-ignore-current nil))
(when (boundp 'aw-display-mode-overlay)
  (setq aw-display-mode-overlay t))
(when (boundp 'aw-leading-char-style)
  (setq aw-leading-char-style 'char))


(global-set-key (kbd "M-o") 'ace-window)

(provide 'bv-window)
;;; bv-window.el ends here