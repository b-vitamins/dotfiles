;;; bv-re-builder.el --- Regular expression builder configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for the regular expression builder.
;; This module configures re-builder to use the rx syntax and
;; provides key bindings for easy access.

;;; Code:


(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "r") 're-builder)))

(with-eval-after-load 're-builder
  (when (boundp 'reb-re-syntax)
    (setq reb-re-syntax 'rx))
  (when (boundp 'reb-blink-delay)
    (setq reb-blink-delay 0))
  (when (boundp 'reb-auto-match-limit)
    (setq reb-auto-match-limit nil)))

(provide 'bv-re-builder)
;;; bv-re-builder.el ends here