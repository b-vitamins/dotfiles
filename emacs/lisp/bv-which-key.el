;;; bv-which-key.el --- Which-key configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for which-key package.

;;; Code:

(require 'which-key)

(when (boundp 'which-key-min-display-lines)
  (setq which-key-min-display-lines 1))
(when (boundp 'which-key-ellipsis)
  (setq which-key-ellipsis "..."))
(when (boundp 'which-key-idle-delay)
  (setq which-key-idle-delay 0.5))

(which-key-mode 1)

(when (boundp 'global-map)
  (define-key global-map (kbd "C-h C-k") 'which-key-show-top-level))

(provide 'bv-which-key)
;;; bv-which-key.el ends here