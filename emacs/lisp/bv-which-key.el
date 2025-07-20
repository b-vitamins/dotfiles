;;; bv-which-key.el --- Which-key configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Display available key bindings in popup.

;;; Code:

(require 'which-key)

(when (boundp 'which-key-min-display-lines)
  (setq which-key-min-display-lines 1))
(when (boundp 'which-key-max-display-columns)
  (setq which-key-max-display-columns nil))
(when (boundp 'which-key-ellipsis)
  (setq which-key-ellipsis "…"))
(when (boundp 'which-key-idle-delay)
  (setq which-key-idle-delay 0.5))
(when (boundp 'which-key-idle-secondary-delay)
  (setq which-key-idle-secondary-delay 0.1))
(when (boundp 'which-key-show-docstrings)
  (setq which-key-show-docstrings nil))
(when (boundp 'which-key-separator)
  (setq which-key-separator " → "))
(when (boundp 'which-key-prefix-prefix)
  (setq which-key-prefix-prefix "+"))
(when (boundp 'which-key-sort-order)
  (setq which-key-sort-order 'which-key-key-order-alpha))
(when (boundp 'which-key-max-description-length)
  (setq which-key-max-description-length 32))

(when (boundp 'which-key-popup-type)
  (setq which-key-popup-type 'side-window))
(when (boundp 'which-key-side-window-location)
  (setq which-key-side-window-location 'bottom))
(when (boundp 'which-key-side-window-max-height)
  (setq which-key-side-window-max-height 0.33))
(when (boundp 'which-key-side-window-max-width)
  (setq which-key-side-window-max-width 0.66))


(when (fboundp 'which-key-mode)
  (which-key-mode 1))

(when (boundp 'global-map)
  (define-key global-map (kbd "C-h C-k") 'which-key-show-top-level))

(provide 'bv-which-key)
;;; bv-which-key.el ends here