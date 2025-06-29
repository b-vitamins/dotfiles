;;; bv-all-the-icons.el --- All the icons configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for all-the-icons package, providing icon support
;; for various modes and completion frameworks.

;;; Code:


(with-eval-after-load 'all-the-icons
  (when (boundp 'all-the-icons-scale-factor)
    (setq all-the-icons-scale-factor 1.0))
  (when (boundp 'all-the-icons-default-adjust)
    (setq all-the-icons-default-adjust 0))
  (when (boundp 'all-the-icons-octicon-scale-factor)
    (setq all-the-icons-octicon-scale-factor 0.9)))

(autoload 'all-the-icons-completion-mode "all-the-icons-completion")
(autoload 'all-the-icons-completion-marginalia-setup "all-the-icons-completion")

(all-the-icons-completion-mode)

(when (boundp 'marginalia-mode-hook)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(provide 'bv-all-the-icons)
;;; bv-all-the-icons.el ends here