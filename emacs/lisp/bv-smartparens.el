;;; bv-smartparens.el --- Structural editing  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Smart parentheses for structural editing.

;;; Code:

(eval-when-compile (require 'smartparens))

(autoload 'smartparens-mode "smartparens")
(autoload 'smartparens-strict-mode "smartparens")
(autoload 'sp-use-smartparens-bindings "smartparens")
(autoload 'show-smartparens-global-mode "smartparens")
(autoload 'sp-forward-slurp-sexp "smartparens")

(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'smartparens-strict-mode)

(with-eval-after-load 'smartparens
  (sp-use-smartparens-bindings)
  (with-eval-after-load 'paren
    (when (boundp 'show-paren-style)
      (setq show-paren-style 'mixed))
    (show-paren-mode -1)
    (show-smartparens-global-mode 1))
  (require 'smartparens-config)
  (when (boundp 'smartparens-mode-map)
    (define-key smartparens-mode-map (kbd "M-s") nil)
    (define-key smartparens-mode-map (kbd "M-S") 'sp-forward-slurp-sexp))
  (when (boundp 'sp-highlight-pair-overlay)
    (setq sp-highlight-pair-overlay nil)))

(provide 'bv-smartparens)
;;; bv-smartparens.el ends here