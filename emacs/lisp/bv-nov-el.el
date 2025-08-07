;;; bv-nov-el.el --- EPUB reader configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Beautiful EPUB reading experience.

;;; Code:

(autoload 'nov-mode "nov")
(autoload 'olivetti-mode "olivetti")

(defvar nov-text-width)
(defvar nov-save-place-file)

(declare-function bv-nov-setup-faces "bv-nov-el")

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(with-eval-after-load 'nov
  ;; Reading setup
  (when (boundp 'nov-text-width)
    (setq nov-text-width t))
  (when (boundp 'nov-save-place-file)
    (setq nov-save-place-file
          (expand-file-name "nov-places"
                           (or (getenv "XDG_CACHE_HOME") "~/.cache"))))

  ;; Typography
  (add-hook 'nov-mode-hook #'olivetti-mode)
  (add-hook 'nov-mode-hook #'visual-line-mode)
  (add-hook 'nov-mode-hook (lambda () (setq-local line-spacing 0.2)))

  ;; Face customization
  (defun bv-nov-setup-faces ()
    "Setup faces for nov mode."
    (face-remap-add-relative 'variable-pitch :family "Libertinus Serif" :height 1.2))

  (add-hook 'nov-mode-hook #'bv-nov-setup-faces))

(provide 'bv-nov-el)
;;; bv-nov-el.el ends here