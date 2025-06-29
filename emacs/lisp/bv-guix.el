;;; bv-guix.el --- Guix configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for GNU Guix integration with Emacs.
;; Provides prettification, keybindings, and development environment setup.

;;; Code:

(eval-when-compile (require 'guix))
(declare-function global-guix-prettify-mode "guix-prettify")
(declare-function guix-prettify-mode "guix-prettify")

(autoload 'info-lookup-add-help "info-look")

(with-eval-after-load 'info-look
  (info-lookup-add-help
   :mode 'scheme-mode
   :regexp "[^()`',\"        \n]+"
   :ignore-case t
   :doc-spec '(("(r5rs)Index" nil "^[ \t]+-+ [^:]+:[ \t]*" "\\b")
               ("(Guile)Procedure Index" nil nil nil)
               ("(Guile)Variable Index" nil nil nil)
               ("(Guix)Programming Index" nil nil nil))))

(autoload 'global-guix-prettify-mode "guix-prettify")
(autoload 'guix-prettify-mode "guix-prettify")
(autoload 'guix "guix" nil t)

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "p") 'guix-prettify-mode)
    (define-key bv-toggle-map (kbd "P") 'global-guix-prettify-mode)))

(if after-init-time
    (global-guix-prettify-mode 1)
  (when (boundp 'after-init-hook)
    (add-hook 'after-init-hook 'global-guix-prettify-mode)))

(with-eval-after-load 'daemons
  (when (boundp 'daemons-init-system-submodules)
    (setq daemons-init-system-submodules '(daemons-shepherd)))
  (when (boundp 'daemons-always-sudo)
    (setq daemons-always-sudo nil)))

(with-eval-after-load 'guix
  (when (file-directory-p "~/projects/guix-mirror")
    (when (boundp 'guix-directory)
      (setq guix-directory "~/projects/guix-mirror"))))

(global-set-key (kbd "s-G") 'guix)

(provide 'bv-guix)
;;; bv-guix.el ends here