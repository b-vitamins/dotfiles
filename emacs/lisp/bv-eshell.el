;;; bv-eshell.el --- Eshell configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced Eshell configuration with project integration, aliases,
;; and improved prompt and history management.

;;; Code:

(eval-when-compile
  (require 'eshell)
  (require 'em-alias)
  (require 'em-hist)
  (require 'project))

(autoload 'eshell/alias "em-alias")


(defgroup bv-eshell nil
  "Eshell customizations."
  :group 'bv)

(defun bv-project-eshell-or-eshell (&optional arg)
  "Open Eshell in project root if in project, otherwise open regular Eshell.
With prefix ARG, force creation of a new Eshell buffer."
  (interactive "P")
  (if (project-current)
      (project-eshell)
    (eshell arg)))

(defun switch-to-prev-buffer-or-eshell (arg)
  "Switch to previous buffer, or open Eshell with prefix ARG."
  (interactive "P")
  (if arg
      (eshell arg)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(define-minor-mode bv-eshell-mode-setup
  "Set up eshell environment."
  :group 'bv-eshell
  (if bv-eshell-mode-setup
      (progn
        (if (and (boundp 'envrc-global-mode) envrc-global-mode)
            (when (boundp 'envrc-mode-hook)
              (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" ""))))
          (setenv "PAGER" ""))
        (when (boundp 'eshell-load-hook)
          (add-hook 'eshell-load-hook 'eat-eshell-mode)
          (add-hook 'eshell-load-hook 'eat-eshell-visual-command-mode))
        (eshell/alias "e" "find-file $1")
        (eshell/alias "ee" "find-file-other-window $1")
        (eshell/alias "d" "dired $1")
        (with-eval-after-load 'magit
          (eshell/alias "gd" "magit-diff-unstaged"))
        (when (boundp 'eshell-mode-map)
          (define-key eshell-mode-map (kbd "C-c M-o") 'eshell/clear)
          (define-key eshell-mode-map (kbd "s-e") 'switch-to-prev-buffer-or-eshell)))
    (local-unset-key 'eshell/clear)))

(when (boundp 'global-map)
  (define-key global-map (kbd "s-e") 'eshell))

(when (boundp 'eshell-mode-hook)
  (add-hook 'eshell-mode-hook 'bv-eshell-mode-setup))

(with-eval-after-load 'eshell
  (let ((eshell-cache (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/eshell/")))
    (when (boundp 'eshell-aliases-file)
      (setq eshell-aliases-file (concat eshell-cache "alias")))
    (when (boundp 'eshell-history-file-name)
      (setq eshell-history-file-name (concat eshell-cache "history")))
    (when (boundp 'eshell-last-dir-ring-file-name)
      (setq eshell-last-dir-ring-file-name (concat eshell-cache "lastdir"))))
  
  (when (boundp 'eshell-banner-message)
    (setq eshell-banner-message ""))
  
  (autoload 'eshell-syntax-highlighting-global-mode "eshell-syntax-highlighting")
  (when (fboundp 'eshell-syntax-highlighting-global-mode)
    (eshell-syntax-highlighting-global-mode))
  
  (when (boundp 'eshell-hist-mode-hook)
    (add-hook 'eshell-hist-mode-hook
              (lambda () 
                (when (boundp 'eshell-hist-mode-map)
                  (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history)))))
  
  (when (boundp 'eshell-preoutput-filter-functions)
    (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply))
  
  (with-eval-after-load 'em-prompt
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (when (boundp 'eshell-prompt-function)
      (setq eshell-prompt-function 'epe-theme-lambda))
    (when (boundp 'eshell-highlight-prompt)
      (setq eshell-highlight-prompt nil))))

(provide 'bv-eshell)
;;; bv-eshell.el ends here