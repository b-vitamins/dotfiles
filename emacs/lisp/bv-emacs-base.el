;;; bv-emacs-base.el --- Base Emacs configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module provides the base Emacs configuration.
;; It sets up fundamental settings, file handling, keybindings,
;; and core functionality for the BV Emacs configuration.

;;; Code:

(defgroup bv nil
  "Base customization group for BV configuration."
  :group 'external
  :prefix 'bv-)

(eval-when-compile
  (let ((dir (or (and load-file-name (file-name-directory load-file-name))
                 (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
    (add-to-list 'load-path dir))
  (require 'bv-keymaps))
(let ((dir (or (and load-file-name (file-name-directory load-file-name))
               (and buffer-file-name (file-name-directory buffer-file-name))
               default-directory)))
  (add-to-list 'load-path dir))
(require 'bv-keymaps)


(setq native-comp-jit-compilation nil)
(setq user-full-name "Ayan Das")
(when (boundp 'user-mail-address)
  (setq user-mail-address "bvits@riseup.net"))

(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(setq minibuffer-message-timeout 0)

(setq custom-file
      (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/custom.el"))
(load custom-file t)

(when (boundp 'backup-directory-alist)
  (setq backup-directory-alist
        `(,(cons "." (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                             "/emacs/backup")))))

(when (boundp 'recentf-save-file)
  (setq recentf-save-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/recentf")))
(recentf-mode 1)
(run-with-idle-timer 30 t 'recentf-save-list)

(when (boundp 'savehist-file)
  (setq savehist-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/history")))
(savehist-mode 1)
(run-with-idle-timer 30 t 'savehist-save)

(when (boundp 'bookmark-default-file)
  (setq bookmark-default-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/bookmarks")))

(when (boundp 'auto-save-list-file-prefix)
  (setq auto-save-list-file-prefix
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                "/emacs/auto-save-list")))

(pixel-scroll-precision-mode 1)
(column-number-mode 1)
(save-place-mode 1)
(when (boundp 'save-place-file)
  (setq save-place-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/places")))
(show-paren-mode 1)
(subword-mode 1)
(setq-default indent-tabs-mode nil)
(when (boundp 'save-interprogram-paste-before-kill)
  (setq save-interprogram-paste-before-kill t))
(when (boundp 'mouse-yank-at-point)
  (setq mouse-yank-at-point t))
(when (boundp 'require-final-newline)
  (setq require-final-newline t))

(defun bv-whitespace-mode ()
  "Equivalent of `whitespace-mode', but highlight only tabs."
  (interactive)
  (require 'whitespace)
  (defvar whitespace-mode)
  (defvar whitespace-style)
  (if (and (boundp 'whitespace-mode) whitespace-mode)
      (whitespace-mode 0)
    (let ((whitespace-style '(face tabs)))
      (whitespace-mode 1))))

(when (boundp 'prog-mode-hook)
  (add-hook 'prog-mode-hook
            (lambda ()
              (bv-whitespace-mode)
              (setq show-trailing-whitespace t))))

(if after-init-time
    (require 'org-protocol)
  (when (boundp 'after-init-hook)
    (add-hook 'after-init-hook (lambda () (require 'org-protocol)))))

(set-face-background 'glyphless-char "red")

(autoload 'er/expand-region "expand-region")
(when (boundp 'global-map)
  (define-key global-map (kbd "C-=") 'er/expand-region))

(defun bv-display-load-time ()
  "Display Emacs initialization time and helpful keybinding hints."
  (interactive)
  (message
   "BV emacs loaded in %s, C-h r i for search in emacs manual by topic. C-h C-a to open About Emacs buffer."
   (emacs-init-time)))

(defun display-startup-echo-area-message ()
  "Display startup message in echo area.
This function overrides the default startup message."
  (bv-display-load-time))

(when (boundp 'isearch-lazy-count)
  (setq isearch-lazy-count t))
(when (boundp 'search-whitespace-regexp)
  (setq search-whitespace-regexp ".*?"))

(when (boundp 'prog-mode-hook)
  (dolist (mode-hook '(prog-mode-hook))
    (add-hook mode-hook (lambda () (setq truncate-lines t)))))

(when (boundp 'global-map)
  (define-key global-map (kbd "s-b") 'switch-to-buffer)
  (define-key global-map (kbd "s-w") 'kill-current-buffer)
  (define-key global-map (kbd "s-W") 'kill-buffer-and-window)
  (define-key global-map (kbd "s-o") 'other-window)
  (define-key global-map (kbd "C-z") nil))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when (boundp 'goto-map)
  (let ((map goto-map))
    (define-key map "L" 'find-library)
    (define-key map "F" 'find-function)
    (define-key map "K" 'find-function-on-key)
    (define-key map "V" 'find-variable)))

(defun bv-kill-region-dwim (&optional count)
  "Kill region if mark is active, otherwise kill a word.
Prefix argument COUNT kills that many words."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) 'region)
    (backward-kill-word count)))

(when (boundp 'global-map)
  (define-key global-map (kbd "M-K") 'kill-whole-line)
  (define-key global-map (kbd "M-c") 'capitalize-dwim)
  (define-key global-map (kbd "M-l") 'downcase-dwim)
  (define-key global-map (kbd "M-u") 'upcase-dwim)
  (define-key global-map (kbd "C-w") 'bv-kill-region-dwim))
(when (boundp 'mode-specific-map)
  (define-key mode-specific-map (kbd "a") '("applications" . bv-app-map))
  (define-key mode-specific-map (kbd "t") '("toggles" . bv-toggle-map)))

(when (boundp 'large-file-warning-threshold)
  (setq large-file-warning-threshold nil))
(when (boundp 'vc-follow-symlinks)
  (setq vc-follow-symlinks t))
(setq ad-redefinition-action 'accept)

(when (boundp 'global-auto-revert-non-file-buffers)
  (setq global-auto-revert-non-file-buffers t))
(global-auto-revert-mode 1)

(eval-when-compile (require 'ws-butler))
(autoload 'ws-butler-mode "ws-butler")
(when (boundp 'text-mode-hook)
  (add-hook 'text-mode-hook 'ws-butler-mode))
(when (boundp 'prog-mode-hook)
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(with-eval-after-load 'alert
  (when (boundp 'alert-default-style)
    (setq alert-default-style 'notifications)))

(when (boundp 'even-window-sizes)
  (setq even-window-sizes nil))
(when (boundp 'ediff-diff-options)
  (setq ediff-diff-options "-w"))
(when (boundp 'ediff-split-window-function)
  (setq ediff-split-window-function 'split-window-horizontally))
(when (boundp 'ediff-window-setup-function)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(defun bv-kill-emacs (&optional _arg restart)
  "Make GNU Shepherd kill the Emacs server.
If RESTART is non-nil, herd will restart the Emacs server.
ARG is ignored."
  (interactive)
  (call-process
   "/run/current-system/profile/bin/herd"
   nil 0 nil
   (if restart "restart" "stop")
   (concat "emacs-" (if (boundp 'server-name) server-name "server"))))

(when (boundp 'emacs-startup-hook)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (and (boundp 'server-mode) server-mode)
                (advice-add 'kill-emacs :override 'bv-kill-emacs)))))

(provide 'bv-emacs-base)
;;; bv-emacs-base.el ends here