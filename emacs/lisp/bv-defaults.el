;;; bv-defaults.el --- Default settings inspired by nano-defaults  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; A set of sane default settings for Emacs following nano-emacs philosophy.

;;; Code:

;; Keep custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-buffer-choice nil
      frame-title-format nil
      use-file-dialog nil
      use-dialog-box nil
      pop-up-windows nil
      indicate-empty-lines nil
      cursor-in-non-selected-windows nil
      cursor-type '(bar . 1)              ; Thin vertical bar cursor (1 pixel wide)
      blink-cursor-mode nil               ; Disable cursor blinking
      initial-major-mode 'text-mode
      default-major-mode 'text-mode
      font-lock-maximum-decoration nil
      font-lock-maximum-size nil
      auto-fill-mode nil
      fill-column 80
      confirm-nonexistent-file-or-buffer nil
      completion-styles '(basic substring)
      org-return-follows-link t)

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(menu-bar-mode -1)

;; Enable line numbers globally
(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1))

(setq ring-bell-function 'ignore
      visible-bell nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

(setq window-min-height 1)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Ensure cursor type is applied to frames
(add-to-list 'default-frame-alist '(cursor-type . (bar . 1)))

;; Force cursor type after frame creation
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (setq cursor-type '(bar . 1)))))

;; Also set it for the initial frame
(when (display-graphic-p)
  (setq cursor-type '(bar . 1)))

(provide 'bv-defaults)
;;; bv-defaults.el ends here