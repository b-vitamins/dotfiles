;;; bv-defaults.el --- Default settings inspired by nano-defaults  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; A set of sane default settings for Emacs following nano-emacs philosophy.

;;; Code:

;; Declare external variables to avoid elint warnings
(defvar user-emacs-directory)
(defvar inhibit-startup-screen)
(defvar inhibit-startup-message)
(defvar inhibit-startup-echo-area-message)
(defvar initial-scratch-message)
(defvar initial-buffer-choice)
(defvar frame-title-format)
(defvar use-file-dialog)
(defvar use-dialog-box)
(defvar pop-up-windows)
(defvar indicate-empty-lines)
(defvar cursor-in-non-selected-windows)
(defvar cursor-type)
(defvar blink-cursor-blinks)
(defvar initial-major-mode)
(defvar font-lock-maximum-decoration)
(defvar font-lock-maximum-size)
(defvar auto-fill-mode)
(defvar fill-column)
(defvar confirm-nonexistent-file-or-buffer)
(defvar completion-styles)
(defvar org-return-follows-link)
(defvar ring-bell-function)
(defvar visible-bell)
(defvar temp-buffer-max-height)
(defvar window-min-height)
(defvar uniquify-buffer-name-style)
(defvar uniquify-separator)
(defvar uniquify-after-kill-buffer-p)
(defvar uniquify-ignore-buffers-re)
(defvar explicit-shell-file-name)
(defvar backup-directory-alist)
(defvar auto-save-file-name-transforms)
(defvar backup-by-copying)
(defvar version-control)
(defvar delete-old-versions)
(defvar kept-new-versions)
(defvar kept-old-versions)
(defvar create-lockfiles)
(defvar recentf-max-menu-items)
(defvar recentf-max-saved-items)
(defvar recentf-exclude)
(defvar savehist-file)
(defvar read-process-output-max)

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
      blink-cursor-blinks 0               ; Disable cursor blinking
      initial-major-mode 'text-mode
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

;; Enable line numbers in programming modes
(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode))

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

;; Kill terminal buffer on process exit
(defun bv-term-sentinel-advice (orig-fun proc msg)
  "Kill terminal buffer when process exits.
ORIG-FUN is the original sentinel function, PROC is the process,
and MSG is the process message."
  (funcall orig-fun proc msg)
  (when (memq (process-status proc) '(signal exit))
    (let ((buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(advice-add 'term-sentinel :around #'bv-term-sentinel-advice)

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

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Recent files tracking
(require 'recentf)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 200
      recentf-exclude '("/tmp/" "/ssh:" "/sudo:"
                        "COMMIT_EDITMSG" ".*-autoloads\\.el"
                        "[/\\]\\elpa/"))
(recentf-mode 1)

;; Minibuffer history
(require 'savehist)
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(savehist-mode 1)

;; Auto-save and backup configuration
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil)

;; Create directories if they don't exist
(dolist (dir (list (expand-file-name "backups" user-emacs-directory)
                   (expand-file-name "auto-saves" user-emacs-directory)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; Enable electric pairs in programming modes
(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;; Performance optimization for LSP and other processes
(setq read-process-output-max (* 1024 1024)) ; 1mb

(provide 'bv-defaults)
;;; bv-defaults.el ends here