;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Early initialization for performance and basic frame setup.
;; Executed before init.el and before package initialization.

;;; Code:

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el loading packages prior to init.el
(setq package-enable-at-startup nil)

;; Disable file handlers during startup
(defvar bv--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Basic frame parameters
(setq default-frame-alist
      '((min-height . 1)
        (height . 45)
        (min-width . 1)
        (width . 81)
        (vertical-scroll-bars . nil)
        (internal-border-width . 24)
        (left-fringe . 1)
        (right-fringe . 1)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (undecorated . t)))

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil)

;; Silence compiler warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Prevent resize during startup
(setq frame-inhibit-implied-resize t)

;; Remove command line options from startup
(setq inhibit-startup-echo-area-message t)

;; Restore settings after startup
(defun bv-early-init-restore ()
  "Restore settings after startup."
  ;; Reset garbage collection
  (setq gc-cons-threshold (* 20 1024 1024)  ; 20MB
        gc-cons-percentage 0.1)
  ;; Restore file handlers
  (setq file-name-handler-alist bv--file-name-handler-alist)
  ;; Re-enable runtime compilation
  (setq native-comp-deferred-compilation t))

(add-hook 'emacs-startup-hook #'bv-early-init-restore)

(provide 'early-init)
;;; early-init.el ends here
