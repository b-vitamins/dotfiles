;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Early initialization for performance and basic frame setup.
;; Executed before init.el and before package initialization.

;;; Code:

;; Declare external variables to silence elint warnings
(defvar native-comp-jit-compilation)
(defvar native-comp-async-report-warnings-errors)
(defvar inhibit-startup-echo-area-message)

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el loading packages prior to init.el
(setq package-enable-at-startup nil)

;; Prefer newer source files over stale `.elc' during iteration.
(setq load-prefer-newer t)

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
        (left-fringe . 8)
        (right-fringe . 8)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (undecorated . t)))

;; Prevent unwanted runtime compilation
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; Silence compiler warnings
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Prevent resize during startup
(setq frame-inhibit-implied-resize t)

;; Remove command line options from startup
(when (boundp 'inhibit-startup-echo-area-message)
  (setq inhibit-startup-echo-area-message t))

;; Restore settings after startup
(defun bv-early-init-restore ()
  "Restore settings after startup."
  ;; Reset garbage collection
  (setq gc-cons-threshold (* 20 1024 1024)  ; 20MB
        gc-cons-percentage 0.1)
  ;; Restore file handlers
  (setq file-name-handler-alist bv--file-name-handler-alist)
  ;; Re-enable runtime compilation
  (when (boundp 'native-comp-jit-compilation)
    (setq native-comp-jit-compilation t)))

(add-hook 'emacs-startup-hook #'bv-early-init-restore)

(provide 'early-init)
;;; early-init.el ends here
