;;; early-init.el --- BV Emacs early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; Early initialization for Emacs 30+
;; Focuses on startup performance and minimal UI setup
;; Target: sub-0.5s startup time

;;; Code:

;;;; Performance optimizations

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset gc-cons-threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 1.0)))

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil)

;; Disable package.el in favor of use-package
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable unnecessary startup features
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Prevent stale elisp bytecode
(setq load-prefer-newer t)

;;;; UI optimizations

;; Remove all bars early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Set up minimal UI inspired by RDE's appearance settings
(push '(internal-border-width . 8) default-frame-alist)
(push '(left-fringe . 8) default-frame-alist)
(push '(right-fringe . 8) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

;; Set frame size
(push '(width . 140) default-frame-alist)
(push '(height . 50) default-frame-alist)

;; Disable UI elements before they're loaded
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Smoother scrolling
(when (boundp 'pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-use-momentum t))

;; Optimize file handling
(setq vc-handled-backends nil)  ; Disable VC in early init
(setq create-lockfiles nil)     ; No lock files
(setq make-backup-files nil)    ; Handle backups later

;; Silence compiler warnings
(setq native-comp-async-report-warnings-errors nil
      warning-minimum-level :error)

;; Font rendering optimizations for macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

(provide 'early-init)
;;; early-init.el ends here

