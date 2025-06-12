;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; Early initialization for Emacs 30+
;; Target: sub-0.5s startup time

;;; Code:

;;;; Performance optimizations

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 1.0)))

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil)

;; Disable package.el (using Guix)
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable startup features
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Prevent stale bytecode
(setq load-prefer-newer t)

;;;; UI optimizations

;; Remove all bars early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Minimal UI with margins
(push '(internal-border-width . 8) default-frame-alist)
(push '(left-fringe . 8) default-frame-alist)
(push '(right-fringe . 8) default-frame-alist)

;; Frame size
(push '(width . 140) default-frame-alist)
(push '(height . 50) default-frame-alist)

;; Disable UI elements before load
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Smooth scrolling
(when (boundp 'pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-use-momentum t))

;; Optimize file handling
(setq vc-handled-backends nil)  ; Disable VC early
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Silence warnings
(setq native-comp-async-report-warnings-errors nil
      warning-minimum-level :error)

(provide 'early-init)
;;; early-init.el ends here
