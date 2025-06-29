;;; early-init.el --- Early initialization -- lexical-binding: t; no-byte-compile: t --

;; Copyright (C) 2024 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Early initialization for Emacs 30+.
;;
;; This file focuses on:
;; - Time-aware flash prevention via frame parameters
;; - Package system configuration
;; - Minimal performance optimizations for startup
;;
;; All other configuration belongs in init.el.

;;; Code:

;;;; Constants - Time Configuration

(defconst bv-morning-start 5
  "Hour when morning theme starts.")

(defconst bv-day-start 10
  "Hour when day theme starts.")

(defconst bv-evening-start 18
  "Hour when evening theme starts.")

(defconst bv-night-start 23
  "Hour when night theme starts.")

(defconst bv-theme-colors
  '((morning . ((background-color . "#fbf7f0") (foreground-color . "#000000")))
    (day     . ((background-color . "#ffffff") (foreground-color . "#000000")))
    (evening . ((background-color . "#0d0e1c") (foreground-color . "#ffffff")))
    (night   . ((background-color . "#000000") (foreground-color . "#ffffff"))))
  "Theme colors for different times of day.")

;;;; Constants - Frame Configuration

(defconst bv-frame-width 120
  "Default frame width in characters.")

(defconst bv-frame-height 40
  "Default frame height in lines.")

(defconst bv-frame-internal-border 8
  "Internal border width in pixels.")

(defconst bv-frame-fringe-width 8
  "Fringe width in pixels.")

(defconst bv-frame-alpha 98
  "Frame transparency (0-100, where 100 is opaque).")

;;;; Constants - Performance

(defconst bv-gc-cons-threshold-startup (* 1024 1024 1024)
  "Garbage collection threshold during startup (1GB).")

(defconst bv-gc-cons-threshold-default (* 2 1024 1024)
  "Default garbage collection threshold after startup (2MB).")

(defconst bv-native-comp-jobs 8
  "Number of async native compilation jobs.")

(defconst bv-native-comp-speed 2
  "Native compilation optimization level (0-3).")

;;;; Frame Setup - Flash Prevention

;; Determine time period and apply appropriate colors
(let* ((hour (string-to-number (format-time-string "%H")))
       (time-period (cond
                     ((and (>= hour bv-morning-start) (< hour bv-day-start)) 'morning)
                     ((and (>= hour bv-day-start) (< hour bv-evening-start)) 'day)
                     ((and (>= hour bv-evening-start) (< hour bv-night-start)) 'evening)
                     (t 'night)))
       (colors (alist-get time-period bv-theme-colors)))
  (setq default-frame-alist
        (append colors
                `((menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (internal-border-width . ,bv-frame-internal-border)
                  (left-fringe . ,bv-frame-fringe-width)
                  (right-fringe . ,bv-frame-fringe-width)
                  (width . ,bv-frame-width)
                  (height . ,bv-frame-height)
                  (alpha . (,bv-frame-alpha . ,bv-frame-alpha))
                  (undecorated . t)))))

;;;; Package System

;; Disable package.el as we're using Guix/straight/elpaca/etc.
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;;;; Startup Behavior

;; Minimize startup UI
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      inhibit-startup-buffer-menu t)

;; Skip X resources to avoid conflicts
(setq inhibit-x-resources t)

;; Prevent resize during startup
(setq frame-inhibit-implied-resize t)

;;;; Performance - Garbage Collection

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold bv-gc-cons-threshold-startup
      gc-cons-percentage 0.6)

;;;; Performance - File Handlers

;; Temporarily disable file handlers for faster startup
(defvar bv--file-name-handler-alist-cache file-name-handler-alist
  "Cached value of `file-name-handler-alist'.")
(setq file-name-handler-alist nil)

;;;; Performance - Native Compilation

(when (featurep 'native-compile)
  ;; Configure native compilation settings
  (setq native-comp-async-jobs-number bv-native-comp-jobs
        native-comp-async-query-on-exit nil
        native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t
        native-comp-deferred-compilation t
        native-comp-speed bv-native-comp-speed))

;;;; Performance - File Loading

;; Prefer newer files during load to avoid stale bytecode
(setq load-prefer-newer t)

;; Temporarily disable version control for startup performance
(setq vc-handled-backends nil)

;; Don't compact font caches during startup
(setq inhibit-compacting-font-caches t)

;;;; Startup Hook - Restore Settings

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Reset GC threshold to a reasonable default
            (setq gc-cons-threshold bv-gc-cons-threshold-default
                  gc-cons-percentage 0.1)
            ;; Restore file handlers
            (setq file-name-handler-alist bv--file-name-handler-alist-cache)
            ;; Re-enable VC
            (setq vc-handled-backends '(Git))
            ;; Clear the cache variable
            (makunbound 'bv--file-name-handler-alist-cache))
          -50)  ; Run early in startup hook

(provide 'early-init)
;;; early-init.el ends here
