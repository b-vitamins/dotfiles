;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; Early initialization for Emacs 30+
;; Optimized for fast startup with theme-aware flash prevention
;;; Code:

;;;; Flash Prevention
;; Set initial background based on time to prevent white flash
(defun bv-set-initial-background ()
  "Set initial background color based on time of day to reduce startup flash."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
     ;; Morning: soft light
     ((and (>= hour 5) (< hour 10))
      (set-face-background 'default "#fbf7f0")
      (set-face-foreground 'default "#000000"))
     ;; Day: full light
     ((and (>= hour 10) (< hour 18))
      (set-face-background 'default "#ffffff")
      (set-face-foreground 'default "#000000"))
     ;; Evening: soft dark
     ((and (>= hour 18) (< hour 23))
      (set-face-background 'default "#0d0e1c")
      (set-face-foreground 'default "#ffffff"))
     ;; Night: full dark
     (t
      (set-face-background 'default "#000000")
      (set-face-foreground 'default "#ffffff")))))

;; Apply immediately to prevent flash
(bv-set-initial-background)

;;;; Performance Optimizations

;; Defer garbage collection during startup
(setq gc-cons-threshold (* 4 1024 1024 1024)
      gc-cons-percentage 0.8)

;; Store initial file-name-handler-alist
(defvar bv--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Reset after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 512 1024 1024)
                  gc-cons-percentage 0.2
                  file-name-handler-alist bv--file-name-handler-alist)))

;; Native compilation
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-async-jobs-number 8)
  (setq native-comp-async-query-on-exit nil)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-jit-compilation t)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-speed 3))

;; Process output
(setq read-process-output-max (* 4 1024 1024))

;; Increase undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 128 1024 1024))

;; Disable package.el (using Guix)
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Prevent stale bytecode
(setq load-prefer-newer t)

;; Better subprocess handling
(setq process-adaptive-read-buffering nil)

;;;; UI Optimizations

;; Disable UI elements before they're loaded
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Frame parameters
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars)
        (horizontal-scroll-bars)
        (internal-border-width . 8)
        (left-fringe . 8)
        (right-fringe . 8)
        (width . 120)
        (height . 40)
        (alpha . (98 . 98))
        (undecorated . t)))

;; Frame behavior
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Startup settings
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      inhibit-x-resources t
      inhibit-startup-buffer-menu t)

;; Smooth scrolling preparation
(when (boundp 'pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-use-momentum t
        pixel-scroll-precision-interpolation-total-time 0.18
        pixel-scroll-precision-interpolation-between-scroll 0.01))

;; Font rendering
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-defer-time 0)
(setq redisplay-skip-fontification-on-input t)

;; File handling
(setq vc-handled-backends nil  ; Disable VC during startup
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil)

;; History and cache sizes
(setq history-length 10000
      kill-ring-max 10000
      mark-ring-max 64
      global-mark-ring-max 128)

;; Silence warnings
(setq native-comp-async-report-warnings-errors nil
      warning-minimum-level :error)

;; Misc performance
(setq idle-update-delay 0.5)
(setq inhibit-compacting-font-caches t)

(provide 'early-init)
;;; early-init.el ends here
