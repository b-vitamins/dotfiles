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

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el loading packages prior to init.el
(setq package-enable-at-startup nil)

;; Prefer newer source files over stale `.elc' during iteration.
(setq load-prefer-newer t)

(add-to-list 'load-path
             (expand-file-name "lisp" (file-name-directory load-file-name)))

;; Emacs 31 loads site-start.el, and with it every Guix package's autoloads,
;; before this file.  Some store autoload files lack a `lexical-binding'
;; cookie, which Emacs 31 reports on every start.  Those files are read-only
;; and not actionable here, so drop only those reports.
(defun bv-early-init--store-lexbind-warning-p (type)
  "Return non-nil if warning TYPE is a missing cookie in a store file."
  (and (eq (car-safe type) 'files)
       (eq (nth 1 type) 'missing-lexbind-cookie)
       (stringp (nth 2 type))
       (string-prefix-p "/gnu/store/" (nth 2 type))))

(defun bv-early-init-drop-store-lexbind-warnings ()
  "Discard startup warnings about store files lacking a lexical cookie."
  (setq delayed-warnings-list
        (seq-remove (lambda (warning)
                      (bv-early-init--store-lexbind-warning-p (car warning)))
                    delayed-warnings-list))
  ;; A daemon logs warnings immediately and queues a one-shot hook that shows
  ;; *Warnings* on the first client frame.  Clear both only when every logged
  ;; warning is one of these store reports, so real warnings still surface.
  (when-let* (((daemonp))
              (buffer (get-buffer "*Warnings*")))
    (with-current-buffer buffer
      (when (= (count-matches "Warning (" (point-min) (point-max))
               (count-matches "Missing .lexical-binding. cookie in \"/gnu/store/"
                              (point-min) (point-max)))
        (setq after-make-frame-functions
              (seq-remove (lambda (fn)
                            (and (not (symbolp fn))
                                 (string-match-p "warning--display-buffer"
                                                 (prin1-to-string fn))))
                          after-make-frame-functions))
        (kill-buffer buffer)))))

(bv-early-init-drop-store-lexbind-warnings)

;; Disable file handlers during startup
(defvar bv--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Basic frame parameters
(require 'bv-layout-core)
(bv-layout-apply-default-frame-alist)

;; Prevent unwanted runtime compilation
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; Silence compiler warnings
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Prevent resize during startup
(setq frame-inhibit-implied-resize t)

;; Suppress the "For information about GNU Emacs..." startup echo.
;; `inhibit-startup-echo-area-message' only honors a literal login name
;; written in the user init file, so override the display function instead.
(advice-add 'display-startup-echo-area-message :override #'ignore)

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
