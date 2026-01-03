;;; bv-dape.el --- Debug adapter protocol configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Debug adapter protocol support.

;;; Code:

(require 'dape nil t)

;; Silence byte-compiler warnings
(defvar user-emacs-directory)
(defvar bv-debug-map)

(when (featurep 'dape)
  (when (boundp 'dape-buffer-window-arrangement)
    (setq dape-buffer-window-arrangement 'right))
  (when (boundp 'dape-stepping-granularity)
    (setq dape-stepping-granularity 'line))
  (when (boundp 'dape-on-start-hooks)
    (setq dape-on-start-hooks '(dape-repl)))
  (when (boundp 'dape-hide-info-mode-line)
    (setq dape-hide-info-mode-line nil))
  (when (boundp 'dape-info-hide-mode-line)
    (setq dape-info-hide-mode-line nil)))

(with-eval-after-load 'dape
  ;; Only save breakpoints if we have any
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (and (fboundp 'dape-breakpoint-save)
                         (boundp 'dape--breakpoints)
                         dape--breakpoints)
                (dape-breakpoint-save))))

  ;; Only load if file exists
  (when (and (fboundp 'dape-breakpoint-load)
             (file-exists-p (expand-file-name "dape-breakpoints" user-emacs-directory)))
    (dape-breakpoint-load))

  (when (fboundp 'dape-breakpoint-global-mode)
    (dape-breakpoint-global-mode 1))

  (add-hook 'dape-stopped-hook 'dape-info)
  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t))))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-debug-map)
    (define-key bv-debug-map (kbd "d") 'dape)
    (define-key bv-debug-map (kbd "b") 'dape-breakpoint-toggle)
    (define-key bv-debug-map (kbd "c") 'dape-continue)
    (define-key bv-debug-map (kbd "n") 'dape-next)
    (define-key bv-debug-map (kbd "s") 'dape-step-in)
    (define-key bv-debug-map (kbd "o") 'dape-step-out)
    (define-key bv-debug-map (kbd "r") 'dape-restart)
    (define-key bv-debug-map (kbd "q") 'dape-quit)))

(provide 'bv-dape)
;;; bv-dape.el ends here
