;;; bv-dape.el --- Debug adapter protocol configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for Dape (Debug Adapter Protocol for Emacs) with breakpoint
;; management and debugging session setup.

;;; Code:


(autoload 'dape-breakpoint-load "dape")
(autoload 'dape-breakpoint-global-mode "dape")

(when (boundp 'dape-buffer-window-arrangement)
  (setopt dape-buffer-window-arrangement 'right))
(when (boundp 'dape-stepping-granularity)
  (setopt dape-stepping-granularity 'line))
(when (boundp 'dape-on-start-hooks)
  (setopt dape-on-start-hooks '(dape-repl)))

(when (boundp 'dape-hide-info-mode-line)
  (setq dape-hide-info-mode-line nil))

(with-eval-after-load 'dape
  (when (boundp 'kill-emacs-hook)
    (add-hook 'kill-emacs-hook 'dape-breakpoint-save))
  (dape-breakpoint-load)
  
  (let ((fringe 4))
    (when (and (numberp fringe) (>= fringe 1))
      (dape-breakpoint-global-mode)))
  
  (dolist (mode '(dape-info))
    (add-hook 'dape-on-stopped-hooks (intern (format "%s" mode))))
  
  (when (boundp 'dape-on-start-hooks)
    (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))))

(provide 'bv-dape)
;;; bv-dape.el ends here