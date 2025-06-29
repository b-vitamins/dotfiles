;;; bv-keycast.el --- Keycast configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for keycast mode to display current commands and key bindings.
;; Provides a custom keycast mode with mode line integration.

;;; Code:

(eval-when-compile (require 'keycast))


(with-eval-after-load 'keycast
  (require 'moody)
  (when (boundp 'keycast-mode-line-window-predicate)
    (setq keycast-mode-line-window-predicate 'moody-window-active-p))
  (when (boundp 'keycast-mode-line-format)
    (setq keycast-mode-line-format "%k%c%r "))
  (when (boundp 'global-mode-string)
    (when (boundp 'keycast-mode-line)
      (add-to-list 'global-mode-string keycast-mode-line))))

(autoload 'keycast--update "keycast")

(define-minor-mode bv-keycast-mode
  "Show current command and its key binding in the mode line."
  :global t
  :group 'bv
  (if bv-keycast-mode
      (when (boundp 'post-command-hook)
        (add-hook 'post-command-hook 'keycast--update t))
    (when (boundp 'keycast--this-command)
      (setq keycast--this-command nil))
    (when (boundp 'keycast--this-command-keys)
      (setq keycast--this-command-keys nil))
    (when (boundp 'keycast--command-repetitions)
      (setq keycast--command-repetitions 0))
    (when (boundp 'post-command-hook)
      (remove-hook 'post-command-hook 'keycast--update))))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "k") 'bv-keycast-mode)
    (define-key bv-toggle-map (kbd "K") 'bv-keycast-mode)))

(provide 'bv-keycast)
;;; bv-keycast.el ends here