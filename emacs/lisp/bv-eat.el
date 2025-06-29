;;; bv-eat.el --- Emulate A Terminal configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for the eat terminal emulator package.

;;; Code:


(with-eval-after-load 'eat
  (when (boundp 'eat-line-input-ring-size)
    (setq eat-line-input-ring-size 4096))
  (when (boundp 'eat-kill-buffer-on-exit)
    (setq eat-kill-buffer-on-exit t))
  (when (boundp 'eat-term-scrollback-size)
    (setq eat-term-scrollback-size nil))
  (when (boundp 'eat-enable-mouse)
    (setq eat-enable-mouse t)))

(provide 'bv-eat)
;;; bv-eat.el ends here