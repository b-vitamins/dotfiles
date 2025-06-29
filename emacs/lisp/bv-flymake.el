;;; bv-flymake.el --- Flymake configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Basic Flymake configuration for syntax checking with keybindings
;; for navigating between errors.

;;; Code:


(with-eval-after-load 'flymake
  (when (boundp 'flymake-mode-map)
    (let ((map flymake-mode-map))
      (define-key map (kbd "M-n") 'flymake-goto-next-error)
      (define-key map (kbd "M-p") 'flymake-goto-prev-error))))

(provide 'bv-flymake)
;;; bv-flymake.el ends here