;;; bv-keymaps.el --- Keymap configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Definition of custom prefix keymaps for applications and toggles.
;; Provides centralized keymap infrastructure for other modules.

;;; Code:

(define-prefix-command 'bv-app-map nil)

(define-prefix-command 'bv-toggle-map nil)

(provide 'bv-keymaps)
;;; bv-keymaps.el ends here