;;; bv-undo.el --- Undo/redo UX and Vundo integration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Small undo/redo quality-of-life improvements:
;; - Provide easy redo keys via `undo-redo' (Emacs 28+).
;; - Add `vundo' for a visual undo tree when available.

;;; Code:

;; Better redo defaults (GUI-friendly, harmless in TTY).
(when (fboundp 'undo-redo)
  (global-set-key (kbd "C-S-z") #'undo-redo)
  (global-set-key (kbd "s-Z") #'undo-redo))

;; macOS-style undo on Super-z when available.
(global-set-key (kbd "s-z") #'undo)

;; Visual undo tree.
(when (require 'vundo nil t)
  (when (boundp 'vundo-compact-display)
    (setq vundo-compact-display t))
  (when (and (boundp 'vundo-glyph-alist)
             (boundp 'vundo-unicode-symbols))
    (setq vundo-glyph-alist vundo-unicode-symbols))
  (global-set-key (kbd "C-x u") #'vundo))

(provide 'bv-undo)
;;; bv-undo.el ends here
