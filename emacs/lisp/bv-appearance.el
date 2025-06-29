;;; bv-appearance.el --- Appearance configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Visual appearance settings including cursor, fringe, modeline,
;; and window decorations.

;;; Code:


(when (boundp 'cursor-type)
  (set-default 'cursor-type '(bar . 1)))
(blink-cursor-mode 0)
(when (boundp 'cursor-in-non-selected-windows)
  (setq-default cursor-in-non-selected-windows nil))
(when (boundp 'bookmark-set-fringe-mark)
  (setq bookmark-set-fringe-mark nil))

(with-eval-after-load 'minions
  (when (boundp 'minions-mode-line-lighter)
    (setq minions-mode-line-lighter ";")))

(when (boundp 'after-init-hook)
  (add-hook 'after-init-hook 'minions-mode))

(when (boundp 'mode-line-compact)
  (setq mode-line-compact 'long))

(when (boundp 'minions-mode-line-minor-modes-map)
  (setq minions-mode-line-minor-modes-map
        (let ((map (make-sparse-keymap)))
          (define-key map
            (vector 'header-line 'down-mouse-1)
            'minions-minor-modes-menu)
          map)))

(defun bv--move-mode-line-to-header ()
  "Move mode-line to header-line."
  (when (boundp 'header-line-format)
    (setq-local header-line-format mode-line-format))
  (when (boundp 'mode-line-format)
    (setq-local mode-line-format nil)))

(when (boundp 'calendar-initial-window-hook)
  (add-hook 'calendar-initial-window-hook 'bv--move-mode-line-to-header))

(when (and (boundp 'header-line-format) (boundp 'mode-line-format))
  (setq-default header-line-format mode-line-format)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil))

(with-eval-after-load 'menu-bar (menu-bar-mode 0))
(with-eval-after-load 'tool-bar (tool-bar-mode 0))
(with-eval-after-load 'scroll-bar (scroll-bar-mode 0))
(with-eval-after-load 'fringe (fringe-mode 4))

(set-frame-parameter (selected-frame) 'internal-border-width 12)

(when (boundp 'use-dialog-box)
  (setq use-dialog-box nil))
(when (boundp 'use-file-dialog)
  (setq use-file-dialog nil))

(when (boundp 'window-divider-default-right-width)
  (setq window-divider-default-right-width 12))
(window-divider-mode)

(provide 'bv-appearance)
;;; bv-appearance.el ends here