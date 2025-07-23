;;; bv-layout.el --- Layout configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Frame and window layout configuration following nano-layout.

;;; Code:

(require 'disp-table)

(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height     . 45)
               '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))


(defface fallback '((t :family "Fira Code"))
  "Fallback face for glyphs missing in primary font.")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))


(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(tooltip-mode -1)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(setq x-underline-at-descent-line t)

(setq window-divider-default-right-width 0
      window-divider-default-places 'right-only)
(window-divider-mode 1)

(setq widget-image-enable nil)

(setq org-hide-emphasis-markers t)

(provide 'bv-layout)
;;; bv-layout.el ends here
