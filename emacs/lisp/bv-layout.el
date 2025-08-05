;;; bv-layout.el --- Layout configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Frame and window layout configuration following nano-layout.

;;; Code:

(require 'disp-table)

;; Declare external variables to silence elint warnings
(defvar inhibit-startup-screen)
(defvar inhibit-startup-message)
(defvar inhibit-startup-echo-area-message)
(defvar initial-scratch-message)
(defvar window-divider-default-right-width)
(defvar window-divider-default-places)
(defvar widget-image-enable)
(defvar org-hide-emphasis-markers)

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

(setq window-divider-default-right-width 6
      window-divider-default-places 'right-only)
(window-divider-mode 1)

;; Add margins to text buffers for better readability
;; This creates visual breathing room around text content
(defun bv-layout--set-text-margins ()
  "Set comfortable margins for text-based buffers."
  (let ((margin-width 2))  ; 2 characters of margin on each side
    (setq left-margin-width margin-width
          right-margin-width margin-width)
    ;; Update the window to show the margins
    (set-window-buffer (selected-window) (current-buffer))))

;; Apply margins to text-oriented modes for better readability
(add-hook 'text-mode-hook #'bv-layout--set-text-margins)
(add-hook 'org-mode-hook #'bv-layout--set-text-margins)
(add-hook 'markdown-mode-hook #'bv-layout--set-text-margins)
(add-hook 'latex-mode-hook #'bv-layout--set-text-margins)
(add-hook 'LaTeX-mode-hook #'bv-layout--set-text-margins)

;; Documentation and reading modes
(add-hook 'help-mode-hook #'bv-layout--set-text-margins)
(add-hook 'info-mode-hook #'bv-layout--set-text-margins)
(add-hook 'woman-mode-hook #'bv-layout--set-text-margins)
(add-hook 'Man-mode-hook #'bv-layout--set-text-margins)

;; Web and feed reading
(add-hook 'eww-mode-hook #'bv-layout--set-text-margins)
(add-hook 'elfeed-show-mode-hook #'bv-layout--set-text-margins)

;; Email reading and composition
(add-hook 'mu4e-view-mode-hook #'bv-layout--set-text-margins)
(add-hook 'message-mode-hook #'bv-layout--set-text-margins)
(add-hook 'mail-mode-hook #'bv-layout--set-text-margins)

;; Other text formats
(add-hook 'rst-mode-hook #'bv-layout--set-text-margins)  ; reStructuredText
(add-hook 'adoc-mode-hook #'bv-layout--set-text-margins)  ; AsciiDoc

(setq widget-image-enable nil)

(setq org-hide-emphasis-markers t)

;; Enable visual line mode globally
(global-visual-line-mode 1)

(provide 'bv-layout)
;;; bv-layout.el ends here
