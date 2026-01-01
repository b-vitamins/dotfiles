;;; bv-pulse.el --- Cursor pulse for better point tracking -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Use built-in `pulse' to momentarily highlight the current line after common
;; navigation commands (buffer/window switches, jumps, and recentering).

;;; Code:

(require 'pulse)

(defgroup bv-pulse ()
  "Cursor pulse for better point tracking."
  :group 'convenience
  :prefix "bv-pulse-")

(defcustom bv-pulse-enabled t
  "When non-nil, enable line pulsing."
  :type 'boolean
  :group 'bv-pulse)

(defcustom bv-pulse-commands
  '(recenter-top-bottom
    other-window
    windmove-left windmove-right windmove-up windmove-down
    switch-to-buffer switch-to-buffer-other-window
    pop-to-buffer pop-to-buffer-same-window
    find-file find-file-other-window
    xref-find-definitions xref-find-references xref-go-back xref-go-forward
    imenu goto-line)
  "Commands that should trigger a pulse after executing."
  :type '(repeat symbol)
  :group 'bv-pulse)

(defvar bv-pulse--setup-complete nil
  "Non-nil when `bv-pulse-setup' has already run.")

(defun bv-pulse-line (&rest _)
  "Pulse the current line."
  (when bv-pulse-enabled
    (pulse-momentary-highlight-one-line (point))))

(defun bv-pulse-setup ()
  "Setup cursor pulsing for common navigation commands."
  (unless bv-pulse--setup-complete
    (setq bv-pulse--setup-complete t)

    ;; Gentle, non-distracting pulse.
    (setq pulse-iterations 8
          pulse-delay 0.04)

    (dolist (fn bv-pulse-commands)
      (when (fboundp fn)
        (advice-add fn :after #'bv-pulse-line)))

    ;; Integrations for packages that expose jump hooks.
    (with-eval-after-load 'consult
      (when (boundp 'consult-after-jump-hook)
        (add-hook 'consult-after-jump-hook #'bv-pulse-line)))

    (with-eval-after-load 'bookmark
      (when (boundp 'bookmark-after-jump-hook)
        (add-hook 'bookmark-after-jump-hook #'bv-pulse-line)))

    (with-eval-after-load 'imenu
      (when (boundp 'imenu-after-jump-hook)
        (add-hook 'imenu-after-jump-hook #'bv-pulse-line)))))

(bv-pulse-setup)

(provide 'bv-pulse)
;;; bv-pulse.el ends here
