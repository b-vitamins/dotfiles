;;; bv-help.el --- Help configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for enhanced help systems using helpful and embark.
;; Replaces default help commands with more informative alternatives.

;;; Code:

(when (boundp 'global-map)
  (let ((map global-map))
  (define-key map (vector 'remap 'describe-function) 'helpful-callable)
  (define-key map (vector 'remap 'describe-variable) 'helpful-variable)
  (define-key map (vector 'remap 'describe-key) 'helpful-key)
  (define-key map (vector 'remap 'describe-command) 'helpful-command)
  (define-key map (vector 'Info-goto-emacs-command-node) 'helpful-function)))

(when (boundp 'help-map)
  (define-key help-map "o" 'helpful-at-point))

(with-eval-after-load 'embark
  (when (boundp 'embark-symbol-map)
    (define-key embark-symbol-map
      (vector 'remap 'describe-symbol)
      'helpful-symbol))
  (when (boundp 'embark-become-help-map)
    (let ((map embark-become-help-map))
      (define-key map (vector 'remap 'describe-function) 'helpful-callable)
      (define-key map (vector 'remap 'describe-variable) 'helpful-variable)
      (define-key map (vector 'remap 'describe-symbol) 'helpful-symbol)
      (define-key map (vector 'remap 'describe-command) 'helpful-command))))

(when (boundp 'helpful-mode-hook)
  (add-hook 'helpful-mode-hook 'visual-line-mode))

(with-eval-after-load 'helpful
  (when (boundp 'helpful-mode-map)
    (define-key helpful-mode-map "q" 'kill-this-buffer)))

(with-eval-after-load 'help-mode
  (when (boundp 'help-mode-map)
    (define-key help-mode-map "q" 'kill-this-buffer))
  (when (boundp 'help-window-select)
    (setq help-window-select t)))

(provide 'bv-help)
;;; bv-help.el ends here