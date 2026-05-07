;;; bv-re-builder.el --- Regex builder configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Interactive regular expression builder.

;;; Code:

(autoload 're-builder "re-builder")
(defvar bv-search-map)

(with-eval-after-load 're-builder
  (when (boundp 'reb-re-syntax)
    (setq reb-re-syntax 'string))
  (when (boundp 'reb-blink-delay)
    (setq reb-blink-delay 0.5)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-search-map)
    (define-key bv-search-map (kbd "x") 're-builder)))

(provide 'bv-re-builder)
;;; bv-re-builder.el ends here
