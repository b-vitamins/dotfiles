;;; bv-marginalia.el --- Marginalia configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for marginalia - rich annotations in the minibuffer.

;;; Code:

(require 'marginalia)

;; Align annotations to the left
(setq marginalia-align 'left)

;; Enable marginalia mode
(marginalia-mode 1)

(provide 'bv-marginalia)
;;; bv-marginalia.el ends here