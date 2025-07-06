;;; bv-theme-light.el --- Light theme colors  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Light theme with muted color palette.

;;; Code:

(require 'bv-base-colors)

(defun bv-theme-set-light ()
  "Apply light theme base."
  (setq frame-background-mode    'light)
  (setq bv-color-foreground "#3a3a3a") ;; Charcoal
  (setq bv-color-background "#FFFFFF") ;; Pure white
  (setq bv-color-highlight  "#F7F7F7") ;; Off-white
  (setq bv-color-critical   "#d7875f") ;; Terracotta
  (setq bv-color-salient    "#5f87d7") ;; Cornflower blue
  (setq bv-color-strong     "#262626") ;; Dark charcoal
  (setq bv-color-popout     "#87afaf") ;; Sage green
  (setq bv-color-subtle     "#eeeeee") ;; Light gray
  (setq bv-color-faded      "#767676") ;; Medium gray
  (setq bv-theme-var "light"))

(provide 'bv-theme-light)
;;; bv-theme-light.el ends here