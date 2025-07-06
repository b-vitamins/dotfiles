;;; bv-theme-dark.el --- Dark theme colors  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Dark theme with muted color palette.

;;; Code:

(require 'bv-base-colors)

(defun bv-theme-set-dark ()
  "Apply dark theme base."
  (setq frame-background-mode     'dark)
  (setq bv-color-foreground "#dadada") ;; Platinum
  (setq bv-color-background "#1c1c1c") ;; Eerie black
  (setq bv-color-highlight  "#303030") ;; Jet
  (setq bv-color-critical   "#d7875f") ;; Terracotta
  (setq bv-color-salient    "#5f87d7") ;; Cornflower blue
  (setq bv-color-strong     "#eeeeee") ;; White smoke
  (setq bv-color-popout     "#afaf87") ;; Olive
  (setq bv-color-subtle     "#303030") ;; Jet
  (setq bv-color-faded      "#6c6c6c") ;; Dim gray
  (setq bv-theme-var "dark"))

(provide 'bv-theme-dark)
;;; bv-theme-dark.el ends here