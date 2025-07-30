;;; bv-graphviz.el --- Graph visualization  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Graphviz DOT language and diagram generation.

;;; Code:

(autoload 'graphviz-dot-mode "graphviz-dot-mode")

;; Declare external functions to satisfy elint
(declare-function org-redisplay-inline-images "org" (&optional include-linked refresh beg end))

;; Forward declarations
(declare-function bv-graphviz-display-inline-images "bv-graphviz")

;; File associations
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

;; Org-babel support
(with-eval-after-load 'org
  (require 'ob-dot)

  ;; Default settings for dot blocks
  (when (boundp 'org-babel-default-header-args:dot)
    (setq org-babel-default-header-args:dot
          '((:cmdline . "-Kdot -Tsvg")
            (:results . "file graphics")
            (:exports . "results"))))

  ;; Auto-display images after execution
  (defun bv-graphviz-display-inline-images ()
    "Refresh inline images after graphviz execution."
    (when (derived-mode-p 'org-mode)
      (org-redisplay-inline-images)))

  (add-hook 'org-babel-after-execute-hook #'bv-graphviz-display-inline-images))

;; Graphviz mode settings
(with-eval-after-load 'graphviz-dot-mode
  (when (boundp 'graphviz-dot-indent-width)
    (setq graphviz-dot-indent-width 2))
  (when (boundp 'graphviz-dot-auto-indent-on-newline)
    (setq graphviz-dot-auto-indent-on-newline t))
  (when (boundp 'graphviz-dot-auto-indent-on-braces)
    (setq graphviz-dot-auto-indent-on-braces t))
  (when (boundp 'graphviz-dot-auto-indent-on-semi)
    (setq graphviz-dot-auto-indent-on-semi t)))

(provide 'bv-graphviz)
;;; bv-graphviz.el ends here