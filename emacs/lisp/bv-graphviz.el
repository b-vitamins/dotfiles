;;; bv-graphviz.el --- Graphviz configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Graphviz configuration for DOT language support and Org-mode
;; babel integration with inline image display.

;;; Code:

(autoload 'org-redisplay-inline-images "org")

(defun bv-graphviz-fix-inline-images ()
  "Fix display of inline images after Graphviz babel execution."
  (interactive)
  (when (and (boundp 'org-inline-image-overlays) org-inline-image-overlays)
    (org-redisplay-inline-images)))

(when (boundp 'org-babel-after-execute-hook)
  (add-hook 'org-babel-after-execute-hook 'bv-graphviz-fix-inline-images))

(with-eval-after-load 'ob-core
  (require 'ob-dot))

(with-eval-after-load 'ob-dot
  (when (boundp 'org-babel-default-header-args:dot)
    (add-to-list 'org-babel-default-header-args:dot
                 '(:cmdline . "-Kdot -Tpng"))))

(provide 'bv-graphviz)
;;; bv-graphviz.el ends here