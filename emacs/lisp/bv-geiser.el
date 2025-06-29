;;; bv-geiser.el --- Geiser Scheme configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Geiser configuration for Scheme development with Guile integration,
;; REPL setup, and Org-mode babel support.

;;; Code:

(autoload 'xdg-cache-home "xdg")

(with-eval-after-load 'geiser-repl
  (require 'xdg)
  (when (boundp 'geiser-repl-query-on-kill-p)
    (setq geiser-repl-query-on-kill-p nil))
  (when (boundp 'geiser-repl-history-filename)
    (setq geiser-repl-history-filename
          (expand-file-name "emacs/geiser_history" (xdg-cache-home))))
  (when (boundp 'geiser-repl-add-project-paths)
    (setq geiser-repl-add-project-paths nil)))

(with-eval-after-load 'geiser-mode
  (geiser-eros-mode)
  (geiser-mode))

(with-eval-after-load 'geiser-impl
  (when (boundp 'geiser-default-implementation)
    (setq geiser-default-implementation 'guile))
  (when (boundp 'geiser-active-implementations)
    (setq geiser-active-implementations '(guile)))
  (when (boundp 'geiser-implementations-alist)
    (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))))

(with-eval-after-load 'org
  (when (boundp 'org-structure-template-alist)
    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))))

(with-eval-after-load 'ob-core
  (require 'ob-scheme))

(with-eval-after-load 'ob-scheme
  (when (boundp 'org-babel-default-header-args:scheme)
    (setq org-babel-default-header-args:scheme '((:results . "scalar")))))

(provide 'bv-geiser)
;;; bv-geiser.el ends here