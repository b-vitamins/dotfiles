;;; bv-citar.el --- Citar configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Citar configuration for citation management UI.

;;; Code:

(eval-when-compile
  (require 'citar))

(with-eval-after-load 'citar
  (when (boundp 'citar-library-paths)
    (setq citar-library-paths (list "~/documents/library")))
  (when (boundp 'citar-notes-paths)
    (setq citar-notes-paths (list "~/documents/slipbox/slips")))
  (when (boundp 'citar-bibliography)
    (setq citar-bibliography org-cite-global-bibliography)))

(autoload 'citar-embark-mode "citar-embark")
(with-eval-after-load 'citar
  (citar-embark-mode 1))

(autoload 'citar-org-roam-mode "citar-org-roam")
(with-eval-after-load 'citar
  (when (boundp 'citar-org-roam-note-title-template)
    (setq citar-org-roam-note-title-template "${title} - ${author editor}"))
  (when (boundp 'citar-org-roam-subdir)
    (setq citar-org-roam-subdir "~/documents/slipbox/slips"))
  (citar-org-roam-mode 1))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "b") 'citar-open)
    (define-key bv-app-map (kbd "B") 'citar-insert-citation)))

(provide 'bv-citar)
;;; bv-citar.el ends here