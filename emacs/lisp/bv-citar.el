;;; bv-citar.el --- Citar configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Citar configuration for citation management UI.

;;; Code:

(eval-when-compile
  (require 'citar))

;; Declare external variables to avoid warnings
(defvar org-cite-global-bibliography)

(with-eval-after-load 'citar
  (when (boundp 'citar-library-paths)
    (setq citar-library-paths (list "~/documents/library")))
  (when (boundp 'citar-notes-paths)
    (setq citar-notes-paths (list "~/documents/slipbox/slips")))
  (when (and (boundp 'citar-bibliography)
             (boundp 'org-cite-global-bibliography))
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
  (when (and (featurep 'org-roam)
             (file-directory-p (expand-file-name "~/documents/slipbox/slips")))
    (citar-org-roam-mode 1)))

;; Keybindings moved to bv-citation.el for unified citation interface

(provide 'bv-citar)
;;; bv-citar.el ends here