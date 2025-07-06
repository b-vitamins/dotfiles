;;; bv-citation.el --- Citation management  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Research citation management with org-cite and citar.

;;; Code:

(eval-when-compile
  (require 'citar)
  (require 'oc-biblatex)
  (require 'oc-csl))
(with-eval-after-load 'bibtex
  (when (boundp 'bibtex-dialect)
    (setq bibtex-dialect 'biblatex)))
(with-eval-after-load 'oc
  (require 'oc-csl)
  (when (boundp 'org-cite-global-bibliography)
    (setq org-cite-global-bibliography
          (list "~/documents/slipbox/bibliography/physics.bib"
                "~/documents/slipbox/bibliography/ml.bib"
                "~/documents/slipbox/bibliography/neurosymbolic.bib")))
  (when (boundp 'org-cite-insert-processor)
    (setq org-cite-insert-processor 'citar))
  (when (boundp 'org-cite-follow-processor)
    (setq org-cite-follow-processor 'citar))
  (when (boundp 'org-cite-activate-processor)
    (setq org-cite-activate-processor 'citar))
  (when (boundp 'org-cite-export-processors)
    (setq org-cite-export-processors '((latex biblatex) (t csl)))))
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
(defun bv-find-main-bibliography ()
  "Find and open main bibliography file."
  (interactive)
  (find-file "~/documents/slipbox/bibliography/physics.bib"))
(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "b") 'citar-open)
    (define-key bv-app-map (kbd "B") 'citar-insert-citation)))

(with-eval-after-load 'org
  (when (boundp 'org-mode-map)
    (define-key org-mode-map (kbd "C-c b") 'org-cite-insert)))
(provide 'bv-citation)
;;; bv-citation.el ends here
