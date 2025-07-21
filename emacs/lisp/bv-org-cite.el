;;; bv-org-cite.el --- Org-cite configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Org-cite configuration for academic citation management.

;;; Code:

(eval-when-compile
  (require 'oc-biblatex)
  (require 'oc-csl))

(with-eval-after-load 'oc
  (require 'oc-csl)
  (when (boundp 'org-cite-global-bibliography)
    (setq org-cite-global-bibliography
          (list "~/documents/slipbox/bibliography/neurips.bib"
                "~/documents/slipbox/bibliography/icml.bib"
                "~/documents/slipbox/bibliography/iclr.bib"
                "~/documents/slipbox/bibliography/neurosymbolic.bib")))
  (when (boundp 'org-cite-insert-processor)
    (setq org-cite-insert-processor 'citar))
  (when (boundp 'org-cite-follow-processor)
    (setq org-cite-follow-processor 'citar))
  (when (boundp 'org-cite-activate-processor)
    (setq org-cite-activate-processor 'citar))
  (when (boundp 'org-cite-export-processors)
    (setq org-cite-export-processors '((latex biblatex) (t csl)))))

(with-eval-after-load 'org
  (when (boundp 'org-mode-map)
    (define-key org-mode-map (kbd "C-c b") 'org-cite-insert)))

(provide 'bv-org-cite)
;;; bv-org-cite.el ends here