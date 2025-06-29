;;; bv-citation.el --- Citation configuration  -*- lexical-binding: t -*-
;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;;; Commentary:
;; Configuration for citation management using org-cite, citar, and related tools.
;; Sets up bibliography paths, processors, and visual icons for citation handling.
;;; Code:
(eval-when-compile
  (require 'citar)
  (require 'oc-biblatex)
  (require 'oc-csl))
(autoload 'all-the-icons-faicon "all-the-icons")
(autoload 'all-the-icons-material "all-the-icons")
(autoload 'all-the-icons-octicon "all-the-icons")
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
  (eval-when-compile (require 'all-the-icons))
  (when (boundp 'citar-symbols)
    (setq citar-symbols
          `((file ,(all-the-icons-faicon "file-o"
                                         :face 'all-the-icons-green
                                         :v-adjust -0.1)
                  . " ")
            (note ,(all-the-icons-material "speaker_notes"
                                           :face 'all-the-icons-blue
                                           :v-adjust -0.3)
                  . " ")
            (link ,(all-the-icons-octicon "link"
                                           :face 'all-the-icons-orange
                                           :v-adjust 0.01)
                  . " "))))
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
(when (boundp 'mode-specific-map)
  (let ((map mode-specific-map))
    (define-key map (kbd "b") 'org-cite-insert)
    (define-key map (kbd "n b") 'bv-find-main-bibliography)))
(provide 'bv-citation)
;;; bv-citation.el ends here
