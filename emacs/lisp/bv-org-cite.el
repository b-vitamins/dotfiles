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

;; Declare external functions and variables
(autoload 'citar-dwim "citar")
(autoload 'embark-act "embark")

;; Declare org-cite variables that might not be loaded yet
(defvar org-cite-global-bibliography)
(defvar org-cite-insert-processor)
(defvar org-cite-follow-processor)
(defvar org-cite-activate-processor)
(defvar org-cite-export-processors)
(defvar org-cite-csl-styles-dir)

(defun bv-org-cite-setup-oc ()
  "Setup org-cite configuration."
  (require 'oc-csl)
  (when (boundp 'org-cite-global-bibliography)
    (setq org-cite-global-bibliography
          (bv-org-cite-find-bibliography-files)))
  (when (boundp 'org-cite-insert-processor)
    (setq org-cite-insert-processor 'citar))
  (when (boundp 'org-cite-follow-processor)
    (setq org-cite-follow-processor 'citar))
  (when (boundp 'org-cite-activate-processor)
    (setq org-cite-activate-processor 'citar))
  (when (boundp 'org-cite-export-processors)
    (setq org-cite-export-processors '((latex biblatex) (t csl)))))

(defun bv-org-cite-find-bibliography-files ()
  "Find all .bib files in bibliography directories."
  (let ((dirs '("~/projects/bibliography/curated"
                "~/projects/bibliography/meta"
                "~/projects/bibliography/books"))
        (bib-files '()))
    (dolist (dir dirs)
      (when (file-directory-p (expand-file-name dir))
        (setq bib-files
              (append bib-files
                      (directory-files (expand-file-name dir) t "\\.bib$")))))
    bib-files))

(with-eval-after-load 'oc
  (bv-org-cite-setup-oc))

;; Setup local keymap for citations in org buffers
(defvar bv-org-cite-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'citar-dwim)
    (when (fboundp 'embark-act)
      (define-key map (kbd "<mouse-3>") 'embark-act))
    map)
  "Local keymap for citations in `org-mode' buffers.")

;; Additional org-cite configuration
(with-eval-after-load 'org
  ;; Support citation styles and variants
  (when (boundp 'org-cite-csl-styles-dir)
    (setq org-cite-csl-styles-dir "~/documents/styles")))

;; Keybindings moved to bv-citation.el for unified citation interface

(provide 'bv-org-cite)
;;; bv-org-cite.el ends here