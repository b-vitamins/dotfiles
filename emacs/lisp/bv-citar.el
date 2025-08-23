;;; bv-citar.el --- Citar configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Citar configuration for citation management UI.

;;; Code:

(eval-when-compile
  (require 'citar))

;; Declare external functions
(autoload 'citar-indicator-create "citar")
(autoload 'citar-has-files "citar")
(autoload 'citar-has-links "citar")
(autoload 'citar-is-cited "citar")
(autoload 'citar-capf-setup "citar")
(autoload 'citar-insert-preset "citar")
(autoload 'citar-file-parser-default "citar-file")
(autoload 'citar-file-parser-triplet "citar-file")

;; Declare external variables to avoid warnings
(defvar org-cite-global-bibliography)
(defvar citar-indicators)
(defvar citar-templates)
(defvar citar-presets)
(defvar citar-history)
(defvar citar-file-parser-functions)
(defvar citar-at-point-function)
(defvar citar-at-point-fallback)

(with-eval-after-load 'citar
  (when (boundp 'citar-library-paths)
    (setq citar-library-paths (list "~/documents")))
  (when (and (boundp 'citar-bibliography)
             (boundp 'org-cite-global-bibliography))
    (setq citar-bibliography org-cite-global-bibliography)))

(autoload 'citar-embark-mode "citar-embark")
(with-eval-after-load 'citar
  (citar-embark-mode 1))

;; Customize templates for better display
(with-eval-after-load 'citar
  (when (boundp 'citar-templates)
    (setq citar-templates
          '((main . "${author editor:20%sn}   ${date year issued:4}   ${title:60}")
            (suffix . "  ${=key= id:20}  ${=type=:12}  ${tags keywords:*}")
            (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
            (note . "Notes on ${author editor:%etal}, ${title}")))))

;; Custom faces for better visual distinction
(with-eval-after-load 'citar
  ;; Make citation keys stand out
  (when (facep 'citar-highlight)
    (set-face-attribute 'citar-highlight nil
                        :foreground 'unspecified
                        :inherit 'font-lock-keyword-face)))

;; Additional file extensions for PDFs and common document formats
(with-eval-after-load 'citar
  (when (boundp 'citar-library-file-extensions)
    (setq citar-library-file-extensions '("pdf" "djvu" "epub" "html")))

  ;; Use a hyphen separator for additional files (e.g., paper-supplement.pdf)
  (when (boundp 'citar-file-additional-files-separator)
    (setq citar-file-additional-files-separator "-")))

;; Configure predefined searches
(with-eval-after-load 'citar
  (when (boundp 'citar-presets)
    (setq citar-presets
          '("has:file"
            "has:notes"
            "has:links"
            "is:cited"))))

;; Configure file parsers for multiple formats (Zotero, Mendeley, Calibre)
(with-eval-after-load 'citar
  (when (boundp 'citar-file-parser-functions)
    (setq citar-file-parser-functions
          '(citar-file-parser-default
            citar-file-parser-triplet))))

;; Configure at-point behavior
(with-eval-after-load 'citar
  (when (boundp 'citar-at-point-function)
    (setq citar-at-point-function 'citar-open))
  (when (boundp 'citar-at-point-fallback)
    (setq citar-at-point-fallback t)))

;; Save citar history across sessions
(with-eval-after-load 'savehist
  (when (boundp 'savehist-additional-variables)
    (add-to-list 'savehist-additional-variables 'citar-history)))

;; Setup completion-at-point for citation keys
(defun bv-citar-setup-capf ()
  "Setup citar `completion-at-point' in supported modes."
  (when (fboundp 'citar-capf-setup)
    (citar-capf-setup)))

;; Add hooks for completion-at-point
(add-hook 'org-mode-hook #'bv-citar-setup-capf)
(add-hook 'LaTeX-mode-hook #'bv-citar-setup-capf)
(add-hook 'latex-mode-hook #'bv-citar-setup-capf)
(add-hook 'markdown-mode-hook #'bv-citar-setup-capf)

;; Keybindings moved to bv-citation.el for unified citation interface

(provide 'bv-citar)
;;; bv-citar.el ends here
