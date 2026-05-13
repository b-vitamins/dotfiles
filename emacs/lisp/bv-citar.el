;;; bv-citar.el --- Citar configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Citar configuration for citation management UI.

;;; Code:

(require 'bibtex)

(eval-when-compile
  (require 'citar))

;; Declare external functions
(autoload 'citar-indicator-create "citar")
(autoload 'citar-has-files "citar")
(autoload 'citar-has-links "citar")
(autoload 'citar-has-notes "citar")
(autoload 'citar-is-cited "citar")
(autoload 'citar-insert-preset "citar")
(autoload 'embark-act "embark" nil t)
(autoload 'nerd-icons-faicon "nerd-icons")
(autoload 'nerd-icons-mdicon "nerd-icons")
(autoload 'nerd-icons-octicon "nerd-icons")

;; Declare external variables to avoid warnings
(defvar org-cite-global-bibliography)
(defvar citar-indicators)
(defvar citar-templates)
(defvar citar-presets)
(defvar citar-history)
(defvar citar-file-parser-functions)
(defvar citar-at-point-function)
(defvar citar-at-point-fallback)
(defvar citar-ellipsis)
(defvar bv-completion-truncation-marker)

;;; Rich UI

(defun bv-citar--templates ()
  "Return Citar templates following its documented rich UI segments."
  '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
    (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
    (note . "Notes on ${author editor:%etal}, ${title}")))

(defun bv-citar--indicator (function icon fallback face &optional v-adjust)
  "Return a Citar indicator glyph from FUNCTION, or FALLBACK.
ICON is passed to FUNCTION and FACE colors the glyph."
  (if (fboundp function)
      (condition-case nil
          (funcall function icon :face face :v-adjust (or v-adjust -0.1))
        (error (propertize fallback 'face face)))
    (propertize fallback 'face face)))

(defun bv-citar--indicators ()
  "Return BV-styled Citar resource indicators."
  (list
   (citar-indicator-create
    :symbol (bv-citar--indicator #'nerd-icons-faicon "nf-fa-file_pdf_o"
                                 "F" 'bv-icon-warning)
    :function #'citar-has-files
    :padding "  "
    :tag "has:files")
   (citar-indicator-create
    :symbol (bv-citar--indicator #'nerd-icons-mdicon "nf-md-notebook"
                                 "N" 'bv-icon-note -0.3)
    :function #'citar-has-notes
    :padding "  "
    :tag "has:notes")
   (citar-indicator-create
    :symbol (bv-citar--indicator #'nerd-icons-octicon "nf-oct-link"
                                 "L" 'bv-icon-info)
    :function #'citar-has-links
    :padding "  "
    :tag "has:links")
   (citar-indicator-create
    :symbol (bv-citar--indicator #'nerd-icons-octicon "nf-oct-check_circle"
                                 "C" 'bv-icon-success)
    :function #'citar-is-cited
    :padding "  "
    :tag "is:cited")))

(defun bv-citar--ellipsis ()
  "Return the Citar truncation marker."
  (if (boundp 'bv-completion-truncation-marker)
      bv-completion-truncation-marker
    "  ->"))

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
    (setq citar-templates (bv-citar--templates)))
  (when (boundp 'citar-indicators)
    (setq citar-indicators (bv-citar--indicators)))
  (when (boundp 'citar-ellipsis)
    (setq citar-ellipsis (bv-citar--ellipsis))))

;; Citar faces are owned by the BV theme adapter so citation pickers keep the
;; same completion-current/metadata language as the rest of the stack.

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
;; Note: citar already includes both parsers by default:
;;   citar-file--parser-default (for Zotero-style paths)
;;   citar-file--parser-triplet (for Mendeley/Calibre format)
;; No configuration needed as both are already in the default value

;; Configure at-point behavior
(with-eval-after-load 'citar
  (when (boundp 'citar-at-point-function)
    (setq citar-at-point-function 'embark-act))
  (when (boundp 'citar-at-point-fallback)
    (setq citar-at-point-fallback t)))

;; Save citar history across sessions
(with-eval-after-load 'savehist
  (when (boundp 'savehist-additional-variables)
    (add-to-list 'savehist-additional-variables 'citar-history)))

;; Setup completion-at-point for citation keys
(defun bv-citar-setup-capf ()
  "Setup citar `completion-at-point' in supported modes."
  ;; Ensure citar is loaded before adding capf
  (require 'citar nil t)
  (when (fboundp 'citar-capf)
    (add-to-list 'completion-at-point-functions #'citar-capf)))

;; Add hooks for completion-at-point
(add-hook 'org-mode-hook #'bv-citar-setup-capf)
(add-hook 'LaTeX-mode-hook #'bv-citar-setup-capf)
(add-hook 'latex-mode-hook #'bv-citar-setup-capf)
(add-hook 'markdown-mode-hook #'bv-citar-setup-capf)

;; Keybindings moved to bv-citation.el for unified citation interface

(provide 'bv-citar)
;;; bv-citar.el ends here
