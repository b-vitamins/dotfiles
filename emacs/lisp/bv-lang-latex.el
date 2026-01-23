;;; bv-lang-latex.el --- LaTeX development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; LaTeX development environment with AUCTeX, tree-sitter, and comprehensive tooling.

;;; Code:

(require 'tex-site nil t)
(require 'latex nil t)

;; External variables
(defvar LaTeX-mode-map)
(defvar TeX-master-file)
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(defvar TeX-save-query)
(defvar TeX-master)
(defvar LaTeX-indent-level)
(defvar LaTeX-item-indent)
(defvar TeX-brace-indent-level)
(defvar TeX-auto-untabify)
(defvar TeX-engine)
(defvar TeX-PDF-mode)
(defvar TeX-show-compilation)
(defvar TeX-clean-confirm)
(defvar TeX-view-program-selection)
(defvar TeX-view-program-list)
(defvar TeX-source-correlate-mode)
(defvar TeX-source-correlate-method)
(defvar TeX-source-correlate-start-server)
(defvar reftex-plug-into-AUCTeX)
(defvar reftex-default-bibliography)
(defvar preview-auto-cache-preamble)
(defvar preview-scale-function)
(defvar auctex-latexmk-inherit-TeX-PDF-mode)
(defvar completion-at-point-functions)
(defvar ispell-parser)

;; External functions
(declare-function TeX-command "tex-buf" (name file))
(declare-function TeX-command-sequence "tex-buf" (commands file))
(declare-function TeX-pdf-tools-sync-view "tex" ())
(declare-function TeX-revert-document-buffer "tex" (file))
(declare-function TeX-next-error "tex-buf" (&optional arg))
(declare-function TeX-previous-error "tex-buf" (&optional arg))
(declare-function LaTeX-environment "latex" (&optional arg))
(declare-function LaTeX-section "latex" (&optional arg))
(declare-function LaTeX-fill-paragraph "latex" (&optional justify))
(declare-function LaTeX-fill-region "latex" (from to &optional justify what))
(declare-function LaTeX-arguments-completion-at-point "latex" ())
(declare-function LaTeX-environment-completion-at-point "latex" ())
(declare-function LaTeX-macro-completion-at-point "latex" ())
(declare-function turn-on-reftex "reftex" ())
(declare-function reftex-citation "reftex-cite" (&optional no-insert format-key))
(declare-function reftex-reference "reftex-ref" (&optional type no-insert cut))
(declare-function reftex-toc "reftex-toc" (&optional rebuild))
(declare-function preview-at-point "preview" ())
(declare-function preview-environment "preview" (count))
(declare-function preview-section "preview" ())
(declare-function preview-clearout "preview" (&optional start end keep-dir))
(declare-function preview-scale-from-face "preview" ())
(declare-function auctex-latexmk-setup "auctex-latexmk" ())
(declare-function flyspell-mode "flyspell" (&optional arg))

;; Optional package loading
(autoload 'cdlatex-mode "cdlatex" nil t)
(autoload 'latex-preview-pane-mode "latex-preview-pane" nil t)
(autoload 'bibtex-mode "bibtex" nil t)
(autoload 'turn-on-reftex "reftex" nil t)

;; Enable tree-sitter for LaTeX when available
;; NOTE: Emacs doesn't include a built-in latex-ts-mode yet,
;; so we use the built-in latex-mode from tex-mode.el
;; (when (and (treesit-language-available-p 'latex)
;;            (fboundp 'latex-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(latex-mode . latex-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(LaTeX-mode . latex-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(plain-tex-mode . latex-ts-mode)))

;; File associations - use AUCTeX LaTeX-mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.sty\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.cls\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.bib\\'" . bibtex-mode))

;; AUCTeX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-master nil) ; Query for master file
(setq LaTeX-indent-level 2)
(setq LaTeX-item-indent 2)
(setq TeX-brace-indent-level 2)

;; Enable document parsing
(setq TeX-auto-untabify t)
(setq TeX-engine 'default) ; Use pdflatex by default
(setq TeX-PDF-mode t) ; PDF output by default

;; RefTeX integration
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography '("references.bib" "bibliography.bib"))

;; Math input with CDLaTeX
(add-hook 'LaTeX-mode-hook 'cdlatex-mode)

;; Enable math preview
(setq preview-auto-cache-preamble t)
(setq preview-scale-function (lambda () (* 1.2 (preview-scale-from-face))))

;; Compilation settings
(setq TeX-show-compilation nil) ; Don't show compilation buffer
(setq TeX-clean-confirm nil) ; Don't confirm clean

;; Latexmk support
(with-eval-after-load 'auctex-latexmk
  (when (fboundp 'auctex-latexmk-setup)
    (auctex-latexmk-setup))
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; Custom commands
(defun bv-latex-compile ()
  "Compile LaTeX document."
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file))

(defun bv-latex-compile-and-view ()
  "Compile LaTeX document and view result."
  (interactive)
  (TeX-command-sequence '("LaTeX" "View") 'TeX-master-file))

(defun bv-latex-bibtex ()
  "Run BibTeX."
  (interactive)
  (TeX-command "BibTeX" 'TeX-master-file))

(defun bv-latex-clean ()
  "Clean auxiliary files."
  (interactive)
  (TeX-command "Clean" 'TeX-master-file))

(defun bv-latex-clean-all ()
  "Clean all generated files."
  (interactive)
  (TeX-command "Clean All" 'TeX-master-file))

;; Preview pane integration
(defun bv-latex-toggle-preview-pane ()
  "Toggle LaTeX preview pane."
  (interactive)
  (if (fboundp 'latex-preview-pane-mode)
      (latex-preview-pane-mode 'toggle)
    (message "latex-preview-pane not available")))

;; Math preview helpers
(defun bv-latex-preview-math ()
  "Preview math at point."
  (interactive)
  (preview-at-point))

(defun bv-latex-preview-environment ()
  "Preview environment at point."
  (interactive)
  (preview-environment 1))

(defun bv-latex-preview-section ()
  "Preview current section."
  (interactive)
  (preview-section))

(defun bv-latex-preview-clear ()
  "Clear all previews."
  (interactive)
  (preview-clearout))

;; Citation management
(defun bv-latex-insert-citation ()
  "Insert citation using RefTeX."
  (interactive)
  (reftex-citation))

(defun bv-latex-insert-reference ()
  "Insert reference using RefTeX."
  (interactive)
  (reftex-reference))

;; Environment and macro helpers
(defun bv-latex-insert-environment ()
  "Insert LaTeX environment."
  (interactive)
  (LaTeX-environment))

(defun bv-latex-change-environment ()
  "Change current environment."
  (interactive)
  (LaTeX-environment 1))

(defun bv-latex-insert-section ()
  "Insert LaTeX section."
  (interactive)
  (LaTeX-section nil))

;; Spell checking
(defun bv-latex-enable-spell-check ()
  "Enable spell checking for LaTeX."
  (flyspell-mode 1)
  (setq ispell-parser 'tex))

(add-hook 'LaTeX-mode-hook #'bv-latex-enable-spell-check)

;; PDF tools integration
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; SyncTeX support
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

;; LaTeX-specific completions
(defun bv-latex-setup-completion ()
  "Setup completion for LaTeX."
  (setq-local completion-at-point-functions
              (append '(LaTeX-arguments-completion-at-point
                       LaTeX-environment-completion-at-point
                       LaTeX-macro-completion-at-point)
                      completion-at-point-functions)))

(add-hook 'LaTeX-mode-hook #'bv-latex-setup-completion)

;; Error navigation
(defun bv-latex-next-error ()
  "Go to next LaTeX error."
  (interactive)
  (TeX-next-error))

(defun bv-latex-previous-error ()
  "Go to previous LaTeX error."
  (interactive)
  (TeX-previous-error 1))

;; Document structure
(defun bv-latex-show-toc ()
  "Show table of contents."
  (interactive)
  (reftex-toc))

;; Formatting
(defun bv-latex-format-paragraph ()
  "Format current paragraph."
  (interactive)
  (LaTeX-fill-paragraph))

(defun bv-latex-format-region ()
  "Format region."
  (interactive)
  (LaTeX-fill-region (region-beginning) (region-end)))

;; Template insertion
(defun bv-latex-insert-article-template ()
  "Insert basic article template."
  (interactive)
  (insert "\\documentclass[11pt,a4paper]{article}\n"
          "\\usepackage[utf8]{inputenc}\n"
          "\\usepackage{amsmath,amssymb,amsthm}\n"
          "\\usepackage{graphicx}\n"
          "\\usepackage{hyperref}\n\n"
          "\\title{}\n"
          "\\author{Ayan Das}\n"
          "\\date{\\today}\n\n"
          "\\begin{document}\n"
          "\\maketitle\n\n"
          "\\section{Introduction}\n\n"
          "\\end{document}\n")
  (search-backward "\\title{")
  (forward-char 7))

;; Keybindings
(defvar bv-latex-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Compilation
    (define-key map (kbd "C-c C-c") #'bv-latex-compile)
    (define-key map (kbd "C-c C-v") #'bv-latex-compile-and-view)
    (define-key map (kbd "C-c C-b") #'bv-latex-bibtex)
    (define-key map (kbd "C-c C-k") #'bv-latex-clean)
    (define-key map (kbd "C-c C-K") #'bv-latex-clean-all)
    ;; Preview
    (define-key map (kbd "C-c p p") #'bv-latex-preview-math)
    (define-key map (kbd "C-c p e") #'bv-latex-preview-environment)
    (define-key map (kbd "C-c p s") #'bv-latex-preview-section)
    (define-key map (kbd "C-c p c") #'bv-latex-preview-clear)
    (define-key map (kbd "C-c p P") #'bv-latex-toggle-preview-pane)
    ;; References
    (define-key map (kbd "C-c r c") #'bv-latex-insert-citation)
    (define-key map (kbd "C-c r r") #'bv-latex-insert-reference)
    (define-key map (kbd "C-c r t") #'bv-latex-show-toc)
    ;; Structure
    (define-key map (kbd "C-c C-e") #'bv-latex-insert-environment)
    (define-key map (kbd "C-c C-E") #'bv-latex-change-environment)
    (define-key map (kbd "C-c C-s") #'bv-latex-insert-section)
    ;; Navigation
    (define-key map (kbd "C-c n") #'bv-latex-next-error)
    (define-key map (kbd "C-c p") #'bv-latex-previous-error)
    ;; Formatting
    (define-key map (kbd "C-c C-q") #'bv-latex-format-paragraph)
    (define-key map (kbd "C-c C-r") #'bv-latex-format-region)
    ;; Templates
    (define-key map (kbd "C-c t a") #'bv-latex-insert-article-template)
    map)
  "Keymap for LaTeX mode commands.")

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c l") bv-latex-mode-map))

(provide 'bv-lang-latex)
;;; bv-lang-latex.el ends here
