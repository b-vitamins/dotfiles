;;; bv-writing.el --- Writing and documentation tools -*- lexical-binding: t -*-

;;; Commentary:
;; Writing environment for academic papers, documentation, and general content.
;; Includes spell checking, grammar checking, LaTeX, Markdown, and academic writing tools.

;;; Code:

(require 'bv-core)

;;;; Custom Variables
(defgroup bv-writing nil
  "Writing and documentation configuration."
  :group 'bv)

;;; Spell Checking
(defcustom bv-writing-spelling-program
  (cond ((executable-find "aspell") 'aspell)
        ((executable-find "hunspell") 'hunspell)
        (t 'ispell))
  "Spell checking program to use."
  :type '(choice (const :tag "Aspell" aspell)
                 (const :tag "Hunspell" hunspell)
                 (const :tag "Ispell" ispell))
  :group 'bv-writing)

(defcustom bv-writing-spelling-dictionaries
  '("en_US" "en_GB")
  "List of dictionaries to use for spell checking."
  :type '(repeat string)
  :group 'bv-writing)

(defcustom bv-writing-default-dictionary "en_US"
  "Default dictionary for spell checking."
  :type 'string
  :group 'bv-writing)

(defcustom bv-writing-personal-dictionary
  (expand-file-name "aspell.personal" user-emacs-directory)
  "Personal dictionary file."
  :type 'file
  :group 'bv-writing)

(defcustom bv-writing-flyspell-prog-modes
  '(prog-mode-hook)
  "Hooks to enable flyspell-prog-mode."
  :type '(repeat hook)
  :group 'bv-writing)

(defcustom bv-writing-flyspell-text-modes
  '(text-mode-hook
    org-mode-hook
    markdown-mode-hook
    LaTeX-mode-hook)
  "Hooks to enable flyspell-mode."
  :type '(repeat hook)
  :group 'bv-writing)

;;; Grammar Checking
(defcustom bv-writing-grammar-checker
  (cond ((executable-find "languagetool") 'languagetool)
        (t 'writegood))
  "Grammar checker to use."
  :type '(choice (const :tag "LanguageTool" languagetool)
                 (const :tag "Writegood Mode" writegood))
  :group 'bv-writing)

(defcustom bv-writing-languagetool-server-url
  "http://localhost:8081"
  "URL for LanguageTool server."
  :type 'string
  :group 'bv-writing)

;;; LaTeX Configuration
(defcustom bv-writing-latex-engine 'xelatex
  "Default LaTeX engine."
  :type '(choice (const :tag "PDFLaTeX" pdflatex)
                 (const :tag "XeLaTeX" xelatex)
                 (const :tag "LuaLaTeX" lualatex))
  :group 'bv-writing)

(defcustom bv-writing-latex-preview-scale 1.2
  "Scale factor for LaTeX preview."
  :type 'number
  :group 'bv-writing)

;;; Academic Writing
(defcustom bv-writing-bibliography-directory
  (expand-file-name "~/Documents/bibliography/")
  "Directory containing bibliography files."
  :type 'directory
  :group 'bv-writing)

(defcustom bv-writing-default-bibliography
  (expand-file-name "references.bib" bv-writing-bibliography-directory)
  "Default bibliography file."
  :type 'file
  :group 'bv-writing)

;;;; Configuration

;;; Core Spell Checking
(use-package ispell
  :defer t
  :config
  (setq ispell-program-name
        (pcase bv-writing-spelling-program
          ('aspell (executable-find "aspell"))
          ('hunspell (executable-find "hunspell"))
          (_ (executable-find "ispell"))))
  
  (when (eq bv-writing-spelling-program 'aspell)
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
  
  (when (eq bv-writing-spelling-program 'hunspell)
    (setq ispell-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['"]" nil ("-d" "en_US") nil utf-8)))
  
  (setq ispell-dictionary bv-writing-default-dictionary)
  (setq ispell-personal-dictionary bv-writing-personal-dictionary)
  (setq ispell-silently-savep t))

(use-package flyspell
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-large-region 1)
  
  (dolist (hook '(org-mode-hook markdown-mode-hook))
    (add-hook hook
              (lambda ()
                (setq flyspell-generic-check-word-predicate
                      'bv-writing-flyspell-generic-check-word-predicate))))
  
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after (flyspell-correct ivy))

;;; Grammar and Style Checking
(use-package writegood-mode
  :diminish
  :hook ((text-mode . writegood-mode)
         (LaTeX-mode . writegood-mode))
  :config
  (setq writegood-weasel-words
        '("very" "quite" "really" "extremely" "fairly" "pretty"
          "somewhat" "rather" "arguably" "basically" "essentially"
          "generally" "typically" "usually" "often" "sometimes"
          "just" "simply" "merely" "actually" "virtually")))

;; LanguageTool integration
(use-package langtool
  :if (executable-find "languagetool")
  :defer t
  :config
  (setq langtool-language-tool-server-jar
        "/usr/share/java/languagetool/languagetool-server.jar")
  (setq langtool-default-language "en-US")
  (setq langtool-mother-tongue "en")
  :bind (("C-c L c" . langtool-check)
         ("C-c L d" . langtool-check-done)
         ("C-c L s" . langtool-server-mode-toggle)
         ("C-c L n" . langtool-goto-next-error)
         ("C-c L p" . langtool-goto-previous-error)
         ("C-c L ." . langtool-correct-buffer)))

;;; LaTeX Support
(use-package tex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master nil)
  (setq-default TeX-engine bv-writing-latex-engine)
  
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))
  
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")
          (output-dvi "xdvi")))
  (setq TeX-view-program-list
        '(("PDF Tools" TeX-pdf-tools-sync-view)))
  
  (setq LaTeX-clean-intermediate-suffixes
        '("\\.aux" "\\.bbl" "\\.blg" "\\.brf" "\\.fot"
          "\\.glo" "\\.gls" "\\.idx" "\\.ilg" "\\.ind"
          "\\.lof" "\\.log" "\\.lot" "\\.nav" "\\.out"
          "\\.snm" "\\.toc" "\\.url" "\\.synctex\\.gz"
          "\\.bcf" "\\.run\\.xml" "\\.fls" "\\.fdb_latexmk"))
  
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
  
  (setq LaTeX-section-list
        '(("part" 0)
          ("chapter" 1)
          ("section" 2)
          ("subsection" 3)
          ("subsubsection" 4)
          ("paragraph" 5)
          ("subparagraph" 6))))

(use-package reftex
  :defer t
  :config
  (setq reftex-cite-format 'natbib)
  (setq reftex-default-bibliography (list bv-writing-default-bibliography))
  (setq reftex-bibpath-environment-variables '("BIBINPUTS" "TEXBIB"))
  (setq reftex-cite-prompt-optional-args t))

(use-package auctex-latexmk
  :after tex
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :after (tex company)
  :config
  (company-auctex-init))

;; CDLaTeX for fast math input
(use-package cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (org-mode . org-cdlatex-mode))
  :config
  (setq cdlatex-paired-parens "$[{("))

;;; Markdown Support
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command
        (cond ((executable-find "pandoc")
               "pandoc -f markdown -t html5 --mathjax --highlight-style=pygments --standalone")
              ((executable-find "markdown")
               "markdown")
              (t nil)))
  :config
  (setq markdown-enable-math t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-make-gfm-checkboxes-buttons t)
  (setq markdown-gfm-uppercase-checkbox t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-urls nil)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown-light.min.css"))
  
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-export)
              ("C-c C-v" . markdown-preview)
              ("C-c C-c m" . markdown-other-window)
              ("C-c C-c p" . markdown-preview-mode)
              ("C-c C-c l" . markdown-live-preview-mode)
              ("C-c C-x C-i" . markdown-insert-image)))

;; Pandoc integration
(use-package pandoc-mode
  :hook ((markdown-mode . pandoc-mode)
         (org-mode . pandoc-mode))
  :config
  (setq pandoc-data-dir (expand-file-name "pandoc/" user-emacs-directory)))

;; Table of contents for markdown
(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-x t" . markdown-toc-generate-or-refresh-toc)))

;;; Academic Writing Tools
(use-package academic-phrases
  :defer t
  :bind ("C-c w a" . academic-phrases)
  :config
  (setq academic-phrases-by-section t))

;; Better prose editing
(use-package visual-fill-column
  :hook ((text-mode . visual-fill-column-mode)
         (org-mode . visual-fill-column-mode))
  :config
  (setq-default visual-fill-column-width 80)
  (setq-default visual-fill-column-center-text t))

;; Focus mode for writing
(use-package olivetti
  :defer t
  :bind ("C-c w f" . olivetti-mode)
  :config
  (setq-default olivetti-body-width 80)
  (setq olivetti-minimum-body-width 72)
  (setq olivetti-style 'fancy))

;; Word counting
(use-package wc-mode
  :hook ((text-mode . wc-mode)
         (LaTeX-mode . wc-mode))
  :config
  (setq wc-modeline-format "[%tw/%gw]"))

;;; Bibliography Management Integration
(with-eval-after-load 'citar
  (setq citar-bibliography (list bv-writing-default-bibliography)))

;;; Export and Conversion
(use-package ox-pandoc
  :after org
  :config
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))))

;;; Helper Functions
(defun bv-writing-flyspell-generic-check-word-predicate ()
  "Don't check spelling in code blocks, links, etc."
  (let ((face (get-text-property (point) 'face)))
    (not (memq face '(font-lock-comment-face
                      font-lock-doc-face
                      font-lock-string-face
                      font-latex-math-face
                      org-block
                      org-code
                      org-date
                      org-link
                      org-special-keyword
                      markdown-code-face
                      markdown-inline-code-face
                      markdown-pre-face
                      markdown-url-face)))))

(defun bv-writing-count-words-region (start end)
  "Count words in region from START to END."
  (interactive "r")
  (let ((count (count-words start end)))
    (message "Region has %d words" count)))

(defun bv-writing-insert-citation ()
  "Insert citation using citar if available, otherwise use reftex."
  (interactive)
  (cond ((fboundp 'citar-insert-citation)
         (citar-insert-citation))
        ((derived-mode-p 'latex-mode)
         (reftex-citation))
        (t (user-error "No citation method available"))))

(defun bv-writing-export-to-pdf ()
  "Export current document to PDF using appropriate method."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-export-dispatch nil ?p))
        ((derived-mode-p 'markdown-mode)
         (if (fboundp 'pandoc-convert-to-pdf)
             (pandoc-convert-to-pdf)
           (markdown-export)))
        ((derived-mode-p 'latex-mode)
         (TeX-command-master))
        (t (user-error "Don't know how to export this document type"))))

(defun bv-writing-check-grammar ()
  "Check grammar using configured grammar checker."
  (interactive)
  (pcase bv-writing-grammar-checker
    ('languagetool
     (if (fboundp 'langtool-check)
         (langtool-check)
       (writegood-mode 1)))
    ('writegood
     (writegood-mode 1))))

;;; Mode Hooks
(defun bv-writing-text-mode-setup ()
  "Setup for text writing modes."
  (setq-local fill-column 80)
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (writegood-mode 1))

(defun bv-writing-latex-mode-setup ()
  "Additional LaTeX mode setup."
  (setq-local TeX-electric-math '("$" . "$"))
  (setq-local TeX-electric-sub-and-superscript t)
  (turn-on-reftex))

;;; Global Keybindings
(with-eval-after-load 'bv-core
  (bv-leader
    "w" '(:ignore t :which-key "writing")
    "w c" #'bv-writing-insert-citation
    "w e" #'bv-writing-export-to-pdf
    "w g" #'bv-writing-check-grammar
    "w s" #'flyspell-correct-wrapper
    "w S" #'ispell-buffer
    "w w" #'bv-writing-count-words-region
    "w f" #'olivetti-mode
    "w a" #'academic-phrases))

;;;; Feature Definition
(defun bv-writing-load ()
  "Load writing configuration."
  (add-to-list 'bv-enabled-features 'writing)
  
  ;; Setup hooks
  (dolist (hook bv-writing-flyspell-prog-modes)
    (add-hook hook 'flyspell-prog-mode))
  
  (dolist (hook bv-writing-flyspell-text-modes)
    (add-hook hook 'flyspell-mode))
  
  (add-hook 'text-mode-hook #'bv-writing-text-mode-setup)
  (add-hook 'LaTeX-mode-hook #'bv-writing-latex-mode-setup)
  
  ;; Create personal dictionary
  (let ((dict-dir (file-name-directory bv-writing-personal-dictionary)))
    (unless (file-exists-p dict-dir)
      (make-directory dict-dir t)))
  
  (unless (file-exists-p bv-writing-personal-dictionary)
    (with-temp-file bv-writing-personal-dictionary
      (insert "personal_ws-1.1 en 0\n"))))

(provide 'bv-writing)
;;; bv-writing.el ends here
