;;; bv-lang-r.el --- R development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; R development environment with ESS, tree-sitter, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar ess-indent-level)
(defvar ess-indent-with-fancy-comments)
(defvar ess-style)
(defvar ess-history-directory)
(defvar ess-ask-for-ess-directory)
(defvar ess-local-process-name)
(defvar ess-use-flymake)
(defvar ess-R-font-lock-keywords)
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar ess-r-mode-map)
(defvar r-ts-mode-map)

;; External functions
(declare-function ess-eval-buffer "ess-inf" (&optional vis))
(declare-function ess-eval-function "ess-inf" (&optional vis))
(declare-function ess-eval-line "ess-inf" (&optional vis))
(declare-function ess-eval-paragraph "ess-inf" (&optional vis))
(declare-function ess-eval-linewise "ess-inf" (text-withtabs &optional invisibly eob even-empty wait-last-prompt))
(declare-function ess-display-help-on-object "ess-help" (&optional object))
(declare-function ess-describe-object-at-point "ess-help" ())
(declare-function ess-rdired "ess-rdired" ())
(declare-function ess-debug-flag-for-debugging "ess-r-mode" ())
(declare-function ess-debug-unflag-for-debugging "ess-r-mode" ())
(declare-function ess-show-traceback "ess-tracebug" ())

;; Optional package loading
(autoload 'ess-mode "ess-site" nil t)
(autoload 'R-mode "ess-site" nil t)
(autoload 'R "ess-site" nil t)
(autoload 'ess-eval-region "ess-inf" nil t)

;; Enable tree-sitter for R when available
(when (treesit-language-available-p 'r)
  (add-to-list 'major-mode-remap-alist '(ess-r-mode . r-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . poly-noweb+r-mode))

;; ESS settings
(setq ess-indent-level 2)
(setq ess-indent-with-fancy-comments nil)
(setq ess-style 'RStudio)
(setq ess-history-directory "~/.cache/emacs/ess-history/")
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ess-use-flymake 'process)

;; Configure R language server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((ess-r-mode r-ts-mode) . ("R" "--slave" "-e" "languageserver::run()"))))

;; R-specific eglot settings
(defun bv-r-eglot-config ()
  "Configure eglot for R development."
  (setq-local eglot-workspace-configuration
              '(:r (:lsp (:diagnostics t)))))

(add-hook 'ess-r-mode-hook #'bv-r-eglot-config)
(when (fboundp 'r-ts-mode)
  (add-hook 'r-ts-mode-hook #'bv-r-eglot-config))

;; ESS customizations
(with-eval-after-load 'ess
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op% . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t))))

;; REPL interaction
(defun bv-r-start-repl ()
  "Start R REPL."
  (interactive)
  (R))

(defun bv-r-eval-region ()
  "Evaluate selected region."
  (interactive)
  (ess-eval-region (region-beginning) (region-end) nil))

(defun bv-r-eval-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (ess-eval-buffer nil))

(defun bv-r-eval-function ()
  "Evaluate current function."
  (interactive)
  (ess-eval-function))

(defun bv-r-eval-line ()
  "Evaluate current line."
  (interactive)
  (ess-eval-line))

(defun bv-r-eval-paragraph ()
  "Evaluate current paragraph."
  (interactive)
  (ess-eval-paragraph))

;; Package management
(defun bv-r-install-package (package)
  "Install R package."
  (interactive "sPackage: ")
  (ess-eval-linewise (format "install.packages(\"%s\")" package)))

(defun bv-r-load-package (package)
  "Load R package."
  (interactive "sPackage: ")
  (ess-eval-linewise (format "library(%s)" package)))

(defun bv-r-update-packages ()
  "Update all R packages."
  (interactive)
  (ess-eval-linewise "update.packages(ask = FALSE)"))

;; Development tools
(defun bv-r-devtools-load ()
  "Load package with devtools."
  (interactive)
  (ess-eval-linewise "devtools::load_all()"))

(defun bv-r-devtools-test ()
  "Test package with devtools."
  (interactive)
  (ess-eval-linewise "devtools::test()"))

(defun bv-r-devtools-check ()
  "Check package with devtools."
  (interactive)
  (ess-eval-linewise "devtools::check()"))

(defun bv-r-devtools-document ()
  "Document package with devtools."
  (interactive)
  (ess-eval-linewise "devtools::document()"))

(defun bv-r-devtools-build ()
  "Build package with devtools."
  (interactive)
  (ess-eval-linewise "devtools::build()"))

;; Formatting with styler
(defun bv-r-format-buffer ()
  "Format R buffer with styler."
  (interactive)
  (let ((tmpfile (make-temp-file "Remacs" nil ".R")))
    (write-region (point-min) (point-max) tmpfile nil 'silent)
    (ess-eval-linewise
     (format "styler::style_file(\"%s\"); readLines(\"%s\")" tmpfile tmpfile)
     nil nil nil 'wait)
    (delete-file tmpfile)))

(defun bv-r-format-region ()
  "Format selected region with styler."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (ess-eval-linewise
     (format "cat(styler::style_text('%s'))"
             (replace-regexp-in-string "'" "\\\\'" text)))))

;; Linting with lintr
(defun bv-r-lint-file ()
  "Lint current file with lintr."
  (interactive)
  (ess-eval-linewise (format "lintr::lint(\"%s\")" (buffer-file-name))))

(defun bv-r-lint-package ()
  "Lint package with lintr."
  (interactive)
  (ess-eval-linewise "lintr::lint_package()"))

;; Documentation
(defun bv-r-help ()
  "Show R help for object at point."
  (interactive)
  (ess-display-help-on-object))

(defun bv-r-describe-at-point ()
  "Describe object at point."
  (interactive)
  (ess-describe-object-at-point))

(defun bv-r-rdired ()
  "List R objects."
  (interactive)
  (ess-rdired))

;; Plotting
(defun bv-r-save-plot (filename)
  "Save current plot."
  (interactive "sSave plot as: ")
  (ess-eval-linewise (format "ggsave(\"%s\")" filename)))

(defun bv-r-dev-off ()
  "Close graphics device."
  (interactive)
  (ess-eval-linewise "dev.off()"))

;; Debugging
(defun bv-r-debug-function ()
  "Debug function at point."
  (interactive)
  (ess-debug-flag-for-debugging))

(defun bv-r-undebug-function ()
  "Undebug function at point."
  (interactive)
  (ess-debug-unflag-for-debugging))

(defun bv-r-traceback ()
  "Show traceback."
  (interactive)
  (ess-show-traceback))

;; Knitting R Markdown
(defun bv-r-knit ()
  "Knit R Markdown document."
  (interactive)
  (ess-eval-linewise
   (format "rmarkdown::render(\"%s\")" (buffer-file-name))))

(defun bv-r-knit-pdf ()
  "Knit R Markdown to PDF."
  (interactive)
  (ess-eval-linewise
   (format "rmarkdown::render(\"%s\", output_format = \"pdf_document\")"
           (buffer-file-name))))

(defun bv-r-knit-html ()
  "Knit R Markdown to HTML."
  (interactive)
  (ess-eval-linewise
   (format "rmarkdown::render(\"%s\", output_format = \"html_document\")"
           (buffer-file-name))))

;; Shiny applications
(defun bv-r-shiny-run ()
  "Run Shiny application."
  (interactive)
  (ess-eval-linewise "shiny::runApp()"))

;; Electric pairs
(defun bv-r-electric-pairs ()
  "Setup electric pairs for R."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?` . ?`)))))

(add-hook 'ess-r-mode-hook #'electric-pair-local-mode)
(add-hook 'ess-r-mode-hook #'bv-r-electric-pairs)
(when (fboundp 'r-ts-mode)
  (add-hook 'r-ts-mode-hook #'electric-pair-local-mode)
  (add-hook 'r-ts-mode-hook #'bv-r-electric-pairs))

;; Pretty symbols
(defun bv-r-pretty-symbols ()
  "Enable pretty symbols for R."
  (setq prettify-symbols-alist
        '(("<-" . ?←)
          ("->" . ?→)
          ("=>" . ?⇒)
          ("%>%" . ?⤚)
          ("%<>%" . ?⇄)
          ("%in%" . ?∈)
          ("!=" . ?≠)
          ("<=" . ?≤)
          (">=" . ?≥)))
  (prettify-symbols-mode 1))

(add-hook 'ess-r-mode-hook #'bv-r-pretty-symbols)
(when (fboundp 'r-ts-mode)
  (add-hook 'r-ts-mode-hook #'bv-r-pretty-symbols))

;; Keybindings
(defvar bv-r-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-r-start-repl)
    ;; Evaluation
    (define-key map (kbd "C-c C-r") #'bv-r-eval-region)
    (define-key map (kbd "C-c C-b") #'bv-r-eval-buffer)
    (define-key map (kbd "C-c C-f") #'bv-r-eval-function)
    (define-key map (kbd "C-c C-l") #'bv-r-eval-line)
    (define-key map (kbd "C-c C-p") #'bv-r-eval-paragraph)
    ;; Package management
    (define-key map (kbd "C-c p i") #'bv-r-install-package)
    (define-key map (kbd "C-c p l") #'bv-r-load-package)
    (define-key map (kbd "C-c p u") #'bv-r-update-packages)
    ;; Devtools
    (define-key map (kbd "C-c d l") #'bv-r-devtools-load)
    (define-key map (kbd "C-c d t") #'bv-r-devtools-test)
    (define-key map (kbd "C-c d c") #'bv-r-devtools-check)
    (define-key map (kbd "C-c d d") #'bv-r-devtools-document)
    (define-key map (kbd "C-c d b") #'bv-r-devtools-build)
    ;; Formatting
    (define-key map (kbd "C-c C-F") #'bv-r-format-buffer)
    (define-key map (kbd "C-c F r") #'bv-r-format-region)
    ;; Linting
    (define-key map (kbd "C-c L f") #'bv-r-lint-file)
    (define-key map (kbd "C-c L p") #'bv-r-lint-package)
    ;; Documentation
    (define-key map (kbd "C-c C-h") #'bv-r-help)
    (define-key map (kbd "C-c C-d") #'bv-r-describe-at-point)
    (define-key map (kbd "C-c C-o") #'bv-r-rdired)
    ;; Plotting
    (define-key map (kbd "C-c P s") #'bv-r-save-plot)
    (define-key map (kbd "C-c P d") #'bv-r-dev-off)
    ;; Debugging
    (define-key map (kbd "C-c D d") #'bv-r-debug-function)
    (define-key map (kbd "C-c D u") #'bv-r-undebug-function)
    (define-key map (kbd "C-c D t") #'bv-r-traceback)
    ;; Knitting
    (define-key map (kbd "C-c k k") #'bv-r-knit)
    (define-key map (kbd "C-c k p") #'bv-r-knit-pdf)
    (define-key map (kbd "C-c k h") #'bv-r-knit-html)
    ;; Shiny
    (define-key map (kbd "C-c s r") #'bv-r-shiny-run)
    map)
  "Keymap for R mode commands.")

(with-eval-after-load 'ess-r-mode
  (define-key ess-r-mode-map (kbd "C-c r") bv-r-mode-map))

(when (fboundp 'r-ts-mode)
  (with-eval-after-load 'r-ts-mode
    (define-key r-ts-mode-map (kbd "C-c r") bv-r-mode-map)))

(provide 'bv-lang-r)
;;; bv-lang-r.el ends here