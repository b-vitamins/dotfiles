;;; bv-development.el --- Language-agnostic development tools -*- lexical-binding: t -*-
;;; Commentary:
;; Core development tools - SIMPLIFIED VERSION
;; Features: smartparens, eglot (LSP), flymake, xref, treesitter
;;
;; Optional Guix packages needed:
;; - emacs-smartparens
;; - emacs-hl-todo
;; - emacs-rainbow-delimiters
;;; Code:
(require 'bv-core)

;;;; External Variable Declarations
(defvar smartparens-mode-map)
(defvar eglot--managed-mode)
(defvar eldoc-echo-area-use-multiline-p)
(defvar eldoc-echo-area-display-truncation-message)
(defvar eldoc-documentation-strategy)
(defvar flymake-no-changes-timeout)
(defvar flymake-start-on-save-buffer)
(defvar flymake-proc-compilation-prevents-syntax-check)
(defvar flymake-wraparound)
(defvar flymake-error-bitmap)
(defvar flymake-warning-bitmap)
(defvar flymake-note-bitmap)
(defvar flymake-mode-map)
(defvar eglot-mode-map)
(defvar imenu--index-alist)
;; New eglot variables (replacing obsolete ones)
(defvar eglot-events-buffer-config)
(defvar eglot-confirm-server-edits)
;; hl-todo variables
(defvar hl-todo-keyword-faces)
;; highlight-indent-guides variables
(defvar highlight-indent-guides-method)
(defvar highlight-indent-guides-character)
(defvar highlight-indent-guides-responsive)
(defvar highlight-indent-guides-delay)

;;;; Function Declarations
(declare-function eglot-server-capable "eglot" (method))
(declare-function eglot-format-buffer "eglot" ())
(declare-function consult-eglot-symbols "consult-eglot" ())
(declare-function consult-xref "consult-xref" (fetcher &optional alist))
(declare-function flymake-proc-legacy-flymake "flymake-proc" ())
(declare-function ansi-color-compilation-filter "ansi-color" ())
(declare-function highlight-indent-guides-mode "highlight-indent-guides" (&optional arg))

;;;; Custom Variables
(defgroup bv-development nil
  "Core development configuration."
  :group 'bv)
;; Smartparens settings
(bv-defcustom bv-dev-smartparens-strict nil
  "Enable strict smartparens mode by default."
  :type 'boolean
  :group 'bv-development)
;; Eglot settings
(bv-defcustom bv-dev-format-on-save nil
  "Format buffer on save using LSP."
  :type 'boolean
  :group 'bv-development)

(bv-defcustom bv-dev-eldoc-documentation-strategy 'eldoc-documentation-compose
  "Eldoc documentation strategy."
  :type 'symbol
  :group 'bv-development)

(bv-defcustom bv-dev-flymake-no-changes-timeout 0.5
  "Time to wait after last change before running flymake."
  :type 'number
  :group 'bv-development)

(bv-defcustom bv-dev-re-builder-syntax 'string
  "Regular expression syntax for `re-builder'."
  :type 'symbol
  :group 'bv-development)

;;;; Treesitter Support (Emacs 30)
(when (fboundp 'treesit-available-p)
  ;; Set up major mode remapping early, before treesit is loaded
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (toml-mode . toml-ts-mode)
          (rust-mode . rust-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (java-mode . java-ts-mode)
          (go-mode . go-ts-mode)))

  (use-package treesit
    :ensure nil
    :defer 1  ; Load after 1 second to ensure it's available
    :config
    ;; Grammar search paths - check Guix locations first
    (let ((grammar-dirs
           (list
            ;; Guix system profile
            (when (fboundp 'bv-expand-system-lib)
              (bv-expand-system-lib "tree-sitter"))
            ;; Guix home profile
            (when (fboundp 'bv-expand-home-lib)
              (bv-expand-home-lib "tree-sitter"))
            ;; Guix user profile
            (when (and (fboundp 'bv-get-value)
                       (bv-get-value 'guix-user-profile))
              (expand-file-name "lib/tree-sitter" (bv-get-value 'guix-user-profile)))
            ;; Local installation directory (fallback)
            (if (and (fboundp 'bv-get-value)
                     (bv-get-value 'var-dir))
                (expand-file-name "tree-sitter" (bv-get-value 'var-dir))
              (expand-file-name "tree-sitter" "~/.cache/emacs/var")))))
      ;; Filter out nil values and non-existent directories
      (setq treesit-extra-load-path
            (seq-filter (lambda (dir)
                          (and dir (file-directory-p dir)))
                        grammar-dirs)))

    ;; Auto-install grammars only if not found in Guix
    (defun bv-dev-ensure-grammar (lang &optional url branch source-dir)
      "Ensure tree-sitter grammar for LANG is installed.
First checks Guix-installed grammars, then installs from URL if needed.
If URL is provided, use it as the recipe for installation."
      (unless (treesit-language-available-p lang)
        (message "Grammar for %s not found in Guix profiles, installing from source..." lang)
        (let* ((var-dir (if (and (fboundp 'bv-get-value)
                                 (bv-get-value 'var-dir))
                            (bv-get-value 'var-dir)
                          "~/.cache/emacs/var"))
               (default-directory (expand-file-name "tree-sitter" var-dir)))
          (unless (file-directory-p default-directory)
            (make-directory default-directory t))
          ;; treesit-install-language-grammar: (LANG &optional RECIPE)
          ;; RECIPE is (URL REVISION SOURCE-DIR CC C++)
          (if url
              (let ((recipe (list url branch source-dir)))
                (treesit-install-language-grammar lang recipe))
            (treesit-install-language-grammar lang)))))

    ;; Helper to check which grammars are available
    (defun bv-dev-list-available-grammars ()
      "List all available tree-sitter grammars and their locations."
      (interactive)
      (with-current-buffer (get-buffer-create "*Tree-sitter Grammars*")
        (erase-buffer)
        (insert "Available Tree-sitter Grammars\n")
        (insert "==============================\n\n")
        (dolist (dir treesit-extra-load-path)
          (when (file-directory-p dir)
            (insert (format "Directory: %s\n" dir))
            (dolist (file (directory-files dir nil "\\.so\\'" t))
              (insert (format "  - %s\n" file)))
            (insert "\n")))
        (insert "\nCurrently loaded languages:\n")
        ;; Use treesit-available-p to get available languages
        (when (fboundp 'treesit-available-p)
          (dolist (lang '(python javascript typescript json css yaml toml
                                 rust c c++ java go))
            (when (treesit-language-available-p lang)
              (insert (format "  - %s\n" lang)))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))

    ;; Font-lock levels
    (setq treesit-font-lock-level 4)))

;;;; Smartparens - Structural Editing
;; Declare required functions for byte-compilation
(declare-function sp-wrap-with-pair "smartparens" (pair))
(declare-function turn-on-smartparens-strict-mode "smartparens" ())
(declare-function show-smartparens-global-mode "smartparens" (&optional arg))
;; Navigation functions
(declare-function sp-beginning-of-sexp "smartparens" (&optional arg))
(declare-function sp-end-of-sexp "smartparens" (&optional arg))
(declare-function sp-down-sexp "smartparens" (&optional arg))
(declare-function sp-up-sexp "smartparens" (&optional arg))
(declare-function sp-backward-down-sexp "smartparens" (&optional arg))
(declare-function sp-backward-up-sexp "smartparens" (&optional arg))
(declare-function sp-forward-sexp "smartparens" (&optional arg))
(declare-function sp-backward-sexp "smartparens" (&optional arg))
(declare-function sp-next-sexp "smartparens" (&optional arg))
(declare-function sp-previous-sexp "smartparens" (&optional arg))
(declare-function sp-forward-symbol "smartparens" (&optional arg))
(declare-function sp-backward-symbol "smartparens" (&optional arg))
;; Slurp/barf functions
(declare-function sp-forward-slurp-sexp "smartparens" (&optional arg))
(declare-function sp-forward-barf-sexp "smartparens" (&optional arg))
(declare-function sp-backward-slurp-sexp "smartparens" (&optional arg))
(declare-function sp-backward-barf-sexp "smartparens" (&optional arg))
;; Manipulation functions
(declare-function sp-transpose-sexp "smartparens" ())
(declare-function sp-kill-sexp "smartparens" (&optional arg))
(declare-function sp-kill-hybrid-sexp "smartparens" (&optional arg))
(declare-function sp-backward-kill-sexp "smartparens" (&optional arg))
(declare-function sp-copy-sexp "smartparens" (&optional arg))
(declare-function sp-backward-unwrap-sexp "smartparens" (&optional arg))
(declare-function sp-unwrap-sexp "smartparens" (&optional arg))
(declare-function sp-backward-kill-word "smartparens" (&optional arg))
(declare-function sp-transpose-hybrid-sexp "smartparens" ())
(declare-function delete-sexp "simple" (&optional arg))

;; Wrapper macro from the article
(defmacro def-pairs (pairs)
  "Define functions for pairing.  PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . "(")
              (bracket . "["))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(intern (concat
                                  "wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                     (&optional _arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

;; Define wrapper functions
(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")
            (underscore . "_")))

;; Load smartparens exactly as shown in the article
(when (locate-library "smartparens")
  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  ;; Add hooks for strict mode
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (with-eval-after-load 'markdown-mode
    (add-hook 'markdown-mode-hook #'turn-on-smartparens-strict-mode))

  ;; Set up all keybindings after smartparens is loaded
  (with-eval-after-load 'smartparens
    ;; Define delete-sexp if not already defined
    (unless (fboundp 'delete-sexp)
      (defun delete-sexp (&optional arg)
        "Delete the sexp following point."
        (interactive "p")
        (let ((opoint (point)))
          (forward-sexp (or arg 1))
          (delete-region opoint (point)))))

    ;; Only set keybindings if smartparens-mode-map exists
    (when (boundp 'smartparens-mode-map)
      ;; Keybindings from the article
      ;; Navigation - starts and ends
      (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
      (define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)
      ;; Traversing lists
      (define-key smartparens-mode-map (kbd "C-<down>") 'sp-down-sexp)
      (define-key smartparens-mode-map (kbd "C-<up>") 'sp-up-sexp)
      (define-key smartparens-mode-map (kbd "M-<down>") 'sp-backward-down-sexp)
      (define-key smartparens-mode-map (kbd "M-<up>") 'sp-backward-up-sexp)
      ;; Block movements
      (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
      (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
      ;; Top-level-ish traversal
      (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
      (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
      ;; Free-form movements
      (define-key smartparens-mode-map (kbd "C-S-f") 'sp-forward-symbol)
      (define-key smartparens-mode-map (kbd "C-S-b") 'sp-backward-symbol)
      ;; Slurp and barf
      (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
      (define-key smartparens-mode-map (kbd "M-<right>") 'sp-forward-barf-sexp)
      (define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp)
      (define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-barf-sexp)
      ;; Manipulation
      (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
      (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
      (define-key smartparens-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
      (define-key smartparens-mode-map (kbd "M-k") 'sp-backward-kill-sexp)
      (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
      (define-key smartparens-mode-map (kbd "C-M-d") 'delete-sexp)
      ;; Unwrapping
      (define-key smartparens-mode-map (kbd "M-[") 'sp-backward-unwrap-sexp)
      (define-key smartparens-mode-map (kbd "M-]") 'sp-unwrap-sexp)
      ;; Misc
      (define-key smartparens-mode-map (kbd "M-<backspace>") 'backward-kill-word)
      (define-key smartparens-mode-map (kbd "C-<backspace>") 'sp-backward-kill-word)
      (define-key smartparens-mode-map [remap sp-backward-kill-word] 'backward-kill-word)
      (define-key smartparens-mode-map (kbd "C-x C-t") 'sp-transpose-hybrid-sexp)
      ;; Wrapping with specific characters
      (define-key smartparens-mode-map (kbd "C-c (") 'wrap-with-parens)
      (define-key smartparens-mode-map (kbd "C-c [") 'wrap-with-brackets)
      (define-key smartparens-mode-map (kbd "C-c {") 'wrap-with-braces)
      (define-key smartparens-mode-map (kbd "C-c '") 'wrap-with-single-quotes)
      (define-key smartparens-mode-map (kbd "C-c \"") 'wrap-with-double-quotes)
      (define-key smartparens-mode-map (kbd "C-c _") 'wrap-with-underscores)
      (define-key smartparens-mode-map (kbd "C-c `") 'wrap-with-back-quotes))))

;;;; Eglot Helper Functions
;; Define these at top level to avoid byte-compiler warnings
(defun bv-dev-eglot-format-on-save ()
  "Format buffer on save if eglot is active."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(defun bv-dev-eglot-clear-imenu-cache ()
  "Clear imenu cache when eglot is active."
  (when (bound-and-true-p eglot--managed-mode)
    (setq imenu--index-alist nil)))

;;;; Eglot - Built-in LSP Client
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l f" . eglot-format)
         ("C-c l F" . eglot-format-buffer)
         ("C-c l d" . eldoc)
         ("C-c l s" . consult-eglot-symbols))
  :init
  ;; Performance tuning
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)

  :config
  ;; Configure servers
  (setq eglot-server-programs
        '(((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
          ((rust-mode rust-ts-mode) . ("rust-analyzer"))
          ((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))
          (nix-mode . ("nil"))))

  ;; Performance - use new variable name
  (setq eglot-events-buffer-config '(:size 0))

  ;; Stay out of the way - use new variable name
  (setq eglot-autoshutdown t
        eglot-confirm-server-edits 'confirm
        eglot-extend-to-xref t)

  ;; Eldoc
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil
        eldoc-documentation-strategy bv-dev-eldoc-documentation-strategy)

  ;; Format on save
  (when bv-dev-format-on-save
    (add-hook 'before-save-hook #'bv-dev-eglot-format-on-save))

  ;; Clear imenu cache
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'bv-dev-eglot-clear-imenu-cache nil t)))

  ;; Completion integration
  (setq completion-category-overrides
        '((eglot (styles orderless))
          (eglot-capf (styles orderless)))))

;; Consult integration
(bv-when-feature bv-completion
  (use-package consult-eglot
    :ensure nil
    :after (eglot consult)
    :bind (:map eglot-mode-map
           ("M-g s" . consult-eglot-symbols))))

;;;; Flymake - Syntax Checking
(use-package flymake
  :ensure nil
  :hook ((prog-mode text-mode) . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! L" . flymake-show-project-diagnostics)
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error))
  :config
  (setq flymake-no-changes-timeout bv-dev-flymake-no-changes-timeout
        flymake-start-on-save-buffer t
        flymake-proc-compilation-prevents-syntax-check t
        flymake-wraparound t)

  ;; Better fringe indicators
  (define-fringe-bitmap 'bv-flymake-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b01111100
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (setq flymake-error-bitmap '(bv-flymake-fringe-indicator compilation-error)
        flymake-warning-bitmap '(bv-flymake-fringe-indicator compilation-warning)
        flymake-note-bitmap '(bv-flymake-fringe-indicator compilation-info))

  ;; Don't popup automatically
  (when (fboundp 'flymake-proc-legacy-flymake)
    (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)))

;;;; Xref - Cross References
(use-package xref
  :ensure nil
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)
         ("M-?" . xref-find-references)
         ("C-M-." . xref-find-apropos))
  :config
  ;; Better behavior
  (setq xref-auto-jump-to-first-definition 'move
        xref-auto-jump-to-first-xref 'move
        xref-search-program 'ripgrep)

  ;; Don't prompt for identifier at point
  (setq xref-prompt-for-identifier
        '(not xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame))

  ;; Use completion system
  (bv-when-feature bv-completion
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)))

;;;; Re-builder - Regular Expression Development
(use-package re-builder
  :ensure nil
  :bind (("C-c r" . re-builder))
  :config
  (setq reb-re-syntax bv-dev-re-builder-syntax)
  (setq reb-auto-match-limit 200)
  (setq reb-blink-delay 0.5))

;;;; Compilation
(use-package compile
  :ensure nil
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config
  ;; Auto-scroll compilation output
  (setq compilation-scroll-output 'first-error)
  ;; Kill compilation process before starting new one
  (setq compilation-always-kill t)
  ;; Don't ask about saving buffers
  (setq compilation-ask-about-save nil)
  ;; Colorize compilation buffer
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;;;; Electric Pairs
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :config
  (setq electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t
        electric-pair-skip-self 'electric-pair-default-skip-self))

;;;; Code Folding
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
         ("C-c @" . hs-toggle-hiding)
         ("C-c h" . hs-hide-all)
         ("C-c s" . hs-show-all)))

;;;; Eldoc Configuration
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0.3
        eldoc-echo-area-display-truncation-message nil)
  ;; Use buffer for long documentation
  (setq eldoc-echo-area-prefer-doc-buffer t))

;;;; Feature Registration
(bv-register-feature 'bv-development)
;; Register capabilities
(bv-set-value 'lsp-client 'eglot)
(bv-set-value 'structural-editing 'smartparens)
(bv-set-value 'syntax-checker 'flymake)
(provide 'bv-development)
;;; bv-development.el ends here
