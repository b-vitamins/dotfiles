;;; bv-development.el --- Language-agnostic development tools -*- lexical-binding: t -*-

;;; Commentary:
;; Core development tools
;; Features: smartparens, eglot (LSP), flymake, xref, treesitter

;;; Code:

(require 'bv-core)

;;;; Custom Variables

(defgroup bv-development nil
  "Core development configuration."
  :group 'bv)

;; Smartparens settings
(bv-defcustom bv-dev-smartparens-strict t
  "Enable strict smartparens mode by default."
  :type 'boolean
  :group 'bv-development)

(bv-defcustom bv-dev-smartparens-show-pairs nil
  "Show matching pairs (alternative to show-paren-mode)."
  :type 'boolean
  :group 'bv-development)

(bv-defcustom bv-dev-paredit-bindings nil
  "Use paredit-style bindings instead of smartparens defaults."
  :type 'boolean
  :group 'bv-development)

;; Eglot settings
(bv-defcustom bv-dev-format-on-save t
  "Format buffer on save using LSP."
  :type 'boolean
  :group 'bv-development)

(bv-defcustom bv-dev-eldoc-documentation-strategy 'eldoc-documentation-compose
  "How to display eldoc documentation."
  :type 'symbol
  :group 'bv-development)

;; Flymake settings
(bv-defcustom bv-dev-flymake-no-changes-timeout 0.5
  "Time to wait after last change before running flymake."
  :type 'number
  :group 'bv-development)

;; Re-builder settings
(bv-defcustom bv-dev-re-builder-syntax 'rx
  "Syntax to use in re-builder."
  :type '(choice (const :tag "Read syntax" read)
                 (const :tag "String syntax" string)
                 (const :tag "Rx syntax" rx))
  :group 'bv-development)

;;;; Treesitter Support (Emacs 30)

(when (fboundp 'treesit-available-p)
  (use-package treesit
    :ensure nil
    :config
    ;; Grammar installation directory
    (setq treesit-extra-load-path
          (list (expand-file-name "tree-sitter" bv-var-dir)))

    ;; Auto-install grammars
    (defun bv-dev-ensure-grammar (lang url &optional branch)
      "Ensure tree-sitter grammar for LANG is installed from URL."
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang url nil branch)))

    ;; Remap modes to use tree-sitter
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

    ;; Font-lock levels
    (setq treesit-font-lock-level 4)))

;;;; Smartparens - Structural Editing

(use-package smartparens
  :defer t
  :init
  (when (locate-library "smartparens")
    (autoload 'smartparens-mode "smartparens" nil t)
    (autoload 'smartparens-strict-mode "smartparens" nil t)
    (dolist (hook '(prog-mode-hook text-mode-hook))
      (add-hook hook #'smartparens-mode)
      (when bv-dev-smartparens-strict
        (add-hook hook #'smartparens-strict-mode))))
  :bind (:map smartparens-mode-map
         ;; Navigation
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-a" . sp-backward-down-sexp)
         ("C-S-a" . sp-beginning-of-sexp)
         ("C-S-d" . sp-end-of-sexp)
         ("C-M-e" . sp-up-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ;; Selection
         ("C-M-k" . sp-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-<delete>" . sp-unwrap-sexp)
         ("M-<backspace>" . sp-backward-unwrap-sexp)
         ;; Manipulation
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("M-D" . sp-splice-sexp)
         ("C-M-<delete>" . sp-splice-sexp-killing-forward)
         ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
         ("C-S-<backspace>" . sp-splice-sexp-killing-around)
         ;; Wrapping
         ("C-(" . sp-wrap-round)
         ("C-[" . sp-wrap-square)
         ("C-{" . sp-wrap-curly))
  :config
  ;; Load default configuration
  (require 'smartparens-config)

  ;; Use paredit bindings if requested
  (when bv-dev-paredit-bindings
    (sp-use-paredit-bindings))

  ;; Remove conflicting binding
  (define-key smartparens-mode-map (kbd "M-s") nil)

  ;; Show smartparens mode
  (if bv-dev-smartparens-show-pairs
      (progn
        (show-smartparens-global-mode 1)
        (show-paren-mode -1))
    (show-paren-mode 1))

  ;; Better pair highlighting
  (setq sp-show-pair-delay 0.1
        sp-show-pair-from-inside t
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  ;; Don't insert pairs when in string/comment
  (setq sp-autoskip-closing-pair 'always
        sp-autoskip-opening-pair t
        sp-navigate-skip-match t)

  ;; Additional pairs
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "*" "*")
  (sp-local-pair 'org-mode "/" "/")
  (sp-local-pair 'org-mode "_" "_")
  (sp-local-pair 'org-mode "+" "+")
  (sp-local-pair 'markdown-mode "`" "`"))

;;;; Eglot - Built-in LSP Client

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
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
        '((python-mode python-ts-mode . ("pylsp"))
          (rust-mode rust-ts-mode . ("rust-analyzer"))
          ((js-mode js-ts-mode typescript-mode typescript-ts-mode) . ("typescript-language-server" "--stdio"))
          ((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))
          (go-mode go-ts-mode . ("gopls"))
          (java-mode java-ts-mode . ("jdtls"))
          ((ruby-mode ruby-ts-mode) . ("solargraph" "stdio"))
          ;; Haskell LSP disabled
          ;; (haskell-mode . ("haskell-language-server-wrapper" "--lsp"))
          (nix-mode . ("nil"))))

  ;; Performance
  (setq eglot-events-buffer-size 0)

  ;; Stay out of the way
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil
        eglot-extend-to-xref t)

  ;; Eldoc
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil
        eldoc-documentation-strategy bv-dev-eldoc-documentation-strategy)

  ;; Format on save
  (when bv-dev-format-on-save
    (defun bv-dev-eglot-format-on-save ()
      "Format buffer on save if eglot is active."
      (when (and (bound-and-true-p eglot--managed-mode)
                 (eglot--server-capable :documentFormattingProvider))
        (eglot-format-buffer)))

    (add-hook 'before-save-hook #'bv-dev-eglot-format-on-save))

  ;; Clear imenu cache
  (defun bv-dev-eglot-clear-imenu-cache ()
    "Clear imenu cache when eglot is active."
    (when (bound-and-true-p eglot--managed-mode)
      (setq imenu--index-alist nil)))

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
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))


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

;;;; Additional Development Utilities

;; Highlight TODO keywords
(use-package hl-todo
  :defer t
  :init
  (when (locate-library "hl-todo")
    (autoload 'hl-todo-mode "hl-todo" nil t)
    (add-hook 'prog-mode-hook #'hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#FF0000")
          ("FIXME" . "#FF0000")
          ("HACK" . "#7C7C75")
          ("REVIEW" . "#FF6C00")
          ("NOTE" . "#0000FF")
          ("DEPRECATED" . "#7C7C75"))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :defer t
  :init
  (when (locate-library "rainbow-delimiters")
    (autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'conf-mode-hook #'rainbow-delimiters-mode)))

;; Indentation guides
(use-package highlight-indent-guides
  :defer t
  :init
  (when (locate-library "highlight-indent-guides")
    (autoload 'highlight-indent-guides-mode "highlight-indent-guides" nil t)
    (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\│
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0.1))

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
