;;; bv-lang-haskell.el --- Haskell development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Haskell development environment with tree-sitter, LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar haskell-process-type)
(defvar haskell-process-suggest-remove-import-lines)
(defvar haskell-process-auto-import-loaded-modules)
(defvar haskell-process-log)
(defvar haskell-tags-on-save)
(defvar haskell-stylish-on-save)
(defvar haskell-indentation-layout-offset)
(defvar haskell-indentation-left-offset)
(defvar haskell-indentation-where-pre-offset)
(defvar haskell-indentation-where-post-offset)
(defvar haskell-interactive-mode-history-file)
(defvar prettify-symbols-alist)
(defvar haskell-mode-map)

;; External functions
(declare-function haskell-process-cabal-repl "haskell-mode" ())
(declare-function haskell-process-stack-ghci "haskell-mode" ())
(declare-function haskell-process-load-file "haskell-mode" ())
(declare-function haskell-process-reload "haskell-mode" ())
(declare-function haskell-process-send-region "haskell-mode" (start end))
(declare-function haskell-mode-show-type-at "haskell-mode" ())
(declare-function haskell-process-do-info "haskell-mode" ())
(declare-function url-encode-url "url-util" (url))
(declare-function haskell-sort-imports "haskell-mode" ())
(declare-function haskell-align-imports "haskell-mode" ())
(declare-function haskell-mode-goto-loc "haskell-mode" ())

;; Optional package loading
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'interactive-haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal-mode "haskell-cabal" nil t)

;; Enable tree-sitter for Haskell when available
(when (treesit-language-available-p 'haskell)
  (add-to-list 'major-mode-remap-alist '(haskell-mode . haskell-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . haskell-literate-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))

;; Haskell settings
(setq haskell-process-type 'cabal-repl)
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-tags-on-save nil)
(setq haskell-stylish-on-save nil) ; We'll use fourmolu/ormolu instead

;; Indentation
(setq haskell-indentation-layout-offset 2)
(setq haskell-indentation-left-offset 2)
(setq haskell-indentation-where-pre-offset 2)
(setq haskell-indentation-where-post-offset 2)

;; Configure haskell-language-server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((haskell-mode haskell-ts-mode) . ("haskell-language-server-wrapper" "--lsp"))))

;; Haskell-specific eglot settings
(defun bv-haskell-eglot-config ()
  "Configure eglot for Haskell development."
  (setq-local eglot-workspace-configuration
              '(:haskell (:plugin (:stan (:globalOn :json-false))
                         :formattingProvider "fourmolu"))))

(add-hook 'haskell-mode-hook #'bv-haskell-eglot-config)
(when (fboundp 'haskell-ts-mode)
  (add-hook 'haskell-ts-mode-hook #'bv-haskell-eglot-config))

;; Interactive Haskell
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)

;; Formatting
(defun bv-haskell-format-buffer ()
  "Format current Haskell buffer."
  (interactive)
  (cond
   ((executable-find "fourmolu")
    (shell-command-on-region (point-min) (point-max)
                            "fourmolu" nil t))
   ((executable-find "ormolu")
    (shell-command-on-region (point-min) (point-max)
                            "ormolu" nil t))
   ((executable-find "stylish-haskell")
    (shell-command-on-region (point-min) (point-max)
                            "stylish-haskell" nil t))
   (t
    (eglot-format-buffer))))

;; Cabal commands
(defun bv-haskell-cabal-build ()
  "Build Cabal project."
  (interactive)
  (compile "cabal build"))

(defun bv-haskell-cabal-test ()
  "Run Cabal tests."
  (interactive)
  (compile "cabal test"))

(defun bv-haskell-cabal-run ()
  "Run Cabal project."
  (interactive)
  (compile "cabal run"))

(defun bv-haskell-cabal-repl ()
  "Start Cabal REPL."
  (interactive)
  (haskell-process-cabal-repl))

(defun bv-haskell-cabal-clean ()
  "Clean Cabal project."
  (interactive)
  (compile "cabal clean"))

;; Stack commands (if using Stack)
(defun bv-haskell-stack-build ()
  "Build Stack project."
  (interactive)
  (compile "stack build"))

(defun bv-haskell-stack-test ()
  "Run Stack tests."
  (interactive)
  (compile "stack test"))

(defun bv-haskell-stack-run ()
  "Run Stack project."
  (interactive)
  (compile "stack run"))

(defun bv-haskell-stack-repl ()
  "Start Stack REPL."
  (interactive)
  (haskell-process-stack-ghci))

;; GHCi interaction
(defun bv-haskell-load-file ()
  "Load current file in GHCi."
  (interactive)
  (haskell-process-load-file))

(defun bv-haskell-reload ()
  "Reload in GHCi."
  (interactive)
  (haskell-process-reload))

(defun bv-haskell-send-region ()
  "Send region to GHCi."
  (interactive)
  (haskell-process-send-region (region-beginning) (region-end)))

;; Type information
(defun bv-haskell-type-at-point ()
  "Show type at point."
  (interactive)
  (haskell-mode-show-type-at))

(defun bv-haskell-info-at-point ()
  "Show info at point."
  (interactive)
  (haskell-process-do-info))

;; Documentation
(defun bv-haskell-hoogle (query)
  "Search Hoogle for QUERY."
  (interactive "sHoogle query: ")
  (browse-url (format "https://hoogle.haskell.org/?hoogle=%s"
                     (url-encode-url query))))

(defun bv-haskell-hoogle-at-point ()
  "Search Hoogle for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (bv-haskell-hoogle symbol))))

;; Imports management
(defun bv-haskell-sort-imports ()
  "Sort Haskell imports."
  (interactive)
  (haskell-sort-imports))

(defun bv-haskell-align-imports ()
  "Align Haskell imports."
  (interactive)
  (haskell-align-imports))

;; Navigation
(defun bv-haskell-goto-definition ()
  "Go to definition."
  (interactive)
  (haskell-mode-goto-loc))

;; Error checking
(defun bv-haskell-check ()
  "Check current buffer for errors."
  (interactive)
  (haskell-process-load-file))

;; REPL history
(setq haskell-interactive-mode-history-file "~/.cache/emacs/haskell-history")

;; Compilation setup
(defun bv-haskell-setup-compilation ()
  "Setup compilation for Haskell."
  (setq-local compile-command
              (cond
               ((file-exists-p "stack.yaml") "stack build")
               ((file-exists-p "cabal.project") "cabal build")
               ((file-exists-p (concat (file-name-base buffer-file-name) ".cabal"))
                "cabal build")
               (t "ghc -Wall -O2 " (buffer-file-name)))))

(add-hook 'haskell-mode-hook #'bv-haskell-setup-compilation)

;; Pretty symbols
(defun bv-haskell-pretty-symbols ()
  "Enable pretty symbols for Haskell."
  (setq prettify-symbols-alist
        '(("\\" . ?λ)
          ("->" . ?→)
          ("=>" . ?⇒)
          ("::" . ?∷)
          ("forall" . ?∀)
          ("<-" . ?←)
          (">=" . ?≥)
          ("<=" . ?≤)
          ("/=" . ?≠)))
  (prettify-symbols-mode 1))

(add-hook 'haskell-mode-hook #'bv-haskell-pretty-symbols)

;; Keybindings
(defvar bv-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Building
    (define-key map (kbd "C-c C-b") #'bv-haskell-cabal-build)
    (define-key map (kbd "C-c C-t") #'bv-haskell-cabal-test)
    (define-key map (kbd "C-c C-r") #'bv-haskell-cabal-run)
    (define-key map (kbd "C-c C-k") #'bv-haskell-cabal-clean)
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-haskell-cabal-repl)
    (define-key map (kbd "C-c C-l") #'bv-haskell-load-file)
    (define-key map (kbd "C-c C-R") #'bv-haskell-reload)
    (define-key map (kbd "C-c C-s") #'bv-haskell-send-region)
    ;; Type info
    (define-key map (kbd "C-c C-t") #'bv-haskell-type-at-point)
    (define-key map (kbd "C-c C-i") #'bv-haskell-info-at-point)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-haskell-hoogle-at-point)
    (define-key map (kbd "C-c C-h") #'bv-haskell-hoogle)
    ;; Navigation
    (define-key map (kbd "M-.") #'bv-haskell-goto-definition)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-haskell-format-buffer)
    (define-key map (kbd "C-c i s") #'bv-haskell-sort-imports)
    (define-key map (kbd "C-c i a") #'bv-haskell-align-imports)
    ;; Stack commands
    (define-key map (kbd "C-c s b") #'bv-haskell-stack-build)
    (define-key map (kbd "C-c s t") #'bv-haskell-stack-test)
    (define-key map (kbd "C-c s r") #'bv-haskell-stack-run)
    (define-key map (kbd "C-c s z") #'bv-haskell-stack-repl)
    map)
  "Keymap for Haskell mode commands.")

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-c") bv-haskell-mode-map))

(provide 'bv-lang-haskell)
;;; bv-lang-haskell.el ends here