;;; bv-lang-clojure.el --- Clojure development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Clojure development environment with tree-sitter, CIDER, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar clojure-indent-style)
(defvar clojure-align-forms-automatically)
(defvar electric-pair-pairs)
(defvar clojure-mode-map)
(defvar clojure-ts-mode-map)
(defvar cider-repl-display-help-banner)
(defvar cider-repl-pop-to-buffer-on-connect)
(defvar cider-repl-use-pretty-printing)
(defvar cider-repl-history-file)
(defvar cider-repl-wrap-history)
(defvar cider-font-lock-dynamically)
(defvar cider-overlays-use-font-lock)
(defvar cider-eldoc-display-for-symbol-at-point)
(defvar cider-eldoc-display-context-dependent-info)

;; External functions
(declare-function cider-eval-buffer "cider-eval" ())
(declare-function cider-eval-last-sexp "cider-eval" (&optional output-to-current-buffer))
(declare-function cider-eval-defun-at-point "cider-eval" (&optional output-to-current-buffer))
(declare-function cider-eval-region "cider-eval" (start end &optional output-to-current-buffer))
(declare-function cider-test-run-ns-tests "cider-test" (&optional ns))
(declare-function cider-test-run-project-tests "cider-test" (&optional arg))
(declare-function cider-test-run-test "cider-test" ())
(declare-function cider-doc "cider-doc" (&optional arg))
(declare-function cider-javadoc "cider-doc" (&optional arg))
(declare-function cider-apropos "cider-apropos" (query &optional ns docs-p privates-p case-sensitive-p))
(declare-function cider-find-var "cider-find" (&optional arg line))
(declare-function cider-pop-back "cider-find" ())
(declare-function cider-format-buffer "cider-format" ())
(declare-function cider-format-region "cider-format" (start end))
(declare-function clojure-sort-ns "clojure-mode" ())
(declare-function clojure-align "clojure-mode" (beg end))
(declare-function cider-macroexpand-1 "cider-macroexpansion" (&optional prefix))
(declare-function cider-macroexpand-all "cider-macroexpansion" (&optional prefix))
(declare-function cider-inspect "cider-inspector" (&optional arg))
(declare-function cider-pprint-eval-last-sexp "cider-eval" (&optional output-to-current-buffer))
(declare-function rainbow-delimiters-mode "rainbow-delimiters" (&optional arg))

;; Optional package loading
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'clojure-ts-mode "clojure-ts-mode" nil t)
(autoload 'cider-jack-in "cider" nil t)
(autoload 'cider-connect "cider" nil t)
(autoload 'cider-mode "cider" nil t)

;; Enable tree-sitter for Clojure when available
(when (treesit-language-available-p 'clojure)
  (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . clojure-mode))

;; Clojure settings
(setq clojure-indent-style 'align-arguments)
(setq clojure-align-forms-automatically t)

;; Configure clojure-lsp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojure-ts-mode clojurescript-mode clojurec-mode) .
                 ("clojure-lsp"))))

;; Clojure-specific eglot settings
(defun bv-clojure-eglot-config ()
  "Configure eglot for Clojure development."
  (setq-local eglot-workspace-configuration
              '(:clojure-lsp (:semantic-tokens? t
                             :show-docs-arity-on-same-line? t))))

(add-hook 'clojure-mode-hook #'bv-clojure-eglot-config)
(add-hook 'clojure-ts-mode-hook #'bv-clojure-eglot-config)
(add-hook 'clojurescript-mode-hook #'bv-clojure-eglot-config)
(add-hook 'clojurec-mode-hook #'bv-clojure-eglot-config)

;; CIDER configuration
(with-eval-after-load 'cider
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-history-file "~/.cache/emacs/cider-history")
  (setq cider-repl-wrap-history t)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-overlays-use-font-lock t)
  (setq cider-eldoc-display-for-symbol-at-point t)
  (setq cider-eldoc-display-context-dependent-info t))

;; Enable CIDER in Clojure buffers
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'clojure-ts-mode-hook #'cider-mode)

;; REPL interaction
(defun bv-clojure-jack-in ()
  "Start CIDER REPL."
  (interactive)
  (cider-jack-in))

(defun bv-clojure-connect ()
  "Connect to running REPL."
  (interactive)
  (cider-connect))

(defun bv-clojure-eval-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (cider-eval-buffer))

(defun bv-clojure-eval-last-sexp ()
  "Evaluate last s-expression."
  (interactive)
  (cider-eval-last-sexp))

(defun bv-clojure-eval-defun ()
  "Evaluate current defun."
  (interactive)
  (cider-eval-defun-at-point))

(defun bv-clojure-eval-region ()
  "Evaluate selected region."
  (interactive)
  (cider-eval-region (region-beginning) (region-end)))

;; Testing
(defun bv-clojure-run-tests ()
  "Run test in current namespace."
  (interactive)
  (cider-test-run-ns-tests))

(defun bv-clojure-run-all-tests ()
  "Run all project test."
  (interactive)
  (cider-test-run-project-tests))

(defun bv-clojure-run-test-at-point ()
  "Run test at point."
  (interactive)
  (cider-test-run-test))

;; Documentation
(defun bv-clojure-doc ()
  "Show documentation for symbol at point."
  (interactive)
  (cider-doc))

(defun bv-clojure-javadoc ()
  "Show Javadoc for symbol at point."
  (interactive)
  (cider-javadoc))

(defun bv-clojure-apropos (query)
  "Search for symbols matching QUERY."
  (interactive "sQuery: ")
  (cider-apropos query))

;; Navigation
(defun bv-clojure-find-var ()
  "Jump to definition."
  (interactive)
  (cider-find-var))

(defun bv-clojure-pop-back ()
  "Pop back to previous location."
  (interactive)
  (cider-pop-back))

;; Refactoring
(defun bv-clojure-format-buffer ()
  "Format current buffer."
  (interactive)
  (cider-format-buffer))

(defun bv-clojure-format-region ()
  "Format selected region."
  (interactive)
  (cider-format-region (region-beginning) (region-end)))

;; Build tools
(defun bv-clojure-lein-run ()
  "Run lein run."
  (interactive)
  (compile "lein run"))

(defun bv-clojure-lein-test ()
  "Run lein test."
  (interactive)
  (compile "lein test"))

(defun bv-clojure-lein-compile ()
  "Run lein compile."
  (interactive)
  (compile "lein compile"))

(defun bv-clojure-lein-clean ()
  "Run lein clean."
  (interactive)
  (compile "lein clean"))

(defun bv-clojure-deps-run ()
  "Run clojure with deps.edn."
  (interactive)
  (compile "clojure -M -m main"))

;; Namespace management
(defun bv-clojure-sort-ns ()
  "Sort namespace declarations."
  (interactive)
  (clojure-sort-ns))

(defun bv-clojure-align ()
  "Align Clojure forms."
  (interactive)
  (clojure-align (point-min) (point-max)))

;; Macroexpansion
(defun bv-clojure-macroexpand ()
  "Expand macro at point."
  (interactive)
  (cider-macroexpand-1))

(defun bv-clojure-macroexpand-all ()
  "Fully expand macro at point."
  (interactive)
  (cider-macroexpand-all))

;; Inspector
(defun bv-clojure-inspect ()
  "Inspect value at point."
  (interactive)
  (cider-inspect))

;; Pretty printing
(defun bv-clojure-pprint-eval-last-sexp ()
  "Pretty print eval result."
  (interactive)
  (cider-pprint-eval-last-sexp))

;; Electric pairs
(defun bv-clojure-electric-pairs ()
  "Setup electric pairs for Clojure."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?` . ?`)))))

(add-hook 'clojure-mode-hook #'electric-pair-local-mode)
(add-hook 'clojure-mode-hook #'bv-clojure-electric-pairs)
(add-hook 'clojure-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'clojure-ts-mode-hook #'bv-clojure-electric-pairs)

;; Rainbow delimiters
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-ts-mode-hook #'rainbow-delimiters-mode)

;; Smartparens for structural editing
(with-eval-after-load 'smartparens
  (require 'smartparens-clojure)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-ts-mode-hook #'smartparens-strict-mode))

;; Compilation setup
(defun bv-clojure-setup-compilation ()
  "Setup compilation for Clojure."
  (setq-local compile-command
              (cond
               ((file-exists-p "project.clj") "lein run")
               ((file-exists-p "deps.edn") "clojure -M -m main")
               ((file-exists-p "build.boot") "boot run")
               (t "clojure"))))

(add-hook 'clojure-mode-hook #'bv-clojure-setup-compilation)
(add-hook 'clojure-ts-mode-hook #'bv-clojure-setup-compilation)

;; Keybindings
(defvar bv-clojure-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-j") #'bv-clojure-jack-in)
    (define-key map (kbd "C-c C-x") #'bv-clojure-connect)
    ;; Evaluation
    (define-key map (kbd "C-c C-e") #'bv-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'bv-clojure-eval-defun)
    (define-key map (kbd "C-c C-b") #'bv-clojure-eval-buffer)
    (define-key map (kbd "C-c C-r") #'bv-clojure-eval-region)
    (define-key map (kbd "C-c C-p") #'bv-clojure-pprint-eval-last-sexp)
    ;; Testing
    (define-key map (kbd "C-c t n") #'bv-clojure-run-tests)
    (define-key map (kbd "C-c t a") #'bv-clojure-run-all-tests)
    (define-key map (kbd "C-c t t") #'bv-clojure-run-test-at-point)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-clojure-doc)
    (define-key map (kbd "C-c C-J") #'bv-clojure-javadoc)
    (define-key map (kbd "C-c C-a") #'bv-clojure-apropos)
    ;; Navigation
    (define-key map (kbd "M-.") #'bv-clojure-find-var)
    (define-key map (kbd "M-,") #'bv-clojure-pop-back)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-clojure-format-buffer)
    (define-key map (kbd "C-c f r") #'bv-clojure-format-region)
    (define-key map (kbd "C-c f n") #'bv-clojure-sort-ns)
    (define-key map (kbd "C-c f a") #'bv-clojure-align)
    ;; Macros
    (define-key map (kbd "C-c C-m") #'bv-clojure-macroexpand)
    (define-key map (kbd "C-c C-M") #'bv-clojure-macroexpand-all)
    ;; Inspector
    (define-key map (kbd "C-c C-i") #'bv-clojure-inspect)
    ;; Build tools
    (define-key map (kbd "C-c l r") #'bv-clojure-lein-run)
    (define-key map (kbd "C-c l t") #'bv-clojure-lein-test)
    (define-key map (kbd "C-c l c") #'bv-clojure-lein-compile)
    (define-key map (kbd "C-c l k") #'bv-clojure-lein-clean)
    map)
  "Keymap for Clojure mode commands.")

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c c") bv-clojure-mode-map))

(with-eval-after-load 'clojure-ts-mode
  (define-key clojure-ts-mode-map (kbd "C-c c") bv-clojure-mode-map))

(provide 'bv-lang-clojure)
;;; bv-lang-clojure.el ends here