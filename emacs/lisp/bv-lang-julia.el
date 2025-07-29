;;; bv-lang-julia.el --- Julia development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Julia development environment with tree-sitter, LSP, and Jupyter integration.

;;; Code:

(require 'eglot)

;; External variables
(defvar julia-indent-offset)
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar julia-mode-map)

;; External functions
(declare-function julia-repl-send-string "julia-repl" (string &optional cell))
(declare-function julia-repl-send-region-or-line "julia-repl" (&optional arg))
(declare-function julia-repl-send-buffer "julia-repl" ())
(declare-function julia-repl-set-terminal-backend "julia-repl" (backend))

;; Optional package loading
(autoload 'julia-mode "julia-mode" nil t)
(autoload 'julia-repl "julia-repl" nil t)
(autoload 'julia-repl-mode "julia-repl" nil t)
(autoload 'jupyter-run-julia "jupyter" nil t)
(autoload 'jupyter-repl-associate-buffer "jupyter" nil t)

;; Enable tree-sitter for Julia when available
(when (treesit-language-available-p 'julia)
  (add-to-list 'major-mode-remap-alist '(julia-mode . julia-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

;; Julia settings
(setq julia-indent-offset 4)

;; Configure Julia language server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((julia-mode julia-ts-mode) . ("julia" "--startup-file=no" "--history-file=no"
                                               "-e" "using LanguageServer; runserver()"))))

;; Julia-specific eglot settings
(defun bv-julia-eglot-config ()
  "Configure eglot for Julia development."
  (setq-local eglot-connect-timeout 60) ; Julia LS can be slow to start
  (setq-local eglot-workspace-configuration
              '(:julia (:format (:indent 4
                               :margin 92)))))

(add-hook 'julia-mode-hook #'bv-julia-eglot-config)
(when (fboundp 'julia-ts-mode)
  (add-hook 'julia-ts-mode-hook #'bv-julia-eglot-config))

;; Julia REPL integration
(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'vterm))

(add-hook 'julia-mode-hook #'julia-repl-mode)

;; Jupyter integration
(defun bv-julia-jupyter-repl ()
  "Start Jupyter Julia REPL."
  (interactive)
  (jupyter-run-julia)
  (jupyter-repl-associate-buffer))

;; Formatting
(defun bv-julia-format-buffer ()
  "Format current Julia buffer."
  (interactive)
  (if (executable-find "julia")
      (shell-command-on-region
       (point-min) (point-max)
       "julia -e 'using JuliaFormatter; print(format_text(read(stdin, String)))'"
       nil t)
    (message "Julia not found")))

(defun bv-julia-format-on-save ()
  "Format buffer before saving."
  (when (eq major-mode 'julia-mode)
    (bv-julia-format-buffer)))

;; Optional: Enable format on save
;; (add-hook 'before-save-hook #'bv-julia-format-on-save)

;; Running Julia code
(defun bv-julia-run-file ()
  "Run current Julia file."
  (interactive)
  (compile (format "julia %s" (buffer-file-name))))

(defun bv-julia-run-region ()
  "Run selected region in Julia REPL."
  (interactive)
  (julia-repl-send-region-or-line))

(defun bv-julia-run-buffer ()
  "Run entire buffer in Julia REPL."
  (interactive)
  (julia-repl-send-buffer))

;; Package management
(defun bv-julia-pkg-activate ()
  "Activate Julia project in current directory."
  (interactive)
  (julia-repl-send-string "using Pkg; Pkg.activate(\".\")"))

(defun bv-julia-pkg-instantiate ()
  "Instantiate Julia project."
  (interactive)
  (julia-repl-send-string "using Pkg; Pkg.instantiate()"))

(defun bv-julia-pkg-update ()
  "Update Julia packages."
  (interactive)
  (julia-repl-send-string "using Pkg; Pkg.update()"))

(defun bv-julia-pkg-test ()
  "Run Julia package test."
  (interactive)
  (julia-repl-send-string "using Pkg; Pkg.test()"))

(defun bv-julia-pkg-add (package)
  "Add Julia PACKAGE."
  (interactive "sPackage name: ")
  (julia-repl-send-string (format "using Pkg; Pkg.add(\"%s\")" package)))

;; Testing
(defun bv-julia-run-tests ()
  "Run Julia test."
  (interactive)
  (compile "julia --project test/runtests.jl"))

;; Documentation
(defun bv-julia-doc-at-point ()
  "Show documentation for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (julia-repl-send-string (format "?%s" symbol)))))

(defun bv-julia-apropos (query)
  "Search Julia documentation for QUERY."
  (interactive "sSearch query: ")
  (julia-repl-send-string (format "apropos(\"%s\")" query)))

;; Benchmarking
(defun bv-julia-benchmark-region ()
  "Benchmark selected region."
  (interactive)
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (julia-repl-send-string
     (format "using BenchmarkTools; @benchmark %s" code))))

;; Profiling
(defun bv-julia-profile-region ()
  "Profile selected region."
  (interactive)
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (julia-repl-send-string
     (format "using Profile; @profile %s; Profile.print()" code))))

;; Macroexpansion
(defun bv-julia-macroexpand-at-point ()
  "Expand macro at point."
  (interactive)
  (let ((expr (thing-at-point 'sexp)))
    (when expr
      (julia-repl-send-string (format "@macroexpand %s" expr)))))

;; Environment
(defun bv-julia-versioninfo ()
  "Show Julia version info."
  (interactive)
  (julia-repl-send-string "versioninfo()"))

;; Compilation setup
(defun bv-julia-setup-compilation ()
  "Setup compilation for Julia."
  (setq-local compile-command (format "julia %s" (buffer-file-name))))

(add-hook 'julia-mode-hook #'bv-julia-setup-compilation)

;; Electric pairs
(defun bv-julia-electric-pairs ()
  "Setup electric pairs for Julia."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?\` . ?\`)))))

(add-hook 'julia-mode-hook #'electric-pair-local-mode)
(add-hook 'julia-mode-hook #'bv-julia-electric-pairs)

;; Pretty symbols
(defun bv-julia-pretty-symbols ()
  "Enable pretty symbols for Julia."
  (setq prettify-symbols-alist
        '(("function" . ?ƒ)
          ("end" . ?∎)
          ("in" . ?∈)
          ("!=" . ?≠)
          (">=" . ?≥)
          ("<=" . ?≤)
          ("sqrt" . ?√)
          ("pi" . ?π)
          ("Inf" . ?∞)))
  (prettify-symbols-mode 1))

(add-hook 'julia-mode-hook #'bv-julia-pretty-symbols)

;; Keybindings
(defvar bv-julia-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'julia-repl)
    (define-key map (kbd "C-c C-j") #'bv-julia-jupyter-repl)
    ;; Evaluation
    (define-key map (kbd "C-c C-c") #'bv-julia-run-region)
    (define-key map (kbd "C-c C-b") #'bv-julia-run-buffer)
    (define-key map (kbd "C-c C-r") #'bv-julia-run-file)
    ;; Package management
    (define-key map (kbd "C-c p a") #'bv-julia-pkg-activate)
    (define-key map (kbd "C-c p i") #'bv-julia-pkg-instantiate)
    (define-key map (kbd "C-c p u") #'bv-julia-pkg-update)
    (define-key map (kbd "C-c p t") #'bv-julia-pkg-test)
    (define-key map (kbd "C-c p +") #'bv-julia-pkg-add)
    ;; Testing
    (define-key map (kbd "C-c C-t") #'bv-julia-run-tests)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-julia-doc-at-point)
    (define-key map (kbd "C-c C-a") #'bv-julia-apropos)
    ;; Performance
    (define-key map (kbd "C-c b r") #'bv-julia-benchmark-region)
    (define-key map (kbd "C-c b p") #'bv-julia-profile-region)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-julia-format-buffer)
    ;; Macro
    (define-key map (kbd "C-c C-m") #'bv-julia-macroexpand-at-point)
    ;; Info
    (define-key map (kbd "C-c C-v") #'bv-julia-versioninfo)
    map)
  "Keymap for Julia mode commands.")

(with-eval-after-load 'julia-mode
  (define-key julia-mode-map (kbd "C-c j") bv-julia-mode-map))

(provide 'bv-lang-julia)
;;; bv-lang-julia.el ends here