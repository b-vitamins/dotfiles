;;; bv-lang-erlang-elixir.el --- Erlang/Elixir development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Erlang and Elixir development environment with tree-sitter, LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar erlang-indent-level)
(defvar erlang-indent-guard)
(defvar elixir-format-arguments)
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar erlang-mode-map)
(defvar elixir-mode-map)
(defvar erlang-ts-mode-map)
(defvar elixir-ts-mode-map)

;; External functions
(declare-function erlang-compile-display "erlang" ())
(declare-function run-elixir "inf-elixir" ())
(declare-function erlang-man-function "erlang" (function))

;; Optional package loading
(autoload 'erlang-mode "erlang" nil t)
(autoload 'erlang-shell "erlang" nil t)
(autoload 'elixir-mode "elixir-mode" nil t)
(autoload 'alchemist-mode "alchemist" nil t)

;; Enable tree-sitter when available
(when (treesit-language-available-p 'erlang)
  (add-to-list 'major-mode-remap-alist '(erlang-mode . erlang-ts-mode)))
(when (treesit-language-available-p 'elixir)
  (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
  (add-to-list 'major-mode-remap-alist '(elixir-ts-mode . heex-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app\\.src\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.leex\\'" . elixir-mode))

;; Erlang settings
(setq erlang-indent-level 4)
(setq erlang-indent-guard 2)

;; Elixir settings
(setq elixir-format-arguments '("--dot-formatter" ".formatter.exs"))

;; Configure language servers
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((erlang-mode erlang-ts-mode) . ("erlang_ls")))
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) .
                 ("elixir-ls"))))

;; Language-specific eglot settings
(defun bv-erlang-elixir-eglot-config ()
  "Configure eglot for Erlang/Elixir development."
  (cond
   ((derived-mode-p 'erlang-mode 'erlang-ts-mode)
    (setq-local eglot-workspace-configuration
                '(:erlang_ls (:macros (:enabled t)))))
   ((derived-mode-p 'elixir-mode 'elixir-ts-mode 'heex-ts-mode)
    (setq-local eglot-workspace-configuration
                '(:elixirLS (:fetchDeps t
                            :dialyzerEnabled t
                            :suggestSpecs t))))))

(add-hook 'erlang-mode-hook #'bv-erlang-elixir-eglot-config)
(add-hook 'elixir-mode-hook #'bv-erlang-elixir-eglot-config)
(when (fboundp 'erlang-ts-mode)
  (add-hook 'erlang-ts-mode-hook #'bv-erlang-elixir-eglot-config))
(when (fboundp 'elixir-ts-mode)
  (add-hook 'elixir-ts-mode-hook #'bv-erlang-elixir-eglot-config))

;; Enable alchemist for Elixir if available
(defun bv-elixir-maybe-enable-alchemist ()
  "Enable alchemist-mode if available."
  (when (fboundp 'alchemist-mode)
    (alchemist-mode 1)))

(add-hook 'elixir-mode-hook #'bv-elixir-maybe-enable-alchemist)
(when (fboundp 'elixir-ts-mode)
  (add-hook 'elixir-ts-mode-hook #'bv-elixir-maybe-enable-alchemist))

;; Erlang shell
(defun bv-erlang-shell ()
  "Start Erlang shell."
  (interactive)
  (erlang-shell))

(defun bv-erlang-compile ()
  "Compile current Erlang file."
  (interactive)
  (erlang-compile))

(defun bv-erlang-compile-display ()
  "Compile and display Erlang file."
  (interactive)
  (when (fboundp 'erlang-compile-display)
    (erlang-compile-display)))

;; Elixir IEx REPL
(defun bv-elixir-iex ()
  "Start IEx REPL."
  (interactive)
  (if (fboundp 'alchemist-iex-run)
      (alchemist-iex-run)
    (when (fboundp 'run-elixir)
      (run-elixir))))

(defun bv-elixir-iex-project ()
  "Start IEx with project."
  (interactive)
  (if (fboundp 'alchemist-iex-project-run)
      (alchemist-iex-project-run)
    (let ((default-directory (locate-dominating-file default-directory "mix.exs")))
      (when (fboundp 'run-elixir)
        (run-elixir))))

;; Mix commands
(defun bv-elixir-mix-compile ()
  "Run mix compile."
  (interactive)
  (compile "mix compile"))

(defun bv-elixir-mix-test ()
  "Run mix test."
  (interactive)
  (compile "mix test"))

(defun bv-elixir-mix-test-file ()
  "Run mix test on current file."
  (interactive)
  (compile (format "mix test %s" (buffer-file-name))))

(defun bv-elixir-mix-test-at-point ()
  "Run mix test at point."
  (interactive)
  (compile (format "mix test %s:%d" (buffer-file-name) (line-number-at-pos))))

(defun bv-elixir-mix-format ()
  "Format current Elixir file."
  (interactive)
  (shell-command (format "mix format %s" (buffer-file-name)))
  (revert-buffer t t))

(defun bv-elixir-mix-deps-get ()
  "Run mix deps.get."
  (interactive)
  (compile "mix deps.get"))

(defun bv-elixir-mix-clean ()
  "Run mix clean."
  (interactive)
  (compile "mix clean"))

(defun bv-elixir-mix-credo ()
  "Run mix credo."
  (interactive)
  (compile "mix credo"))

(defun bv-elixir-mix-dialyzer ()
  "Run mix dialyzer."
  (interactive)
  (compile "mix dialyzer"))

;; Phoenix commands
(defun bv-elixir-phoenix-server ()
  "Start Phoenix server."
  (interactive)
  (compile "mix phx.server"))

(defun bv-elixir-phoenix-routes ()
  "Show Phoenix routes."
  (interactive)
  (compile "mix phx.routes"))

(defun bv-elixir-phoenix-migrate ()
  "Run Phoenix migrations."
  (interactive)
  (compile "mix ecto.migrate"))

;; Documentation
(defun bv-erlang-man-at-point ()
  "Show Erlang man page for symbol at point."
  (interactive)
  (when (fboundp 'erlang-man-function)
    (erlang-man-function (thing-at-point 'symbol)))))

(defun bv-elixir-help-at-point ()
  "Show Elixir help for symbol at point."
  (interactive)
  (if (fboundp 'alchemist-help)
      (alchemist-help)
    (message "Alchemist not available")))

;; Formatting
(defun bv-erlang-format-buffer ()
  "Format Erlang buffer."
  (interactive)
  (if (executable-find "erlfmt")
      (shell-command-on-region (point-min) (point-max)
                              "erlfmt -" nil t)
    (eglot-format-buffer)))

(defun bv-elixir-format-buffer ()
  "Format Elixir buffer."
  (interactive)
  (bv-elixir-mix-format))

;; Compilation setup
(defun bv-erlang-setup-compilation ()
  "Setup compilation for Erlang."
  (setq-local compile-command
              (cond
               ((file-exists-p "rebar3") "rebar3 compile")
               ((file-exists-p "rebar") "rebar compile")
               ((file-exists-p "Makefile") "make")
               (t (format "erlc %s" (buffer-file-name))))))

(defun bv-elixir-setup-compilation ()
  "Setup compilation for Elixir."
  (setq-local compile-command "mix compile"))

(add-hook 'erlang-mode-hook #'bv-erlang-setup-compilation)
(add-hook 'elixir-mode-hook #'bv-elixir-setup-compilation)
(when (fboundp 'erlang-ts-mode)
  (add-hook 'erlang-ts-mode-hook #'bv-erlang-setup-compilation))
(when (fboundp 'elixir-ts-mode)
  (add-hook 'elixir-ts-mode-hook #'bv-elixir-setup-compilation))

;; Electric pairs
(defun bv-erlang-elixir-electric-pairs ()
  "Setup electric pairs for Erlang/Elixir."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?< . ?>)))))

(add-hook 'erlang-mode-hook #'electric-pair-local-mode)
(add-hook 'erlang-mode-hook #'bv-erlang-elixir-electric-pairs)
(add-hook 'elixir-mode-hook #'electric-pair-local-mode)
(add-hook 'elixir-mode-hook #'bv-erlang-elixir-electric-pairs)

;; Pretty symbols
(defun bv-elixir-pretty-symbols ()
  "Enable pretty symbols for Elixir."
  (setq prettify-symbols-alist
        '(("->" . ?→)
          ("=>" . ?⇒)
          ("|>" . ?▷)
          ("<-" . ?←)
          ("!=" . ?≠)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("fn" . ?λ)))
  (prettify-symbols-mode 1))

(add-hook 'elixir-mode-hook #'bv-elixir-pretty-symbols)
(when (fboundp 'elixir-ts-mode)
  (add-hook 'elixir-ts-mode-hook #'bv-elixir-pretty-symbols))

;; Keybindings for Erlang
(defvar bv-erlang-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-erlang-shell)
    ;; Compilation
    (define-key map (kbd "C-c C-k") #'bv-erlang-compile)
    (define-key map (kbd "C-c C-l") #'bv-erlang-compile-display)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-erlang-man-at-point)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-erlang-format-buffer)
    map)
  "Keymap for Erlang mode commands.")

;; Keybindings for Elixir
(defvar bv-elixir-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-elixir-iex)
    (define-key map (kbd "C-c C-Z") #'bv-elixir-iex-project)
    ;; Mix commands
    (define-key map (kbd "C-c m c") #'bv-elixir-mix-compile)
    (define-key map (kbd "C-c m t") #'bv-elixir-mix-test)
    (define-key map (kbd "C-c m T") #'bv-elixir-mix-test-file)
    (define-key map (kbd "C-c m s") #'bv-elixir-mix-test-at-point)
    (define-key map (kbd "C-c m f") #'bv-elixir-mix-format)
    (define-key map (kbd "C-c m d") #'bv-elixir-mix-deps-get)
    (define-key map (kbd "C-c m k") #'bv-elixir-mix-clean)
    (define-key map (kbd "C-c m r") #'bv-elixir-mix-credo)
    (define-key map (kbd "C-c m D") #'bv-elixir-mix-dialyzer)
    ;; Phoenix
    (define-key map (kbd "C-c p s") #'bv-elixir-phoenix-server)
    (define-key map (kbd "C-c p r") #'bv-elixir-phoenix-routes)
    (define-key map (kbd "C-c p m") #'bv-elixir-phoenix-migrate)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-elixir-help-at-point)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-elixir-format-buffer)
    map)
  "Keymap for Elixir mode commands.")

(with-eval-after-load 'erlang
  (define-key erlang-mode-map (kbd "C-c C-c") bv-erlang-mode-map))

(with-eval-after-load 'elixir-mode
  (define-key elixir-mode-map (kbd "C-c C-c") bv-elixir-mode-map))

(when (fboundp 'erlang-ts-mode)
  (with-eval-after-load 'erlang-ts-mode
    (define-key erlang-ts-mode-map (kbd "C-c C-c") bv-erlang-mode-map)))

(when (fboundp 'elixir-ts-mode)
  (with-eval-after-load 'elixir-ts-mode
    (define-key elixir-ts-mode-map (kbd "C-c C-c") bv-elixir-mode-map)))

(provide 'bv-lang-erlang-elixir)
;;; bv-lang-erlang-elixir.el ends here