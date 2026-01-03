;;; bv-lang-rust.el --- Rust development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Rust development environment with tree-sitter, rust-analyzer, and cargo integration.

;;; Code:

(require 'eglot)
(require 'bv-format)

;; External variables
(defvar rust-ts-mode-map)
(defvar electric-pair-pairs)

;; Optional package loading
(autoload 'cargo-process-build "cargo" nil t)
(autoload 'cargo-process-run "cargo" nil t)
(autoload 'cargo-process-test "cargo" nil t)
(autoload 'cargo-process-current-test "cargo" nil t)
(autoload 'cargo-process-clippy "cargo" nil t)
(autoload 'cargo-process-fmt "cargo" nil t)
(autoload 'cargo-process-check "cargo" nil t)
(autoload 'cargo-process-bench "cargo" nil t)
(autoload 'cargo-process-doc "cargo" nil t)
(autoload 'cargo-process-add "cargo" nil t)
(autoload 'cargo-minor-mode "cargo" nil t)

;; Remap to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

;; Rust-analyzer configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer"))))

;; Rust-specific eglot settings
(defun bv-rust-eglot-config ()
  "Configure eglot for Rust development."
  (setq-local eglot-workspace-configuration
              '(:rust-analyzer
                (:cargo (:features "all")
                 :checkOnSave (:enable t
                              :command "clippy"
                              :allTargets t)
                 :procMacro (:enable t)
                 :inlayHints (:typeHints (:enable t)
                             :parameterHints (:enable t)
                             :chainingHints (:enable t))))))

(add-hook 'rust-ts-mode-hook #'bv-rust-eglot-config)

;; Cargo commands - use cargo.el when available, fallback to compile
(defun bv-rust-cargo-build ()
  "Run cargo build."
  (interactive)
  (if (fboundp 'cargo-process-build)
      (cargo-process-build)
    (compile "cargo build")))

(defun bv-rust-cargo-run ()
  "Run cargo run."
  (interactive)
  (if (fboundp 'cargo-process-run)
      (cargo-process-run)
    (compile "cargo run")))

(defun bv-rust-cargo-test ()
  "Run cargo test."
  (interactive)
  (if (fboundp 'cargo-process-test)
      (cargo-process-test)
    (compile "cargo test")))

(defun bv-rust-cargo-test-current ()
  "Run cargo test for current test."
  (interactive)
  (if (fboundp 'cargo-process-current-test)
      (cargo-process-current-test)
    (let ((test-name (thing-at-point 'symbol)))
      (compile (format "cargo test %s" test-name)))))

(defun bv-rust-cargo-clippy ()
  "Run cargo clippy."
  (interactive)
  (if (fboundp 'cargo-process-clippy)
      (cargo-process-clippy)
    (compile "cargo clippy -- -W clippy::all")))

(defun bv-rust-cargo-fmt ()
  "Run cargo fmt on current project."
  (interactive)
  (if (fboundp 'cargo-process-fmt)
      (cargo-process-fmt)
    (progn
      (shell-command "cargo fmt")
      (revert-buffer t t))))

(defun bv-rust-cargo-check ()
  "Run cargo check."
  (interactive)
  (if (fboundp 'cargo-process-check)
      (cargo-process-check)
    (compile "cargo check")))

(defun bv-rust-cargo-bench ()
  "Run cargo bench."
  (interactive)
  (if (fboundp 'cargo-process-bench)
      (cargo-process-bench)
    (compile "cargo bench")))

(defun bv-rust-cargo-doc ()
  "Run cargo doc."
  (interactive)
  (if (fboundp 'cargo-process-doc)
      (cargo-process-doc)
    (compile "cargo doc --open")))

(defun bv-rust-cargo-add (crate)
  "Add a CRATE dependency."
  (interactive "sCrate name: ")
  (if (fboundp 'cargo-process-add)
      (cargo-process-add crate)
    (compile (format "cargo add %s" crate))))

;; Format on save with rustfmt
(defun bv-rust-format-on-save ()
  "Format buffer with rustfmt before saving."
  (when (eq major-mode 'rust-ts-mode)
    (eglot-format-buffer)))

;; Optional: Enable format on save
;; (add-hook 'before-save-hook #'bv-rust-format-on-save)

;; Enhanced compilation for Rust
(defun bv-rust-setup-compilation ()
  "Setup compilation for Rust."
  (setq-local compile-command "cargo build")
  (setq-local compilation-read-command nil))

(add-hook 'rust-ts-mode-hook #'bv-rust-setup-compilation)

;; Enable cargo-mode when available
(with-eval-after-load 'rust-ts-mode
  (when (fboundp 'cargo-minor-mode)
    (add-hook 'rust-ts-mode-hook 'cargo-minor-mode)))

;; Keybindings
(defvar bv-rust-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-b") #'bv-rust-cargo-build)
    (define-key map (kbd "C-c C-c C-r") #'bv-rust-cargo-run)
    (define-key map (kbd "C-c C-c C-t") #'bv-rust-cargo-test)
    (define-key map (kbd "C-c C-c C-l") #'bv-rust-cargo-clippy)
    (define-key map (kbd "C-c C-c C-k") #'bv-rust-cargo-check)
    (define-key map (kbd "C-c C-c C-f") #'bv-rust-cargo-fmt)
    (define-key map (kbd "C-c C-c C-B") #'bv-rust-cargo-bench)
    (define-key map (kbd "C-c C-c C-d") #'bv-rust-cargo-doc)
    (define-key map (kbd "C-c C-c C-a") #'bv-rust-cargo-add)
    (define-key map (kbd "C-c C-t") #'bv-rust-cargo-test-current)
    map)
  "Keymap for Rust mode commands.")

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-f") #'bv-format-buffer)
  (define-key rust-ts-mode-map (kbd "C-c C-c") bv-rust-mode-map))

;; Cargo.toml support
(add-to-list 'auto-mode-alist '("Cargo\\.toml\\'" . conf-toml-mode))

;; Enable electric pair mode for Rust
(defun bv-rust-electric-pairs ()
  "Setup electric pairs for Rust."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?\' . ?\')))))

(add-hook 'rust-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'rust-ts-mode-hook #'bv-rust-electric-pairs)

;; Rust documentation
(defun bv-rust-open-docs ()
  "Open Rust documentation for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (browse-url (format "https://docs.rs/%s" symbol)))))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-d") #'bv-rust-open-docs))

(provide 'bv-lang-rust)
;;; bv-lang-rust.el ends here
