;;; bv-lang-go.el --- Go development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Go development environment with tree-sitter, gopls LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar gofmt-command)
(defvar electric-pair-pairs)
(defvar go-ts-mode-map)

;; Optional package loading
(autoload 'go-mode "go-mode" nil t)

;; Remap to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; File associations
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.sum\\'" . go-mod-ts-mode))

;; Go settings
(setq gofmt-command "gofmt")

;; Configure gopls as the LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls"))))

;; Go-specific eglot settings
(defun bv-go-eglot-config ()
  "Configure eglot for Go development."
  (setq-local eglot-workspace-configuration
              '(:gopls (:usePlaceholders t
                       :staticcheck t
                       :completeUnimported t
                       :matcher "fuzzy"
                       :gofumpt t
                       :analyses (:unusedparams t
                                 :nilness t
                                 :shadow t)))))

(add-hook 'go-ts-mode-hook #'bv-go-eglot-config)

;; Format on save
(defun bv-go-format-buffer ()
  "Format current Go buffer."
  (interactive)
  (eglot-format-buffer))

(defun bv-go-format-on-save ()
  "Format buffer with gofmt before saving."
  (when (eq major-mode 'go-ts-mode)
    (bv-go-format-buffer)))

;; Enable format on save by default for Go
(add-hook 'before-save-hook #'bv-go-format-on-save)

;; Go commands
(defun bv-go-run ()
  "Run current Go file."
  (interactive)
  (compile (format "go run %s" (buffer-file-name))))

(defun bv-go-build ()
  "Build current Go package."
  (interactive)
  (compile "go build -v"))

(defun bv-go-test ()
  "Run Go tests in current package."
  (interactive)
  (compile "go test -v"))

(defun bv-go-test-current ()
  "Run Go test at point."
  (interactive)
  (let ((test-name (save-excursion
                    (re-search-backward "^func \\(Test[^ ]*\\)" nil t)
                    (match-string 1))))
    (if test-name
        (compile (format "go test -v -run %s" test-name))
      (message "No test function found"))))

(defun bv-go-test-coverage ()
  "Run Go tests with coverage."
  (interactive)
  (compile "go test -v -cover"))

(defun bv-go-mod-tidy ()
  "Run go mod tidy."
  (interactive)
  (compile "go mod tidy"))

(defun bv-go-mod-download ()
  "Run go mod download."
  (interactive)
  (compile "go mod download"))

(defun bv-go-get (package)
  "Run go get for PACKAGE."
  (interactive "sPackage: ")
  (compile (format "go get %s" package)))

(defun bv-go-install ()
  "Install current package."
  (interactive)
  (compile "go install"))

;; Go imports management
(defun bv-go-imports ()
  "Run goimports on current buffer."
  (interactive)
  (let ((goimports (executable-find "goimports")))
    (if goimports
        (shell-command-on-region (point-min) (point-max)
                                goimports nil t)
      (message "goimports not found"))))

;; Go documentation
(defun bv-go-doc-at-point ()
  "Show Go documentation for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (compile (format "go doc %s" symbol)))))

;; Go playground
(defun bv-go-playground ()
  "Create a Go playground buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*go-playground*")))
    (switch-to-buffer buf)
    (go-ts-mode)
    (insert "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, playground\")\n}\n")))

;; Error checking
(defun bv-go-vet ()
  "Run go vet."
  (interactive)
  (compile "go vet"))

(defun bv-go-lint ()
  "Run golint if available."
  (interactive)
  (if (executable-find "golint")
      (compile "golint")
    (message "golint not found")))

;; Benchmarking
(defun bv-go-bench ()
  "Run Go benchmarks."
  (interactive)
  (compile "go test -bench=."))

(defun bv-go-bench-current ()
  "Run Go benchmark at point."
  (interactive)
  (let ((bench-name (save-excursion
                     (re-search-backward "^func \\(Benchmark[^ ]*\\)" nil t)
                     (match-string 1))))
    (if bench-name
        (compile (format "go test -bench=%s" bench-name))
      (message "No benchmark function found"))))

;; Compilation setup
(defun bv-go-setup-compilation ()
  "Setup compilation for Go."
  (setq-local compile-command "go build -v")
  (setq-local compilation-read-command nil))

(add-hook 'go-ts-mode-hook #'bv-go-setup-compilation)

;; Electric pairs
(defun bv-go-electric-pairs ()
  "Setup electric pairs for Go."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?` . ?`)))))

(add-hook 'go-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'go-ts-mode-hook #'bv-go-electric-pairs)

;; Keybindings
(defvar bv-go-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Build/Run
    (define-key map (kbd "C-c C-r") #'bv-go-run)
    (define-key map (kbd "C-c C-b") #'bv-go-build)
    (define-key map (kbd "C-c C-i") #'bv-go-install)
    ;; Testing
    (define-key map (kbd "C-c C-t") #'bv-go-test)
    (define-key map (kbd "C-c t t") #'bv-go-test-current)
    (define-key map (kbd "C-c t c") #'bv-go-test-coverage)
    (define-key map (kbd "C-c t b") #'bv-go-bench)
    (define-key map (kbd "C-c t B") #'bv-go-bench-current)
    ;; Modules
    (define-key map (kbd "C-c m t") #'bv-go-mod-tidy)
    (define-key map (kbd "C-c m d") #'bv-go-mod-download)
    (define-key map (kbd "C-c m g") #'bv-go-get)
    ;; Tools
    (define-key map (kbd "C-c C-f") #'bv-go-format-buffer)
    (define-key map (kbd "C-c C-I") #'bv-go-imports)
    (define-key map (kbd "C-c C-v") #'bv-go-vet)
    (define-key map (kbd "C-c C-l") #'bv-go-lint)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-go-doc-at-point)
    (define-key map (kbd "C-c C-p") #'bv-go-playground)
    map)
  "Keymap for Go mode commands.")

(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-c C-c") bv-go-mode-map))

(provide 'bv-lang-go)
;;; bv-lang-go.el ends here