;;; bv-lang-rust.el --- Rust development environment -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Rust development with eglot, rust-analyzer, and cargo integration.

;;; Code:

;;;; Dependencies
(require 'bv-core)
(require 'bv-development)

;;;; Custom Variables
(defgroup bv-rust nil
  "Rust development configuration."
  :group 'bv-languages)

(defcustom bv-rust-format-on-save t
  "Run rustfmt on save."
  :type 'boolean
  :group 'bv-rust)

(defcustom bv-rust-analyzer-check-command "clippy"
  "Command to use for cargo check."
  :type '(choice (const "check")
                 (const "clippy"))
  :group 'bv-rust)

(defcustom bv-rust-analyzer-cargo-watch t
  "Enable cargo watch for continuous checking."
  :type 'boolean
  :group 'bv-rust)

(defcustom bv-rust-analyzer-proc-macro t
  "Enable procedural macro support."
  :type 'boolean
  :group 'bv-rust)

(defcustom bv-rust-analyzer-inlay-hints t
  "Enable inlay hints for types and parameters."
  :type 'boolean
  :group 'bv-rust)

(defcustom bv-rust-analyzer-display-chaining-hints t
  "Display chaining hints."
  :type 'boolean
  :group 'bv-rust)

(defcustom bv-rust-analyzer-display-parameter-hints t
  "Display parameter hints."
  :type 'boolean
  :group 'bv-rust)

;;;; Rust Mode
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-format-on-save bv-rust-format-on-save)
  (rust-format-show-buffer nil)
  :config
  ;; Better indentation settings
  (setq rust-indent-method-chain t)
  (setq rust-indent-where-clause t)
  
  ;; Compilation settings
  (setq compilation-error-regexp-alist-alist
        (cons '(cargo
                "\\(?:error\\|\\(warning\\)\\|\\(note\\)\\)[^:]*:[[:space:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)"
                3 4 5 (1 . 2))
              compilation-error-regexp-alist-alist))
  
  (add-to-list 'compilation-error-regexp-alist 'cargo))

;;;; Cargo Integration
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map cargo-mode-map
              ("C-c C-c C-b" . cargo-process-build)
              ("C-c C-c C-r" . cargo-process-run)
              ("C-c C-c C-t" . cargo-process-test)
              ("C-c C-c C-l" . cargo-process-clippy)
              ("C-c C-c C-d" . cargo-process-doc)
              ("C-c C-c C-f" . cargo-process-fmt)
              ("C-c C-c C-c" . cargo-process-check)
              ("C-c C-c C-o" . cargo-process-doc-open))
  :custom
  (cargo-process--command-clippy "clippy --all-targets --all-features -- -D warnings")
  (cargo-process--enable-rust-backtrace t))

;;;; Rust Analyzer Configuration
(with-eval-after-load 'eglot
  ;; Add rust-analyzer to eglot
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  
  ;; Configure rust-analyzer
  (defun bv-rust-eglot-config ()
    "Configure eglot for Rust."
    (when (derived-mode-p 'rust-mode)
      (setq-local eglot-workspace-configuration
                  `(:rust-analyzer
                    (:procMacro (:enable ,bv-rust-analyzer-proc-macro)
                     :cargo (:watch (:enable ,bv-rust-analyzer-cargo-watch))
                     :checkOnSave (:command ,bv-rust-analyzer-check-command
                                   :allTargets t
                                   :allFeatures t
                                   :extraArgs ["--" "-D" "warnings"])
                     :inlayHints (:enable ,bv-rust-analyzer-inlay-hints
                                  :chainingHints ,bv-rust-analyzer-display-chaining-hints
                                  :parameterHints ,bv-rust-analyzer-display-parameter-hints
                                  :typeHints t
                                  :maxLength 25)
                     :lens (:enable t
                            :implementations t
                            :run t
                            :methodReferences t)
                     :completion (:autoimport t
                                  :autoself t
                                  :postfix t)
                     :diagnostics (:enable t
                                   :experimental (:enable t))
                     :hover (:documentation t
                             :links t)
                     :rustfmt (:enableRangeFormatting t))))))
  
  (add-hook 'eglot-managed-mode-hook 'bv-rust-eglot-config))

;;;; Cargo Edit Commands
(defun bv-rust-cargo-add (crate)
  "Add CRATE dependency using cargo-edit."
  (interactive "sCrate name: ")
  (compile (format "cargo add %s" crate)))

(defun bv-rust-cargo-rm (crate)
  "Remove CRATE dependency using cargo-edit."
  (interactive
   (list (completing-read "Remove crate: "
                         (bv-rust-get-dependencies))))
  (compile (format "cargo rm %s" crate)))

(defun bv-rust-cargo-upgrade ()
  "Upgrade dependencies using cargo-edit."
  (interactive)
  (compile "cargo upgrade"))

(defun bv-rust-cargo-update ()
  "Update Cargo.lock."
  (interactive)
  (compile "cargo update"))

(defun bv-rust-get-dependencies ()
  "Get list of dependencies from Cargo.toml."
  (let ((cargo-toml (expand-file-name "Cargo.toml"
                                      (locate-dominating-file
                                       default-directory "Cargo.toml"))))
    (when (file-exists-p cargo-toml)
      (with-temp-buffer
        (insert-file-contents cargo-toml)
        (let ((deps '()))
          (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\) =" nil t)
            (push (match-string 1) deps))
          deps)))))

;;;; Enhanced Rust Commands
(defun bv-rust-run-bin ()
  "Run a binary target."
  (interactive)
  (let ((bins (bv-rust-get-bin-targets)))
    (if bins
        (let ((bin (if (= 1 (length bins))
                       (car bins)
                     (completing-read "Binary: " bins))))
          (compile (format "cargo run --bin %s" bin)))
      (compile "cargo run"))))

(defun bv-rust-run-example ()
  "Run an example."
  (interactive)
  (let ((examples (bv-rust-get-examples)))
    (when examples
      (let ((example (completing-read "Example: " examples)))
        (compile (format "cargo run --example %s" example))))))

(defun bv-rust-test-current ()
  "Run test at point."
  (interactive)
  (let ((test-name (bv-rust-get-current-test)))
    (if test-name
        (compile (format "cargo test %s" test-name))
      (message "No test found at point"))))

(defun bv-rust-get-current-test ()
  "Get the name of the test at point."
  (save-excursion
    (when (re-search-backward "^[[:space:]]*#[\\[]test\\]" nil t)
      (forward-line 1)
      (when (re-search-forward "fn[[:space:]]+\\([a-zA-Z0-9_]+\\)" nil t)
        (match-string 1)))))

(defun bv-rust-get-bin-targets ()
  "Get list of binary targets from Cargo.toml."
  (let ((cargo-toml (expand-file-name "Cargo.toml"
                                      (locate-dominating-file
                                       default-directory "Cargo.toml"))))
    (when (file-exists-p cargo-toml)
      (with-temp-buffer
        (insert-file-contents cargo-toml)
        (let ((bins '()))
          (while (re-search-forward "^\\[\\[bin\\]\\]" nil t)
            (when (re-search-forward "name[[:space:]]*=[[:space:]]*\"\\([^\"]+\\)\"" nil t)
              (push (match-string 1) bins)))
          bins)))))

(defun bv-rust-get-examples ()
  "Get list of examples from examples directory."
  (let ((examples-dir (expand-file-name "examples"
                                        (locate-dominating-file
                                         default-directory "Cargo.toml"))))
    (when (file-directory-p examples-dir)
      (mapcar (lambda (f)
                (file-name-sans-extension f))
              (directory-files examples-dir nil "\\.rs$"))))

;;;; Rust Playground
(defun bv-rust-playground ()
  "Create a new Rust playground."
  (interactive)
  (let* ((name (format "rust-playground-%s"
                       (format-time-string "%Y%m%d-%H%M%S")))
         (dir (expand-file-name name temporary-file-directory)))
    (make-directory dir)
    (let ((default-directory dir))
      (shell-command "cargo init --name playground")
      (find-file (expand-file-name "src/main.rs" dir))
      (setq-local compile-command "cargo run"))))

;;;; Macro Expansion
(defun bv-rust-expand-macro ()
  "Expand Rust macro at point."
  (interactive)
  (when (eglot-current-server)
    (eglot-code-actions (point) (point) "rust-analyzer.expandMacro" t)))

;;;; Documentation
(defun bv-rust-doc-std ()
  "Open Rust standard library documentation."
  (interactive)
  (browse-url "https://doc.rust-lang.org/std/"))

(defun bv-rust-doc-search (query)
  "Search Rust documentation for QUERY."
  (interactive "sSearch docs for: ")
  (browse-url (format "https://docs.rs/releases/search?query=%s" query)))

;;;; Debugging Support
(bv-with-feature dape
  (with-eval-after-load 'dape
    ;; Add rust-lldb configuration
    (add-to-list 'dape-configs
                 '(rust-lldb
                   modes (rust-mode)
                   command "lldb-vscode"
                   :type "lldb"
                   :request "launch"
                   :program (lambda ()
                              (let ((bin (bv-rust-get-debug-binary)))
                                (read-file-name "Binary: " nil nil t bin)))
                   :cwd dape-cwd-fn
                   :stopOnEntry nil))
    
    ;; Add rust-gdb configuration
    (add-to-list 'dape-configs
                 '(rust-gdb
                   modes (rust-mode)
                   command "gdb"
                   :type "gdb"
                   :request "launch"
                   :target (lambda ()
                            (let ((bin (bv-rust-get-debug-binary)))
                              (read-file-name "Binary: " nil nil t bin)))
                   :cwd dape-cwd-fn))))

(defun bv-rust-get-debug-binary ()
  "Get path to debug binary."
  (let* ((root (locate-dominating-file default-directory "Cargo.toml"))
         (target-dir (expand-file-name "target/debug" root)))
    (when (file-directory-p target-dir)
      (car (directory-files target-dir t "^[^.].*[^.]$" t))))))

;;;; Cargo.toml Support
(use-package toml-mode
  :mode ("Cargo\\.lock\\'" "\\.cargo/config\\'" "\\.cargo/credentials\\'")
  )

;;;; Snippets Support
(bv-with-feature tempel
  (with-eval-after-load 'tempel
    (defvar bv-rust-tempel-templates
      '(rust-mode
        (test "#[test]\nfn " (p "test_name") "() {\n    " r> "\n}")
        (bench "#[bench]\nfn " (p "bench_name") "(b: &mut Bencher) {\n    " r> "\n}")
        (derive "#[derive(" (p "Debug") ")]\n")
        (println "println!(\"" (p "{}") "\", " (p "value") ");")
        (eprintln "eprintln!(\"" (p "{}") "\", " (p "value") ");")
        (todo "todo!(\"" (p "implement this") "\")")
        (unimplemented "unimplemented!(\"" (p "not implemented") "\")")
        (result "Result<" (p "T") ", " (p "E") ">")
        (option "Option<" (p "T") ">")
        (match "match " (p "expr") " {\n    " r> "\n}")
        (impl "impl " (p "Type") " {\n    " r> "\n}")
        (struct "struct " (p "Name") " {\n    " r> "\n}")
        (enum "enum " (p "Name") " {\n    " r> "\n}")
        (trait "trait " (p "Name") " {\n    " r> "\n}"))
      "Rust templates for Tempel.")
    
    (add-to-list 'tempel-template-sources 'bv-rust-tempel-templates)))

;;;; Keybindings
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-d") nil) ; Remove default
  (define-key rust-mode-map (kbd "C-c d s") 'bv-rust-doc-std)
  (define-key rust-mode-map (kbd "C-c d d") 'bv-rust-doc-search)
  (define-key rust-mode-map (kbd "C-c r r") 'bv-rust-run-bin)
  (define-key rust-mode-map (kbd "C-c r e") 'bv-rust-run-example)
  (define-key rust-mode-map (kbd "C-c r t") 'bv-rust-test-current)
  (define-key rust-mode-map (kbd "C-c r p") 'bv-rust-playground)
  (define-key rust-mode-map (kbd "C-c r m") 'bv-rust-expand-macro)
  (define-key rust-mode-map (kbd "C-c c a") 'bv-rust-cargo-add)
  (define-key rust-mode-map (kbd "C-c c r") 'bv-rust-cargo-rm)
  (define-key rust-mode-map (kbd "C-c c u") 'bv-rust-cargo-upgrade)
  (define-key rust-mode-map (kbd "C-c c U") 'bv-rust-cargo-update))

;;;; Feature Definition
(defun bv-rust-load ()
  "Load Rust configuration."
  (add-to-list 'bv-enabled-features 'rust)
  
  ;; Check for required tools
  (dolist (tool '("rustc" "cargo" "rust-analyzer"))
    (unless (executable-find tool)
      (message "Warning: %s not found in PATH" tool)))
  
  ;; Check for optional tools
  (dolist (tool '("cargo-edit" "rustfmt" "clippy"))
    (unless (or (executable-find tool)
                (executable-find (concat "cargo-" tool)))
      (message "Optional tool %s not found. Some features may be unavailable." tool)))
  
  ;; Add rust to org-babel
  (bv-with-feature org
    (with-eval-after-load 'org
      (add-to-list 'org-babel-load-languages '(rust . t)))))

(provide 'bv-lang-rust)
;;; bv-lang-rust.el ends here
