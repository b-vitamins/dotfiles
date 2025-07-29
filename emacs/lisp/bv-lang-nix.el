;;; bv-lang-nix.el --- Nix development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Nix development environment with tree-sitter, LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar nix-indent-function)
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar nix-mode-map)
(defvar nix-ts-mode-map)

;; External functions
(declare-function nix-repl "nix-mode" ())
(declare-function nix-repl-send-region "nix-mode" (start end))

;; Optional package loading
(autoload 'nix-mode "nix-mode" nil t)
(autoload 'nix-drv-mode "nix-mode" nil t)
(autoload 'nix-shell-mode "nix-mode" nil t)
(autoload 'nix-repl "nix-mode" nil t)

;; Enable tree-sitter for Nix when available
(when (treesit-language-available-p 'nix)
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.drv\\'" . nix-drv-mode))

;; Nix settings
(setq nix-indent-function 'nix-indent-line)

;; Configure nil (Nix LSP)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((nix-mode nix-ts-mode) . ("nil"))))

;; Nix-specific eglot settings
(defun bv-nix-eglot-config ()
  "Configure eglot for Nix development."
  (setq-local eglot-workspace-configuration
              '(:nil (:formatting (:command ["nixpkgs-fmt"])))))

(add-hook 'nix-mode-hook #'bv-nix-eglot-config)
(when (fboundp 'nix-ts-mode)
  (add-hook 'nix-ts-mode-hook #'bv-nix-eglot-config))

;; REPL interaction
(defun bv-nix-repl ()
  "Start Nix REPL."
  (interactive)
  (nix-repl))

(defun bv-nix-repl-send-region ()
  "Send region to Nix REPL."
  (interactive)
  (nix-repl-send-region (region-beginning) (region-end)))

;; Building and evaluation
(defun bv-nix-build ()
  "Build current Nix expression."
  (interactive)
  (compile (format "nix-build %s" (buffer-file-name))))

(defun bv-nix-build-attribute (attr)
  "Build specific attribute."
  (interactive "sAttribute: ")
  (compile (format "nix-build %s -A %s" (buffer-file-name) attr)))

(defun bv-nix-instantiate ()
  "Instantiate current Nix expression."
  (interactive)
  (compile (format "nix-instantiate %s" (buffer-file-name))))

(defun bv-nix-eval ()
  "Evaluate current Nix file."
  (interactive)
  (compile (format "nix eval -f %s" (buffer-file-name))))

(defun bv-nix-eval-expression (expr)
  "Evaluate Nix expression."
  (interactive "sExpression: ")
  (compile (format "nix eval --expr '%s'" expr)))

;; Shell commands
(defun bv-nix-shell ()
  "Enter nix-shell for current expression."
  (interactive)
  (let ((default-directory (file-name-directory (buffer-file-name))))
    (compile "nix-shell")))

(defun bv-nix-shell-run (command)
  "Run COMMAND in nix-shell."
  (interactive "sCommand: ")
  (compile (format "nix-shell --run '%s'" command)))

(defun bv-nix-develop ()
  "Enter nix develop shell."
  (interactive)
  (compile "nix develop"))

;; Flakes support
(defun bv-nix-flake-check ()
  "Check Nix flake."
  (interactive)
  (compile "nix flake check"))

(defun bv-nix-flake-update ()
  "Update Nix flake."
  (interactive)
  (compile "nix flake update"))

(defun bv-nix-flake-show ()
  "Show Nix flake outputs."
  (interactive)
  (compile "nix flake show"))

(defun bv-nix-flake-init ()
  "Initialize Nix flake."
  (interactive)
  (compile "nix flake init"))

;; Formatting
(defun bv-nix-format-buffer ()
  "Format current Nix buffer."
  (interactive)
  (cond
   ((executable-find "nixpkgs-fmt")
    (shell-command-on-region (point-min) (point-max)
                            "nixpkgs-fmt" nil t))
   ((executable-find "alejandra")
    (shell-command-on-region (point-min) (point-max)
                            "alejandra -" nil t))
   ((executable-find "nixfmt")
    (shell-command-on-region (point-min) (point-max)
                            "nixfmt" nil t))
   (t
    (eglot-format-buffer))))

;; Linting
(defun bv-nix-lint ()
  "Lint current Nix file."
  (interactive)
  (if (executable-find "statix")
      (compile (format "statix check %s" (buffer-file-name)))
    (message "statix not found")))

(defun bv-nix-deadnix ()
  "Check for dead code with deadnix."
  (interactive)
  (if (executable-find "deadnix")
      (compile (format "deadnix %s" (buffer-file-name)))
    (message "deadnix not found")))

;; Documentation
(defun bv-nix-doc-search (query)
  "Search Nix documentation."
  (interactive "sSearch query: ")
  (browse-url (format "https://search.nixos.org/packages?query=%s" query)))

(defun bv-nix-option-search (query)
  "Search NixOS options."
  (interactive "sOption: ")
  (browse-url (format "https://search.nixos.org/options?query=%s" query)))

;; Package management
(defun bv-nix-search (package)
  "Search for Nix package."
  (interactive "sPackage: ")
  (compile (format "nix search nixpkgs %s" package)))

(defun bv-nix-info (package)
  "Show info about Nix package."
  (interactive "sPackage: ")
  (compile (format "nix-env -qa --description '*%s*'" package)))

;; NixOS specific
(defun bv-nixos-rebuild (command)
  "Run nixos-rebuild command."
  (interactive (list (completing-read "Command: "
                                     '("switch" "boot" "test" "build" "dry-build"))))
  (compile (format "sudo nixos-rebuild %s" command)))

(defun bv-nixos-option (option)
  "Query NixOS option."
  (interactive "sOption: ")
  (compile (format "nixos-option %s" option)))

;; Home Manager
(defun bv-home-manager-switch ()
  "Switch Home Manager configuration."
  (interactive)
  (compile "home-manager switch"))

(defun bv-home-manager-build ()
  "Build Home Manager configuration."
  (interactive)
  (compile "home-manager build"))

;; Electric pairs
(defun bv-nix-electric-pairs ()
  "Setup electric pairs for Nix."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?{ . ?})
                        (?\[ . ?\])))))

(add-hook 'nix-mode-hook #'electric-pair-local-mode)
(add-hook 'nix-mode-hook #'bv-nix-electric-pairs)
(when (fboundp 'nix-ts-mode)
  (add-hook 'nix-ts-mode-hook #'electric-pair-local-mode)
  (add-hook 'nix-ts-mode-hook #'bv-nix-electric-pairs))

;; Compilation setup
(defun bv-nix-setup-compilation ()
  "Setup compilation for Nix."
  (setq-local compile-command
              (cond
               ((file-exists-p "flake.nix") "nix build")
               ((file-exists-p "default.nix") "nix-build")
               ((file-exists-p "shell.nix") "nix-shell")
               (t (format "nix-build %s" (buffer-file-name))))))

(add-hook 'nix-mode-hook #'bv-nix-setup-compilation)
(when (fboundp 'nix-ts-mode)
  (add-hook 'nix-ts-mode-hook #'bv-nix-setup-compilation))

;; Pretty symbols
(defun bv-nix-pretty-symbols ()
  "Enable pretty symbols for Nix."
  (setq prettify-symbols-alist
        '(("->" . ?→)
          ("=>" . ?⇒)))
  (prettify-symbols-mode 1))

(add-hook 'nix-mode-hook #'bv-nix-pretty-symbols)
(when (fboundp 'nix-ts-mode)
  (add-hook 'nix-ts-mode-hook #'bv-nix-pretty-symbols))

;; Keybindings
(defvar bv-nix-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-nix-repl)
    (define-key map (kbd "C-c C-r") #'bv-nix-repl-send-region)
    ;; Building
    (define-key map (kbd "C-c C-b") #'bv-nix-build)
    (define-key map (kbd "C-c C-a") #'bv-nix-build-attribute)
    (define-key map (kbd "C-c C-i") #'bv-nix-instantiate)
    (define-key map (kbd "C-c C-e") #'bv-nix-eval)
    (define-key map (kbd "C-c C-E") #'bv-nix-eval-expression)
    ;; Shell
    (define-key map (kbd "C-c C-s") #'bv-nix-shell)
    (define-key map (kbd "C-c C-S") #'bv-nix-shell-run)
    (define-key map (kbd "C-c C-D") #'bv-nix-develop)
    ;; Flakes
    (define-key map (kbd "C-c f c") #'bv-nix-flake-check)
    (define-key map (kbd "C-c f u") #'bv-nix-flake-update)
    (define-key map (kbd "C-c f s") #'bv-nix-flake-show)
    (define-key map (kbd "C-c f i") #'bv-nix-flake-init)
    ;; Formatting/Linting
    (define-key map (kbd "C-c C-f") #'bv-nix-format-buffer)
    (define-key map (kbd "C-c C-l") #'bv-nix-lint)
    (define-key map (kbd "C-c C-L") #'bv-nix-deadnix)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-nix-doc-search)
    (define-key map (kbd "C-c C-o") #'bv-nix-option-search)
    ;; Package management
    (define-key map (kbd "C-c p s") #'bv-nix-search)
    (define-key map (kbd "C-c p i") #'bv-nix-info)
    ;; NixOS
    (define-key map (kbd "C-c n r") #'bv-nixos-rebuild)
    (define-key map (kbd "C-c n o") #'bv-nixos-option)
    ;; Home Manager
    (define-key map (kbd "C-c h s") #'bv-home-manager-switch)
    (define-key map (kbd "C-c h b") #'bv-home-manager-build)
    map)
  "Keymap for Nix mode commands.")

(with-eval-after-load 'nix-mode
  (define-key nix-mode-map (kbd "C-c n") bv-nix-mode-map))

(when (fboundp 'nix-ts-mode)
  (with-eval-after-load 'nix-ts-mode
    (define-key nix-ts-mode-map (kbd "C-c n") bv-nix-mode-map)))

(provide 'bv-lang-nix)
;;; bv-lang-nix.el ends here