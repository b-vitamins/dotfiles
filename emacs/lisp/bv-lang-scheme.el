;;; bv-lang-scheme.el --- Guile Scheme development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Guile Scheme development environment with Arei (async nREPL) and Guix integration.
;; Uses Arei as the primary REPL backend with fallback to Geiser when needed.

;;; Code:

(require 'scheme)

;; External variables
(defvar scheme-indent-function)
(defvar geiser-mode-auto-p)

;; Optional package loading
(autoload 'geiser-mode "geiser-mode" nil t)
(autoload 'geiser-eval-definition "geiser-mode" nil t)
(autoload 'geiser-eval-region "geiser-mode" nil t)
(autoload 'geiser-repl "geiser-repl" nil t)
(autoload 'arei-mode "arei" nil t)
(autoload 'arei-eval-last-sexp "arei" nil t)
(autoload 'arei-eval-region "arei" nil t)
(autoload 'arei-eval-defun "arei" nil t)
(autoload 'arei-switch-to-repl "arei" nil t)
(autoload 'arei-interrupt "arei" nil t)
(autoload 'sesman-start "sesman" nil t)
(autoload 'guix-devel-mode "guix-devel" nil t)
(autoload 'guix-prettify-mode "guix-prettify" nil t)

;; File associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.guile\\'" . scheme-mode))

;; Guile-specific settings
(setq scheme-program-name "guile")

;; Disable geiser auto-mode to prevent interference with Arei
(setq geiser-mode-auto-p nil)

;; Indentation settings
(setq scheme-indent-function 'scheme-smart-indent-function)

;; REPL backend selection
(defvar bv-scheme-repl-backend 'arei
  "Scheme REPL backend to use.  Either `geiser' or `arei'.")

;; Enhanced REPL interaction (works with both Geiser and Arei)
(defun bv-scheme-send-region-or-defun ()
  "Send region if active, otherwise send current definition."
  (interactive)
  (cond
   ((and (eq bv-scheme-repl-backend 'arei) (fboundp 'arei-eval-region))
    (if (use-region-p)
        (arei-eval-region (region-beginning) (region-end))
      (arei-eval-defun)))
   ((fboundp 'geiser-eval-region)
    (if (use-region-p)
        (geiser-eval-region (region-beginning) (region-end))
      (geiser-eval-definition)))
   (t
    (message "No Scheme REPL backend available"))))

(defun bv-scheme-switch-to-repl ()
  "Switch to Scheme REPL."
  (interactive)
  (cond
   ((and (eq bv-scheme-repl-backend 'arei) (fboundp 'arei-switch-to-repl))
    (arei-switch-to-repl))
   ((fboundp 'geiser-repl)
    (geiser-repl))
   (t
    (message "No Scheme REPL backend available"))))

(defun bv-scheme-connect-repl ()
  "Connect to Scheme nREPL server (Arei/Ares)."
  (interactive)
  (if (and (eq bv-scheme-repl-backend 'arei) (fboundp 'sesman-start))
      (sesman-start)
    (message "Arei not available or not selected as backend")))

(defun bv-scheme-interrupt-evaluation ()
  "Interrupt current Scheme evaluation."
  (interactive)
  (if (and (eq bv-scheme-repl-backend 'arei) (fboundp 'arei-interrupt))
      (arei-interrupt)
    (message "Interrupt not available for current backend")))

;; Guile-specific compilation
(defun bv-scheme-compile-file ()
  "Compile current Scheme file with Guile."
  (interactive)
  (compile (format "guild compile %s" (buffer-file-name))))

(defun bv-scheme-guix-build ()
  "Build current file as a Guix package."
  (interactive)
  (compile (format "guix build -f %s" (buffer-file-name))))

(defun bv-scheme-guix-lint ()
  "Lint current file as a Guix package."
  (interactive)
  (compile (format "guix lint -f %s" (buffer-file-name))))

;; Formatting with guix style
(defun bv-scheme-guix-style ()
  "Format current buffer with guix style."
  (interactive)
  (save-buffer)
  (shell-command (format "guix style -f %s" (buffer-file-name)))
  (revert-buffer t t))

;; Smart parentheses handling
(defun bv-scheme-setup-pairs ()
  "Setup smart parentheses for Scheme."
  (setq-local show-paren-mode t)
  (setq-local electric-pair-preserve-balance t))

(add-hook 'scheme-mode-hook #'bv-scheme-setup-pairs)

;; Keybindings
(defvar bv-scheme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'bv-scheme-compile-file)
    (define-key map (kbd "C-c C-s") #'bv-scheme-send-region-or-defun)
    (define-key map (kbd "C-c C-z") #'bv-scheme-switch-to-repl)
    (define-key map (kbd "C-c C-r") #'bv-scheme-connect-repl)
    (define-key map (kbd "C-c C-b") #'bv-scheme-interrupt-evaluation)
    (define-key map (kbd "C-c g b") #'bv-scheme-guix-build)
    (define-key map (kbd "C-c g l") #'bv-scheme-guix-lint)
    (define-key map (kbd "C-c g f") #'bv-scheme-guix-style)
    map)
  "Keymap for Scheme mode commands.")

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") bv-scheme-mode-map))

;; Enable Guix development features
(defun bv-scheme-setup-guix-dev ()
  "Setup Guix development features for Scheme buffers."
  (when (and (fboundp 'guix-devel-mode)
             (or (string-match-p "/guix/" (buffer-file-name))
                 (string-match-p "\\.scm\\'" (buffer-file-name))))
    (guix-devel-mode 1)
    (when (fboundp 'guix-prettify-mode)
      (guix-prettify-mode 1))))

(add-hook 'scheme-mode-hook #'bv-scheme-setup-guix-dev)

;; Integration with bv-geiser (if loaded)
(with-eval-after-load 'bv-geiser
  ;; Geiser provides excellent Scheme support
  ;; This ensures our custom functions work well with it
  (setq geiser-mode-auto-p t))

;; Setup Arei mode for Scheme buffers
(defun bv-scheme-setup-arei ()
  "Setup Arei mode for Scheme development."
  (when (and (eq bv-scheme-repl-backend 'arei)
             (fboundp 'arei-mode))
    (arei-mode 1)))

(add-hook 'scheme-mode-hook #'bv-scheme-setup-arei)

;; Integration with Arei (if available)
(with-eval-after-load 'arei
  ;; Arei provides excellent async Scheme support with nREPL protocol
  ;; Additional configuration can be added here
  nil)

;; Guix development helpers
(defun bv-scheme-insert-guix-package ()
  "Insert a Guix package template."
  (interactive)
  (insert "(define-public package-name
  (package
    (name \"package-name\")
    (version \"0.1.0\")
    (source (origin
              (method url-fetch)
              (uri (string-append \"https://example.com/\"
                                  name \"-\" version \".tar.gz\"))
              (sha256
               (base32
                \"0000000000000000000000000000000000000000000000000000\"))))
    (build-system gnu-build-system)
    (home-page \"https://example.com\")
    (synopsis \"Short description\")
    (description \"Longer description.\")
    (license license:gpl3+)))"))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c i p") #'bv-scheme-insert-guix-package))

;; Enable prettify-symbols for Scheme
(defun bv-scheme-prettify-symbols ()
  "Setup prettify symbols for Scheme."
  (setq-local prettify-symbols-alist
              '(("lambda" . ?λ)
                ("lambda*" . ?λ)
                ("->" . ?→)
                ("=>" . ?⇒)))
  (prettify-symbols-mode 1))

(add-hook 'scheme-mode-hook #'bv-scheme-prettify-symbols)

(provide 'bv-lang-scheme)
;;; bv-lang-scheme.el ends here