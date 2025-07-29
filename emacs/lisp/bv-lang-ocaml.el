;;; bv-lang-ocaml.el --- OCaml development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; OCaml development environment with tree-sitter, LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar tuareg-indent-align-with-first-arg)
(defvar tuareg-match-patterns-aligned)
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar tuareg-mode-map)
(defvar ocaml-ts-mode-map)

;; External functions
(declare-function utop-eval-phrase "utop" ())
(declare-function utop-eval-region "utop" (start end))
(declare-function utop-eval-buffer "utop" ())
(declare-function merlin-type-enclosing "merlin" ())
(declare-function merlin-locate "merlin" ())
(declare-function merlin-error-next "merlin" ())
(declare-function merlin-error-prev "merlin" ())
(declare-function ocamlformat "ocamlformat" ())

;; Optional package loading
(autoload 'tuareg-mode "tuareg" nil t)
(autoload 'merlin-mode "merlin" nil t)
(autoload 'utop "utop" nil t)
(autoload 'utop-minor-mode "utop" nil t)
(autoload 'dune-mode "dune" nil t)

;; Enable tree-sitter for OCaml when available
(when (treesit-language-available-p 'ocaml)
  (add-to-list 'major-mode-remap-alist '(tuareg-mode . ocaml-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.ocamlinit\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("dune\\(?:\\.inc\\)?\\'" . dune-mode))
(add-to-list 'auto-mode-alist '("dune-project\\'" . dune-mode))

;; OCaml settings
(setq tuareg-indent-align-with-first-arg t)
(setq tuareg-match-patterns-aligned t)

;; Configure ocaml-lsp-server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((tuareg-mode ocaml-ts-mode reason-mode) . ("ocamllsp"))))

;; OCaml-specific eglot settings
(defun bv-ocaml-eglot-config ()
  "Configure eglot for OCaml development."
  (setq-local eglot-workspace-configuration
              '(:ocaml (:lens (:enable t))
                      :format (:enable t))))

(add-hook 'tuareg-mode-hook #'bv-ocaml-eglot-config)
(when (fboundp 'ocaml-ts-mode)
  (add-hook 'ocaml-ts-mode-hook #'bv-ocaml-eglot-config))

;; Enable merlin if available
(defun bv-ocaml-maybe-enable-merlin ()
  "Enable merlin-mode if available."
  (when (fboundp 'merlin-mode)
    (merlin-mode 1)))

(add-hook 'tuareg-mode-hook #'bv-ocaml-maybe-enable-merlin)
(add-hook 'reason-mode-hook #'bv-ocaml-maybe-enable-merlin)

;; UTop integration
(add-hook 'tuareg-mode-hook #'utop-minor-mode)
(when (fboundp 'ocaml-ts-mode)
  (add-hook 'ocaml-ts-mode-hook #'utop-minor-mode))

;; REPL interaction
(defun bv-ocaml-utop ()
  "Start UTop REPL."
  (interactive)
  (utop))

(defun bv-ocaml-eval-phrase ()
  "Evaluate phrase at point."
  (interactive)
  (utop-eval-phrase))

(defun bv-ocaml-eval-region ()
  "Evaluate selected region."
  (interactive)
  (utop-eval-region (region-beginning) (region-end)))

(defun bv-ocaml-eval-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (utop-eval-buffer))

;; Build commands
(defun bv-ocaml-dune-build ()
  "Build with dune."
  (interactive)
  (compile "dune build"))

(defun bv-ocaml-dune-build-all ()
  "Build all with dune."
  (interactive)
  (compile "dune build @all"))

(defun bv-ocaml-dune-test ()
  "Run tests with dune."
  (interactive)
  (compile "dune test"))

(defun bv-ocaml-dune-clean ()
  "Clean with dune."
  (interactive)
  (compile "dune clean"))

(defun bv-ocaml-dune-exec (target)
  "Execute TARGET with dune."
  (interactive "sTarget: ")
  (compile (format "dune exec %s" target)))

(defun bv-ocaml-dune-install ()
  "Install with dune."
  (interactive)
  (compile "dune install"))

;; OPAM commands
(defun bv-ocaml-opam-install (package)
  "Install PACKAGE with opam."
  (interactive "sPackage: ")
  (compile (format "opam install %s" package)))

(defun bv-ocaml-opam-switch (switch)
  "Switch to OPAM switch."
  (interactive "sSwitch: ")
  (shell-command (format "opam switch %s" switch))
  (setenv "OPAM_SWITCH_PREFIX"
          (shell-command-to-string "opam var prefix | tr -d '\n'"))
  (message "Switched to %s" switch))

;; Documentation
(defun bv-ocaml-search-documentation ()
  "Search OCaml documentation."
  (interactive)
  (browse-url "https://ocaml.org/api/"))

(defun bv-ocaml-type-at-point ()
  "Show type at point."
  (interactive)
  (if (fboundp 'merlin-type-enclosing)
      (merlin-type-enclosing)
    (message "Merlin not available")))

(defun bv-ocaml-locate ()
  "Jump to definition."
  (interactive)
  (if (fboundp 'merlin-locate)
      (merlin-locate)
    (xref-find-definitions (thing-at-point 'symbol))))

;; Formatting
(defun bv-ocaml-format-buffer ()
  "Format current OCaml buffer."
  (interactive)
  (cond
   ((executable-find "ocamlformat")
    (shell-command-on-region (point-min) (point-max)
                            "ocamlformat -" nil t))
   ((fboundp 'ocamlformat)
    (ocamlformat))
   (t
    (eglot-format-buffer))))

(defun bv-ocaml-format-on-save ()
  "Format buffer before saving."
  (when (or (eq major-mode 'tuareg-mode)
            (eq major-mode 'ocaml-ts-mode))
    (bv-ocaml-format-buffer)))

;; Optional: Enable format on save
;; (add-hook 'before-save-hook #'bv-ocaml-format-on-save)

;; Reason support
(defun bv-ocaml-refmt-buffer ()
  "Format Reason buffer."
  (interactive)
  (when (executable-find "refmt")
    (shell-command-on-region (point-min) (point-max)
                            "refmt" nil t)))

;; Error navigation
(defun bv-ocaml-next-error ()
  "Go to next error."
  (interactive)
  (if (fboundp 'merlin-error-next)
      (merlin-error-next)
    (flymake-goto-next-error)))

(defun bv-ocaml-previous-error ()
  "Go to previous error."
  (interactive)
  (if (fboundp 'merlin-error-prev)
      (merlin-error-prev)
    (flymake-goto-prev-error)))

;; Compilation setup
(defun bv-ocaml-setup-compilation ()
  "Setup compilation for OCaml."
  (setq-local compile-command
              (cond
               ((file-exists-p "dune-project") "dune build")
               ((file-exists-p "Makefile") "make")
               (t "ocamlc"))))

(add-hook 'tuareg-mode-hook #'bv-ocaml-setup-compilation)
(when (fboundp 'ocaml-ts-mode)
  (add-hook 'ocaml-ts-mode-hook #'bv-ocaml-setup-compilation))

;; Electric pairs
(defun bv-ocaml-electric-pairs ()
  "Setup electric pairs for OCaml."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?\' . ?\')))))

(add-hook 'tuareg-mode-hook #'electric-pair-local-mode)
(add-hook 'tuareg-mode-hook #'bv-ocaml-electric-pairs)
(when (fboundp 'ocaml-ts-mode)
  (add-hook 'ocaml-ts-mode-hook #'electric-pair-local-mode)
  (add-hook 'ocaml-ts-mode-hook #'bv-ocaml-electric-pairs))

;; Pretty symbols
(defun bv-ocaml-pretty-symbols ()
  "Enable pretty symbols for OCaml."
  (setq prettify-symbols-alist
        '(("fun" . ?λ)
          ("->" . ?→)
          ("=>" . ?⇒)
          ("::" . ?∷)
          (":=" . ?≔)
          ("<>" . ?≠)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("&&" . ?∧)
          ("||" . ?∨)))
  (prettify-symbols-mode 1))

(add-hook 'tuareg-mode-hook #'bv-ocaml-pretty-symbols)
(when (fboundp 'ocaml-ts-mode)
  (add-hook 'ocaml-ts-mode-hook #'bv-ocaml-pretty-symbols))

;; Keybindings
(defvar bv-ocaml-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-ocaml-utop)
    ;; Evaluation
    (define-key map (kbd "C-c C-e") #'bv-ocaml-eval-phrase)
    (define-key map (kbd "C-c C-r") #'bv-ocaml-eval-region)
    (define-key map (kbd "C-c C-b") #'bv-ocaml-eval-buffer)
    ;; Build
    (define-key map (kbd "C-c b b") #'bv-ocaml-dune-build)
    (define-key map (kbd "C-c b a") #'bv-ocaml-dune-build-all)
    (define-key map (kbd "C-c b t") #'bv-ocaml-dune-test)
    (define-key map (kbd "C-c b c") #'bv-ocaml-dune-clean)
    (define-key map (kbd "C-c b e") #'bv-ocaml-dune-exec)
    (define-key map (kbd "C-c b i") #'bv-ocaml-dune-install)
    ;; OPAM
    (define-key map (kbd "C-c o i") #'bv-ocaml-opam-install)
    (define-key map (kbd "C-c o s") #'bv-ocaml-opam-switch)
    ;; Navigation
    (define-key map (kbd "M-.") #'bv-ocaml-locate)
    (define-key map (kbd "C-c n") #'bv-ocaml-next-error)
    (define-key map (kbd "C-c p") #'bv-ocaml-previous-error)
    ;; Type info
    (define-key map (kbd "C-c C-t") #'bv-ocaml-type-at-point)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-ocaml-format-buffer)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-ocaml-search-documentation)
    map)
  "Keymap for OCaml mode commands.")

(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map (kbd "C-c C-c") bv-ocaml-mode-map))

(when (fboundp 'ocaml-ts-mode)
  (with-eval-after-load 'ocaml-ts-mode
    (define-key ocaml-ts-mode-map (kbd "C-c C-c") bv-ocaml-mode-map)))

(provide 'bv-lang-ocaml)
;;; bv-lang-ocaml.el ends here