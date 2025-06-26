;;; bv-lang-lisp.el --- Lisp languages support -*- lexical-binding: t -*-

;;; Commentary:
;; Support for Lisp dialects: Emacs Lisp, Scheme/Guile, and Common Lisp.

;;; Code:

;;;; Dependencies
(require 'bv-core)
(require 'bv-development)

;;;; Custom Variables
(defgroup bv-lisp nil
  "Lisp languages configuration."
  :group 'bv-languages)

(defcustom bv-lisp-structural-editing-mode 'smartparens
  "Structural editing mode to use for Lisp."
  :type '(choice (const :tag "Smartparens" smartparens)
                 (const :tag "Paredit" paredit)
                 (const :tag "Lispy" lispy)
                 (const :tag "None" nil))
  :group 'bv-lisp)

(defcustom bv-lisp-repl-pop-to-buffer t
  "Pop to REPL buffer after evaluation."
  :type 'boolean
  :group 'bv-lisp)

(defcustom bv-lisp-pretty-print-results t
  "Pretty print evaluation results."
  :type 'boolean
  :group 'bv-lisp)

(defcustom bv-lisp-common-lisp-implementation 'sbcl
  "Common Lisp implementation to use."
  :type '(choice (const :tag "SBCL" sbcl)
                 (const :tag "CCL" ccl)
                 (const :tag "ECL" ecl)
                 (const :tag "CLISP" clisp))
  :group 'bv-lisp)

;;;; Common Lisp Functions
(defun bv-lisp-indent-setup ()
  "Common indentation setup for all Lisps."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2))

(defun bv-lisp-enable-structural-editing ()
  "Enable configured structural editing mode."
  (pcase bv-lisp-structural-editing-mode
    ('smartparens (smartparens-strict-mode 1))
    ('paredit (enable-paredit-mode))
    ('lispy (lispy-mode 1))))

;;;; Emacs Lisp
(use-package elisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook ((emacs-lisp-mode . bv-lisp-indent-setup)
         (emacs-lisp-mode . bv-lisp-enable-structural-editing)
         (emacs-lisp-mode . flymake-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-x C-e" . pp-eval-last-sexp)
              ("M-:" . pp-eval-expression)
              ("C-c C-m" . pp-macroexpand-last-sexp)
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . bv-elisp-eval-defun)
              ("C-c C-r" . eval-region)
              ("C-c C-l" . load-file)
              ("C-c C-z" . ielm)
              ("C-c C-d" . describe-function-at-point)
              ("C-c C-v" . describe-variable-at-point))
  :config
  ;; Enhanced eval-defun
  (defun bv-elisp-eval-defun ()
    "Evaluate defun at point and pretty-print result."
    (interactive)
    (let ((result (eval-defun nil)))
      (when bv-lisp-pretty-print-results
        (pp result))
      result))
  
  ;; Better help integration
  (defun describe-function-at-point ()
    "Describe function at point."
    (interactive)
    (let ((fn (function-called-at-point)))
      (if fn
          (describe-function fn)
        (call-interactively 'describe-function))))
  
  (defun describe-variable-at-point ()
    "Describe variable at point."
    (interactive)
    (let ((var (variable-at-point)))
      (if (and var (not (equal var 0)))
          (describe-variable var)
        (call-interactively 'describe-variable)))))

;;;; IELM (Emacs Lisp REPL)
(use-package ielm
  :ensure nil
  :defer t
  :config
  (setq ielm-header "")
  (setq ielm-prompt "λ> ")
  (setq ielm-dynamic-return nil)
  (setq ielm-dynamic-multiline-inputs t)
  
  ;; Enhanced IELM
  (defun bv-ielm-return ()
    "Custom return for IELM with paredit compatibility."
    (interactive)
    (if (and (bound-and-true-p paredit-mode)
             (not (paredit--region-ok-p (ielm-pm) (point))))
        (paredit-newline)
      (ielm-return)))
  
  (define-key ielm-map (kbd "RET") 'bv-ielm-return)
  
  ;; Clear IELM buffer
  (defun bv-ielm-clear ()
    "Clear IELM buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ielm-return)))
  
  (define-key ielm-map (kbd "C-c M-o") 'bv-ielm-clear))

;;;; Scheme/Guile Support
(use-package geiser
  :defer t
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-repl-query-on-kill-p nil)
  (geiser-repl-query-on-exit-p nil)
  :config
  (setq geiser-repl-history-filename
        (expand-file-name "geiser-history" bv-cache-directory))
  (setq geiser-repl-add-project-paths nil))

(use-package geiser-guile
  :after geiser
  :config
  (setq geiser-guile-binary (executable-find "guile")))

;; Enhanced Geiser features
(use-package geiser-eros
  :hook (geiser-mode . geiser-eros-mode))

(use-package gider
  :hook (geiser-mode . gider-mode))

;; Scheme mode configuration
(use-package scheme
  :ensure nil
  :mode ("\\.scm\\'" . scheme-mode)
  :hook ((scheme-mode . bv-lisp-indent-setup)
         (scheme-mode . bv-lisp-enable-structural-editing))
  :bind (:map scheme-mode-map
              ("C-c C-z" . geiser-repl)
              ("C-c C-c" . geiser-eval-definition)
              ("C-c C-r" . geiser-eval-region)
              ("C-c C-b" . geiser-eval-buffer)
              ("C-c C-l" . geiser-load-file)))

;;;; Common Lisp Support (SLY)
(use-package sly
  :defer t
  :commands sly
  :custom
  (inferior-lisp-program (pcase bv-lisp-common-lisp-implementation
                          ('sbcl "sbcl")
                          ('ccl "ccl")
                          ('ecl "ecl")
                          ('clisp "clisp")))
  :config
  ;; Better defaults
  (setq sly-net-coding-system 'utf-8-unix)
  (setq sly-complete-symbol-function 'sly-flex-completions)
  
  ;; History file
  (setq sly-mrepl-history-file-name
        (expand-file-name "sly-mrepl-history" bv-cache-directory))
  
  ;; Enhanced REPL
  (defun bv-sly-eval-dwim ()
    "Eval last sexp or region in SLY."
    (interactive)
    (if (use-region-p)
        (sly-eval-region (region-beginning) (region-end))
      (sly-eval-last-expression)))
  
  ;; Debugger improvements
  (setq sly-db-initial-restart-limit 10))

;; SLY contrib modules
(use-package sly-asdf
  :after sly)

(use-package sly-quicklisp
  :after sly)

(use-package sly-repl-ansi-color
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

;; Common Lisp mode configuration
(use-package lisp-mode
  :ensure nil
  :mode ("\\.lisp\\'" "\\.cl\\'" "\\.asd\\'")
  :hook ((lisp-mode . bv-lisp-indent-setup)
         (lisp-mode . bv-lisp-enable-structural-editing)
         (lisp-mode . sly-mode))
  :bind (:map lisp-mode-map
              ("C-c C-z" . sly)
              ("C-c C-c" . sly-compile-defun)
              ("C-c C-r" . sly-eval-region)
              ("C-c C-b" . sly-eval-buffer)
              ("C-c C-l" . sly-compile-and-load-file)
              ("C-c C-d" . sly-documentation-lookup)))

;;;; Structural Editing
;; Lisp-specific config for smartparens
(with-eval-after-load 'smartparens
  ;; Lisp-specific pairs
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  
  ;; Additional keybindings for Lisp
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp))

;;;; Lispy Alternative
(use-package lispy
  :when (eq bv-lisp-structural-editing-mode 'lispy)
  :hook ((emacs-lisp-mode scheme-mode lisp-mode) . lispy-mode)
  :config
  (setq lispy-compat '(edebug cider)))

;;;; Common REPL Commands
(defun bv-lisp-switch-to-repl ()
  "Switch to appropriate REPL for current mode."
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode (ielm))
    ('scheme-mode (geiser-repl-switch))
    ('lisp-mode (sly-mrepl))))

(defun bv-lisp-eval-and-switch ()
  "Evaluate and switch to REPL."
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode
     (call-interactively 'pp-eval-last-sexp)
     (when bv-lisp-repl-pop-to-buffer (ielm)))
    ('scheme-mode
     (call-interactively 'geiser-eval-last-sexp)
     (when bv-lisp-repl-pop-to-buffer (geiser-repl-switch)))
    ('lisp-mode
     (call-interactively 'sly-eval-last-expression)
     (when bv-lisp-repl-pop-to-buffer (sly-mrepl)))))

;;;; Debugging Support
;; Emacs Lisp debugging
(use-package edebug
  :ensure nil
  :defer t
  :config
  (setq edebug-print-length 50)
  (setq edebug-print-level 5)
  (setq edebug-trace nil))

;; Enhanced debugging commands
(defun bv-elisp-debug-defun ()
  "Instrument function for debugging."
  (interactive)
  (edebug-defun))

(defun bv-elisp-remove-instrumentation ()
  "Remove debugging instrumentation."
  (interactive)
  (eval-defun t))

;;;; Lisp Documentation
(defun bv-lisp-online-documentation ()
  "Open online documentation for current Lisp."
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode
     (browse-url "https://www.gnu.org/software/emacs/manual/html_node/elisp/"))
    ('scheme-mode
     (browse-url "https://www.gnu.org/software/guile/manual/"))
    ('lisp-mode
     (browse-url "http://www.lispworks.com/documentation/HyperSpec/Front/"))))

;;;; Org Babel Support
(bv-with-feature org
  (with-eval-after-load 'org
    ;; Emacs Lisp
    (add-to-list 'org-babel-load-languages '(emacs-lisp . t))
    (setq org-babel-default-header-args:emacs-lisp
          '((:lexical . "t")
            (:results . "scalar")))
    
    ;; Scheme
    (add-to-list 'org-babel-load-languages '(scheme . t))
    (setq org-babel-default-header-args:scheme
          '((:results . "scalar")))
    
    ;; Common Lisp
    (add-to-list 'org-babel-load-languages '(lisp . t))))

;;;; Global Keybindings
(with-eval-after-load 'bv-core
  ;; Common keybinding for all Lisp modes
  (dolist (mode '(emacs-lisp-mode-map
                  scheme-mode-map
                  lisp-mode-map))
    (when (boundp mode)
      (define-key (symbol-value mode) (kbd "C-c C-s") 'bv-lisp-switch-to-repl)
      (define-key (symbol-value mode) (kbd "C-x C-S-e") 'bv-lisp-eval-and-switch)
      (define-key (symbol-value mode) (kbd "C-c C-D") 'bv-lisp-online-documentation))))

;;;; Feature Definition
(defun bv-lisp-load ()
  "Load Lisp languages configuration."
  (add-to-list 'bv-enabled-features 'lisp)
  
  ;; Add IELM to app map
  (with-eval-after-load 'bv-core
    (define-key bv-app-map "I" 'ielm))
  
  ;; Check for Lisp implementations
  (unless (executable-find "guile")
    (message "Warning: Guile not found. Scheme support limited."))
  
  (let ((cl-impl (pcase bv-lisp-common-lisp-implementation
                   ('sbcl "sbcl")
                   ('ccl "ccl")
                   ('ecl "ecl")
                   ('clisp "clisp"))))
    (unless (executable-find cl-impl)
      (message "Warning: %s not found. Common Lisp support unavailable." cl-impl))))

(provide 'bv-lang-lisp)
;;; bv-lang-lisp.el ends here
