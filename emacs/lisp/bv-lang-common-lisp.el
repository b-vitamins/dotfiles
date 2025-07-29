;;; bv-lang-common-lisp.el --- Common Lisp development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Common Lisp development environment with SLY and comprehensive tooling.

;;; Code:

(require 'sly nil t)

;; External variables
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar lisp-mode-map)

;; External functions
(declare-function sly-apropos "sly" (string &optional only-external-p package case-sensitive-p))
(declare-function sly-inspect "sly" (string &optional inspector-name))
(declare-function sly-trace-dialog-toggle-trace "sly-trace-dialog" (&optional function))
(declare-function sly-asdf-load-system "sly-asdf" (name))
(declare-function sly-asdf-test-system "sly-asdf" (name))
(declare-function sly-asdf-compile-system "sly-asdf" (name))
(declare-function sly-scratch "sly-scratch" ())
(declare-function rainbow-delimiters-mode "rainbow-delimiters" (&optional arg))

;; Optional package loading
(autoload 'sly "sly" nil t)
(autoload 'sly-mode "sly" nil t)
(autoload 'sly-editing-mode "sly" nil t)
(autoload 'sly-mrepl "sly-mrepl" nil t)

;; File associations
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

;; Common Lisp implementation
(setq inferior-lisp-program "sbcl")

;; SLY configuration
(with-eval-after-load 'sly
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
          (ecl ("ecl"))
          (clisp ("clisp"))))
  (setq sly-default-lisp 'sbcl)
  (setq sly-net-coding-system 'utf-8-unix)
  (setq sly-kill-without-query-p t)
  (setq sly-description-autofocus t)
  (setq sly-inhibit-pipelining nil)
  (setq sly-load-failed-fasl 'never))

;; Enable SLY contribs
(with-eval-after-load 'sly
  (require 'sly-quicklisp nil t)
  (require 'sly-asdf nil t)
  (require 'sly-scratch nil t)
  (require 'sly-repl-ansi-color nil t))

;; Enable SLY in Lisp buffers
(add-hook 'lisp-mode-hook #'sly-mode)
(add-hook 'lisp-mode-hook #'sly-editing-mode)

;; REPL interaction
(defun bv-common-lisp-start-sly ()
  "Start SLY."
  (interactive)
  (sly))

(defun bv-common-lisp-connect ()
  "Connect to running Lisp."
  (interactive)
  (sly-connect "localhost" 4005))

(defun bv-common-lisp-eval-defun ()
  "Evaluate current defun."
  (interactive)
  (sly-eval-defun))

(defun bv-common-lisp-eval-last-expression ()
  "Evaluate last expression."
  (interactive)
  (sly-eval-last-expression))

(defun bv-common-lisp-eval-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (sly-eval-buffer))

(defun bv-common-lisp-eval-region ()
  "Evaluate selected region."
  (interactive)
  (sly-eval-region (region-beginning) (region-end)))

;; Compilation
(defun bv-common-lisp-compile-defun ()
  "Compile current defun."
  (interactive)
  (sly-compile-defun))

(defun bv-common-lisp-compile-file ()
  "Compile current file."
  (interactive)
  (sly-compile-file))

(defun bv-common-lisp-compile-and-load ()
  "Compile and load current file."
  (interactive)
  (sly-compile-and-load-file))

;; Navigation
(defun bv-common-lisp-goto-definition ()
  "Go to definition."
  (interactive)
  (sly-edit-definition))

(defun bv-common-lisp-pop-back ()
  "Pop back to previous location."
  (interactive)
  (sly-pop-find-definition-stack))

;; Documentation
(defun bv-common-lisp-describe-symbol ()
  "Describe symbol at point."
  (interactive)
  (sly-describe-symbol (sly-symbol-at-point)))

(defun bv-common-lisp-documentation ()
  "Show documentation."
  (interactive)
  (sly-documentation (sly-symbol-at-point)))

(defun bv-common-lisp-apropos (string)
  "Search for symbols matching STRING."
  (interactive "sSymbol: ")
  (sly-apropos string))

(defun bv-common-lisp-apropos-package (package)
  "Search symbols in PACKAGE."
  (interactive "sPackage: ")
  (sly-apropos-package package))

;; Inspection
(defun bv-common-lisp-inspect (string)
  "Inspect value STRING."
  (interactive "sExpression: ")
  (sly-inspect string))

(defun bv-common-lisp-trace (function)
  "Trace FUNCTION."
  (interactive "sFunction: ")
  (when (fboundp 'sly-trace-dialog-toggle-trace)
    (sly-trace-dialog-toggle-trace function)))

(defun bv-common-lisp-untrace-all ()
  "Untrace all functions."
  (interactive)
  (sly-untrace-all))

;; Macroexpansion
(defun bv-common-lisp-macroexpand ()
  "Expand macro at point."
  (interactive)
  (sly-expand-1))

(defun bv-common-lisp-macroexpand-all ()
  "Fully expand macro at point."
  (interactive)
  (sly-macroexpand-all))

;; ASDF operations
(defun bv-common-lisp-load-system (system)
  "Load ASDF SYSTEM."
  (interactive "sSystem: ")
  (when (fboundp 'sly-asdf-load-system)
    (sly-asdf-load-system system)))

(defun bv-common-lisp-test-system (system)
  "Test ASDF SYSTEM."
  (interactive "sSystem: ")
  (when (fboundp 'sly-asdf-test-system)
    (sly-asdf-test-system system)))

(defun bv-common-lisp-compile-system (system)
  "Compile ASDF SYSTEM."
  (interactive "sSystem: ")
  (when (fboundp 'sly-asdf-compile-system)
    (sly-asdf-compile-system system)))

;; Quicklisp
(defun bv-common-lisp-quickload (system)
  "Quickload SYSTEM."
  (interactive "sSystem: ")
  (sly-eval `(ql:quickload ,system)))

(defun bv-common-lisp-list-quicklisp-systems ()
  "List available Quicklisp systems."
  (interactive)
  (sly-eval '(ql:system-list)))

;; Formatting
(defun bv-common-lisp-format-defun ()
  "Format current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun bv-common-lisp-format-buffer ()
  "Format entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; Debugging
(defun bv-common-lisp-toggle-debug-on-error ()
  "Toggle debug on error."
  (interactive)
  (sly-eval '(setf *debugger-hook*
                   (if *debugger-hook* nil #'invoke-debugger))))

;; Profiling
(defun bv-common-lisp-profile-start ()
  "Start profiler."
  (interactive)
  (sly-eval '(sb-profile:profile)))

(defun bv-common-lisp-profile-report ()
  "Show profile report."
  (interactive)
  (sly-eval '(sb-profile:report)))

(defun bv-common-lisp-profile-reset ()
  "Reset profiler."
  (interactive)
  (sly-eval '(sb-profile:reset)))

;; Package management
(defun bv-common-lisp-in-package ()
  "Set package in REPL."
  (interactive)
  (call-interactively 'sly-mrepl-set-package))

;; Scratch buffer
(defun bv-common-lisp-scratch ()
  "Open Lisp scratch buffer."
  (interactive)
  (when (fboundp 'sly-scratch)
    (sly-scratch)))

;; Pretty printing
(defun bv-common-lisp-pprint-eval-last-expression ()
  "Pretty print last expression."
  (interactive)
  (sly-pprint-eval-last-expression))

;; Electric pairs
(defun bv-common-lisp-electric-pairs ()
  "Setup electric pairs for Common Lisp."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?` . ?`)))))

(add-hook 'lisp-mode-hook #'electric-pair-local-mode)
(add-hook 'lisp-mode-hook #'bv-common-lisp-electric-pairs)

;; Rainbow delimiters
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

;; Smartparens for structural editing
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (add-hook 'lisp-mode-hook #'smartparens-strict-mode))

;; Pretty symbols
(defun bv-common-lisp-pretty-symbols ()
  "Enable pretty symbols for Common Lisp."
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)
          ("defun" . ?ƒ)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("/=" . ?≠)))
  (prettify-symbols-mode 1))

(add-hook 'lisp-mode-hook #'bv-common-lisp-pretty-symbols)

;; Keybindings
(defvar bv-common-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-common-lisp-start-sly)
    (define-key map (kbd "C-c C-x") #'bv-common-lisp-connect)
    ;; Evaluation
    (define-key map (kbd "C-c C-c") #'bv-common-lisp-eval-defun)
    (define-key map (kbd "C-c C-e") #'bv-common-lisp-eval-last-expression)
    (define-key map (kbd "C-c C-b") #'bv-common-lisp-eval-buffer)
    (define-key map (kbd "C-c C-r") #'bv-common-lisp-eval-region)
    (define-key map (kbd "C-c C-p") #'bv-common-lisp-pprint-eval-last-expression)
    ;; Compilation
    (define-key map (kbd "C-c C-k") #'bv-common-lisp-compile-defun)
    (define-key map (kbd "C-c C-K") #'bv-common-lisp-compile-file)
    (define-key map (kbd "C-c C-l") #'bv-common-lisp-compile-and-load)
    ;; Navigation
    (define-key map (kbd "M-.") #'bv-common-lisp-goto-definition)
    (define-key map (kbd "M-,") #'bv-common-lisp-pop-back)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-common-lisp-describe-symbol)
    (define-key map (kbd "C-c C-D") #'bv-common-lisp-documentation)
    (define-key map (kbd "C-c C-a") #'bv-common-lisp-apropos)
    (define-key map (kbd "C-c C-A") #'bv-common-lisp-apropos-package)
    ;; Inspection
    (define-key map (kbd "C-c C-i") #'bv-common-lisp-inspect)
    (define-key map (kbd "C-c C-t") #'bv-common-lisp-trace)
    (define-key map (kbd "C-c C-T") #'bv-common-lisp-untrace-all)
    ;; Macros
    (define-key map (kbd "C-c C-m") #'bv-common-lisp-macroexpand)
    (define-key map (kbd "C-c C-M") #'bv-common-lisp-macroexpand-all)
    ;; ASDF
    (define-key map (kbd "C-c a l") #'bv-common-lisp-load-system)
    (define-key map (kbd "C-c a t") #'bv-common-lisp-test-system)
    (define-key map (kbd "C-c a c") #'bv-common-lisp-compile-system)
    ;; Quicklisp
    (define-key map (kbd "C-c q l") #'bv-common-lisp-quickload)
    (define-key map (kbd "C-c q s") #'bv-common-lisp-list-quicklisp-systems)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-common-lisp-format-defun)
    (define-key map (kbd "C-c C-F") #'bv-common-lisp-format-buffer)
    ;; Package
    (define-key map (kbd "C-c C-n") #'bv-common-lisp-in-package)
    ;; Scratch
    (define-key map (kbd "C-c C-s") #'bv-common-lisp-scratch)
    ;; Profiling
    (define-key map (kbd "C-c p s") #'bv-common-lisp-profile-start)
    (define-key map (kbd "C-c p r") #'bv-common-lisp-profile-report)
    (define-key map (kbd "C-c p R") #'bv-common-lisp-profile-reset)
    map)
  "Keymap for Common Lisp mode commands.")

(with-eval-after-load 'lisp-mode
  (define-key lisp-mode-map (kbd "C-c l") bv-common-lisp-mode-map))

(provide 'bv-lang-common-lisp)
;;; bv-lang-common-lisp.el ends here