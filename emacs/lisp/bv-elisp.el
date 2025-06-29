;;; bv-elisp.el --- Emacs Lisp configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module configures Emacs Lisp development environment.
;; It sets up enhanced evaluation commands, IELM configuration,
;; and Org Babel integration for Elisp code blocks.

;;; Code:


(when (boundp 'emacs-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode))

(with-eval-after-load 'elisp-mode
  (when (boundp 'emacs-lisp-mode-map)
    (let ((map emacs-lisp-mode-map))
    (define-key map (kbd "C-x C-e") 'pp-eval-last-sexp)
    (define-key map (kbd "M-:") 'pp-eval-expression)
    (define-key map (kbd "C-c C-m") 'pp-macroexpand-last-sexp)
    (define-key map (kbd "C-c C-b") 'eval-buffer)
    (autoload 'embark-pp-eval-defun "embark")
    (define-key map (kbd "C-c C-c") 'embark-pp-eval-defun))))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "I") 'ielm)))

(with-eval-after-load 'ielm
  (when (boundp 'ielm-header)
    (setq ielm-header ""))
  (when (boundp 'ielm-noisy)
    (setq ielm-noisy nil)))

(with-eval-after-load 'org
  (when (boundp 'org-structure-template-alist)
    (add-to-list 'org-structure-template-alist '("el" . "src elisp"))))

(with-eval-after-load 'ob-emacs-lisp
  (when (boundp 'org-babel-default-header-args:elisp)
    (setq org-babel-default-header-args:elisp
          '((:lexical . "t") (:results . "scalar")))))

(provide 'bv-elisp)
;;; bv-elisp.el ends here