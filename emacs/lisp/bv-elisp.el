;;; bv-elisp.el --- Emacs Lisp configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Emacs Lisp development configuration.

;;; Code:

(require 'elisp-mode)
(autoload 'pp-eval-last-sexp "pp")
(autoload 'pp-eval-expression "pp")
(autoload 'pp-macroexpand-last-sexp "pp")

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(when (boundp 'emacs-lisp-mode-map)
  (let ((map emacs-lisp-mode-map))
    (define-key map (kbd "C-x C-e") 'pp-eval-last-sexp)
    (define-key map (kbd "C-c C-m") 'pp-macroexpand-last-sexp)
    (define-key map (kbd "C-c C-b") 'eval-buffer)
    (define-key map (kbd "C-c C-c") 'eval-defun)))

(global-set-key (kbd "M-:") 'pp-eval-expression)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "i") 'ielm)))

(with-eval-after-load 'ielm
  (when (boundp 'ielm-header)
    (setq ielm-header ""))
  (when (boundp 'ielm-noisy)
    (setq ielm-noisy nil)))

(defun bv-elisp-outline-level ()
  "Custom outline level for Emacs Lisp."
  (- (match-end 0) (match-beginning 0)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local outline-regexp ";;[;*]+\\|\\s-*([^)]")
            (setq-local outline-level 'bv-elisp-outline-level)
            (outline-minor-mode 1)))

(provide 'bv-elisp)
;;; bv-elisp.el ends here