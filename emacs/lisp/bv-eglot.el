;;; bv-eglot.el --- Eglot LSP configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Language Server Protocol client with eglot.

;;; Code:

(require 'eglot)
(autoload 'consult-eglot-symbols "consult-eglot" nil t)

(when (boundp 'goto-map)
  (define-key goto-map (kbd "s") 'consult-eglot-symbols))

(when (boundp 'eldoc-echo-area-use-multiline-p)
  (setq eldoc-echo-area-use-multiline-p nil))
(when (boundp 'eglot-confirm-server-initiated-edits)
  (setq eglot-confirm-server-initiated-edits nil))
(when (boundp 'eglot-extend-to-xref)
  (setq eglot-extend-to-xref t))
(when (boundp 'eglot-autoshutdown)
  (setq eglot-autoshutdown t))
(when (boundp 'eglot-sync-connect)
  (setq eglot-sync-connect nil))

(defun bv-eglot-ensure ()
  "Start eglot in programming modes."
  (when (derived-mode-p 'prog-mode)
    (unless (or (derived-mode-p 'emacs-lisp-mode)
                (eq major-mode 'lisp-interaction-mode))
      (eglot-ensure))))

(add-hook 'prog-mode-hook 'bv-eglot-ensure)

(with-eval-after-load 'eglot
  (when (boundp 'eglot-mode-map)
    (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c l F") 'eglot-format-buffer)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "l") 'eglot)
    (define-key bv-app-map (kbd "L") 'eglot-shutdown)))

(provide 'bv-eglot)
;;; bv-eglot.el ends here