;;; bv-eglot.el --- Eglot LSP configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module configures Eglot, the built-in Language Server Protocol client
;; for Emacs.  It sets up key bindings and customizes Eglot behavior for
;; better integration with other tools like Consult.

;;; Code:


(when (boundp 'goto-map)
  (define-key goto-map (kbd "s") 'consult-eglot-symbols))

(with-eval-after-load 'eglot
  (when (boundp 'eldoc-echo-area-use-multiline-p)
    (setq eldoc-echo-area-use-multiline-p nil))
  (when (boundp 'eglot-confirm-server-initiated-edits)
    (setq eglot-confirm-server-initiated-edits nil))
  (when (boundp 'eglot-managed-mode-hook)
    (add-hook 'eglot-managed-mode-hook
              (lambda () 
                (when (boundp 'consult-imenu--cache)
                  (setq consult-imenu--cache nil)))))
  (when (boundp 'eglot-extend-to-xref)
    (setq eglot-extend-to-xref t)))

(provide 'bv-eglot)
;;; bv-eglot.el ends here