;;; bv-tempel.el --- Template system configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Template expansion with tempel.

;;; Code:

(require 'tempel)
(declare-function tempel-complete "tempel")

(when (boundp 'tempel-trigger-prefix)
  (setq tempel-trigger-prefix ";"))
(when (boundp 'tempel-path)
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

(defun bv-tempel-setup-capf ()
  "Add tempel to completion-at-point."
  (setq-local completion-at-point-functions
              (cons 'tempel-complete completion-at-point-functions)))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'bv-tempel-setup-capf))

(global-set-key (kbd "M-+") 'tempel-insert)

(when (boundp 'tempel-map)
  (define-key tempel-map (kbd "TAB") 'tempel-next)
  (define-key tempel-map (kbd "<tab>") 'tempel-next)
  (define-key tempel-map (kbd "S-TAB") 'tempel-previous)
  (define-key tempel-map (kbd "<backtab>") 'tempel-previous))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "t") 'tempel-insert)))

(global-tempel-abbrev-mode 1)

(provide 'bv-tempel)
;;; bv-tempel.el ends here