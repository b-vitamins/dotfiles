;;; bv-eshell.el --- Eshell configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Emacs shell with project integration.

;;; Code:

(require 'eshell)
(require 'em-alias)
(require 'em-hist)
(declare-function project-current "project")
(declare-function project-eshell "project")
(declare-function consult-history "consult")
(declare-function ansi-color-filter-apply "ansi-color")
(defvar user-emacs-directory)
(defvar eshell-hist-mode-map)
(defvar bv-terminal-map)

(defun bv-eshell-project (&optional arg)
  "Open eshell in project root or current directory.
With prefix ARG, prompt for project selection."
  (interactive "P")
  (if (and (fboundp 'project-current) (project-current))
      (project-eshell)
    (eshell arg)))

(defun bv-eshell-setup-aliases ()
  "Setup eshell aliases."
  (eshell/alias "e" "find-file $1")
  (eshell/alias "ee" "find-file-other-window $1")
  (eshell/alias "d" "dired $1")
  (eshell/alias "ll" "ls -la")
  (eshell/alias "la" "ls -a")
  (eshell/alias "." "eshell/pwd")
  (eshell/alias ".." "cd ..")
  (eshell/alias "..." "cd ../.."))

(defun bv-eshell-setup ()
  "Setup eshell mode."
  (setenv "PAGER" "")
  (bv-eshell-setup-aliases)
  (when (fboundp 'eat-eshell-mode)
    (eat-eshell-mode 1))
  (when (fboundp 'eat-eshell-visual-command-mode)
    (eat-eshell-visual-command-mode 1)))

(add-hook 'eshell-mode-hook 'bv-eshell-setup)

(global-set-key (kbd "s-e") 'eshell)

(let ((eshell-cache (expand-file-name "eshell" user-emacs-directory)))
  (when (boundp 'eshell-aliases-file)
    (setq eshell-aliases-file (expand-file-name "alias" eshell-cache)))
  (when (boundp 'eshell-history-file-name)
    (setq eshell-history-file-name (expand-file-name "history" eshell-cache)))
  (when (boundp 'eshell-last-dir-ring-file-name)
    (setq eshell-last-dir-ring-file-name (expand-file-name "lastdir" eshell-cache))))

(when (boundp 'eshell-banner-message)
  (setq eshell-banner-message ""))
(when (boundp 'eshell-highlight-prompt)
  (setq eshell-highlight-prompt nil))
(when (boundp 'eshell-prompt-regexp)
  (setq eshell-prompt-regexp "^[^#$\n]* [#$] "))

(add-hook 'eshell-hist-mode-hook
          (lambda ()
            (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history)))

(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-terminal-map)
    (define-key bv-terminal-map (kbd "s") 'eshell)
    (define-key bv-terminal-map (kbd "P") 'bv-eshell-project)))

(provide 'bv-eshell)
;;; bv-eshell.el ends here
