;;; bv-spelling.el --- Spell checking configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for spell checking with hunspell and flyspell.

;;; Code:

(when (boundp 'org-mode-hook)
  (add-hook 'org-mode-hook 'flyspell-mode))
(when (boundp 'latex-mode-hook)
  (add-hook 'latex-mode-hook 'flyspell-mode))
(when (boundp 'text-mode-hook)
  (add-hook 'text-mode-hook 'flyspell-mode))

(when (boundp 'prog-mode-hook)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(with-eval-after-load 'ispell
  (when (boundp 'ispell-program-name)
    (setq ispell-program-name
          "/run/current-system/profile/bin/hunspell"))
  (when (boundp 'ispell-personal-dictionary)
    (setq ispell-personal-dictionary "~/documents/hunspell.personal")))

(with-eval-after-load 'flyspell
  (when (boundp 'flyspell-issue-welcome-flag)
    (setq flyspell-issue-welcome-flag nil))
  (when (boundp 'flyspell-issue-message-flag)
    (setq flyspell-issue-message-flag nil)))

(with-eval-after-load 'dictionary
  (when (boundp 'dictionary-server)
    (setq dictionary-server "dict.org")))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "d") 'dictionary-search)))

(provide 'bv-spelling)
;;; bv-spelling.el ends here