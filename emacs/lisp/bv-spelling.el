;;; bv-spelling.el --- Spell checking configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Spell checking with hunspell and flyspell.

;;; Code:

;; Enable spell checking in text modes
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'latex-mode-hook #'flyspell-mode)
(add-hook 'markdown-mode-hook #'flyspell-mode)

;; Check comments and strings in programming modes
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Ispell configuration
(with-eval-after-load 'ispell
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell"))
  (when (boundp 'ispell-personal-dictionary)
    (setq ispell-personal-dictionary
          (expand-file-name "hunspell.personal" "~/documents/")))
  (when (boundp 'ispell-dictionary)
    (setq ispell-dictionary "en_US")))

;; Flyspell configuration
(with-eval-after-load 'flyspell
  (when (boundp 'flyspell-issue-welcome-flag)
    (setq flyspell-issue-welcome-flag nil))
  (when (boundp 'flyspell-issue-message-flag)
    (setq flyspell-issue-message-flag nil))

  ;; Better flyspell faces
  (defun bv-spelling-setup-faces ()
    "Setup spell checking faces."
    (when (facep 'flyspell-incorrect)
      (set-face-attribute 'flyspell-incorrect nil
                          :underline `(:style wave :color ,(face-foreground 'bv-face-critical))))
    (when (facep 'flyspell-duplicate)
      (set-face-attribute 'flyspell-duplicate nil
                          :underline `(:style wave :color ,(face-foreground 'bv-face-popout)))))

  (add-hook 'flyspell-mode-hook #'bv-spelling-setup-faces)
  (add-hook 'bv-after-theme-hook #'bv-spelling-setup-faces))

;; Dictionary lookup
(with-eval-after-load 'dictionary
  (when (boundp 'dictionary-server)
    (setq dictionary-server "dict.org")))

;; Keybindings
(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "s") 'flyspell-mode)
    (define-key bv-app-map (kbd "S") 'ispell-word)
    (define-key bv-app-map (kbd "D") 'dictionary-search)))

(provide 'bv-spelling)
;;; bv-spelling.el ends here