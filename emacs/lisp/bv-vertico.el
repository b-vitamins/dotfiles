;;; bv-vertico.el --- Vertico completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Vertical completion with vertico.

;;; Code:

(require 'vertico)
(require 'vertico-directory)
(require 'vertico-multiform)
(require 'vertico-repeat)
(autoload 'consult-completion-in-region "consult")

(when (boundp 'vertico-cycle)
  (setq vertico-cycle t))
(when (boundp 'vertico-resize)
  (setq vertico-resize t))
(when (boundp 'vertico-count)
  (setq vertico-count 15))

(when (boundp 'completion-in-region-function)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     'consult-completion-in-region
                   'completion--in-region)
                 args))))

(add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
(global-set-key (kbd "s-s") 'vertico-repeat)
(defun bv-vertico-kill-region-dwim (&optional count)
  "Kill region or delete word/directory."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) 'region)
    (vertico-directory-delete-word count)))

(when (boundp 'vertico-map)
  (define-key vertico-map (kbd "C-w") 'bv-vertico-kill-region-dwim))
(defun bv-vertico--prepare-header-line ()
  "Move mode line to header."
  (setq-local header-line-format mode-line-format)
  (setq-local mode-line-format nil))

(advice-add 'vertico--setup :after #'bv-vertico--prepare-header-line)
(when (boundp 'vertico-multiform-categories)
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer)
          (buffer)
          (consult-org-heading buffer)
          (consult-history buffer)
          (consult-xref buffer)
          (embark-keybinding buffer))))

(when (boundp 'vertico-multiform-commands)
  (setq vertico-multiform-commands
        '((consult-yank-pop buffer))))

(vertico-mode 1)
(vertico-multiform-mode 1)

(provide 'bv-vertico)
;;; bv-vertico.el ends here
