;;; bv-vertico.el --- Vertico completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for vertico completion framework.

;;; Code:

(eval-when-compile
  (require 'vertico)
  (require 'vertico-multiform))

(autoload 'cl-copy-list "cl-lib")
(autoload 'consult-completion-in-region "consult")
(autoload 'vertico-multiform-buffer "vertico-multiform")
(autoload 'vertico-directory-delete-word "vertico-directory")
(autoload 'vertico-multiform-mode "vertico-multiform")
(autoload 'vertico-mode "vertico")

;; Forward declarations for elint
(declare-function bv-vertico-toggle-monocle-full "bv-vertico")

(with-eval-after-load 'minibuffer
  (when (boundp 'completion-in-region-function)
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if (and (boundp 'vertico-mode) vertico-mode)
                       'consult-completion-in-region
                       'completion--in-region)
                   args)))))


(with-eval-after-load 'vertico
  (advice-add 'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index start)
                (let ((cand (funcall orig cand prefix suffix index start)))
                  (concat
                   (if (and (boundp 'vertico--index) vertico--index (= vertico--index index))
                       (propertize "» " 'face 'vertico-current)
                     "  ")
                   cand))))
  
  (when (boundp 'global-map)
    (define-key global-map (kbd "s-s") 'vertico-repeat))
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
  (when (and (boundp 'vertico-cycle))
    (setq vertico-cycle t))
  
  (defun bv-vertico-toggle-monocle-full ()
    (when (and (boundp 'bv--vertico-monocle-initial-configuration)
               bv--vertico-monocle-initial-configuration
               (member '(vertico-buffer-mode)
                       (cdr bv--vertico-monocle-initial-configuration)))
      (vertico-multiform-buffer))
    (vertico-multiform-buffer)
    (setq-local header-line-format mode-line-format)
    (setq-local mode-line-format nil))
  
  (defun bv-vertico-toggle-monocle ()
    (interactive)
    (unless (and (boundp 'bv--vertico-monocle-initial-configuration)
                 bv--vertico-monocle-initial-configuration)
      (setq-local bv--vertico-monocle-initial-configuration
                  (cons 'initial-config (cl-copy-list (if (boundp 'vertico-multiform--stack) vertico-multiform--stack nil)))))
    (if (and (boundp 'bv--vertico-monocle-full) bv--vertico-monocle-full)
        (progn
          (when (boundp 'vertico-buffer-display-action)
            (setq-local vertico-buffer-display-action '(display-buffer-reuse-window)))
          (bv-vertico-toggle-monocle-full)
          (when (and (boundp 'bv--monocle-previous-window-configuration)
                     bv--monocle-previous-window-configuration)
            (set-window-configuration bv--monocle-previous-window-configuration))
          (when (boundp 'bv--vertico-monocle-full)
            (setq-local bv--vertico-monocle-full nil))
          (when (boundp 'bv--monocle-previous-window-configuration)
            (setq-local bv--monocle-previous-window-configuration nil)))
      (progn
        (unless (boundp 'bv--monocle-previous-window-configuration)
          (setq-local bv--monocle-previous-window-configuration nil))
        (setq-local bv--monocle-previous-window-configuration (current-window-configuration))
        (when (boundp 'vertico-buffer-display-action)
          (setq-local vertico-buffer-display-action '(display-buffer-full-frame)))
        (unless (boundp 'bv--vertico-monocle-full)
          (setq-local bv--vertico-monocle-full nil))
        (setq-local bv--vertico-monocle-full t)
        (bv-vertico-toggle-monocle-full))))
  
  (when (boundp 'vertico-map)
    (keymap-set vertico-map "<remap> <bv-toggle-monocle>" 'bv-vertico-toggle-monocle))
  
  (require 'vertico-directory)
  
  (defun bv-vertico-kill-region-dwim (&optional count)
    "Kill region if mark is active, otherwise delete word.
Prefix argument can be used to kill COUNT words or directories."
    (interactive "p")
    (if (use-region-p)
        (kill-region (region-beginning) (region-end) 'region)
      (vertico-directory-delete-word count)))
  
  (when (boundp 'vertico-map)
    (define-key vertico-map (kbd "C-w") 'bv-vertico-kill-region-dwim))
  
  (defun bv--vertico-prepare-header-line ()
    "Move mode line to header and increase vertico-count by 1."
    (setq-local header-line-format mode-line-format)
    (setq-local mode-line-format nil))
  
  (advice-add 'vertico--setup :after 'bv--vertico-prepare-header-line)
  
  (when (boundp 'vertico-multiform-categories)
    (setq vertico-multiform-categories
          '((consult-grep buffer)
            (imenu buffer)
            (buffer)
            (info-menu buffer)
            (consult-org-heading buffer)
            (consult-history buffer)
            (consult-lsp-symbols buffer)
            (consult-xref buffer)
            (embark-keybinding buffer)
            (consult-location buffer))))
  
  (when (boundp 'vertico-multiform-commands)
    (setq vertico-multiform-commands
          '((telega-chat-with buffer)
            (magit:--author flat)
            (Info-goto-node buffer)
            (info-lookup-symbol buffer)
            (Info-follow-reference buffer)
            (consult-yank-pop buffer))))
  
  (vertico-multiform-mode))
(if after-init-time
    (vertico-mode 1)
  (add-hook 'after-init-hook 'vertico-mode))

(provide 'bv-vertico)
;;; bv-vertico.el ends here
