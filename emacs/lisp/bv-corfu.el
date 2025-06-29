;;; bv-corfu.el --- Corfu completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for Corfu completion-at-point framework with documentation
;; support and minibuffer integration.

;;; Code:

(eval-when-compile
  (require 'corfu)
  (require 'corfu-candidate-overlay))


(autoload 'corfu-doc-mode "corfu-doc")
(autoload 'global-corfu-mode "corfu")
(autoload 'corfu-candidate-overlay-mode "corfu-candidate-overlay")

(with-eval-after-load 'corfu
  (setq corfu-min-width 60)
  (setq corfu-cycle t)
  (setq corfu-quit-no-match t)
  (setq corfu-auto nil)
  (when (boundp 'corfu-doc-auto)
    (setq corfu-doc-auto nil))

  (defun corfu-move-to-minibuffer ()
    "Move current Corfu completion session to minibuffer."
    (interactive)
    (let ((completion-extra-properties (if (boundp 'corfu--extra) corfu--extra nil))
          completion-cycle-threshold
          completion-cycling)
      (when (boundp 'completion-in-region--data)
        (apply 'consult-completion-in-region completion-in-region--data))))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu completion in minibuffer when completion-at-point is available."
    (when (where-is-internal 'completion-at-point
                             (list (current-local-map)))
      (corfu-mode 1)))

  (when (boundp 'corfu-map)
    (define-key corfu-map (kbd "M-m") 'corfu-move-to-minibuffer)
    (define-key corfu-map (kbd "M-D") 'corfu-doc-toggle))

  (when (boundp 'minibuffer-setup-hook)
    (add-hook 'minibuffer-setup-hook 'corfu-enable-in-minibuffer))
  (when (boundp 'corfu-mode-hook)
    (add-hook 'corfu-mode-hook 'corfu-doc-mode)))

(if after-init-time
    (progn
      (corfu-candidate-overlay-mode 1)
      (global-corfu-mode 1))
  (progn
    (when (boundp 'after-init-hook)
      (add-hook 'after-init-hook 'corfu-candidate-overlay-mode)
      (add-hook 'after-init-hook 'global-corfu-mode))))

(provide 'bv-corfu)
;;; bv-corfu.el ends here