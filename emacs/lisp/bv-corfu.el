;;; bv-corfu.el --- Corfu completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; In-buffer completion with corfu.

;;; Code:

(require 'corfu)
(autoload 'corfu-doc-mode "corfu-doc" nil t)
(declare-function consult-completion-in-region "consult")

(when (boundp 'corfu-min-width)
  (setq corfu-min-width 60))
(when (boundp 'corfu-cycle)
  (setq corfu-cycle t))
(when (boundp 'corfu-quit-no-match)
  (setq corfu-quit-no-match t))
(when (boundp 'corfu-auto)
  (setq corfu-auto nil))
(when (boundp 'corfu-doc-auto)
  (setq corfu-doc-auto nil))
(when (boundp 'corfu-popupinfo-delay)
  (setq corfu-popupinfo-delay 0.5))
(when (boundp 'corfu-preview-current)
  (setq corfu-preview-current nil))
(when (boundp 'corfu-preselect)
  (setq corfu-preselect 'prompt))
(when (boundp 'corfu-bar-width)
  (setq corfu-bar-width 2))
(defun corfu-move-to-minibuffer ()
  "Move completion to minibuffer."
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold
        completion-cycling)
    (when (boundp 'completion-in-region--data)
      (apply 'consult-completion-in-region completion-in-region--data))))
(defun corfu-enable-in-minibuffer ()
  "Enable corfu in minibuffer."
  (when (where-is-internal 'completion-at-point
                           (list (current-local-map)))
    (corfu-mode 1)))
(when (boundp 'corfu-map)
  (define-key corfu-map (kbd "M-m") 'corfu-move-to-minibuffer)
  (define-key corfu-map (kbd "M-D") 'corfu-doc-toggle))

(add-hook 'minibuffer-setup-hook 'corfu-enable-in-minibuffer)
(add-hook 'corfu-mode-hook 'corfu-doc-mode)


(global-corfu-mode 1)

;; Enable candidate overlay without the message
(when (require 'corfu-candidate-overlay nil t)
  (cl-letf (((symbol-function 'message) #'ignore))
    (corfu-candidate-overlay-mode 1)))

(provide 'bv-corfu)
;;; bv-corfu.el ends here