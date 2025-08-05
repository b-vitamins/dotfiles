;;; bv-monocle.el --- Monocle configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Focus mode with olivetti and window management.

;;; Code:

(require 'cl-lib)
(require 'olivetti nil t)
(autoload 'hide-mode-line-mode "hide-mode-line" nil t)
(autoload 'global-hide-mode-line-mode "hide-mode-line" nil t)


(defvar bv--monocle-previous-window-configuration nil
  "Window configuration before entering monocle mode.")

(when (boundp 'olivetti-body-width)
  (setq olivetti-body-width nil))
(when (boundp 'olivetti-margin-width)
  (setq olivetti-margin-width 0))

(defun bv-monocle--preserve-olivetti (orig-fun &rest args)
  "Preserve olivetti state when calling ORIG-FUN with ARGS."
  (let ((olivetti-active (and (boundp 'olivetti-mode) olivetti-mode)))
    (apply orig-fun args)
    (when (and olivetti-active (fboundp 'olivetti-mode))
      (olivetti-mode 1))))

(with-eval-after-load 'org-agenda
  (advice-add 'org-agenda-redo :around 'bv-monocle--preserve-olivetti))


(defvar bv-monocle-ignore-modes
  '(minibuffer-mode which-key-mode minibuffer-inactive-mode)
  "Modes to exclude from global olivetti.")

(defvar bv-monocle-ignore-buffers
  '(" *which-key*" "*Calendar*" "*Messages*" "*Completions*")
  "Buffer patterns to exclude from global olivetti.")

(defun bv-monocle--should-enable-p ()
  "Check if olivetti should be enabled in current buffer."
  (not (or (memq major-mode bv-monocle-ignore-modes)
           (cl-some (lambda (pattern)
                      (string-match-p pattern (buffer-name)))
                    bv-monocle-ignore-buffers))))

(define-globalized-minor-mode global-olivetti-mode
  olivetti-mode
  (lambda ()
    (when (bv-monocle--should-enable-p)
      (olivetti-mode 1)))
  :group 'olivetti)

(defun bv-monocle-toggle (arg)
  "Toggle focus mode.
With prefix ARG, toggle text centering and mode line.
Without ARG, toggle single window configuration."
  (interactive "P")
  (if arg
      (progn
        (if (and (boundp 'global-olivetti-mode) global-olivetti-mode)
            (global-olivetti-mode -1)
          (global-olivetti-mode 1))
        (if (and (boundp 'global-hide-mode-line-mode) global-hide-mode-line-mode)
            (global-hide-mode-line-mode -1)
          (global-hide-mode-line-mode 1)))
    (if (one-window-p)
        (when bv--monocle-previous-window-configuration
          (let ((buffer (current-buffer)))
            (set-window-configuration bv--monocle-previous-window-configuration)
            (setq bv--monocle-previous-window-configuration nil)
            (switch-to-buffer buffer)))
      (setq bv--monocle-previous-window-configuration
            (current-window-configuration))
      (delete-other-windows))))

(global-set-key (kbd "s-f") 'bv-monocle-toggle)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "o") 'olivetti-mode)
    (define-key bv-toggle-map (kbd "O") 'global-olivetti-mode)
    (define-key bv-toggle-map (kbd "m") 'hide-mode-line-mode)
    (define-key bv-toggle-map (kbd "M") 'global-hide-mode-line-mode)
    (define-key bv-toggle-map (kbd "f") 'bv-monocle-toggle)))

(provide 'bv-monocle)
;;; bv-monocle.el ends here