;;; bv-monocle.el --- Monocle configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for distraction-free editing with olivetti and mode line hiding.
;; Provides window management and focus modes for concentrated work.

;;; Code:

(eval-when-compile (require 'hide-mode-line))


(declare-function global-hide-mode-line-mode "hide-mode-line")
(declare-function olivetti-mode "olivetti")
(declare-function global-olivetti-mode "bv-monocle")

(when (boundp 'olivetti-body-width)
  (setq olivetti-body-width nil))
(when (boundp 'olivetti-margin-width)
  (setq olivetti-margin-width 0))

(with-eval-after-load 'org-agenda
  (defun ensure-olivetti (orig-fun &rest r)
    (let ((olivetti-p (if (and (boundp 'olivetti-mode) olivetti-mode) 1 0)))
      (apply orig-fun r)
      (when (fboundp 'olivetti-mode)
        (olivetti-mode olivetti-p))))
  (advice-add 'org-agenda-redo :around 'ensure-olivetti))

(with-eval-after-load 'hide-mode-line
  (when (boundp 'hide-mode-line-excluded-modes)
    (setq hide-mode-line-excluded-modes '())))

(defun bv--match-modes (modes)
  "Return MODES that match the current buffer's major mode."
  (seq-filter 'derived-mode-p modes))


(defun bv--turn-on-olivetti-mode ()
  "Turn on olivetti mode unless in ignored modes or buffers."
  (let ((global-olivetti-ignore-modes '(minibuffer-mode which-key-mode minibuffer-inactive-mode))
        (global-olivetti-ignore-buffers '(" *which-key*")))
    (unless (or (and (boundp 'major-mode) 
                     (memq major-mode global-olivetti-ignore-modes))
                (seq-filter (lambda (s) (string-match (buffer-name) s))
                            global-olivetti-ignore-buffers))
      (olivetti-mode 1))))

(when (fboundp 'olivetti-mode)
  (define-globalized-minor-mode global-olivetti-mode
    olivetti-mode
    bv--turn-on-olivetti-mode
    :require 'olivetti
    :group 'olivetti))


(defun bv-toggle-monocle (arg)
  "Toggle monocle mode.
With prefix ARG, toggle olivetti and mode line hiding.
Otherwise, toggle window configuration between single and multiple windows."
  (interactive "P")
  (if arg
      (if (and (boundp 'global-olivetti-mode) global-olivetti-mode
               (boundp 'global-hide-mode-line-mode) global-hide-mode-line-mode)
          (progn
            (global-hide-mode-line-mode -1)
            (global-olivetti-mode -1))
        (progn
          (global-hide-mode-line-mode 1)
          (global-olivetti-mode 1)))
    (if (one-window-p)
        (when (and (boundp 'bv--monocle-previous-window-configuration)
                   bv--monocle-previous-window-configuration)
          (let ((cur-buffer (current-buffer)))
            (set-window-configuration bv--monocle-previous-window-configuration)
            (set (make-local-variable 'bv--monocle-previous-window-configuration) nil)
            (switch-to-buffer cur-buffer)))
      (set (make-local-variable 'bv--monocle-previous-window-configuration)
           (current-window-configuration))
      (delete-other-windows))))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "o") 'olivetti-mode)
    (define-key bv-toggle-map (kbd "O") 'global-olivetti-mode)
    (define-key bv-toggle-map (kbd "m") 'hide-mode-line-mode)
    (define-key bv-toggle-map (kbd "M") 'global-hide-mode-line-mode)))

(when (boundp 'global-map)
  (define-key global-map (kbd "s-f") 'bv-toggle-monocle))

(provide 'bv-monocle)
;;; bv-monocle.el ends here