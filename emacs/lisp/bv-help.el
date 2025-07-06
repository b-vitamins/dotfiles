;;; bv-help.el --- Enhanced help system  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced help with quick access and better defaults.

;;; Code:

(require 'help)

;; Better help defaults
(when (boundp 'help-window-select)
  (setq help-window-select t))
(when (boundp 'help-enable-variable-value-editing)
  (setq help-enable-variable-value-editing t))

;; Quick help message
(defun bv-quick-help ()
  "Display quick help in echo area."
  (interactive)
  (let ((message-log-max nil))
    (message
     (concat
      (propertize "\n" 'face '(:height 0.4))
      " [C-x C-f] Open  [M-w] Copy   [C-w] Cut   [C-s] Search           "
      (propertize "[C-g]   Cancel" 'face 'bold)
      "\n"
      " [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [M-x] Command          "
      (propertize "[C-x C-c] Quit" 'face 'bold)
      (propertize "\n " 'face '(:height 0.5))))
    (sit-for 30)))

;; Enhanced describe functions
(defun bv-describe-symbol-at-point ()
  "Describe symbol at point."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if symbol
        (describe-symbol symbol)
      (call-interactively 'describe-symbol))))

;; Help transient
(defun bv-help-transient ()
  "Show help transient menu."
  (interactive)
  (let ((help-map (make-sparse-keymap)))
    (define-key help-map (kbd "f") 'describe-function)
    (define-key help-map (kbd "v") 'describe-variable)
    (define-key help-map (kbd "k") 'describe-key)
    (define-key help-map (kbd "m") 'describe-mode)
    (define-key help-map (kbd "s") 'describe-symbol)
    (define-key help-map (kbd "p") 'describe-package)
    (define-key help-map (kbd "i") 'info)
    (define-key help-map (kbd "?") 'help-for-help)
    (set-transient-map help-map t)
    (message "Help: [f]unction [v]ariable [k]ey [m]ode [s]ymbol [p]ackage [i]nfo [?]help")))

;; Keybindings
(global-set-key (kbd "C-h C-h") 'bv-help-transient)
(global-set-key (kbd "C-h .") 'bv-describe-symbol-at-point)
(global-set-key (kbd "M-p") 'bv-quick-help)

(provide 'bv-help)
;;; bv-help.el ends here