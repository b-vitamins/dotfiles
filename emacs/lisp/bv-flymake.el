;;; bv-flymake.el --- Flymake configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; On-the-fly syntax checking with flymake.

;;; Code:

(require 'flymake)

(declare-function consult-flymake "consult-flymake" (&optional project))

(defvar-keymap bv-flymake-fix-map
  :doc "Keymap for fast Flymake navigation and fix loop."
  "n" #'bv-flymake-goto-next-error
  "p" #'bv-flymake-goto-prev-error
  "d" #'bv-flymake-show-at-point
  "q" #'bv-flymake-quickfix
  "b" #'flymake-show-buffer-diagnostics
  "l" #'flymake-show-buffer-diagnostics
  "P" #'flymake-show-project-diagnostics)

(defun bv-flymake-show-at-point ()
  "Show the diagnostic at point."
  (interactive)
  (if (fboundp 'flymake-show-diagnostic)
      (call-interactively #'flymake-show-diagnostic)
    (flymake-show-buffer-diagnostics)))

(defun bv-flymake-goto-next-error (count)
  "Go to next Flymake error and show it at point.
COUNT is the number of errors to move by."
  (interactive "p")
  (flymake-goto-next-error count)
  (bv-flymake-show-at-point))

(defun bv-flymake-goto-prev-error (count)
  "Go to previous Flymake error and show it at point.
COUNT is the number of errors to move by."
  (interactive "p")
  (flymake-goto-prev-error count)
  (bv-flymake-show-at-point))

(defun bv-flymake-quickfix (&optional project)
  "Show Flymake diagnostics using a quick, interactive UI.

With prefix argument PROJECT, show project diagnostics."
  (interactive "P")
  (cond
   ((fboundp 'consult-flymake)
    (consult-flymake project))
   (project
    (flymake-show-project-diagnostics))
   (t
    (flymake-show-buffer-diagnostics))))

(when (boundp 'flymake-no-changes-timeout)
  (setq flymake-no-changes-timeout 0.5))
(when (boundp 'flymake-start-on-save-buffer)
  (setq flymake-start-on-save-buffer t))
(when (boundp 'flymake-fringe-indicator-position)
  (setq flymake-fringe-indicator-position 'right-fringe))

(when (boundp 'flymake-mode-map)
  (let ((map flymake-mode-map))
    (define-key map (kbd "M-n") 'bv-flymake-goto-next-error)
    (define-key map (kbd "M-p") 'bv-flymake-goto-prev-error)
    (define-key map (kbd "C-c !") bv-flymake-fix-map)))


(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "f") 'flymake-mode)
    (define-key bv-app-map (kbd "F") 'flymake-show-project-diagnostics)))

(provide 'bv-flymake)
;;; bv-flymake.el ends here
