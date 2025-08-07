;;; bv-shell.el --- Shell script configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Shell script editing and shell-mode.

;;; Code:

(require 'sh-script nil t)

;; Declare external functions
(declare-function project-root "project" (project))
(declare-function project-current "project" (&optional may-prompt dir))
(declare-function consult-history "consult" (&rest args))

(when (featurep 'sh-script)
  (when (boundp 'sh-basic-offset)
    (setq sh-basic-offset 2))
  (when (boundp 'sh-indent-comment)
    (setq sh-indent-comment nil))
  (when (boundp 'sh-first-lines-indent)
    (setq sh-first-lines-indent nil)))

(when (boundp 'display-buffer-alist)
  (add-to-list 'display-buffer-alist
               '("\\*Async Shell Command.*\\*"
                 (display-buffer-no-window))))


(defun bv-shell ()
  "Start shell in current directory."
  (interactive)
  (shell))

(defun bv-shell-project ()
  "Start shell in project root."
  (interactive)
  (if (and (fboundp 'project-current) (project-current))
      (let ((default-directory (project-root (project-current))))
        (shell))
    (shell)))

(global-set-key (kbd "s-S") 'bv-shell)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "s") 'bv-shell)
    (define-key bv-app-map (kbd "S") 'bv-shell-project)))

(with-eval-after-load 'shell
  (when (boundp 'shell-mode-map)
    (define-key shell-mode-map (kbd "M-r") 'consult-history)))

(provide 'bv-shell)
;;; bv-shell.el ends here