;;; bv-bindings.el --- Key bindings inspired by nano-bindings  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Key bindings following nano-emacs philosophy.
;; Minimal, clean, and focused on essential operations.

;;; Code:

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun new-frame ()
  "Create a new frame with scratch buffer."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-`") 'other-frame)

(global-set-key (kbd "<M-return>") 'toggle-frame-maximized)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<M-return>") 'toggle-frame-maximized))

(defun bv--delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'bv--delete-frame-or-kill-emacs)

(global-set-key (kbd "C-c r") 'recentf-open-files)

(provide 'bv-bindings)
;;; bv-bindings.el ends here