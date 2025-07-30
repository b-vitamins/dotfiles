;;; bv-dired.el --- Dired configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Dired with external file opening and rsync support.

;;; Code:

(require 'dired)
(autoload 'dired-rsync "dired-rsync" nil t)
(autoload 'embark-open-externally "embark" nil t)

(defvar dired-mode-map)
(declare-function dired-get-marked-files "dired")
(declare-function dired-hide-details-mode "dired")
(declare-function kill-current-buffer "simple")
(declare-function toggle-truncate-lines "simple")
(declare-function dired-jump "dired-x")

(defun bv-dired-open-externally ()
  "Open marked files with external program."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (embark-open-externally file)))

(when (boundp 'dired-dwim-target)
  (setq dired-dwim-target t))
(when (boundp 'dired-listing-switches)
  (setq dired-listing-switches "-l --group-directories-first -h -A --time-style=long-iso"))
(when (boundp 'dired-hide-details-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil))
(when (boundp 'delete-by-moving-to-trash)
  (setq delete-by-moving-to-trash nil))
(when (boundp 'dired-recursive-deletes)
  (setq dired-recursive-deletes 'always))
(when (boundp 'dired-recursive-copies)
  (setq dired-recursive-copies 'always))
(when (boundp 'dired-clean-confirm-killing-deleted-buffers)
  (setq dired-clean-confirm-killing-deleted-buffers nil))

(with-eval-after-load 'dired
  (define-key dired-mode-map "V" 'bv-dired-open-externally)
  (define-key dired-mode-map (kbd "C-c C-r") 'dired-rsync)
  (define-key dired-mode-map "q" 'kill-current-buffer)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'toggle-truncate-lines))

(with-eval-after-load 'dired-rsync
  (when (boundp 'dired-rsync-options)
    (setq dired-rsync-options "-az --info=progress2 --delete")))

(with-eval-after-load 'ls-lisp
  (when (boundp 'ls-lisp-use-insert-directory-program)
    (setq ls-lisp-use-insert-directory-program nil)))

(global-set-key (kbd "s-d") 'dired-jump)

(provide 'bv-dired)
;;; bv-dired.el ends here