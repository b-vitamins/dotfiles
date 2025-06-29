;;; bv-dired.el --- Dired configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for Dired file manager with enhanced functionality
;; including external file opening, rsync support, and visual improvements.

;;; Code:

(eval-when-compile (require 'dired))


(autoload 'dired-get-marked-files "dired")
(autoload 'embark-open-externally "embark")

(defun bv-dired-open-externally ()
  "Open marked files in Dired through an external program."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (mapc 'embark-open-externally files)))

(when (boundp 'global-map)
  (define-key global-map (kbd "s-d") 'dired-jump))

(with-eval-after-load 'dired
  (when (boundp 'dired-mode-map)
    (define-key dired-mode-map "V" 'bv-dired-open-externally)
    (define-key dired-mode-map (kbd "C-c C-r") 'dired-rsync)
    (define-key dired-mode-map "q" 'kill-current-buffer))
  
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
  (setq dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-copies 'always)
  
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'toggle-truncate-lines))

(with-eval-after-load 'all-the-icons-dired
  (when (boundp 'all-the-icons-dired-monochrome)
    (setq all-the-icons-dired-monochrome nil)))

(with-eval-after-load 'dired-rsync
  (when (boundp 'dired-rsync-options)
    (setq dired-rsync-options "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete")))

(with-eval-after-load 'ls-lisp
  (when (boundp 'ls-lisp-use-insert-directory-program)
    (setq ls-lisp-use-insert-directory-program nil)))

(provide 'bv-dired)
;;; bv-dired.el ends here