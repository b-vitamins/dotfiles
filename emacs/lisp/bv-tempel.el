;;; bv-tempel.el --- Template system configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for tempel template system.

;;; Code:

(eval-when-compile (require 'tempel))


(autoload 'tempel-complete "tempel")
(autoload 'tempel-insert "tempel")

(with-eval-after-load 'tempel
  (when (boundp 'tempel-trigger-prefix)
    (setq tempel-trigger-prefix ";"))
  (defun bv-tempel-setup-capf ()
    "Prepends `tempel-complete' to `completion-at-point-functions'."
    (when (boundp 'completion-at-point-functions)
      (setq-local completion-at-point-functions
                  (cons 'tempel-complete completion-at-point-functions))))
  (dolist (mode '(prog-mode-hook text-mode-hook conf-mode-hook fundamental-mode))
    (when (boundp mode)
      (add-hook mode 'bv-tempel-setup-capf))))

(when (boundp 'global-map)
  (define-key global-map (kbd "M-+") 'tempel-insert))

(autoload 'global-tempel-abbrev-mode "tempel")

(if after-init-time
    (global-tempel-abbrev-mode 1)
  (when (boundp 'after-init-hook)
    (add-hook 'after-init-hook 'global-tempel-abbrev-mode)))

(provide 'bv-tempel)
;;; bv-tempel.el ends here