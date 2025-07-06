;;; bv-eat.el --- Emulate A Terminal configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Terminal emulation with eat.

;;; Code:

(require 'eat nil t)

(when (featurep 'eat)
  (when (boundp 'eat-line-input-ring-size)
    (setq eat-line-input-ring-size 4096))
  (when (boundp 'eat-kill-buffer-on-exit)
    (setq eat-kill-buffer-on-exit t))
  (when (boundp 'eat-term-scrollback-size)
    (setq eat-term-scrollback-size nil))
  (when (boundp 'eat-enable-mouse)
    (setq eat-enable-mouse t)))

(defun bv-eat ()
  "Open eat terminal."
  (interactive)
  (if (featurep 'eat)
      (eat)
    (ansi-term (or (getenv "SHELL") "/bin/bash"))))

(defun bv-eat-project ()
  "Open eat terminal in project root."
  (interactive)
  (let ((default-directory (or (and (fboundp 'project-root)
                                   (project-root (project-current)))
                              default-directory)))
    (bv-eat)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "e") 'bv-eat)
    (define-key bv-app-map (kbd "E") 'bv-eat-project)))

(global-set-key (kbd "s-t") 'bv-eat)

(provide 'bv-eat)
;;; bv-eat.el ends here