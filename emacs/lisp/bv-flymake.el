;;; bv-flymake.el --- Flymake configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; On-the-fly syntax checking with flymake.

;;; Code:

(require 'flymake)

(when (boundp 'flymake-no-changes-timeout)
  (setq flymake-no-changes-timeout 0.5))
(when (boundp 'flymake-start-on-save-buffer)
  (setq flymake-start-on-save-buffer t))
(when (boundp 'flymake-fringe-indicator-position)
  (setq flymake-fringe-indicator-position 'right-fringe))

(when (boundp 'flymake-mode-map)
  (let ((map flymake-mode-map))
    (define-key map (kbd "M-n") 'flymake-goto-next-error)
    (define-key map (kbd "M-p") 'flymake-goto-prev-error)
    (define-key map (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
    (define-key map (kbd "C-c ! p") 'flymake-show-project-diagnostics)))

(defun bv-flymake-setup-faces ()
  "Apply theme-aware faces to flymake."
  (when (facep 'flymake-error)
    (set-face-attribute 'flymake-error nil
                        :underline `(:style wave :color ,(face-foreground 'bv-face-critical))))
  (when (facep 'flymake-warning)
    (set-face-attribute 'flymake-warning nil
                        :underline `(:style wave :color ,(face-foreground 'bv-face-popout))))
  (when (facep 'flymake-note)
    (set-face-attribute 'flymake-note nil
                        :underline `(:style wave :color ,(face-foreground 'bv-face-salient)))))

(add-hook 'after-init-hook #'bv-flymake-setup-faces)
(add-hook 'bv-after-theme-hook #'bv-flymake-setup-faces)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "f") 'flymake-mode)
    (define-key bv-app-map (kbd "F") 'flymake-show-project-diagnostics)))

(provide 'bv-flymake)
;;; bv-flymake.el ends here