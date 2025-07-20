;;; bv-perspective.el --- Workspace management configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Workspace management with perspective.el.

;;; Code:

(require 'perspective nil t)
(declare-function project-prompt-project-dir "project")
(declare-function project-switch-project "project")

(when (boundp 'persp-mode-prefix-key)
  (setq persp-mode-prefix-key (kbd "C-x P")))
(when (boundp 'persp-show-modestring)
  (setq persp-show-modestring nil))
(when (boundp 'persp-state-default-file)
  (setq persp-state-default-file
        (expand-file-name "perspectives" user-emacs-directory)))
(when (boundp 'persp-suppress-no-prefix-key-warning)
  (setq persp-suppress-no-prefix-key-warning t))

(defun bv-persp-switch-project (dir)
  "Switch to project DIR in dedicated perspective."
  (interactive (list (project-prompt-project-dir)))
  (let ((name (file-name-nondirectory
               (directory-file-name dir))))
    (persp-switch name)
    (project-switch-project dir)))


(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "s") 'persp-switch)
    (define-key bv-app-map (kbd "S") 'bv-persp-switch-project)
    (define-key bv-app-map (kbd "k") 'persp-kill)
    (define-key bv-app-map (kbd "r") 'persp-rename)
    (define-key bv-app-map (kbd "a") 'persp-add-buffer)
    (define-key bv-app-map (kbd "A") 'persp-set-buffer)
    (define-key bv-app-map (kbd "b") 'persp-switch-to-buffer)
    (define-key bv-app-map (kbd "i") 'persp-import)
    (define-key bv-app-map (kbd "n") 'persp-next)
    (define-key bv-app-map (kbd "p") 'persp-prev)))

(with-eval-after-load 'project
  (when (and (boundp 'project-switch-commands)
             (listp project-switch-commands))
    (add-to-list 'project-switch-commands
                 '(bv-persp-switch-project "Perspective" ?P) t)))

(add-hook 'after-init-hook 'persp-mode)

(with-eval-after-load 'consult
  (when (fboundp 'consult-customize)
    (consult-customize persp-switch-to-buffer :preview-key "M-.")))

(provide 'bv-perspective)
;;; bv-perspective.el ends here
