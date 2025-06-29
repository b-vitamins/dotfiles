;;; bv-perspective.el --- Workspace management configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for perspective-based workspace management.

;;; Code:

(autoload 'persp-mode "perspective")
(autoload 'persp-switch "perspective")
(autoload 'project-prompt-project-dir "project")
(autoload 'project-switch-project "project")

(with-eval-after-load 'perspective
  (when (boundp 'persp-mode-prefix-key)
  (setq persp-mode-prefix-key (kbd "C-x P")))
  (when (boundp 'persp-show-modestring)
    (setq persp-show-modestring t))
  (when (boundp 'persp-modestring-dividers)
    (setq persp-modestring-dividers '(" [" "]" "|"))))

(defun bv-persp-switch-project (dir)
  "Switch to a project DIR in its own perspective.
This function creates or switches to a perspective named after the
project directory and then switches to that project.

Argument DIR is the project directory path."
  (interactive (list (project-prompt-project-dir)))
  (let ((name (file-name-nondirectory
               (directory-file-name
                (file-name-directory dir)))))
    (persp-switch name)
    (project-switch-project dir)))

(with-eval-after-load 'project
  (when (boundp 'project-prefix-map)
    (define-key project-prefix-map (kbd "P") 'bv-persp-switch-project)))

(if after-init-time
    (persp-mode 1)
  (when (boundp 'after-init-hook)
    (add-hook 'after-init-hook 'persp-mode)))

(provide 'bv-perspective)
;;; bv-perspective.el ends here
