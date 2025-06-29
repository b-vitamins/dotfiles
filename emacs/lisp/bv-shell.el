;;; bv-shell.el --- Shell script configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for shell script editing and execution.

;;; Code:


(with-eval-after-load 'sh-script
  (when (boundp 'sh-basic-offset)
    (setq sh-basic-offset 2))
  (when (boundp 'sh-indent-comment)
    (setq sh-indent-comment nil))
  (when (boundp 'sh-first-lines-indent)
    (setq sh-first-lines-indent nil)))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command.*\\*"
               (display-buffer-no-window)))

(with-eval-after-load 'org
  (when (boundp 'org-structure-template-alist)
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))))

(with-eval-after-load 'ob-core
  (require 'ob-shell))

(with-eval-after-load 'project
  (when (boundp 'project-prefix-map)
    (define-key project-prefix-map "s" 'project-shell))
  (when (and (boundp 'project-switch-commands)
             (listp project-switch-commands))
    (add-to-list 'project-switch-commands
                 '(project-shell "Start an inferior shell"))))

(provide 'bv-shell)
;;; bv-shell.el ends here