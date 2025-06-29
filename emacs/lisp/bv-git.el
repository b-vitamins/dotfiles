;;; bv-git.el --- Git configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Git integration with Magit, git-gutter, and git-link.
;; Provides enhanced Git workflow with visual diff indicators.

;;; Code:


(autoload 'git-link--exec "git-link")
(autoload 'git-link--branch "git-link")
(autoload 'git-link--parse-remote "git-link")

(with-eval-after-load 'git-link
  (advice-add 'git-link :around
              (lambda (f remote start end)
                (when (boundp 'git-link--last-commit-from-remote)
                  (let ((git-link--last-commit-from-remote
                         (lambda ()
                           (car (git-link--exec "--no-pager" "log" "-n1"
                                                "--pretty=format:%h"
                                                (concat remote "/" (git-link--branch)))))))
                    (advice-add 'git-link--last-commit :override git-link--last-commit-from-remote)
                    (funcall f remote start end)
                    (advice-remove 'git-link--last-commit git-link--last-commit-from-remote))))))

(autoload 'git-link--relative-filename "git-link")

(defun bv-git-link ()
  "Create a Git link using commit hash when possible."
  (interactive)
  (when (boundp 'git-link-use-commit)
    (let ((git-link-use-commit t))
      (if (git-link--relative-filename)
          (call-interactively 'git-link)
        (call-interactively 'git-link-commit)))))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "g") 'git-gutter-mode)
    (define-key bv-toggle-map (kbd "G") 'global-git-gutter-mode)))

(when (boundp 'global-map)
  (define-key global-map (kbd "C-x g") 'git-gutter-transient))

(with-eval-after-load 'transient
  (when (boundp 'transient-history-file)
    (setq transient-history-file
          (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
                  "/emacs/transient/history.el"))))

(with-eval-after-load 'git-gutter-transient
  (transient-insert-suffix 'git-gutter-transient "Q"
    '("C-x g" "Quit and disable" git-gutter-transient:quit-and-disable
      :transient transient--do-exit)))

(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-local-branches
                          'magit-insert-stashes)
  
  (defvar bv-projects-directory "~/projects")
  
  (autoload 'git-link--parse-remote "git-link")
  
  (defun bv-get-local-repo-path-from-url (url)
    (let* ((path (cadr (git-link--parse-remote url)))
           (dir (file-name-directory (directory-file-name path))))
      (if bv-projects-directory
          (expand-file-name dir bv-projects-directory)
        dir)))
  
  (when (boundp 'magit-clone-default-directory)
    (setq magit-clone-default-directory 'bv-get-local-repo-path-from-url)))

(when (boundp 'git-gutter:lighter)
  (setq git-gutter:lighter " GG"))

(with-eval-after-load 'git-gutter
  (require 'git-gutter-fringe)
  (when (boundp 'git-gutter:update-hooks)
    (add-to-list 'git-gutter:update-hooks 'focus-in-hook))
  (when (boundp 'git-gutter:update-commands)
    (add-to-list 'git-gutter:update-commands 'other-window))
  (add-hook 'magit-post-stage-hook 'git-gutter:update-all-windows)
  (add-hook 'magit-post-unstage-hook 'git-gutter:update-all-windows)
  
  (defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
    (cl-letf (((symbol-function 'yes-or-no-p) 'y-or-n-p))
      (apply orig-fun r)))
  
  (dolist (fn '(git-gutter:stage-hunk git-gutter:revert-hunk))
    (advice-add fn :around 'yes-or-no-p->-y-or-n-p))
  
  (advice-add 'git-gutter:stage-hunk :around
              (lambda (orig-fun &rest args)
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t)))
                  (apply orig-fun args))))
  
  (dolist (fringe '(git-gutter-fr:added git-gutter-fr:modified))
    (define-fringe-bitmap fringe (vector 8) nil nil '(top repeat)))
  
  (define-fringe-bitmap 'git-gutter-fr:deleted
                        (vector 8 12 14 15) nil nil 'bottom))

(provide 'bv-git)
;;; bv-git.el ends here