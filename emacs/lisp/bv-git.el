;;; bv-git.el --- Version control with Git -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Git support with Magit, git-gutter, and related tools.

;;; Code:

;;;; Dependencies
(require 'bv-core)

;;;; Custom Variables
(defgroup bv-git nil
  "Git and version control configuration."
  :group 'bv)

(defcustom bv-git-project-directory nil
  "Directory where project repositories are stored."
  :type '(choice (const nil) directory)
  :group 'bv-git)

(defcustom bv-git-enable-todos t
  "Enable magit-todos integration."
  :type 'boolean
  :group 'bv-git)

(defcustom bv-git-enable-gutter t
  "Enable git-gutter mode."
  :type 'boolean
  :group 'bv-git)

(defcustom bv-git-gutter-update-on-focus t
  "Update git-gutter when window gains focus."
  :type 'boolean
  :group 'bv-git)

;;;; Git Link
(use-package git-link
  :defer t
  :config
  ;; Add advice to support specific remote for commit links
  (advice-add
   'git-link :around
   (lambda (f remote start end)
     "`git-link--last-commit' advice with specific remote."
     (let ((git-link--last-commit-from-remote
            (lambda ()
              (car (git-link--exec
                    "--no-pager" "log" "-n1" "--pretty=format:%h"
                    (concat remote "/" (git-link--branch)))))))
       (advice-add 'git-link--last-commit :override
                   git-link--last-commit-from-remote)
       (funcall f remote start end)
       (advice-remove 'git-link--last-commit
                      git-link--last-commit-from-remote))))
  
  ;; Custom command that always includes commit hash
  (defun bv-git-link ()
    "Same as `git-link', but with commit hash specified."
    (interactive)
    (let ((git-link-use-commit t))
      (if (git-link--relative-filename)
          (call-interactively 'git-link)
        (call-interactively 'git-link-commit)))))

;;;; Git Timemachine
(use-package git-timemachine
  :defer t)

;;;; Git Gutter
(use-package git-gutter
  :defer t
  :diminish git-gutter-mode
  :init
  (when bv-git-enable-gutter
    (add-hook 'prog-mode-hook 'git-gutter-mode)
    (add-hook 'text-mode-hook 'git-gutter-mode))
  :custom
  (git-gutter:lighter " GG")
  :config
  ;; Update hooks
  (when bv-git-gutter-update-on-focus
    (add-to-list 'git-gutter:update-hooks 'focus-in-hook))
  (add-to-list 'git-gutter:update-commands 'other-window)
  
  ;; Update after magit operations
  (with-eval-after-load 'magit
    (add-hook 'magit-post-stage-hook 'git-gutter:update-all-windows)
    (add-hook 'magit-post-unstage-hook 'git-gutter:update-all-windows))
  
  ;; Use y-or-n-p for revert-hunk
  (defun bv-git-gutter-y-or-n-p (orig-fun &rest r)
    "Use y-or-n-p instead of yes-or-no-p."
    (cl-letf (((symbol-function 'yes-or-no-p) 'y-or-n-p))
      (apply orig-fun r)))
  
  (advice-add 'git-gutter:revert-hunk :around #'bv-git-gutter-y-or-n-p)
  
  ;; Auto-confirm staging (no prompts)
  (defun bv-git-gutter-auto-stage (orig-fun &rest args)
    "Auto-confirm git-gutter:stage-hunk."
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (apply orig-fun args)))
  
  (advice-add 'git-gutter:stage-hunk :around #'bv-git-gutter-auto-stage))

;;;; Git Gutter Fringe
(use-package git-gutter-fringe
  :after git-gutter
  :config
  ;; Custom fringe bitmaps
  (dolist (fringe '(git-gutter-fr:added
                    git-gutter-fr:modified))
    (define-fringe-bitmap fringe (vector 8) nil nil '(top repeat)))
  (define-fringe-bitmap 'git-gutter-fr:deleted
    (vector 8 12 14 15)
    nil nil 'bottom))

;;;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :custom
  ;; Performance optimizations
  (magit-refresh-status-buffer nil)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  ;; Show local branches after stashes
  (magit-status-sections-hook
   '(magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-local-branches
     magit-insert-unpushed-to-pushremote
     magit-insert-unpushed-to-upstream-or-recent
     magit-insert-unpulled-from-pushremote
     magit-insert-unpulled-from-upstream))
  :config
  ;; Clone directory logic
  (defun bv-get-local-repo-path-from-url (url)
    "Get directory from repository URL."
    (require 'git-link)
    (let* ((path (cadr (git-link--parse-remote url)))
           (dir (file-name-directory (directory-file-name path))))
      (if bv-git-project-directory
          (expand-file-name dir bv-git-project-directory)
        dir)))
  
  (setq magit-clone-default-directory 'bv-get-local-repo-path-from-url)
  
  ;; Performance for large repos
  (setq magit-revision-insert-related-refs nil))

;;;; Magit Todos
(use-package magit-todos
  :after magit
  :when bv-git-enable-todos
  :config
  (magit-todos-mode 1))

;;;; Transient History
(use-package transient
  :ensure nil
  :custom
  (transient-history-file
   (expand-file-name "transient/history.el" bv-cache-dir)))

;;;; Keybindings
(with-eval-after-load 'bv-core
  ;; Toggle git-gutter
  (define-key bv-toggle-map "g" 'git-gutter-mode)
  (define-key bv-toggle-map "G" 'global-git-gutter-mode))

;;;; Feature Definition
(defun bv-git-load ()
  "Load Git configuration."
  (add-to-list 'bv-enabled-features 'git)
  
  ;; Git aliases for eshell
  (with-eval-after-load 'em-alias
    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gs" "magit-status")
    (eshell/alias "gl" "magit-log-current")
    (eshell/alias "gp" "magit-push-current")
    (eshell/alias "gf" "magit-fetch")))

;;;; Integration with Other Features
(bv-with-feature project
  (with-eval-after-load 'project
    ;; Add magit-project-status to project commands
    (define-key project-prefix-map "m" 'magit-project-status)))

(bv-with-feature embark
  (with-eval-after-load 'embark
    (define-key embark-file-map "g" 'magit-file-dispatch)))

(provide 'bv-git)
;;; bv-git.el ends here
