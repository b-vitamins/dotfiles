;;; bv-project.el --- Project management configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for project management and navigation.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'project)
  (require 'cl-lib))


(defgroup bv-project nil
  "Custom `project.el' enhancements."
  :group 'bv)

(defcustom bv-project-dominating-files '()
  "List of root files that indicate a directory is a project."
  :group 'bv-project
  :type '(repeat string))

(cl-defmethod project-root ((project (head explicit)))
  "Determine the PROJECT root.
Argument PROJECT is a project instance with explicit root specification."
  (cdr project))

(defun bv-project-custom-root (dir)
  "Search in project's DIR for a set of project dominating files.
Checks for files listed in `bv-project-dominating-files' to determine
if DIR is within a project root.

Argument DIR is the directory to search from."
  (let* ((files bv-project-dominating-files)
         (root (seq-find
                (lambda (file)
                  (locate-dominating-file dir file))
                files)))
    (when root
      (cons 'explicit (locate-dominating-file dir root)))))

(defun bv-project-org-capture ()
  "Run `org-capture' in the current project root.
Changes to the project root directory and reads any directory-local
variables before launching `org-capture'."
  (interactive)
  (when-let ((default-dir (project-root (project-current t))))
    (dir-locals-read-from-dir default-dir)
    (org-capture)))

(defun bv-project-compile (&optional comint)
  "Compile current project and choose if buffer will be in COMINT mode.
Runs compilation from the project root directory with project-specific
buffer naming.

Optional argument COMINT when non-nil runs compilation in comint mode."
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively 'compile nil (and comint (vector (list 4))))))

(setq bv-project-dominating-files
      '(".project.el"
        ".dir-locals.el"
        ".gitignore"
        "manifest.scm"
        "main.tex"
        "Cargo.toml"
        "requirements.txt"
        "pyproject.toml"
        "package.yaml"
        "Makefile"))

(when (boundp 'project-find-functions)
  (add-hook 'project-find-functions 'project-try-vc -90)
  (add-hook 'project-find-functions 'bv-project-custom-root 50))

(advice-add 'project-compile :override 'bv-project-compile)

(when (and (boundp 'global-map) (boundp 'project-prefix-map))
  (define-key global-map (kbd "s-p") project-prefix-map))

(with-eval-after-load 'project
  (require 'xdg)
  (when (boundp 'project-switch-use-entire-map)
    (setq project-switch-use-entire-map t))
  (when (boundp 'project-list-file)
    (setq project-list-file (expand-file-name "emacs/projects" (xdg-cache-home))))
  
  (defun bv-compilation-buffer-name (mode)
    "Return project-prefixed buffer name if inside project, default otherwise.
If inside a project, uses `project-prefixed-buffer-name' to create
a buffer name, otherwise falls back to `compilation--default-buffer-name'.

Argument MODE is the major mode for the compilation buffer."
    (if (project-current)
        (project-prefixed-buffer-name mode)
      (compilation--default-buffer-name mode)))
  
  (when (boundp 'compilation-buffer-name-function)
    (setq compilation-buffer-name-function 'bv-compilation-buffer-name))
  (when (boundp 'project-compilation-buffer-name-function)
    (setq project-compilation-buffer-name-function 'bv-compilation-buffer-name))
  (when (boundp 'project-switch-commands)
    (setq project-switch-commands 'project-dired))
  
  (eval-when-compile (require 'consult))
  (with-eval-after-load 'consult
    (when (boundp 'project-prefix-map)
      (define-key project-prefix-map "F" 'consult-find)
      (define-key project-prefix-map "R" 'consult-ripgrep))
    (when (boundp 'consult-project-root-function)
      (setq consult-project-root-function
            (lambda ()
              (when-let (project (project-current))
                (project-root project)))))))

(provide 'bv-project)
;;; bv-project.el ends here