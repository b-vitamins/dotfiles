;;; bv-project.el --- Project management configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Project management with custom root detection and consult integration.

;;; Code:

(require 'project)
(require 'seq)
(require 'cl-lib)
(declare-function consult-find "consult")
(declare-function consult-ripgrep "consult")
(declare-function project-prefixed-buffer-name "project")
(declare-function compilation--default-buffer-name "compile")
(declare-function org-capture "org-capture")

(defvar bv-project-dominating-files
  '(".project.el"
    ".dir-locals.el"
    ".gitignore"
    "manifest.scm"
    "main.tex"
    "Cargo.toml"
    "requirements.txt"
    "pyproject.toml"
    "package.yaml"
    "Makefile")
  "Files that indicate a directory is a project root.")

(cl-defmethod project-root ((project (head explicit)))
  "Return root directory for explicit PROJECT."
  (cdr project))

(defun bv-project-custom-root (dir)
  "Find project root from DIR using dominating files."
  (when-let ((root (seq-find
                    (lambda (file)
                      (locate-dominating-file dir file))
                    bv-project-dominating-files)))
    (cons 'explicit (locate-dominating-file dir root))))

(defun bv-project-org-capture ()
  "Run org-capture in project root."
  (interactive)
  (when-let ((default-dir (project-root (project-current t))))
    (dir-locals-read-from-dir default-dir)
    (org-capture)))

(defun bv-project-compile (&optional comint)
  "Compile in project root, optionally with COMINT mode."
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or (and (boundp 'project-compilation-buffer-name-function)
                  project-compilation-buffer-name-function)
             compilation-buffer-name-function)))
    (call-interactively 'compile nil (and comint (vector (list 4))))))

(when (boundp 'project-find-functions)
  (add-hook 'project-find-functions 'project-try-vc -90)
  (add-hook 'project-find-functions 'bv-project-custom-root 50))

(advice-add 'project-compile :override 'bv-project-compile)

(when (boundp 'project-prefix-map)
  (global-set-key (kbd "s-p") project-prefix-map))

(defun bv-compilation-buffer-name (mode)
  "Return project-aware buffer name for MODE."
  (if (project-current)
      (project-prefixed-buffer-name mode)
    (compilation--default-buffer-name mode)))

(with-eval-after-load 'project
  (when (boundp 'project-switch-use-entire-map)
    (setq project-switch-use-entire-map t))
  (when (boundp 'project-list-file)
    (setq project-list-file
          (expand-file-name "projects" user-emacs-directory)))
  (when (boundp 'project-vc-extra-root-markers)
    (setq project-vc-extra-root-markers bv-project-dominating-files))
  (when (boundp 'compilation-buffer-name-function)
    (setq compilation-buffer-name-function 'bv-compilation-buffer-name))
  (when (boundp 'project-compilation-buffer-name-function)
    (setq project-compilation-buffer-name-function 'bv-compilation-buffer-name))
  (when (boundp 'project-switch-commands)
    (setq project-switch-commands
          '((project-find-file "Find file" ?f)
            (project-find-regexp "Find regexp" ?g)
            (project-dired "Dired" ?d)
            (project-eshell "Eshell" ?e))))

  (with-eval-after-load 'consult
    (when (boundp 'project-prefix-map)
      (define-key project-prefix-map "F" 'consult-find)
      (define-key project-prefix-map "R" 'consult-ripgrep))
    (when (boundp 'consult-project-root-function)
      (setq consult-project-root-function
            (lambda ()
              (when-let (project (project-current))
                (project-root project)))))))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "p") 'project-switch-project)
    (define-key bv-app-map (kbd "P") 'bv-project-org-capture)))

(provide 'bv-project)
;;; bv-project.el ends here