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
(declare-function magit-status "magit" (&optional directory))
(declare-function dape "dape")
(defvar user-emacs-directory)

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
  "Run `org-capture' in project root."
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

(defun bv-project-ripgrep ()
  "Run ripgrep in the current project.

Uses `consult-ripgrep' when available, falling back to `project-find-regexp'."
  (interactive)
  (let ((root (project-root (project-current t))))
    (if (require 'consult nil t)
        (consult-ripgrep root)
      (let ((default-directory root))
        (call-interactively #'project-find-regexp)))))

(defun bv-project-consult-find ()
  "Run a fast file search in the current project.

Uses `consult-find' when available, falling back to `project-find-file'."
  (interactive)
  (let ((root (project-root (project-current t))))
    (if (require 'consult nil t)
        (consult-find root)
      (project-find-file))))

(defun bv-project-magit-status ()
  "Open `magit-status' in the current project."
  (interactive)
  (let ((root (project-root (project-current t))))
    (if (require 'magit nil t)
        (magit-status root)
      (user-error "Magit is not available"))))

(defun bv-project-dape ()
  "Start `dape' in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (if (require 'dape nil t)
        (call-interactively #'dape)
      (user-error "Dape is not available"))))

(defun bv-project--default-test-command (root)
  "Return a reasonable test command guess for project ROOT."
  (cond
   ((file-exists-p (expand-file-name "Cargo.toml" root))
    "cargo test")
   ((or (file-exists-p (expand-file-name "pyproject.toml" root))
        (file-exists-p (expand-file-name "pytest.ini" root))
        (file-exists-p (expand-file-name "setup.py" root)))
    "python -m pytest -q")
   ((file-exists-p (expand-file-name "mix.exs" root))
    "mix test")
   ((file-exists-p (expand-file-name "go.mod" root))
    "go test ./...")
   ((file-exists-p (expand-file-name "package.json" root))
    "npm test")
   ((or (file-exists-p (expand-file-name "package.yaml" root))
        (file-expand-wildcards (expand-file-name "*.cabal" root) t))
    "cabal test")
   ((file-exists-p (expand-file-name "Makefile" root))
    "make test")
   (t "make test")))

(defun bv-project-test (&optional prompt)
  "Run tests in the current project.

With prefix argument PROMPT, edit the suggested command."
  (interactive "P")
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (default-cmd (bv-project--default-test-command root))
         (cmd (if prompt
                  (read-shell-command "Test command: " default-cmd)
                default-cmd)))
    (compile cmd)))

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
            (bv-project-consult-find "Find (external)" ?F)
            (bv-project-ripgrep "Ripgrep" ?R)
            (project-find-regexp "Find regexp" ?g)
            (project-query-replace-regexp "Replace regexp" ?r)
            (project-dired "Dired" ?d)
            (project-shell "Shell" ?s)
            (project-eshell "Eshell" ?e)
            (project-vc-dir "VC dir" ?v)
            (project-compile "Build/compile" ?c)
            (bv-project-test "Test" ?t)
            (bv-project-magit-status "Magit" ?m)
            (bv-project-dape "Debug (dape)" ?z))))

  (with-eval-after-load 'consult
    (when (boundp 'consult-project-root-function)
      (setq consult-project-root-function
            (lambda ()
              (when-let (project (project-current))
                (project-root project)))))))

  (when (boundp 'project-prefix-map)
    (define-key project-prefix-map "F" 'bv-project-consult-find)
    (define-key project-prefix-map "R" 'bv-project-ripgrep)
    (define-key project-prefix-map "t" 'bv-project-test)
    (define-key project-prefix-map "m" 'bv-project-magit-status)
    (define-key project-prefix-map "z" 'bv-project-dape))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "p") 'project-switch-project)
    (define-key bv-app-map (kbd "P") 'bv-project-org-capture)))

(provide 'bv-project)
;;; bv-project.el ends here
