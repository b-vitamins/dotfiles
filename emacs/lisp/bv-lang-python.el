;;; bv-lang-python.el --- Python development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Python development environment with tree-sitter, pyright/ruff-lsp, and comprehensive tooling.

;;; Code:

(require 'python)
(require 'eglot)
(require 'flymake)

;; External variables
(defvar python-ts-mode-map)
(defvar python-flymake)

;; Optional package loading
(autoload 'pyvenv-activate "pyvenv" nil t)
(autoload 'pyvenv-workon "pyvenv" nil t)
(autoload 'poetry-find-project-root "poetry" nil t)
(autoload 'poetry-venv-toggle "poetry" nil t)
(autoload 'python-pytest-dispatch "python-pytest" nil t)
(autoload 'python-black-buffer "python-black" nil t)

;; External functions
(declare-function python-info-current-defun "python" ())

;; Remap to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Python settings
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-shell-completion-native-enable nil) ; Avoid readline issues

;; Configure LSP servers - prefer ruff-lsp when available, fallback to pyright
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
                 ,(if (executable-find "ruff-lsp")
                      '("ruff-lsp")
                    '("pyright-langserver" "--stdio")))))

;; Python-specific eglot settings
(defun bv-python-eglot-config ()
  "Configure eglot for Python development."
  (setq-local eglot-workspace-configuration
              '(:python (:analysis (:autoSearchPaths t
                                   :useLibraryCodeForTypes t
                                   :diagnosticMode "workspace")
                        :venvPath nil))))

(add-hook 'python-ts-mode-hook #'bv-python-eglot-config)

;; Ruff integration for formatting and linting
(defun bv-python-ruff-format-buffer ()
  "Format current buffer with ruff."
  (interactive)
  (save-buffer)
  (shell-command-on-region (point-min) (point-max)
                          "ruff format -" nil t)
  (shell-command (concat "ruff check --fix " (buffer-file-name))))

(defun bv-python-ruff-format-on-save ()
  "Format buffer with ruff before saving."
  (when (eq major-mode 'python-ts-mode)
    (bv-python-ruff-format-buffer)))

;; Optional: Enable ruff format on save
;; (add-hook 'before-save-hook #'bv-python-ruff-format-on-save)

;; Flymake integration for ruff
(defun bv-python-setup-flymake-ruff ()
  "Setup flymake to use ruff for Python."
  (when (executable-find "ruff")
    (add-hook 'flymake-diagnostic-functions 'python-flymake nil t)))

(add-hook 'python-ts-mode-hook #'bv-python-setup-flymake-ruff)

;; Python shell configuration
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-i")

;; Enhanced Python REPL
(defun bv-python-send-region-or-buffer ()
  "Send region if active, otherwise send buffer to Python shell."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-buffer)))

;; Keybindings
(with-eval-after-load 'python
  (define-key python-ts-mode-map (kbd "C-c C-f") #'bv-python-ruff-format-buffer)
  (define-key python-ts-mode-map (kbd "C-c C-s") #'bv-python-send-region-or-buffer)
  (define-key python-ts-mode-map (kbd "C-c C-z") #'python-shell-switch-to-shell))

;; Virtual environment management
(defun bv-python-activate-venv ()
  "Activate Python virtual environment for current project."
  (interactive)
  (cond
   ;; Poetry project
   ((and (fboundp 'poetry-find-project-root)
         (poetry-find-project-root))
    (poetry-venv-toggle))
   ;; Standard venv
   ((file-exists-p ".venv")
    (pyvenv-activate ".venv"))
   ;; Prompt for venv
   ((fboundp 'pyvenv-workon)
    (call-interactively 'pyvenv-workon))
   (t
    (message "No virtual environment found"))))

;; Testing with pytest
(defun bv-python-pytest-current-file ()
  "Run pytest on current file."
  (interactive)
  (if (fboundp 'python-pytest-dispatch)
      (python-pytest-dispatch :current-file)
    (compile (format "pytest -xvs %s" (buffer-file-name)))))

(defun bv-python-pytest-current-function ()
  "Run pytest on current function."
  (interactive)
  (if (fboundp 'python-pytest-dispatch)
      (python-pytest-dispatch :current-defun)
    (let ((test-name (python-info-current-defun)))
      (when test-name
        (compile (format "pytest -xvs %s::%s" (buffer-file-name) test-name))))))

;; Mutation testing with mutmut
(defun bv-python-mutmut-file ()
  "Run mutmut on current file."
  (interactive)
  (compile (format "mutmut run --paths-to-mutate %s" (buffer-file-name))))

;; Profiling functions
(defun bv-python-profile-with-pyspy ()
  "Profile current script with py-spy."
  (interactive)
  (let ((output-file (concat (file-name-sans-extension (buffer-file-name)) ".svg")))
    (compile (format "py-spy record -o %s -- python %s" output-file (buffer-file-name)))))

(defun bv-python-line-profile ()
  "Run line profiler on current file."
  (interactive)
  (compile (format "kernprof -l -v %s" (buffer-file-name))))

;; Black formatting integration
(defun bv-python-format-buffer-black ()
  "Format buffer with python-black if available, else use ruff."
  (interactive)
  (cond
   ((fboundp 'python-black-buffer)
    (python-black-buffer))
   ((executable-find "black")
    (save-buffer)
    (shell-command-on-region (point-min) (point-max)
                            "black -" nil t))
   (t
    (bv-python-ruff-format-buffer))))

;; Enhanced keybindings
(with-eval-after-load 'python
  (define-key python-ts-mode-map (kbd "C-c t f") #'bv-python-pytest-current-file)
  (define-key python-ts-mode-map (kbd "C-c t t") #'bv-python-pytest-current-function)
  (define-key python-ts-mode-map (kbd "C-c t m") #'bv-python-mutmut-file)
  (define-key python-ts-mode-map (kbd "C-c p s") #'bv-python-profile-with-pyspy)
  (define-key python-ts-mode-map (kbd "C-c p l") #'bv-python-line-profile)
  (define-key python-ts-mode-map (kbd "C-c v a") #'bv-python-activate-venv)
  (define-key python-ts-mode-map (kbd "C-c C-b") #'bv-python-format-buffer-black))

(provide 'bv-lang-python)
;;; bv-lang-python.el ends here