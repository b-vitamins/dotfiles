;;; bv-lang-python.el --- Python development environment -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Python development with eglot, pyright, and virtual environment support.

;;; Code:

;;;; Dependencies
(require 'bv-core)
(require 'bv-development)

;;;; Custom Variables
(defgroup bv-python nil
  "Python development configuration."
  :group 'bv-languages)

(defcustom bv-python-formatter 'black
  "Python code formatter to use."
  :type '(choice (const :tag "Black" black)
                 (const :tag "Ruff" ruff)
                 (const :tag "None" nil))
  :group 'bv-python)

(defcustom bv-python-format-on-save t
  "Format Python buffers on save."
  :type 'boolean
  :group 'bv-python)

(defcustom bv-python-shell-interpreter "python"
  "Default Python interpreter."
  :type 'string
  :group 'bv-python)

(defcustom bv-python-venv-auto-activate t
  "Automatically activate virtual environments."
  :type 'boolean
  :group 'bv-python)

(defcustom bv-python-pyenv-mode 'project
  "How to manage Python versions with pyenv."
  :type '(choice (const :tag "Global" global)
                 (const :tag "Project" project)
                 (const :tag "Manual" nil))
  :group 'bv-python)

;;;; Virtual Environment Detection
(defvar bv-python-venv-names '(".venv" "venv" ".env" "env")
  "Common virtual environment directory names.")

(defvar bv-python-venv-indicators
  '("pyvenv.cfg"           ; venv/virtualenv
    "conda-meta"           ; conda
    ".python-version"      ; pyenv
    "poetry.lock"          ; poetry
    "Pipfile.lock"         ; pipenv
    "requirements.txt"     ; pip
    "requirements.in"      ; pip-tools
    "setup.py"            ; setuptools
    "setup.cfg"           ; setuptools
    "pyproject.toml")     ; modern python
  "Files indicating a Python project root.")

(defun bv-python-find-project-root ()
  "Find Python project root by looking for indicator files."
  (let ((indicators (append bv-python-venv-indicators
                           bv-python-venv-names)))
    (locate-dominating-file
     default-directory
     (lambda (dir)
       (cl-some (lambda (name)
                  (file-exists-p (expand-file-name name dir)))
                indicators)))))

(defun bv-python-find-virtualenv ()
  "Find virtual environment for current project."
  (when-let ((root (bv-python-find-project-root)))
    (cond
     ;; Poetry environment
     ((file-exists-p (expand-file-name "poetry.lock" root))
      (bv-python-poetry-get-virtualenv root))
     ;; Standard venv in project
     ((cl-some (lambda (name)
                 (let ((venv-dir (expand-file-name name root)))
                   (when (file-directory-p venv-dir)
                     venv-dir)))
               bv-python-venv-names))
     ;; Conda environment
     ((getenv "CONDA_DEFAULT_ENV")
      (expand-file-name (getenv "CONDA_DEFAULT_ENV")
                        (or (getenv "CONDA_PREFIX")
                            (expand-file-name "~/miniconda3/envs/"))))
     ;; Pipenv
     ((file-exists-p (expand-file-name "Pipfile.lock" root))
      (bv-python-pipenv-get-virtualenv root)))))

(defun bv-python-poetry-get-virtualenv (root)
  "Get Poetry virtual environment path for ROOT."
  (let ((default-directory root))
    (condition-case nil
        (string-trim
         (shell-command-to-string "poetry env info --path"))
      (error nil))))

(defun bv-python-pipenv-get-virtualenv (root)
  "Get Pipenv virtual environment path for ROOT."
  (let ((default-directory root))
    (condition-case nil
        (string-trim
         (shell-command-to-string "pipenv --venv"))
      (error nil))))

;;;; Python Mode
(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyi\\'" . python-mode))
  :custom
  (python-shell-interpreter bv-python-shell-interpreter)
  (python-shell-completion-native-enable nil)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  ;; Auto-activate virtual environments
  (when bv-python-venv-auto-activate
    (add-hook 'python-mode-hook 'bv-python-auto-activate-venv))
  
  ;; Format on save
  (when bv-python-format-on-save
    (add-hook 'python-mode-hook 'bv-python-enable-format-on-save)))

;;;; Pyenv
(use-package pyenv-mode
  :when (executable-find "pyenv")
  :hook ((python-mode . (lambda ()
                          (when (eq bv-python-pyenv-mode 'project)
                            (pyenv-mode 1)))))
  :custom
  (pyenv-mode-set "3.11.0")
  :config
  (defun bv-python-set-project-version ()
    "Set Python version for current project."
    (interactive)
    (when-let ((root (bv-python-find-project-root)))
      (let ((default-directory root))
        (call-interactively 'pyenv-mode-set))))
  
  ;; Auto-detect .python-version files
  (defun bv-python-auto-pyenv ()
    "Automatically activate pyenv version if .python-version exists."
    (when-let ((root (bv-python-find-project-root))
               (version-file (expand-file-name ".python-version" root)))
      (when (file-exists-p version-file)
        (pyenv-mode-set (string-trim
                         (with-temp-buffer
                           (insert-file-contents version-file)
                           (buffer-string))))))))

;;;; Virtual Environment Management
(use-package pyvenv
  :defer t
  :config
  (defun bv-python-auto-activate-venv ()
    "Automatically activate virtual environment."
    (when-let ((venv (bv-python-find-virtualenv)))
      (pyvenv-activate venv)
      (message "Activated venv: %s" venv)))
  
  (defun bv-python-workon ()
    "Enhanced pyvenv-workon with project detection."
    (interactive)
    (if-let ((venv (bv-python-find-virtualenv)))
        (pyvenv-activate venv)
      (call-interactively 'pyvenv-workon))))

;;;; Poetry
(use-package poetry
  :when (executable-find "poetry")
  :hook (python-mode . (lambda ()
                         (when (locate-dominating-file
                                default-directory "poetry.lock")
                           (poetry 1))))
  :config
  (defun bv-python-poetry-shell ()
    "Start a poetry shell in the current project."
    (interactive)
    (let ((default-directory (bv-python-find-project-root)))
      (compile "poetry shell"))))

;;;; Pip-tools
(defun bv-python-pip-compile ()
  "Run pip-compile on requirements.in."
  (interactive)
  (when-let ((root (bv-python-find-project-root))
             (req-in (expand-file-name "requirements.in" root)))
    (when (file-exists-p req-in)
      (let ((default-directory root))
        (compile "pip-compile requirements.in")))))

(defun bv-python-pip-sync ()
  "Run pip-sync to install dependencies."
  (interactive)
  (when-let ((root (bv-python-find-project-root)))
    (let ((default-directory root))
      (compile "pip-sync"))))

;;;; Black Formatter
(use-package python-black
  :when (eq bv-python-formatter 'black)
  :defer t
  :config
  (defun bv-python-enable-black-on-save ()
    "Enable black formatting on save."
    (when (eq bv-python-formatter 'black)
      (python-black-on-save-mode 1))))

;;;; Ruff Formatter
(defun bv-python-ruff-format-buffer ()
  "Format current buffer with ruff."
  (interactive)
  (when (executable-find "ruff")
    (let ((tmp-file (make-temp-file "ruff-format" nil ".py")))
      (write-region nil nil tmp-file)
      (call-process "ruff" nil nil nil "format" tmp-file)
      (let ((formatted (with-temp-buffer
                        (insert-file-contents tmp-file)
                        (buffer-string))))
        (delete-region (point-min) (point-max))
        (insert formatted))
      (delete-file tmp-file))))

(defun bv-python-enable-ruff-on-save ()
  "Enable ruff formatting on save."
  (when (and (eq bv-python-formatter 'ruff)
             (executable-find "ruff"))
    (add-hook 'before-save-hook 'bv-python-ruff-format-buffer nil t)))

;;;; Common Format Setup
(defun bv-python-enable-format-on-save ()
  "Enable appropriate formatter on save."
  (cond
   ((eq bv-python-formatter 'black)
    (bv-python-enable-black-on-save))
   ((eq bv-python-formatter 'ruff)
    (bv-python-enable-ruff-on-save))))

;;;; Pytest
(use-package pytest
  :defer t
  :bind (:map python-mode-map
              ("C-c t t" . pytest-one)
              ("C-c t m" . pytest-module)
              ("C-c t ." . pytest-last-failed)
              ("C-c t x" . pytest-failed)
              ("C-c t a" . pytest-all))
  :custom
  (pytest-project-root-test 'bv-python-find-project-root))

;;;; Python Test (alternative)
(use-package python-test
  :defer t
  :bind (:map python-mode-map
              ("C-c t f" . python-test-function)
              ("C-c t c" . python-test-class)
              ("C-c t b" . python-test-buffer)
              ("C-c t p" . python-test-project)))

;;;; Eglot with Pyright
(with-eval-after-load 'eglot
  ;; Configure pyright as the Python LSP server
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  
  ;; Custom eglot configuration for Python
  (defun bv-python-eglot-config ()
    "Configure eglot for Python."
    (when (derived-mode-p 'python-mode)
      ;; Set workspace configuration
      (let ((venv (bv-python-find-virtualenv)))
        (when venv
          (setq-local eglot-workspace-configuration
                      `(:python (:venvPath ,venv
                                 :pythonPath ,(expand-file-name "bin/python" venv))
                        :pyright (:useLibraryCodeForTypes t)))))))
  
  (add-hook 'eglot-managed-mode-hook 'bv-python-eglot-config))

;;;; Enhanced Python REPL
(use-package python-pytest
  :defer t)

(defun bv-python-send-buffer-or-region ()
  "Send buffer or region to Python shell."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-buffer)))

(defun bv-python-restart-shell ()
  "Restart Python shell."
  (interactive)
  (when (get-buffer "*Python*")
    (kill-buffer "*Python*"))
  (run-python))

;;;; IPython/Jupyter
(use-package jupyter
  :defer t
  :config
  (defun bv-python-jupyter-notebook ()
    "Start Jupyter notebook in project root."
    (interactive)
    (let ((default-directory (bv-python-find-project-root)))
      (start-process "jupyter" "*jupyter*" "jupyter" "notebook"))))

;;;; Dir-locals Support
(defun bv-python-add-dir-locals ()
  "Add Python-specific dir-locals to current project."
  (interactive)
  (when-let ((root (bv-python-find-project-root)))
    (let ((dir-locals-file (expand-file-name ".dir-locals.el" root)))
      (with-temp-file dir-locals-file
        (insert "((python-mode . ((python-shell-interpreter . \"python\")\n")
        (insert "                 (python-shell-virtualenv-root . ")
        (insert (format \"\%s\" (bv-python-find-virtualenv)))
        (insert "))))\n")))))

;;;; Keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-z") 'run-python)
  (define-key python-mode-map (kbd "C-c C-c") 'bv-python-send-buffer-or-region)
  (define-key python-mode-map (kbd "C-c C-r") 'bv-python-restart-shell)
  (define-key python-mode-map (kbd "C-c v a") 'pyvenv-activate)
  (define-key python-mode-map (kbd "C-c v w") 'bv-python-workon)
  (define-key python-mode-map (kbd "C-c v d") 'pyvenv-deactivate)
  (define-key python-mode-map (kbd "C-c p v") 'bv-python-set-project-version)
  (define-key python-mode-map (kbd "C-c p c") 'bv-python-pip-compile)
  (define-key python-mode-map (kbd "C-c p s") 'bv-python-pip-sync))

;;;; Feature Definition
(defun bv-python-load ()
  "Load Python configuration."
  (add-to-list 'bv-enabled-features 'python)
  
  ;; Ensure pyright is available
  (unless (executable-find "pyright-langserver")
    (message "Warning: pyright-langserver not found. Install with: npm install -g pyright"))
  
  ;; Add python to org-babel
  (bv-with-feature org
    (with-eval-after-load 'org
      (add-to-list 'org-babel-load-languages '(python . t)))))

(provide 'bv-lang-python)
;;; bv-lang-python.el ends here
