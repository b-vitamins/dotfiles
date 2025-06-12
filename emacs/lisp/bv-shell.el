;;; bv-shell.el --- Shell and terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Shell and terminal configuration with per-project environments,
;; directory tracking, and modern terminal emulation.

;;; Code:

(require 'bv-core)

;;;; Custom Variables

(defgroup bv-shell nil
  "Shell and terminal configuration."
  :group 'bv)

(defcustom bv-shell-default-shell 'eshell
  "Default shell to use for shell commands."
  :type '(choice (const :tag "Eshell" eshell)
                 (const :tag "Shell" shell)
                 (const :tag "Eat" eat)
                 (const :tag "Vterm" vterm))
  :group 'bv-shell)

(defcustom bv-shell-pop-rule '(window-height . 0.3)
  "Display rule for shell popup windows."
  :type 'sexp
  :group 'bv-shell)

(defcustom bv-shell-history-size 10000
  "Number of history items to keep."
  :type 'integer
  :group 'bv-shell)

(defcustom bv-shell-per-project t
  "Create separate shell buffers per project."
  :type 'boolean
  :group 'bv-shell)

;;;; Comint Base Configuration

(use-package comint
  :hook ((comint-mode . bv-shell--comint-setup))
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 10000
        comint-input-ignoredups t
        comint-completion-addsuffix t
        comint-completion-autolist t
        comint-scroll-show-maximum-output nil
        comint-scroll-to-bottom-on-input 'this
        comint-scroll-to-bottom-on-output 'others
        comint-output-filter-functions
        '(ansi-color-process-output
          comint-watch-for-password-prompt
          comint-truncate-buffer))
  
  (defun bv-shell--comint-setup ()
    "Setup comint buffers."
    (ansi-color-for-comint-mode-on)
    (setq-local completion-at-point-functions
                '(comint-completion-at-point)))
  
  (defun bv-comint-clear-buffer ()
    "Clear comint buffer."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))
  
  (define-key comint-mode-map (kbd "C-c M-o") #'bv-comint-clear-buffer)
  
  ;; Integration with consult
  (bv-with-feature completion
    (defun bv-comint-history ()
      "Search comint history with consult."
      (interactive)
      (require 'consult)
      (consult-history comint-input-ring))
    
    (define-key comint-mode-map (kbd "M-r") #'bv-comint-history)))

;;;; Eshell Configuration

(use-package eshell
  :commands (eshell bv-project-eshell)
  :bind (("s-e" . bv-project-eshell)
         :map eshell-mode-map
         ("C-c M-o" . eshell/clear)
         ("s-e" . bv-eshell-toggle))
  :init
  (defconst bv-eshell-directory
    (expand-file-name "eshell" bv-var-dir)
    "Directory for eshell data.")
  
  (unless (file-directory-p bv-eshell-directory)
    (make-directory bv-eshell-directory t))
  
  :config
  (setq eshell-aliases-file (expand-file-name "alias" bv-eshell-directory)
        eshell-history-file-name (expand-file-name "history" bv-eshell-directory)
        eshell-last-dir-ring-file-name (expand-file-name "lastdir" bv-eshell-directory))
  
  (setq eshell-history-size bv-shell-history-size
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input 'this
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands '("vim" "vi" "screen" "top" "less" "more" "lynx"
                                 "ncftp" "pine" "tin" "trn" "elm" "htop")
        eshell-visual-subcommands '(("git" "log" "diff" "show"))
        eshell-prefer-lisp-functions nil
        eshell-prefer-lisp-variables nil)
  
  (defun bv-eshell-prompt ()
    "Generate eshell prompt."
    (concat
     (propertize (user-login-name) 'face 'font-lock-builtin-face)
     "@"
     (propertize (system-name) 'face 'font-lock-warning-face)
     " "
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face 'font-lock-constant-face)
     (when (and (executable-find "git")
                (locate-dominating-file default-directory ".git"))
       (concat " "
               (propertize
                (string-trim
                 (shell-command-to-string "git branch --show-current"))
                'face 'font-lock-keyword-face)))
     (propertize "\n❯ " 'face 'success)))
  
  (setq eshell-prompt-function #'bv-eshell-prompt
        eshell-prompt-regexp "^❯ ")
  
  (defun bv-eshell-setup-aliases ()
    "Setup useful eshell aliases."
    (eshell/alias "e" "find-file $1")
    (eshell/alias "ee" "find-file-other-window $1")
    (eshell/alias "d" "dired $1")
    (eshell/alias "ll" "ls -la")
    (eshell/alias "la" "ls -la")
    (eshell/alias "l" "ls -l")
    (eshell/alias "gs" "magit-status")
    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged"))
  
  (add-hook 'eshell-mode-hook #'bv-eshell-setup-aliases)
  
  (defun eshell/clear ()
    "Clear eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  
  (defun bv-project-eshell (&optional arg)
    "Open eshell for current project.
With prefix ARG, create new eshell buffer."
    (interactive "P")
    (if (and bv-shell-per-project (project-current))
        (let* ((default-directory (project-root (project-current)))
               (eshell-buffer-name (project-prefixed-buffer-name "eshell")))
          (eshell arg))
      (eshell arg)))
  
  (defun bv-eshell-toggle ()
    "Toggle between eshell and previous buffer."
    (interactive)
    (if (eq major-mode 'eshell-mode)
        (switch-to-prev-buffer)
      (bv-project-eshell)))
  
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local truncate-lines nil)
              (setq-local global-hl-line-mode nil)))
  
  ;; Integration with eat
  (bv-with-feature shell
    (when (and (executable-find "eat")
               (require 'eat nil t))
      (add-hook 'eshell-load-hook #'eat-eshell-mode)
      (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))))

;; Eshell syntax highlighting
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

;; Better eshell prompt
(use-package eshell-prompt-extras
  :after eshell
  :config
  (setq epe-git-dirty-char "±")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function #'epe-theme-lambda))

;;;; Eat - Modern Terminal Emulator

(use-package eat
  :commands (eat bv-project-eat)
  :bind (("C-x t" . bv-project-eat)
         :map eat-mode-map
         ("C-c M-o" . eat-clear))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size nil)
  (eat-enable-mouse t)
  :config
  (defun bv-project-eat (&optional arg)
    "Open eat terminal for current project.
With prefix ARG, create new terminal."
    (interactive "P")
    (if (and bv-shell-per-project (project-current))
        (let* ((default-directory (project-root (project-current)))
               (eat-buffer-name (project-prefixed-buffer-name "eat")))
          (eat nil arg))
      (eat nil arg)))
  
  (defun eat-clear ()
    "Clear eat terminal."
    (interactive)
    (eat-clear-screen)))

;;;; Shell Mode Configuration

(use-package shell
  :commands (shell bv-project-shell)
  :bind (("C-x s" . bv-project-shell))
  :config
  (setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *"
        shell-completion-execonly nil
        shell-completion-fignore '("~" "#" "%"))
  
  (defun bv-project-shell (&optional arg)
    "Open shell for current project.
With prefix ARG, create new shell."
    (interactive "P")
    (if (and bv-shell-per-project (project-current))
        (let* ((default-directory (project-root (project-current)))
               (shell-buffer-name (project-prefixed-buffer-name "shell")))
          (shell arg))
      (shell arg)))
  
  (add-hook 'shell-mode-hook #'shell-dirtrack-mode)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

;;;; Shell Script Support

(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.*rc\\'" . sh-mode))
  :config
  (setq sh-basic-offset 2
        sh-indentation 2
        sh-indent-comment nil
        sh-first-lines-indent nil)
  
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

;;;; Directory Tracking

(defun bv-shell-sync-dir-with-prompt ()
  "Sync shell directory with prompt."
  (when (memq major-mode '(shell-mode eshell-mode))
    (let ((dir (or (ignore-errors
                     (file-name-directory
                      (or (buffer-file-name)
                          list-buffers-directory
                          default-directory)))
                   default-directory)))
      (cd dir))))

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-output-filter-functions
                      #'bv-shell-sync-dir-with-prompt nil t)))

;;;; Visual Enhancements

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               (display-buffer-no-window)))

(defun bv-shell-command-with-compile (command &optional output-buffer error-buffer)
  "Execute COMMAND with output in compilation mode."
  (interactive (list (read-shell-command "Shell command: ")
                     current-prefix-arg
                     shell-command-default-error-buffer))
  (let ((compilation-buffer-name-function
         (lambda (_) (or output-buffer "*Shell Command Output*"))))
    (compile command)))

(global-set-key (kbd "M-&") #'bv-shell-command-with-compile)

;;;; Quick Shell Access

(defun bv-shell-popup ()
  "Pop up a shell in a small window."
  (interactive)
  (let ((shell-fn (cl-case bv-shell-default-shell
                    (eshell #'bv-project-eshell)
                    (eat #'bv-project-eat)
                    (shell #'bv-project-shell)
                    (t #'eshell))))
    (let ((display-buffer-alist
           `((,(rx bos "*" (or "eshell" "shell" "eat") "*")
              (display-buffer-in-side-window)
              (side . bottom)
              ,bv-shell-pop-rule))))
      (call-interactively shell-fn))))

(global-set-key (kbd "C-c !") #'bv-shell-popup)

;;;; Integration with Project System

(with-eval-after-load 'project
  (define-key project-prefix-map "s" #'bv-project-shell)
  (define-key project-prefix-map "e" #'bv-project-eshell)
  (define-key project-prefix-map "t" #'bv-project-eat)
  
  (add-to-list 'project-switch-commands
               '(bv-project-eshell "Eshell") t)
  (add-to-list 'project-switch-commands
               '(bv-project-shell "Shell") t))

;;;; Org Babel Shell Support

(bv-with-feature org
  (with-eval-after-load 'org
    (add-to-list 'org-structure-template-alist
                 '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist
                 '("bash" . "src bash")))
  
  (with-eval-after-load 'ob-core
    (require 'ob-shell)))

;;;; Feature Definition

(defun bv-shell-load ()
  "Load shell configuration."
  (add-to-list 'bv-enabled-features 'shell))

(provide 'bv-shell)
;;; bv-shell.el ends here
