;;; bv-defaults.el --- Better defaults -*- lexical-binding: t -*-

;;; Commentary:
;; Better default settings for Emacs
;; Keymap infrastructure, file handling, editing defaults

;;; Code:

(require 'bv-core)

;;;; Custom Variables

(defgroup bv-defaults nil
  "Better defaults configuration."
  :group 'bv)

(bv-defcustom bv-full-name user-full-name
  "User's full name for various Emacs features."
  :type 'string
  :group 'bv-defaults)

(bv-defcustom bv-email-address user-mail-address
  "User's email address."
  :type 'string
  :group 'bv-defaults)

(bv-defcustom bv-advanced-user nil
  "Non-nil if user is an advanced Emacs user."
  :type 'boolean
  :group 'bv-defaults)

(bv-defcustom bv-auto-update-buffers t
  "Non-nil to automatically revert buffers when files change."
  :type 'boolean
  :group 'bv-defaults)

(bv-defcustom bv-auto-clean-whitespace t
  "Non-nil to automatically clean trailing whitespace."
  :type 'boolean
  :group 'bv-defaults)

;;;; Keymap Infrastructure

(defvar bv-app-map nil
  "Prefix keymap for applications.")
(define-prefix-command 'bv-app-map)

(defvar bv-toggle-map nil
  "Prefix keymap for toggling functionality.")
(define-prefix-command 'bv-toggle-map)

;; Bind prefix maps
(global-set-key (kbd "C-c a") 'bv-app-map)
(global-set-key (kbd "C-c t") 'bv-toggle-map)

;;;; User Identity

(setq user-full-name bv-full-name
      user-mail-address bv-email-address)

;; Register values for other modules
(bv-set-value 'full-name bv-full-name)
(bv-set-value 'email bv-email-address)

;;;; File Handling

;; Cache directory setup
(defconst bv-cache-dir
  (expand-file-name "emacs" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for Emacs cache files.")

(unless (file-directory-p bv-cache-dir)
  (make-directory bv-cache-dir t))

;; Backup files
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" bv-cache-dir))))

;; Create backup directory if needed
(let ((backup-dir (expand-file-name "backup" bv-cache-dir)))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t)))

;; Auto-save files
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" bv-cache-dir))

;; Recent files
(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file
        (expand-file-name "recentf" bv-cache-dir))
  :config
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 200
        recentf-exclude '("/tmp/" "/ssh:" "\\.git/" "COMMIT_EDITMSG"))
  (recentf-mode 1)
  (run-with-idle-timer 30 t 'recentf-save-list))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :init
  (setq savehist-file
        (expand-file-name "history" bv-cache-dir))
  :config
  (setq history-length 10000
        savehist-save-minibuffer-history t
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (savehist-mode 1)
  (run-with-idle-timer 30 t 'savehist-save))

;; Bookmarks
(setq bookmark-default-file
      (expand-file-name "bookmarks" bv-cache-dir))

;; Save place in files
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file
        (expand-file-name "places" bv-cache-dir))
  :config
  (save-place-mode 1))

;; Custom file
(setq custom-file
      (expand-file-name "custom.el" bv-cache-dir))
(when (file-exists-p custom-file)
  (load custom-file t))

;;;; Native Compilation

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-jit-compilation nil)

;;;; Editing Defaults

;; Smooth scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; UI enhancements
(column-number-mode 1)
(show-paren-mode 1)
(global-subword-mode 1)

;; Better defaults
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)

;; Search improvements
(setq isearch-lazy-count t)
(setq search-whitespace-regexp ".*?")

;; Line wrapping
(setq-default truncate-lines nil)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Highlight special characters
(set-face-background 'glyphless-char "red")

;;;; Whitespace Handling

(defun bv-whitespace-mode ()
  "Show tabs in buffers."
  (interactive)
  (if (and (featurep 'whitespace) whitespace-mode)
      (whitespace-mode 0)
    (progn
      (setq-local whitespace-style '(face tabs))
      (whitespace-mode 1))))

;; Programming mode setup
(add-hook 'prog-mode-hook
          (lambda ()
            (bv-whitespace-mode)
            (setq show-trailing-whitespace t)))

;; Auto-cleanup whitespace
(when bv-auto-clean-whitespace
  (if (locate-library "ws-butler")
      (use-package ws-butler
        :hook ((text-mode prog-mode) . ws-butler-mode))
    (add-hook 'before-save-hook #'delete-trailing-whitespace)))

;;;; Window and Buffer Management

;; Kill region DWIM
(defun bv-kill-region-dwim (&optional count)
  "Kill region if active, otherwise kill word backward."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) 'region)
    (backward-kill-word count)))

;; Better buffer management
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Disable dialog boxes
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;;;; Keybindings

;; Window management (Linux-friendly)
(global-set-key (kbd "M-<f4>") 'kill-current-buffer)
(global-set-key (kbd "C-<tab>") 'other-window)

;; Text manipulation
(global-set-key (kbd "M-K") 'kill-whole-line)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "C-w") 'bv-kill-region-dwim)

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Navigation helpers
(let ((map goto-map))
  (define-key map "L" 'find-library)
  (define-key map "F" 'find-function)
  (define-key map "K" 'find-function-on-key)
  (define-key map "V" 'find-variable))

;; Disable suspend
(global-unset-key (kbd "C-z"))

;;;; Startup Configuration

;; Disable startup messages for advanced users
(when bv-advanced-user
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        initial-scratch-message nil))

;; Display load time
(defun bv-display-load-time ()
  "Display Emacs load time."
  (interactive)
  (message "Emacs loaded in %s, C-h r i for manual search, C-h C-a for About."
           (emacs-init-time)))

(add-hook 'emacs-startup-hook #'bv-display-load-time)

;;;; Auto-revert Configuration

(when bv-auto-update-buffers
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t))

;;;; Advanced User Settings

(when bv-advanced-user
  ;; Enable narrowing commands
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; Don't warn for large files
  (setq large-file-warning-threshold nil)

  ;; Don't warn for following symlinks
  (setq vc-follow-symlinks t)

  ;; Don't warn when advice is added
  (setq ad-redefinition-action 'accept))

;;;; Even Window Sizes

(setq even-window-sizes nil)

;;;; Ediff Configuration

(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; Minibuffer

(setq minibuffer-message-timeout 0)

;;;; Org Protocol Support

(bv-after-init
  (require 'org-protocol nil t))

;;;; Feature Registration

(bv-register-feature 'bv-defaults)

(provide 'bv-defaults)
;;; bv-defaults.el ends here
