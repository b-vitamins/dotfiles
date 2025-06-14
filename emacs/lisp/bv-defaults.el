;;; bv-defaults.el --- Better defaults -*- lexical-binding: t -*-

;;; Commentary:
;; Better default settings for Emacs
;; Provides sensible defaults, keymap infrastructure, and core functionality

;;; Code:

(require 'bv-core)

;;;; External Variable Declarations
(defvar bookmark-default-file)
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-jit-compilation)
(defvar native-comp-deferred-compilation)
(defvar global-auto-revert-non-file-buffers)
(defvar auto-revert-verbose)

;;;; Function Declarations
(declare-function recentf-save-list "recentf" ())

;;;; Custom Variables

(defgroup bv-defaults nil
  "Better defaults configuration."
  :group 'bv
  :prefix "bv-")

(bv-defcustom bv-full-name (or user-full-name "")
  "User's full name for various Emacs features."
  :type 'string
  :group 'bv-defaults)

(bv-defcustom bv-email-address (or user-mail-address "")
  "User's email address."
  :type 'string
  :group 'bv-defaults)

(bv-defcustom bv-advanced-user nil
  "Non-nil if user is an advanced Emacs user.
This disables various warnings and enables advanced features."
  :type 'boolean
  :group 'bv-defaults)

(bv-defcustom bv-auto-update-buffers t
  "Non-nil to automatically revert buffers when files change on disk."
  :type 'boolean
  :group 'bv-defaults)

(bv-defcustom bv-auto-clean-whitespace t
  "Non-nil to automatically clean trailing whitespace on save."
  :type 'boolean
  :group 'bv-defaults)

;;;; Directory Infrastructure

(defconst bv-cache-dir
  (expand-file-name "emacs" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for Emacs cache files.")

;; Create all necessary subdirectories
(dolist (subdir '("backup" "auto-save-list" "undo" "transient"))
  (bv-ensure-directory (expand-file-name subdir bv-cache-dir)))

;;;; User Identity

(when (and bv-full-name (not (string-empty-p bv-full-name)))
  (setq user-full-name bv-full-name))

(when (and bv-email-address (not (string-empty-p bv-email-address)))
  (setq user-mail-address bv-email-address))

;;;; Keymap Infrastructure

(defvar bv-app-map (make-sparse-keymap)
  "Keymap for application commands.")

(defvar bv-toggle-map (make-sparse-keymap)
  "Keymap for toggling functionality.")

(defvar bv-buffer-map (make-sparse-keymap)
  "Keymap for buffer operations.")

(defvar bv-window-map (make-sparse-keymap)
  "Keymap for window operations.")

;; Bind prefix maps
(global-set-key (kbd "C-c a") bv-app-map)
(global-set-key (kbd "C-c t") bv-toggle-map)
(global-set-key (kbd "C-c b") bv-buffer-map)
(global-set-key (kbd "C-c w") bv-window-map)

;; Export keymaps for other modules
(bv-set-value 'app-map bv-app-map)
(bv-set-value 'toggle-map bv-toggle-map)
(bv-set-value 'buffer-map bv-buffer-map)
(bv-set-value 'window-map bv-window-map)

;;;; File Handling

;; Backup configuration
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup" bv-cache-dir)))
      backup-by-copying t
      version-control t
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t)

;; Auto-save configuration
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" bv-cache-dir)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" bv-cache-dir) t)))

;; Recent files
(use-package recentf
  :ensure nil
  :defer 1
  :custom
  (recentf-save-file (expand-file-name "recentf" bv-cache-dir))
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 300)
  (recentf-exclude '("\\.git/" "COMMIT_EDITMSG" "/tmp/" "/ssh:" "/sudo:"))
  :config
  (recentf-mode 1)
  (run-with-idle-timer 30 t #'recentf-save-list))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :custom
  (savehist-file (expand-file-name "history" bv-cache-dir))
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode 1))

;; Save place in files
(use-package saveplace
  :ensure nil
  :custom
  (save-place-file (expand-file-name "places" bv-cache-dir))
  :config
  (save-place-mode 1))

;; Bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" bv-cache-dir))

;; Custom file
(setq custom-file (expand-file-name "custom.el" bv-cache-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;;; Native Compilation

(when (featurep 'native-compile)
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))
  ;; Use the non-obsolete variable for Emacs 29+
  (if (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation t)
    (when (boundp 'native-comp-deferred-compilation)
      (setq native-comp-deferred-compilation t))))

;;;; Core Editing Defaults

;; Fundamental settings
(setq-default
 ;; Indentation
 indent-tabs-mode nil
 tab-width 4
 ;; Line endings
 require-final-newline t
 ;; Scrolling
 scroll-step 1
 scroll-margin 2
 scroll-conservatively 101
 scroll-preserve-screen-position t
 ;; Display
 truncate-lines nil
 word-wrap t)

;; Enable useful modes
(column-number-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(global-subword-mode 1)
(delete-selection-mode 1)

;; Auto-revert
(when bv-auto-update-buffers
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Smooth scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Better search
(setq isearch-lazy-count t
      search-whitespace-regexp ".*?"
      isearch-allow-scroll 'unlimited)

;;;; Whitespace Handling

(defun bv-show-trailing-whitespace ()
  "Show trailing whitespace in the current buffer."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show trailing whitespace: %s"
           (if show-trailing-whitespace "on" "off")))

;; Auto-cleanup whitespace
(when bv-auto-clean-whitespace
  (if (locate-library "ws-butler")
      (use-package ws-butler
        :ensure nil
        :hook ((text-mode prog-mode) . ws-butler-mode))
    (add-hook 'before-save-hook
              (lambda ()
                (unless (derived-mode-p 'diff-mode)
                  (delete-trailing-whitespace))))))

;; Show whitespace in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (setq truncate-lines t)))

;;;; Buffer Management

(defun bv-kill-region-or-backward-word (&optional arg)
  "Kill region if active, otherwise kill word backward.
With prefix ARG, kill that many words backward."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun bv-switch-to-previous-buffer ()
  "Switch to the previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Buffer operations
(define-key bv-buffer-map "p" #'bv-switch-to-previous-buffer)
(define-key bv-buffer-map "k" #'kill-current-buffer)
(define-key bv-buffer-map "r" #'revert-buffer)

;; Window operations
(define-key bv-window-map "s" #'split-window-below)
(define-key bv-window-map "v" #'split-window-right)
(define-key bv-window-map "d" #'delete-window)
(define-key bv-window-map "D" #'delete-other-windows)
(define-key bv-window-map "b" #'balance-windows)

;;;; Essential Keybindings

;; Better defaults
(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key (kbd "C-w") #'bv-kill-region-or-backward-word)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Window navigation
(global-set-key (kbd "C-<tab>") #'other-window)

;; Text manipulation
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)

;; Disable suspend
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Help improvements
(define-key help-map "K" #'describe-keymap)

;; Expand region
(use-package expand-region
  :ensure nil
  :bind ("C-=" . er/expand-region))

;;;; UI Behavior

;; No dialog boxes
(setq use-dialog-box nil
      use-file-dialog nil)

;; No bell
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Better prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Minibuffer
(setq enable-recursive-minibuffers t)

;; Window management
(setq split-width-threshold 160
      split-height-threshold nil
      even-window-sizes nil)

;;;; Advanced User Settings

(when bv-advanced-user
  ;; Enable narrowing
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; Enable case changes
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; Disable warnings
  (setq large-file-warning-threshold nil
        vc-follow-symlinks t
        ad-redefinition-action 'accept)

  ;; Minimal startup
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        initial-scratch-message nil))

;;;; Helpful

(use-package helpful
  :ensure nil
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("C-h F" . helpful-function))
  :config
  (add-hook 'helpful-mode-hook #'visual-line-mode))

;;;; Feature Registration

(bv-register-feature 'bv-defaults)

(provide 'bv-defaults)
;;; bv-defaults.el ends here
