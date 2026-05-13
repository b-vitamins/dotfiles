;;; bv-defaults.el --- Baseline Emacs operating defaults  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Baseline Emacs behavior before feature-specific modules take over.
;;
;; This file owns cross-cutting operating assumptions: quiet startup, editing
;; hygiene, session persistence, scrolling, cursor behavior, and role-aware
;; focus.  Package, language, terminal, and tree-sitter details belong in their
;; dedicated modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Declarations

(defvar auto-fill-mode)
(defvar auto-revert-verbose)
(defvar auto-save-file-name-transforms)
(defvar auto-window-vscroll)
(defvar backup-by-copying)
(defvar backup-directory-alist)
(defvar blink-cursor-blinks)
(defvar completion-styles)
(defvar confirm-nonexistent-file-or-buffer)
(defvar create-lockfiles)
(defvar cursor-in-non-selected-windows)
(defvar cursor-type)
(defvar custom-file)
(defvar delete-by-moving-to-trash)
(defvar delete-old-versions)
(defvar explicit-shell-file-name)
(defvar fast-but-imprecise-scrolling)
(defvar fill-column)
(defvar font-lock-maximum-decoration)
(defvar font-lock-maximum-size)
(defvar frame-title-format)
(defvar global-auto-revert-non-file-buffers)
(defvar history-delete-duplicates)
(defvar history-length)
(defvar hl-line-mode)
(defvar indicate-empty-lines)
(defvar inhibit-startup-echo-area-message)
(defvar inhibit-startup-message)
(defvar inhibit-startup-screen)
(defvar indent-tabs-mode)
(defvar initial-buffer-choice)
(defvar initial-major-mode)
(defvar initial-scratch-message)
(defvar kept-new-versions)
(defvar kept-old-versions)
(defvar kill-do-not-save-duplicates)
(defvar large-file-warning-threshold)
(defvar mouse-wheel-follow-mouse)
(defvar mouse-wheel-progressive-speed)
(defvar mouse-wheel-scroll-amount)
(defvar mouse-yank-at-point)
(defvar mode-require-final-newline)
(defvar org-return-follows-link)
(defvar pop-up-windows)
(defvar read-process-output-max)
(defvar recentf-auto-cleanup)
(defvar recentf-exclude)
(defvar recentf-max-menu-items)
(defvar recentf-max-saved-items)
(defvar redisplay-skip-fontification-on-input)
(defvar require-final-newline)
(defvar ring-bell-function)
(defvar save-interprogram-paste-before-kill)
(defvar save-place-file)
(defvar savehist-additional-variables)
(defvar savehist-autosave-interval)
(defvar savehist-file)
(defvar savehist-save-minibuffer-history)
(defvar scroll-conservatively)
(defvar scroll-margin)
(defvar scroll-preserve-screen-position)
(defvar select-enable-clipboard)
(defvar sentence-end-double-space)
(defvar set-mark-command-repeat-pop)
(defvar shell-file-name)
(defvar tab-width)
(defvar uniquify-after-kill-buffer-p)
(defvar uniquify-buffer-name-style)
(defvar uniquify-ignore-buffers-re)
(defvar uniquify-separator)
(defvar use-dialog-box)
(defvar use-file-dialog)
(defvar user-emacs-directory)
(defvar vc-follow-symlinks)
(defvar version-control)
(defvar visible-bell)
(defvar widget-image-enable)
(defvar x-underline-at-descent-line)

(declare-function bv-layout-buffer-role "bv-layout" ())
(declare-function cursor-intangible-mode "simple" (&optional arg))
(declare-function global-hl-line-mode "hl-line" (&optional arg))
(declare-function hl-line-mode "hl-line" (&optional arg))
(declare-function pixel-scroll-precision-mode "pixel-scroll" (&optional arg))
(declare-function xterm-mouse-mode "xt-mouse" (&optional arg))

;;; Customization

(defgroup bv-defaults nil
  "Baseline Emacs operating defaults."
  :group 'convenience
  :prefix "bv-defaults-")

(defcustom bv-defaults-cursor-type '(bar . 1)
  "Default cursor type for all frames."
  :type 'sexp
  :group 'bv-defaults)

(defcustom bv-defaults-hl-line-roles '(code navigation terminal)
  "Layout roles where persistent `hl-line-mode' should be enabled.
Wrapped prose, reading, Org, and documentation buffers deliberately stay out
of this list so a logical paragraph is not painted as one large block."
  :type '(repeat symbol)
  :group 'bv-defaults)

(defcustom bv-defaults-shell-file "/bin/zsh"
  "Preferred shell when the executable exists."
  :type 'file
  :group 'bv-defaults)

(defcustom bv-defaults-history-length 10000
  "Default minibuffer and savehist history length."
  :type 'integer
  :group 'bv-defaults)

(defcustom bv-defaults-recentf-max-items 1000
  "Maximum number of recent files to persist."
  :type 'integer
  :group 'bv-defaults)

(defcustom bv-defaults-backup-directory
  (expand-file-name "backups" user-emacs-directory)
  "Directory where backup files are stored."
  :type 'directory
  :group 'bv-defaults)

(defcustom bv-defaults-auto-save-directory
  (expand-file-name "auto-saves" user-emacs-directory)
  "Directory where auto-save files are stored."
  :type 'directory
  :group 'bv-defaults)

(defcustom bv-defaults-load-custom-faces nil
  "When non-nil, load face customizations from `custom-file'.

BV themes own the visual system.  Local Customize variables are fine, but
saved package faces can silently override the house theme and make one surface
look like it came from a different editor."
  :type 'boolean
  :group 'bv-defaults)

;;; Custom file

(defun bv-defaults-setup-custom-file ()
  "Keep Customize state outside the checked-in init files."
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (if bv-defaults-load-custom-faces
        (load custom-file 'noerror 'nomessage)
      (cl-letf (((symbol-function 'custom-set-faces) #'ignore))
        (load custom-file 'noerror 'nomessage)))))

;;; Quiet startup and chrome

(defun bv-defaults-setup-quiet-ui ()
  "Remove startup ceremony and legacy chrome."
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        initial-buffer-choice nil
        frame-title-format nil
        use-file-dialog nil
        use-dialog-box nil
        pop-up-windows nil
        indicate-empty-lines nil
        cursor-in-non-selected-windows nil
        initial-major-mode 'text-mode
        font-lock-maximum-decoration nil
        font-lock-maximum-size nil
        confirm-nonexistent-file-or-buffer nil
        completion-styles '(basic substring)
        widget-image-enable nil
        x-underline-at-descent-line t
        ring-bell-function #'ignore
        visible-bell nil)
  (when (fboundp 'scroll-bar-mode)
    (set-scroll-bar-mode nil))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (fset 'yes-or-no-p #'y-or-n-p))

;;; Text and editing

(defun bv-defaults-setup-text-editing ()
  "Configure baseline text and editing behavior."
  (setq-default indent-tabs-mode nil
                tab-width 4)
  (setq auto-fill-mode nil
        fill-column 80
        org-return-follows-link t
        sentence-end-double-space nil
        require-final-newline t
        mode-require-final-newline t
        delete-by-moving-to-trash t
        set-mark-command-repeat-pop t
        mouse-yank-at-point t
        select-enable-clipboard t
        save-interprogram-paste-before-kill t)
  (when (boundp 'kill-do-not-save-duplicates)
    (setq kill-do-not-save-duplicates t))
  (when (fboundp 'delete-selection-mode)
    (delete-selection-mode 1))
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'utf-8))

;;; Cursor and focus

(defun bv-defaults-apply-cursor (&optional frame)
  "Apply cursor defaults to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (setq cursor-type bv-defaults-cursor-type
          blink-cursor-blinks 0)))

(defun bv-defaults-buffer-role ()
  "Return the current buffer role, using `bv-layout' when available."
  (cond
   ((fboundp 'bv-layout-buffer-role)
    (bv-layout-buffer-role))
   ((derived-mode-p 'prog-mode 'conf-mode)
    'code)
   ((derived-mode-p 'dired-mode 'ibuffer-mode 'tabulated-list-mode)
    'navigation)
   ((derived-mode-p 'term-mode 'vterm-mode 'eat-mode 'eshell-mode 'shell-mode)
    'terminal)
   ((derived-mode-p 'text-mode 'help-mode 'Info-mode)
    'prose)
   (t
    'default)))

(defun bv-defaults-apply-focus-role ()
  "Enable persistent line focus only for roles that benefit from it."
  (when (fboundp 'hl-line-mode)
    (hl-line-mode
     (if (memq (bv-defaults-buffer-role) bv-defaults-hl-line-roles) 1 -1))))

(defun bv-defaults-refresh-focus ()
  "Re-apply role-aware focus to all live buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (bv-defaults-apply-focus-role))))

(defun bv-defaults-setup-focus ()
  "Configure cursor and role-aware focus behavior."
  (require 'hl-line)
  (add-to-list 'default-frame-alist `(cursor-type . ,bv-defaults-cursor-type))
  (add-hook 'after-make-frame-functions #'bv-defaults-apply-cursor)
  (bv-defaults-apply-cursor)
  (when (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
  (when (fboundp 'global-hl-line-mode)
    (global-hl-line-mode -1))
  (add-hook 'after-change-major-mode-hook #'bv-defaults-apply-focus-role)
  (bv-defaults-refresh-focus))

;;; TTY and scrolling

(defun bv-defaults-tty-setup ()
  "TTY-specific defaults for a first-class `emacs -nw' experience."
  (when (and (not (display-graphic-p))
             (fboundp 'xterm-mouse-mode))
    (xterm-mouse-mode 1))
  (when (and (not (display-graphic-p))
             (fboundp 'mouse-wheel-mode))
    (mouse-wheel-mode 1)))

(defun bv-defaults--maybe-enable-pixel-scroll (&optional frame)
  "Enable `pixel-scroll-precision-mode' for GUI FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p)
               (fboundp 'pixel-scroll-precision-mode))
      (pixel-scroll-precision-mode 1))))

(defun bv-defaults-setup-scrolling ()
  "Configure predictable scrolling."
  (setq scroll-conservatively 101
        scroll-margin 2
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        fast-but-imprecise-scrolling t
        redisplay-skip-fontification-on-input t
        mouse-wheel-follow-mouse t
        mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (add-hook 'tty-setup-hook #'bv-defaults-tty-setup)
  (bv-defaults-tty-setup)
  (add-hook 'after-make-frame-functions #'bv-defaults--maybe-enable-pixel-scroll)
  (bv-defaults--maybe-enable-pixel-scroll))

;;; Names, shell, and persistence

(defun bv-defaults-setup-buffer-names ()
  "Configure readable duplicate buffer names."
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(defun bv-defaults-setup-shell ()
  "Use the preferred shell when it exists."
  (when (and (not (eq system-type 'windows-nt))
             (file-exists-p bv-defaults-shell-file))
    (setq-default shell-file-name bv-defaults-shell-file)
    (setq explicit-shell-file-name bv-defaults-shell-file)))

(defun bv-defaults-setup-recentf ()
  "Configure recent file persistence."
  (require 'recentf)
  (setq recentf-max-menu-items 50
        recentf-max-saved-items bv-defaults-recentf-max-items
        recentf-auto-cleanup 'never
        recentf-exclude
        '("/tmp/"
          "/ssh:"
          "/sudo:"
          "/gnu/store/"
          "/nix/store/"
          "/\\.cache/"
          "COMMIT_EDITMSG"
          ".*-autoloads\\.el"
          "[/\\]\\elpa/"
          "[/\\]auto-saves[/\\]"
          "[/\\]backups[/\\]"))
  (recentf-mode 1))

(defun bv-defaults-setup-savehist ()
  "Configure minibuffer and command history persistence."
  (require 'savehist)
  (setq savehist-file (expand-file-name "history" user-emacs-directory)
        history-length bv-defaults-history-length
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-autosave-interval 300
        savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          extended-command-history
          command-history
          file-name-history
          query-replace-history
          read-expression-history
          minibuffer-history))
  (savehist-mode 1))

(defun bv-defaults-setup-saveplace ()
  "Remember point position in visited files."
  (require 'saveplace)
  (setq save-place-file (expand-file-name "places" user-emacs-directory))
  (save-place-mode 1))

(defun bv-defaults-setup-auto-revert ()
  "Refresh buffers when files change on disk."
  (require 'autorevert)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode 1))

;;; Files, performance, and safety

(defun bv-defaults-setup-files ()
  "Configure backup and auto-save hygiene."
  (dolist (dir (list bv-defaults-backup-directory
                     bv-defaults-auto-save-directory))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (setq backup-directory-alist
        `((".*" . ,bv-defaults-backup-directory))
        auto-save-file-name-transforms
        `((".*" ,(file-name-as-directory bv-defaults-auto-save-directory) t))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        create-lockfiles nil))

(defun bv-defaults-setup-performance ()
  "Configure conservative global performance defaults."
  (setq read-process-output-max (* 4 1024 1024)
        large-file-warning-threshold (* 100 1024 1024)
        vc-follow-symlinks t)
  (when (fboundp 'global-so-long-mode)
    (global-so-long-mode 1)))

(defun bv-defaults-setup-repeat ()
  "Enable repeatable command maps without startup noise."
  (when (fboundp 'repeat-mode)
    (let ((inhibit-message t)
          (message-log-max nil))
      (repeat-mode 1))))

(defun bv-defaults-setup-pairs ()
  "Enable simple pair insertion in programming buffers."
  (add-hook 'prog-mode-hook #'electric-pair-local-mode))

;;; Diagnostics

(defun bv-defaults-report ()
  "Show active defaults policy and state."
  (interactive)
  (with-help-window "*BV Defaults Report*"
    (princ "BV Defaults Report\n\n")
    (princ (format "buffer: %s\n" (buffer-name)))
    (princ (format "role: %s\n" (bv-defaults-buffer-role)))
    (princ (format "hl-line-mode: %s\n"
                   (if (bound-and-true-p hl-line-mode) "on" "off")))
    (princ (format "cursor-type: %S\n" cursor-type))
    (princ (format "scroll-margin: %S\n" scroll-margin))
    (princ (format "scroll-conservatively: %S\n" scroll-conservatively))
    (princ (format "history-length: %S\n" history-length))
    (princ (format "recentf max: %S\n" recentf-max-saved-items))
    (princ (format "savehist file: %s\n" savehist-file))
    (princ (format "save-place file: %s\n" save-place-file))
    (princ (format "backup directory: %s\n" bv-defaults-backup-directory))
    (princ (format "auto-save directory: %s\n" bv-defaults-auto-save-directory))
    (princ (format "shell: %s\n" shell-file-name))))

;;; Initialization

(defun bv-defaults-setup ()
  "Initialize BV baseline defaults."
  (bv-defaults-setup-custom-file)
  (bv-defaults-setup-quiet-ui)
  (bv-defaults-setup-text-editing)
  (bv-defaults-setup-focus)
  (bv-defaults-setup-scrolling)
  (bv-defaults-setup-buffer-names)
  (bv-defaults-setup-shell)
  (bv-defaults-setup-recentf)
  (bv-defaults-setup-savehist)
  (bv-defaults-setup-saveplace)
  (bv-defaults-setup-auto-revert)
  (bv-defaults-setup-files)
  (bv-defaults-setup-performance)
  (bv-defaults-setup-repeat)
  (bv-defaults-setup-pairs))

(bv-defaults-setup)

(provide 'bv-defaults)
;;; bv-defaults.el ends here
