;;; bv-defaults.el --- Better defaults for Emacs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (bv-core "0.1.0"))
;; Keywords: convenience, lisp, extensions
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/bv-defaults.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Better default settings for Emacs with modern conventions
;;
;; This module provides sensible defaults that make Emacs more
;; user-friendly out of the box while maintaining full compatibility
;; with traditional Emacs workflows.
;;
;; Main components:
;; - Smart file handling with XDG-compliant paths
;; - Modern editing defaults (no tabs, smart scrolling)
;; - Enhanced keybindings with dedicated prefix maps
;; - Automatic whitespace management
;; - Native compilation optimization
;; - Better minibuffer and prompt behavior
;; - Optional advanced user features
;;
;; Optional dependencies:
;; - ws-butler: Superior whitespace cleanup (preferred over built-in)
;; - expand-region: Smart region expansion
;; - helpful: Enhanced help buffers with source code
;; - pixel-scroll-precision-mode: Smooth scrolling (Emacs 29+)
;;
;; Keymap architecture:
;; This module exports several prefix keymaps for organization:
;; - `bv-toggle-map' (C-c t): Toggle various features
;; - `bv-defaults-buffer-map' (C-c b): Buffer operations
;; - `bv-defaults-window-map' (C-c w): Window management
;; - Uses existing `bv-app-map' from bv-core for applications
;;
;; Example usage:
;;   ;; Basic setup (loads automatically with bv-core)
;;   (require 'bv-defaults)
;;
;;   ;; Or use the convenience function
;;   (bv-enable-defaults)
;;
;;   ;; Customize behavior
;;   (setq bv-auto-clean-whitespace nil)  ; Disable auto cleanup
;;   (setq bv-advanced-user t)             ; Enable power features
;;   (setq bv-recentf-save-interval 300)  ; Save every 5 minutes
;;
;; Directory structure:
;; All cache/state files are stored under XDG_CACHE_HOME/emacs/:
;;   - backup/: Versioned backups
;;   - auto-save-list/: Auto-save files
;;   - undo/: Persistent undo (if undo-tree available)
;;   - transient/: Transient state
;;   - recentf: Recent file list
;;   - history: Minibuffer history
;;   - places: Saveplace data
;;   - bookmarks: Bookmark file
;;   - custom.el: Custom variables
;;
;; Performance notes:
;; - Directory creation is idempotent and cached
;; - Idle timers are used for periodic saves (recentf)
;; - Native compilation settings optimize for speed
;; - All timers are registered for cleanup with `bv-cancel-all-timers'
;;
;; Testing:
;; - Test suite available in bv-defaults-tests.el
;; - Run with: M-x ert RET ^bv-defaults-test- RET
;; - Tests cover directory creation, keybindings, and variable propagation
;;
;; Common customizations:
;;   (setq bv-full-name "Your Name")
;;   (setq bv-email-address "you@example.com")
;;   (setq bv-auto-update-buffers nil)  ; Disable auto-revert
;;   (setq bv-cache-directory "~/my-cache/emacs")  ; Custom cache
;;
;; Note: Custom variables use the `bv-' prefix for consistency with
;; other modules, while the customization group is `bv-defaults'.
;; This is intentional to avoid overly long variable names.

;;; Code:

(require 'bv-core)
(require 'subr-x)  ;; For string-empty-p and other utilities
(require 'help)    ;; For help-map

(require 'bv-core)
(require 'subr-x)  ;; For string-empty-p and other utilities
(require 'help)    ;; For help-map

;; Compatibility layer for Emacs 30+ changes
(eval-and-compile
  ;; Check if we're on Emacs 30+ or 29+ for different features
  (defconst bv-defaults--emacs30-p (and (boundp 'emacs-major-version)
                                         (>= emacs-major-version 30))
    "Non-nil if running on Emacs 30 or later.")

  (defconst bv-defaults--emacs29-p (and (boundp 'emacs-major-version)
                                         (>= emacs-major-version 29))
    "Non-nil if running on Emacs 29 or later.")

  ;; Compatibility wrapper for keymap-set (Emacs 29+)
  (defalias 'bv-defaults--keymap-set
    (if (fboundp 'keymap-set)
        #'keymap-set
      (lambda (keymap key definition)
        "Compatibility shim for `keymap-set'."
        (define-key keymap (kbd key) definition)))
    "Set KEY to DEFINITION in KEYMAP.")

  ;; Compatibility wrapper for keymap-global-set
  (defalias 'bv-defaults--keymap-global-set
    (if (fboundp 'keymap-global-set)
        #'keymap-global-set
      (lambda (key definition)
        "Compatibility shim for `keymap-global-set'."
        (global-set-key (kbd key) definition)))
    "Set KEY to DEFINITION in the global keymap.")

  ;; Compatibility wrapper for keymap-global-unset
  (defalias 'bv-defaults--keymap-global-unset
    (if (fboundp 'keymap-global-unset)
        #'keymap-global-unset
      (lambda (key)
        "Compatibility shim for `keymap-global-unset'."
        (global-unset-key (kbd key))))
    "Remove KEY from the global keymap.")

  ;; File name concat compatibility (from bv-core pattern)
  (defalias 'bv-defaults--file-name-concat
    (if (fboundp 'file-name-concat)
        #'file-name-concat
      (lambda (dir &rest components)
        "Join DIR and COMPONENTS using `expand-file-name'."
        (let ((result (file-name-as-directory dir)))
          (dolist (comp components result)
            (setq result (expand-file-name comp result))))))
    "Function to concatenate file path components."))

;; Declare external variables
(defvar bookmark-default-file)
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-jit-compilation)
(defvar native-comp-deferred-compilation)
(defvar global-auto-revert-non-file-buffers)
(defvar auto-revert-verbose)
(defvar user-full-name)
(defvar user-mail-address)
(defvar backup-directory-alist)
(defvar backup-by-copying)
(defvar version-control)
(defvar kept-new-versions)
(defvar kept-old-versions)
(defvar delete-old-versions)
(defvar vc-make-backup-files)
(defvar auto-save-list-file-prefix)
(defvar auto-save-file-name-transforms)
(defvar tab-always-indent)
(defvar require-final-newline)
(defvar auto-revert-use-notify)
(defvar auto-revert-check-vc-info)
(defvar isearch-lazy-count)
(defvar lazy-count-prefix-format)
(defvar lazy-count-suffix-format)
(defvar search-whitespace-regexp)
(defvar isearch-allow-scroll)
(defvar help-map)
(defvar split-width-threshold)
(defvar split-height-threshold)
(defvar even-window-sizes)
(defvar window-combination-resize)
(defvar completion-ignore-case)
(defvar read-file-name-completion-ignore-case)
(defvar read-buffer-completion-ignore-case)
(defvar use-dialog-box)
(defvar use-file-dialog)
(defvar ring-bell-function)
(defvar visible-bell)
(defvar enable-recursive-minibuffers)
(defvar minibuffer-depth-indicate-mode)
(defvar large-file-warning-threshold)
(defvar vc-follow-symlinks)
(defvar ad-redefinition-action)
(defvar warning-suppress-types)
(defvar inhibit-startup-screen)
(defvar inhibit-startup-message)
(defvar initial-scratch-message)
(defvar initial-major-mode)
(defvar history-length)
(defvar history-delete-duplicates)
(defvar compile-command)
(defvar kill-ring)
(defvar search-ring)
(defvar regexp-search-ring)
(defvar show-trailing-whitespace)
(defvar indent-tabs-mode)
(defvar truncate-lines)

;; Declare external variables from recentf
(defvar recentf-save-file)
(defvar recentf-max-menu-items)
(defvar recentf-max-saved-items)
(defvar recentf-exclude)

;; Declare external variables from savehist
(defvar savehist-file)
(defvar savehist-save-minibuffer-history)
(defvar savehist-additional-variables)

;; Declare external variables from saveplace
(defvar save-place-file)

;; Declare external functions
(declare-function recentf-save-list "recentf" ())
(declare-function recentf-mode "recentf" (&optional arg))
(declare-function savehist-mode "savehist" (&optional arg))
(declare-function save-place-mode "saveplace" (&optional arg))
(declare-function ws-butler-mode "ws-butler" (&optional arg))
(declare-function pixel-scroll-precision-mode "pixel-scroll" (&optional arg))
(declare-function helpful-callable "helpful" (symbol))
(declare-function helpful-variable "helpful" (symbol))
(declare-function helpful-key "helpful" (key-sequence))
(declare-function helpful-function "helpful" (symbol))
(declare-function expand-region "expand-region" ())
(declare-function er/expand-region "expand-region" (&optional arg))
(declare-function transient-append-suffix "transient" (prefix loc suffix))
(declare-function describe-keymap "help-fns" (keymap))
(declare-function dired "dired" (dirname &optional switches))
(declare-function bv-ensure-directory "bv-core" (dir))
(declare-function bv-set-value "bv-core" (key value))
(declare-function bv-after-init "bv-core" (&rest body))
(declare-function bv-when-feature "bv-core" (feature &rest body))
(declare-function bv-when-packages "bv-core" (packages &rest body))
(declare-function bv-register-feature "bv-core" (feature &optional dependencies))
(declare-function global-auto-revert-mode "autorevert" (&optional arg))
(declare-function bv-defcustom "bv-core" (name default docstring &rest args))
(declare-function column-number-mode "simple" (&optional arg))
(declare-function line-number-mode "simple" (&optional arg))
(declare-function show-paren-mode "paren" (&optional arg))
(declare-function global-subword-mode "subword" (&optional arg))
(declare-function delete-selection-mode "delsel" (&optional arg))
(declare-function ibuffer "ibuffer" (&optional other-window-p name qualifiers noselect shrink filter-groups formats))
(declare-function hippie-expand "hippie-exp" (arg))
(declare-function minibuffer-depth-indicate-mode "mb-depth" (&optional arg))
(declare-function untabify "tabify" (start end &optional _arg))
(declare-function delete-trailing-whitespace "simple" (&optional start end))
(declare-function kill-region "simple" (beg end &optional region))
(declare-function backward-kill-word "simple" (arg))
(declare-function capitalize-dwim "simple" (arg))
(declare-function downcase-dwim "simple" (arg))
(declare-function upcase-dwim "simple" (arg))
(declare-function visual-line-mode "simple" (&optional arg))
(declare-function cancel-timer "timer" (timer))
(declare-function run-with-idle-timer "timer" (secs repeat function &rest args))
(declare-function switch-to-buffer "window" (buffer-or-name &optional norecord force-same-window))
(declare-function other-buffer "buffer" (&optional buffer visible-ok frame))
(declare-function kill-buffer "buffer" (&optional buffer-or-name))
(declare-function other-window "window" (count &optional all-frames))
(declare-function split-window-below "window" (&optional size))
(declare-function split-window-right "window" (&optional size))
(declare-function delete-window "window" (&optional window))
(declare-function delete-other-windows "window" (&optional window))
(declare-function balance-windows "window" (&optional window-or-frame))
(declare-function revert-buffer "files" (&optional ignore-auto noconfirm preserve-modes))
(declare-function rename-buffer "buffer" (newname &optional unique))
(declare-function indent-rigidly "indent" (start end arg &optional interactive))
(declare-function display-buffer "window" (buffer-or-name &optional action frame))
(declare-function display-graphic-p "frame" (&optional display))

;;;; Directory Infrastructure Variables

(defvar bv-defaults--dirs-initialized nil
  "Non-nil when cache directories have been created.")

(defvar bv-defaults--cache-subdirs
  '("backup" "auto-save-list" "undo" "transient")
  "List of subdirectories to create under the cache directory.")

(defvar bv--recentf-setup-timer nil
  "Timer for periodic recentf saves.")

;; Initialize all custom variables with default values
;; This ensures they exist before any code tries to use them
(defvar bv-cache-directory (expand-file-name "emacs" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
(defvar bv-recentf-save-interval 30)
(defvar bv-full-name (or user-full-name ""))
(defvar bv-email-address (or user-mail-address ""))
(defvar bv-advanced-user nil)
(defvar bv-auto-update-buffers t)
(defvar bv-auto-clean-whitespace t)
(defvar bv-smooth-scrolling t)

;;;; Custom Variables

(defgroup bv-defaults nil
  "Better defaults configuration."
  :group 'bv
  :prefix "bv-defaults-")

;; Forward declaration of function used in defcustom
(declare-function bv-defaults--ensure-cache-directories "bv-defaults" ())

;; Define cache directory early since other settings depend on it
(bv-defcustom bv-cache-directory
  (expand-file-name "emacs" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for Emacs cache and state files.
Subdirectories will be created automatically for different file types."
  :type 'directory
  :safe #'stringp
  :set (lambda (sym val)
         (set-default sym val)
         (setq bv-defaults--dirs-initialized nil)
         (when (fboundp 'bv-defaults--ensure-cache-directories)
           (bv-defaults--ensure-cache-directories)))
  :group 'bv-defaults)

(bv-defcustom bv-full-name (or user-full-name "")
  "User's full name for various Emacs features.
This is used for copyright notices, email composition, and version
control systems."
  :type 'string
  :safe #'stringp
  :set (lambda (sym val)
         (set-default sym val)
         (setq user-full-name val))
  :group 'bv-defaults)

(bv-defcustom bv-email-address (or user-mail-address "")
  "User's email address.
Used for email composition, version control, and copyright notices."
  :type 'string
  :safe #'stringp
  :set (lambda (sym val)
         (set-default sym val)
         (setq user-mail-address val))
  :group 'bv-defaults)

(bv-defcustom bv-advanced-user nil
  "Non-nil if user is an advanced Emacs user.
This disables various warnings and enables advanced features like
narrowing commands and region case changes."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-defaults)

(bv-defcustom bv-auto-update-buffers t
  "Non-nil to automatically revert buffers when files change on disk.
This uses `global-auto-revert-mode' to keep buffers synchronized."
  :type 'boolean
  :safe #'booleanp
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (global-auto-revert-mode 1)
           (global-auto-revert-mode -1)))
  :group 'bv-defaults)

(bv-defcustom bv-auto-clean-whitespace t
  "Non-nil to automatically clean trailing whitespace on save.
Uses ws-butler if available for smart cleanup, otherwise uses
`delete-trailing-whitespace'."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-defaults)

(bv-defcustom bv-smooth-scrolling t
  "Non-nil to enable smooth pixel-based scrolling.
Only available in Emacs 29+ with `pixel-scroll-precision-mode'."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-defaults)

(bv-defcustom bv-recentf-save-interval 30
  "Seconds between automatic recentf saves.
Set to nil to disable automatic saving.  Common values are 30 (aggressive),
300 (5 minutes), or 600 (10 minutes)."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :safe (lambda (x) (or (null x) (integerp x)))
  :set (lambda (sym val)
         (set-default sym val)
         ;; Update the timer if recentf is loaded
         (when (and (featurep 'recentf)
                    bv--recentf-setup-timer)
           (cancel-timer bv--recentf-setup-timer)
           (setq bv--recentf-setup-timer nil)
           (when (and val (not noninteractive))
             (setq bv--recentf-setup-timer
                   (run-with-idle-timer val t #'recentf-save-list)))))
  :group 'bv-defaults)

;;;; Directory Infrastructure Functions

(defun bv-defaults--ensure-cache-directories ()
  "Ensure all cache directories exist.
This function is idempotent and will only create directories once.
Called automatically on load and when `bv-cache-directory' changes."
  (when (and (boundp 'bv-cache-directory)
             (not bv-defaults--dirs-initialized))
    (dolist (subdir bv-defaults--cache-subdirs)
      (bv-ensure-directory
       (bv-defaults--file-name-concat bv-cache-directory subdir)))
    (setq bv-defaults--dirs-initialized t)))

;; Initialize directories on load
(bv-defaults--ensure-cache-directories)

;;;; Keymap Infrastructure

;; Don't recreate bv-app-map - it's from bv-core
;; Create our own prefix maps with proper namespacing
(defvar bv-toggle-map (make-sparse-keymap)
  "Keymap for toggling functionality.
Common toggles are bound here for easy access.")

(defvar bv-defaults-buffer-map (make-sparse-keymap)
  "Keymap for buffer operations.
Quick buffer switching and management commands.")

(defvar bv-defaults-window-map (make-sparse-keymap)
  "Keymap for window operations.
Window splitting, deletion, and navigation commands.")

;; Bind prefix maps after init to avoid conflicts
(bv-after-init
  (unless (key-binding (kbd "C-c a"))
    (bv-defaults--keymap-global-set "C-c a" bv-app-map))
  (bv-defaults--keymap-global-set "C-c t" bv-toggle-map)
  (bv-defaults--keymap-global-set "C-c b" bv-defaults-buffer-map)
  (bv-defaults--keymap-global-set "C-c w" bv-defaults-window-map))

;; Export keymaps for other modules
(bv-set-value 'toggle-map bv-toggle-map)
(bv-set-value 'defaults-buffer-map bv-defaults-buffer-map)
(bv-set-value 'defaults-window-map bv-defaults-window-map)

;;;; User Identity Setup

;; Set user identity if configured
(when (and bv-full-name (not (string-empty-p bv-full-name)))
  (setq user-full-name bv-full-name))

(when (and bv-email-address (not (string-empty-p bv-email-address)))
  (setq user-mail-address bv-email-address))

;;;; File Handling Configuration

;; Backup configuration
(when (boundp 'bv-cache-directory)
  (setq backup-directory-alist
        `((".*" . ,(bv-defaults--file-name-concat bv-cache-directory "backup")))
        backup-by-copying t
        version-control t
        kept-new-versions 6
        kept-old-versions 2
        delete-old-versions t
        vc-make-backup-files t))

;; Auto-save configuration
(when (boundp 'bv-cache-directory)
  (setq auto-save-list-file-prefix
        (bv-defaults--file-name-concat bv-cache-directory "auto-save-list" ".saves-")
        auto-save-file-name-transforms
        `((".*" ,(bv-defaults--file-name-concat bv-cache-directory "auto-save-list" "") t))))

;; Recent files configuration
(with-eval-after-load 'recentf
  (when (boundp 'bv-cache-directory)
    (setq recentf-save-file (bv-defaults--file-name-concat bv-cache-directory "recentf")))
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 300
        recentf-exclude '("\\.git/" "COMMIT_EDITMSG" "/tmp/" "/ssh:" "/sudo:"
                          "/TAGS$" "/GTAGS$" "/GRAGS$" "/GPATH$"
                          "\\.elc$" "\\.el\\.gz$"))
  ;; Set up periodic save timer
  (when (and (not noninteractive)
             bv-recentf-save-interval)
    (when bv--recentf-setup-timer
      (cancel-timer bv--recentf-setup-timer))
    (setq bv--recentf-setup-timer
          (run-with-idle-timer bv-recentf-save-interval t #'recentf-save-list))))

(bv-when-feature recentf
  ;; Load recentf mode
  (recentf-mode 1))

;; Save minibuffer history
(with-eval-after-load 'savehist
  (when (boundp 'bv-cache-directory)
    (setq savehist-file (bv-defaults--file-name-concat bv-cache-directory "history")))
  (setq history-length 10000
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring compile-command)))

(bv-when-feature savehist
  (savehist-mode 1))

;; Save place in files
(with-eval-after-load 'saveplace
  (when (boundp 'bv-cache-directory)
    (setq save-place-file (bv-defaults--file-name-concat bv-cache-directory "places"))))

(bv-when-feature saveplace
  (save-place-mode 1))

;; Bookmarks - set path early to avoid prompts
(when (boundp 'bv-cache-directory)
  (setq bookmark-default-file
        (bv-defaults--file-name-concat bv-cache-directory "bookmarks")))
;; Also ensure it's set after bookmark loads
(with-eval-after-load 'bookmark
  (when (boundp 'bv-cache-directory)
    (setq bookmark-default-file
          (bv-defaults--file-name-concat bv-cache-directory "bookmarks"))))

;; Custom file
(when (boundp 'bv-cache-directory)
  (setq custom-file (bv-defaults--file-name-concat bv-cache-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage)))

;;;; Native Compilation

(when (featurep 'native-compile)
  ;; Suppress async warnings
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))

  ;; Enable JIT compilation
  (cond
   ;; Emacs 29+ uses native-comp-jit-compilation
   ((boundp 'native-comp-jit-compilation)
    (setq native-comp-jit-compilation t))
   ;; Older versions use native-comp-deferred-compilation
   ((boundp 'native-comp-deferred-compilation)
    (setq native-comp-deferred-compilation t))))

;;;; Core Editing Defaults

(setq-default
 ;; Indentation - spaces not tabs
 indent-tabs-mode nil
 tab-width 4
 tab-always-indent 'complete

 ;; Line endings
 require-final-newline t

 ;; Scrolling behavior
 scroll-step 1
 scroll-margin 2
 scroll-conservatively 101
 scroll-preserve-screen-position t

 ;; Display preferences
 truncate-lines nil
 word-wrap t
 fill-column 80)

;; Enable useful minor modes
(column-number-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(global-subword-mode 1)
(delete-selection-mode 1)

;; Auto-revert configuration
(when bv-auto-update-buffers
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        auto-revert-use-notify t
        auto-revert-check-vc-info t))

;; Smooth scrolling (Emacs 29+)
(bv-after-init
  (when (and bv-smooth-scrolling
             (fboundp 'pixel-scroll-precision-mode))
    (pixel-scroll-precision-mode 1)))

;; Better search defaults
(setq isearch-lazy-count t
      lazy-count-prefix-format "[%s/%s] "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?"
      isearch-allow-scroll 'unlimited)

;;;; Whitespace Handling

;;;###autoload
(defun bv-toggle-show-trailing-whitespace ()
  "Toggle visibility of trailing whitespace in current buffer.
When enabled, trailing spaces and tabs at line ends are highlighted.
Bound to `C-c t w' in the toggle map."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Show trailing whitespace: %s"
           (if show-trailing-whitespace "enabled" "disabled")))

;;;###autoload
(defun bv-cleanup-buffer-whitespace ()
  "Clean up whitespace issues in current buffer.
This removes trailing whitespace, converts tabs to spaces (unless
`indent-tabs-mode' is non-nil), and ensures the file ends with a
newline.
Bound to `C-c t W' in the toggle map."
  (interactive)
  (let ((start-point (point)))
    (unless indent-tabs-mode
      (untabify (point-min) (point-max)))
    (delete-trailing-whitespace)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (goto-char start-point)
    (message "Buffer whitespace cleaned")))

;; Auto-cleanup whitespace setup
(when bv-auto-clean-whitespace
  (bv-when-packages (ws-butler)
    ;; Prefer ws-butler for smart cleanup
    (add-hook 'text-mode-hook #'ws-butler-mode)
    (add-hook 'prog-mode-hook #'ws-butler-mode))

  ;; Fallback to simple cleanup if ws-butler not available
  (add-hook 'after-init-hook
            (lambda ()
              (unless (featurep 'ws-butler)
                (add-hook 'before-save-hook
                          (lambda ()
                            (unless (or (derived-mode-p 'diff-mode)
                                        (derived-mode-p 'markdown-mode))
                              (delete-trailing-whitespace))))))))

;; Show whitespace in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (setq truncate-lines t)))

;; Bind whitespace commands
(bv-defaults--keymap-set bv-toggle-map "w" #'bv-toggle-show-trailing-whitespace)
(bv-defaults--keymap-set bv-toggle-map "W" #'bv-cleanup-buffer-whitespace)

;;;; Buffer Management

;;;###autoload
(defun bv-kill-region-or-backward-word (&optional arg)
  "Kill region if active, otherwise kill word backward.
With prefix ARG, kill that many words backward.
This provides Ctrl-W behavior similar to terminals."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word (or arg 1))))

;;;###autoload
(defun bv-switch-to-previous-buffer ()
  "Switch to the most recently used buffer.
Ignores special buffers that start with space."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;###autoload
(defun bv-kill-current-buffer ()
  "Kill the current buffer without confirmation.
More convenient than `kill-buffer' for the common case."
  (interactive)
  (kill-buffer (current-buffer)))

;; Buffer operations
(bv-defaults--keymap-set bv-defaults-buffer-map "p" #'bv-switch-to-previous-buffer)
(bv-defaults--keymap-set bv-defaults-buffer-map "k" #'bv-kill-current-buffer)
(bv-defaults--keymap-set bv-defaults-buffer-map "K" #'kill-buffer)
(bv-defaults--keymap-set bv-defaults-buffer-map "r" #'revert-buffer)
(bv-defaults--keymap-set bv-defaults-buffer-map "R" #'rename-buffer)

;; Window operations
(bv-defaults--keymap-set bv-defaults-window-map "s" #'split-window-below)
(bv-defaults--keymap-set bv-defaults-window-map "v" #'split-window-right)
(bv-defaults--keymap-set bv-defaults-window-map "d" #'delete-window)
(bv-defaults--keymap-set bv-defaults-window-map "D" #'delete-other-windows)
(bv-defaults--keymap-set bv-defaults-window-map "b" #'balance-windows)
(bv-defaults--keymap-set bv-defaults-window-map "o" #'other-window)

;;;; Toggle Commands

;;;###autoload
(defun bv-toggle-auto-update-buffers ()
  "Toggle automatic buffer updating.
This affects `global-auto-revert-mode' and updates the
`bv-auto-update-buffers' setting.
Bound to `C-c t a' in the toggle map."
  (interactive)
  (setq bv-auto-update-buffers (not bv-auto-update-buffers))
  (bv-set-value 'auto-update-buffers bv-auto-update-buffers)
  (if bv-auto-update-buffers
      (progn
        (global-auto-revert-mode 1)
        (message "Auto-update buffers enabled"))
    (global-auto-revert-mode -1)
    (message "Auto-update buffers disabled")))

;;;###autoload
(defun bv-toggle-auto-clean-whitespace ()
  "Toggle automatic whitespace cleanup on save.
Updates the `bv-auto-clean-whitespace' setting.
Bound to `C-c t c' in the toggle map."
  (interactive)
  (setq bv-auto-clean-whitespace (not bv-auto-clean-whitespace))
  (bv-set-value 'auto-clean-whitespace bv-auto-clean-whitespace)
  (message "Auto-clean whitespace: %s"
           (if bv-auto-clean-whitespace "enabled" "disabled")))

;; Bind toggle commands
(bv-defaults--keymap-set bv-toggle-map "a" #'bv-toggle-auto-update-buffers)
(bv-defaults--keymap-set bv-toggle-map "c" #'bv-toggle-auto-clean-whitespace)

;;;; Essential Keybindings

;; Better defaults
(autoload 'hippie-expand "hippie-exp" nil t)
(autoload 'ibuffer "ibuffer" nil t)
(bv-defaults--keymap-global-set "C-x k" #'bv-kill-current-buffer)
(bv-defaults--keymap-global-set "C-w" #'bv-kill-region-or-backward-word)
(bv-defaults--keymap-global-set "M-/" #'hippie-expand)
(bv-defaults--keymap-global-set "C-x C-b" #'ibuffer)

;; Window navigation
(bv-defaults--keymap-global-set "C-<tab>" #'other-window)
(when (display-graphic-p)
  (autoload 'indent-rigidly "indent" nil t)
  (bv-defaults--keymap-global-set "C-x <tab>" #'indent-rigidly))

;; Text manipulation with dwim
(bv-defaults--keymap-global-set "M-c" #'capitalize-dwim)
(bv-defaults--keymap-global-set "M-l" #'downcase-dwim)
(bv-defaults--keymap-global-set "M-u" #'upcase-dwim)

;; Disable suspend-frame
(bv-defaults--keymap-global-unset "C-z")
(bv-defaults--keymap-global-unset "C-x C-z")

;; Help improvements
(when (fboundp 'describe-keymap)
  (define-key help-map "K" #'describe-keymap))

;; Expand region
(bv-when-packages (expand-region)
  (autoload 'er/expand-region "expand-region" nil t)
  (bv-defaults--keymap-global-set "C-=" #'er/expand-region))

;;;; UI Behavior

;; No dialog boxes
(setq use-dialog-box nil
      use-file-dialog nil)

;; No annoying bell
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Better prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Minibuffer improvements
(setq enable-recursive-minibuffers t)
(when (fboundp 'minibuffer-depth-indicate-mode)
  (minibuffer-depth-indicate-mode 1))

;; Window management preferences
(setq split-width-threshold 160
      split-height-threshold nil
      even-window-sizes nil
      window-combination-resize t)

;; Better completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;;; Advanced User Settings

(when bv-advanced-user
  ;; Enable narrowing commands
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  ;; Enable case change commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; Disable various warnings
  (setq large-file-warning-threshold nil
        vc-follow-symlinks t
        ad-redefinition-action 'accept
        warning-suppress-types '((comp)))

  ;; Minimal startup
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode))

;;;; Helpful Integration

(bv-when-packages (helpful)
  (autoload 'helpful-callable "helpful" nil t)
  (autoload 'helpful-variable "helpful" nil t)
  (autoload 'helpful-key "helpful" nil t)
  (autoload 'helpful-function "helpful" nil t)

  (bv-defaults--keymap-global-set "<remap> <describe-function>" #'helpful-callable)
  (bv-defaults--keymap-global-set "<remap> <describe-variable>" #'helpful-variable)
  (bv-defaults--keymap-global-set "<remap> <describe-key>" #'helpful-key)
  (bv-defaults--keymap-global-set "C-h F" #'helpful-function)

  ;; Improve helpful buffers
  (add-hook 'helpful-mode-hook #'visual-line-mode))

;;;; Development Support

;;;###autoload
(defun bv-defaults-reset-cache-dirs ()
  "Reset and recreate cache directories.
Useful after changing `bv-cache-directory' programmatically.
This ensures all subdirectories (backup, auto-save-list, etc.) exist
in the new location.

Also available via `M-x bv-menu-advanced' → Path Operations → Reset Cache Dirs."
  (interactive)
  (setq bv-defaults--dirs-initialized nil)
  (when (boundp 'bv-cache-directory)
    (bv-defaults--ensure-cache-directories)
    (message "Cache directories reset to: %s" bv-cache-directory)))

;;;###autoload
(defun bv-defaults-open-cache-dir ()
  "Open the cache directory in Dired.
This shows the directory where Emacs stores backups, auto-saves,
and other temporary files.

Also available via `M-x bv-menu-advanced' → Path Operations → Open Cache Dir."
  (interactive)
  (if (and (boundp 'bv-cache-directory) bv-cache-directory)
      (dired bv-cache-directory)
    (error "Cache directory not configured")))

;;;###autoload
(defun bv-defaults-report ()
  "Display a report of current defaults configuration."
  (interactive)
  (with-current-buffer (get-buffer-create "*bv-defaults-report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "BV Defaults Configuration Report\n")
      (insert "================================\n\n")

      (insert "User Configuration:\n")
      (insert (format "  Full Name: %s\n" (or bv-full-name "(not set)")))
      (insert (format "  Email: %s\n" (or bv-email-address "(not set)")))
      (insert (format "  Advanced User: %s\n" (if bv-advanced-user "yes" "no")))
      (insert (format "  Auto-update Buffers: %s\n" (if bv-auto-update-buffers "yes" "no")))
      (insert (format "  Auto-clean Whitespace: %s\n" (if bv-auto-clean-whitespace "yes" "no")))
      (insert (format "  Smooth Scrolling: %s\n" (if bv-smooth-scrolling "yes" "no")))
      (insert (format "  Recentf Save Interval: %s\n"
                      (if bv-recentf-save-interval
                          (format "%d seconds" bv-recentf-save-interval)
                        "disabled")))

      (insert "\nCache Directory:\n")
      (if (boundp 'bv-cache-directory)
          (progn
            (insert (format "  %s\n" bv-cache-directory))
            (dolist (subdir bv-defaults--cache-subdirs)
              (let ((path (bv-defaults--file-name-concat bv-cache-directory subdir)))
                (insert (format "    %s/: %s\n" subdir
                                (if (file-directory-p path) "exists" "missing"))))))
        (insert "  (not configured)\n"))

      (insert "\nActive Features:\n")
      (insert (format "  Native Compilation: %s\n" (if (featurep 'native-compile) "yes" "no")))
      (insert (format "  Pixel Scroll: %s\n"
                      (if (and (fboundp 'pixel-scroll-precision-mode)
                               (bound-and-true-p pixel-scroll-precision-mode))
                          "yes" "no")))
      (insert (format "  WS Butler: %s\n" (if (featurep 'ws-butler) "yes" "no")))
      (insert (format "  Helpful: %s\n" (if (featurep 'helpful) "yes" "no")))
      (insert (format "  Expand Region: %s\n" (if (featurep 'expand-region) "yes" "no")))

      (goto-char (point-min)))
    (display-buffer (current-buffer))))

;;;; Convenience Entry Point

;;;###autoload
(defun bv-enable-defaults ()
  "Enable better defaults for Emacs.
This is a convenience function for users who want to explicitly
load the defaults module."
  (interactive)
  (require 'bv-defaults)
  (message "BV defaults enabled"))

;;;; Menu Integration

;; Menu entries integration - disabled due to transient issues
;; TODO: Fix transient-append-suffix usage or use alternative approach
;; (with-eval-after-load 'bv-transient
;;   (require 'transient)
;;   ;; Add cache directory operations to path menu
;;   (when (fboundp 'transient-append-suffix)
;;     (transient-append-suffix 'bv-menu-advanced
;;       "pd"
;;       '("pc" "Open Cache Dir" bv-defaults-open-cache-dir
;;         :description "Open cache directory in Dired"))
;;     (transient-append-suffix 'bv-menu-advanced
;;       "pc"
;;       '("pr" "Reset Cache Dirs" bv-defaults-reset-cache-dirs
;;         :description "Recreate cache subdirectories"))))

;;;; Feature Registration

(bv-register-feature 'bv-defaults)

(provide 'bv-defaults)

;;; bv-defaults.el ends here
