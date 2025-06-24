;;; bv-ui.el --- Appearance and UI configuration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (bv-core "0.1.0"))
;; Keywords: convenience, faces, themes
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/bv-ui.el

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
;; UI configuration with automatic theme switching
;;
;; This module provides:
;; - Automatic theme switching based on time of day
;; - Window decoration configuration (margins, fringes, dividers)
;; - Mode line configuration with minions
;; - Font configuration
;; - Optional icon support via nerd-icons
;; - Which-key for keybinding discovery
;;
;; Main components:
;; - Theme system: Modus themes with automatic switching
;; - Window chrome: Configurable margins, fringes, and dividers
;; - Mode line: Header-line option and minor mode management
;; - Font setup: Font family and size configuration
;; - Icon support: Optional nerd-icons integration
;; - Help system: which-key popup for key sequences
;;
;; Dependencies:
;; - modus-themes: Built-in since Emacs 28
;; - which-key: Built-in since Emacs 28
;; - minions: Built-in since Emacs 28
;;
;; Optional dependencies:
;; - nerd-icons: Icon set (requires Nerd Font)
;; - nerd-icons-ibuffer: Icons in ibuffer
;;
;; Usage:
;;   (require 'bv-ui)
;;
;;   ;; Or with use-package:
;;   (use-package bv-ui
;;     :after bv-core
;;     :config
;;     ;; Custom theme switching hours
;;     (setq bv-ui-theme-switch-hours
;;           '((6 . light)      ; 6 AM: light theme
;;             (18 . dark)))    ; 6 PM: dark theme
;;
;;     ;; Use custom font
;;     (setq bv-ui-font-family "JetBrains Mono"
;;           bv-ui-font-size 12))
;;
;; Theme switching:
;; The theme system switches between light and dark themes based on
;; the time of day.  Four theme variants are supported:
;; - light: Standard light theme
;; - light-soft: Softer light theme
;; - dark: Standard dark theme
;; - dark-soft: Softer dark theme
;;
;; Environment variables:
;; - EMACS_THEME_VARIANT: Override theme (light/dark/auto)
;; - EMACS_FONT_FAMILY: Override font family
;; - EMACS_FONT_SIZE: Override font size
;;
;; Keybindings (via toggle-map from bv-defaults):
;; - C-c t t: Toggle theme manually
;; - C-c t T: Toggle automatic theme switching
;; - C-c t h: Toggle header-line as mode-line
;; - C-c t d: Toggle frame decorations
;;
;; Common customizations:
;;   ;; Deuteranopia-friendly colors
;;   (setq bv-ui-theme-deuteranopia t)
;;
;;   ;; Scale org headings
;;   (setq bv-ui-theme-headings-scaling t)
;;
;;   ;; Keep more minor modes visible
;;   (setq bv-ui-prominent-modes
;;         '(flymake-mode projectile-mode lsp-mode))
;;
;;   ;; Faster which-key popup
;;   (setq bv-ui-which-key-idle-delay 0.5)

;;; Code:

(require 'bv-core)
(require 'cl-lib)

;; Compatibility layer for Emacs 30+ changes
(eval-and-compile
  ;; Import compatibility functions from bv-core
  (defalias 'bv-ui--sort-list
    (if (fboundp 'bv--sort-list)
        'bv--sort-list
      ;; Fallback if bv-core not loaded yet
      (if (and (boundp 'emacs-version)
               (string-match "^30\\." emacs-version))
          ;; Emacs 30+ with keyword API
          (lambda (list predicate)
            (sort (copy-sequence list) :lessp predicate))
        ;; Emacs < 30
        (lambda (list predicate)
          (sort (copy-sequence list) predicate))))
    "Compatibility wrapper for sort function.")

  (defalias 'bv-ui--find-if
    (if (fboundp 'bv--find-if)
        'bv--find-if
      ;; Direct cl-find-if for older Emacs
      'cl-find-if)
    "Compatibility wrapper for cl-find-if."))

;;;; External Variable Declarations

(defvar display-time-format)
(defvar display-time-24hr-format)
(defvar display-time-default-load-average)
(defvar display-time-interval)
(defvar modus-themes-mode-line)
(defvar modus-themes-italic-constructs)
(defvar modus-themes-bold-constructs)
(defvar modus-themes-mixed-fonts)
(defvar modus-themes-common-palette-overrides)
(defvar modus-themes-headings)
(defvar minions-mode-line-lighter)
(defvar minions-mode-line-delimiters)
(defvar minions-prominent-modes)
(defvar which-key-idle-delay)
(defvar which-key-min-display-lines)
(defvar which-key-separator)
(defvar which-key-prefix-prefix)
(defvar which-key-popup-type)
(defvar which-key-side-window-location)
(defvar which-key-side-window-max-height)
(defvar nerd-icons-font-family)
(defvar nerd-icons-color-icons)
(defvar nerd-icons-ibuffer-icon)
(defvar nerd-icons-ibuffer-color-icon)
(defvar nerd-icons-ibuffer-icon-size)
(defvar ibuffer-human-readable-size)
(defvar mode-line-compact)
(defvar window-divider-default-right-width)
(defvar custom-enabled-themes)
(defvar custom-known-themes)
(defvar split-width-threshold)
(defvar split-height-threshold)

;;;; Function Declarations

(declare-function modus-themes-toggle "modus-themes" ())
(declare-function modus-themes-with-colors "modus-themes" (&rest body))
(declare-function modus-themes-load-theme "modus-themes" (theme))
(declare-function nerd-icons-install-fonts "nerd-icons" (&optional arg))
(declare-function display-time-mode "time" (&optional arg))
(declare-function minions-mode "minions" (&optional arg))
(declare-function which-key-mode "which-key" (&optional arg))
(declare-function nerd-icons-ibuffer-mode "nerd-icons-ibuffer" (&optional arg))
(declare-function hl-line-mode "hl-line" (&optional arg))
(declare-function display-line-numbers-mode "display-line-numbers" (&optional arg))
(declare-function menu-bar-mode "menu-bar" (&optional arg))
(declare-function tool-bar-mode "tool-bar" (&optional arg))
(declare-function scroll-bar-mode "scroll-bar" (&optional arg))
(declare-function blink-cursor-mode "frame" (&optional arg))
(declare-function window-divider-mode "frame" (&optional arg))
(declare-function column-number-mode "simple" (&optional arg))
(declare-function line-number-mode "simple" (&optional arg))
(declare-function show-paren-mode "paren" (&optional arg))
(declare-function global-subword-mode "subword" (&optional arg))
(declare-function delete-selection-mode "delsel" (&optional arg))
(declare-function font-family-list "font" (&optional frame))
(declare-function find-font "font" (font-spec &optional frame))
(declare-function custom-enabled-themes "custom" ())
(declare-function disable-theme "custom" (theme))
(declare-function load-theme "custom" (theme &optional no-confirm no-enable))
(declare-function run-at-time "timer" (time repeat function &rest args))
(declare-function cancel-timer "timer" (timer))
(declare-function special-mode "simple" ())

;;;; Customization Group

;;;###autoload
(defgroup bv-ui nil
  "UI and appearance configuration."
  :group 'bv
  :prefix "bv-ui-")

;;;; Custom Variables - Appearance

(bv-defcustom bv-ui-margin 8
  "Internal border width for frames in pixels.
This adds padding between the frame edge and buffer content."
  :type 'integer
  :safe #'integerp
  :group 'bv-ui)

(bv-defcustom bv-ui-fringes 8
  "Width of window fringes in pixels.
Fringes are the narrow areas on either side of windows that display
indicators like continuation arrows and breakpoints."
  :type 'integer
  :safe #'integerp
  :group 'bv-ui)

(bv-defcustom bv-ui-mode-line-padding 4
  "Padding for mode line in pixels.
Adds vertical space around mode line content."
  :type 'integer
  :safe #'integerp
  :group 'bv-ui)

(bv-defcustom bv-ui-header-line-padding 4
  "Padding for header line in pixels.
Adds vertical space around header line content."
  :type 'integer
  :safe #'integerp
  :group 'bv-ui)

(bv-defcustom bv-ui-tab-bar-padding 4
  "Padding for tab bar in pixels.
Adds vertical space around tab bar content."
  :type 'integer
  :safe #'integerp
  :group 'bv-ui)

(bv-defcustom bv-ui-header-line-as-mode-line t
  "Move mode line to header line position.
When non-nil, the mode line appears at the top of windows
instead of the bottom."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-ui)

(bv-defcustom bv-ui-undecorated-frame t
  "Remove window title bar.
When non-nil, frames are created without decoration.
Note: Changing this may require creating new frames to take effect."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-ui)

;;;; Custom Variables - Theme

(bv-defcustom bv-ui-theme-auto-switch
    (let ((variant (getenv "EMACS_THEME_VARIANT")))
      (not (member variant '("light" "dark"))))
  "Automatically switch themes based on time of day.
When non-nil, themes change according to `bv-ui-theme-switch-hours'.
Can be overridden by EMACS_THEME_VARIANT environment variable:
- \"light\": Always use light theme
- \"dark\": Always use dark theme
- \"auto\" or unset: Use automatic switching"
  :type 'boolean
  :safe #'booleanp
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-deuteranopia nil
  "Use deuteranopia-friendly colors (red/green color blindness).
When non-nil, uses theme variants optimized for deuteranopia."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-headings-scaling nil
  "Scale headings to different sizes.
When non-nil, `org-mode' and outline headings use varying font sizes."
  :type 'boolean
  :safe #'booleanp
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-switch-hours
    '((5 . light-soft)    ; 5 AM: soft light
      (10 . light)        ; 10 AM: full light
      (18 . dark-soft)    ; 6 PM: soft dark
      (23 . dark))        ; 11 PM: full dark
  "Hours and theme types for automatic switching.
Each element is (HOUR . TYPE) where TYPE is one of:
- `light': Standard light theme
- `light-soft': Softer light theme
- `dark': Standard dark theme
- `dark-soft': Softer dark theme

The theme active at a given time is the one with the latest
hour less than or equal to the current hour."
  :type '(alist :key-type (integer :tag "Hour (0-23)")
                :value-type (choice (const :tag "Light" light)
                                    (const :tag "Light Soft" light-soft)
                                    (const :tag "Dark" dark)
                                    (const :tag "Dark Soft" dark-soft)))
  :safe (lambda (val)
          (and (listp val)
               (cl-every (lambda (entry)
                           (and (consp entry)
                                (integerp (car entry))
                                (<= 0 (car entry) 23)
                                (memq (cdr entry) '(light light-soft dark dark-soft))))
                         val)))
  :group 'bv-ui)

;;;; Custom Variables - Font

(bv-defcustom bv-ui-font-family (or (getenv "EMACS_FONT_FAMILY") "SF Mono")
  "Font family to use.
Set to nil to use system default.  The font must be installed
on your system.  Common monospace fonts include:
- \"SF Mono\" (macOS)
- \"Consolas\" (Windows)
- \"DejaVu Sans Mono\" (Linux)
- \"JetBrains Mono\" (cross-platform)
- \"Fira Code\" (with ligatures)

Can be overridden by EMACS_FONT_FAMILY environment variable."
  :type '(choice (const :tag "System default" nil)
                 (string :tag "Font family"))
  :safe (lambda (val) (or (null val) (stringp val)))
  :group 'bv-ui)

(bv-defcustom bv-ui-font-size
    (let ((env-size (getenv "EMACS_FONT_SIZE")))
      (if (and env-size (string-match "^[0-9]+$" env-size))
          (string-to-number env-size)
        11))
  "Font size in points.
Typical values range from 9 to 14 depending on display DPI.
Can be overridden by EMACS_FONT_SIZE environment variable."
  :type 'integer
  :safe #'integerp
  :group 'bv-ui)

;;;; Custom Variables - Which-key

(bv-defcustom bv-ui-which-key-idle-delay 1.0
  "Seconds to wait before showing which-key popup.
Lower values show help more quickly but may be distracting."
  :type 'number
  :safe #'numberp
  :group 'bv-ui)

;;;; Custom Variables - Minions

(bv-defcustom bv-ui-prominent-modes
    '(flymake-mode
      flycheck-mode
      eglot-mode
      lsp-mode
      projectile-mode
      envrc-mode
      compilation-minor-mode)
  "Minor modes that should remain visible in the mode line.
Other minor modes will be hidden behind the minions menu."
  :type '(repeat symbol)
  :safe (lambda (val) (and (listp val) (cl-every #'symbolp val)))
  :group 'bv-ui)

;;;; Internal Variables

(defvar bv-ui--theme-timers nil
  "List of active theme switching timers.")

(defvar bv-ui--current-theme nil
  "Currently active theme.")

(defvar bv-ui--original-mode-line-format nil
  "Store the original `mode-line-format' for restoration.")

;;;; Font Setup

;;;###autoload
(defun bv-ui-setup-fonts ()
  "Set up fonts if configured.
Applies the font specified by `bv-ui-font-family' and `bv-ui-font-size'
to the default face.  Safe to call multiple times."
  (when bv-ui-font-family
    (when (find-font (font-spec :family bv-ui-font-family))
      (set-face-attribute 'default nil
                          :family bv-ui-font-family
                          :height (* bv-ui-font-size 10)))))

;; Apply font settings to initial frame
(bv-ui-setup-fonts)

;; Apply font settings to new frames
(add-hook 'after-make-frame-functions
          (lambda (_frame)
            "Apply font settings to new frames."
            (bv-ui-setup-fonts)))

;;;; UI Setup

;; Remove unnecessary UI elements
(bv-with-delayed-setup bv-ui-chrome 0.1
  ;; These functions might not exist in terminal Emacs
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Set up fringes
  (when (and bv-ui-fringes (fboundp 'set-fringe-mode))
    (set-fringe-mode bv-ui-fringes))

  ;; Window dividers for separation
  (setq window-divider-default-right-width bv-ui-margin)
  (when (fboundp 'window-divider-mode)
    (window-divider-mode 1))

  ;; Apply undecorated setting to new frames
  (when bv-ui-undecorated-frame
    (add-to-list 'default-frame-alist '(undecorated . t)))

  ;; Cursor configuration
  (when (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
  (setq-default cursor-type '(bar . 2)
                cursor-in-non-selected-windows nil))

;;;; Header Line as Mode Line

(defun bv-ui--use-header-line ()
  "Use header line instead of mode line for new buffers.
This is an internal function used by the header-line-mode setup."
  (unless (local-variable-p 'header-line-format)
    (setq header-line-format (default-value 'header-line-format)))
  (unless (local-variable-p 'mode-line-format)
    (setq mode-line-format nil)))

(defun bv-ui--setup-header-line-mode ()
  "Set up header line as mode line if configured.
Moves the mode line content to the header line position."
  (when bv-ui-header-line-as-mode-line
    ;; Store original mode line format
    (setq bv-ui--original-mode-line-format
          (default-value 'mode-line-format))

    ;; Move mode line to header line
    (setq-default header-line-format
                  (default-value 'mode-line-format))
    (setq-default mode-line-format nil)

    ;; Fix all existing buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unless (local-variable-p 'header-line-format)
          (setq header-line-format (default-value 'header-line-format)))
        (unless (local-variable-p 'mode-line-format)
          (setq mode-line-format nil))))

    ;; Ensure new buffers get the header line
    (add-hook 'find-file-hook #'bv-ui--use-header-line)
    (add-hook 'after-change-major-mode-hook #'bv-ui--use-header-line)))

;; Set up header line after init to ensure mode-line-format is populated
(add-hook 'after-init-hook #'bv-ui--setup-header-line-mode 90)

;;;; Modus Themes Configuration

(defun bv-ui--apply-custom-faces ()
  "Apply custom faces after loading theme.
This function is called after a modus theme is loaded to customize
various UI elements according to our preferences."
  ;; Variables c, bg-main, etc. are bound by modus-themes-with-colors macro
  (with-no-warnings  ; Suppress warnings about free vars from the macro
    (modus-themes-with-colors
      (custom-set-faces
       ;; Window dividers - make them invisible
       `(window-divider ((,c :foreground ,bg-main)))
       `(window-divider-first-pixel ((,c :foreground ,bg-main)))
       `(window-divider-last-pixel ((,c :foreground ,bg-main)))
       `(vertical-border ((,c :foreground ,bg-main)))

       ;; Mode line - add configurable padding
       `(mode-line ((,c :box (:line-width ,bv-ui-mode-line-padding
                                          :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width ,bv-ui-mode-line-padding
                                                   :color ,bg-mode-line-inactive))))

       ;; Header line - add configurable padding
       `(header-line ((,c :box (:line-width ,bv-ui-header-line-padding
                                            :color ,bg-dim))))

       ;; Tab bar - add configurable padding
       `(tab-bar ((,c :background ,bg-dim
                      :box (:line-width ,bv-ui-tab-bar-padding
                                        :color ,bg-dim))))))))

(use-package modus-themes
  :ensure nil  ; Built-in since Emacs 28
  :init
  ;; Theme customization must happen before loading
  (setq modus-themes-italic-constructs t      ; Use italics for comments, etc.
        modus-themes-bold-constructs t        ; Use bold for keywords, etc.
        modus-themes-mixed-fonts t)           ; Allow mixed fonts

  ;; Use palette overrides for theme customization
  (setq modus-themes-common-palette-overrides
        `((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (bg-region bg-ochre)          ; Warmer selection color
          (fg-region unspecified)
          ;; Org blocks with subtle background
          (bg-prose-block-contents bg-dim)
          (bg-prose-block-delimiter bg-dim)))

  ;; Mode line style - borderless
  (setq modus-themes-mode-line '(borderless))

  ;; Heading scaling when enabled
  (when bv-ui-theme-headings-scaling
    (setq modus-themes-headings
          '((1 . (1.3))     ; Level 1: 130%
            (2 . (1.2))     ; Level 2: 120%
            (3 . (1.1))     ; Level 3: 110%
            (t . (1.0)))))  ; Others: 100%

  :config
  ;; Apply our custom faces after any modus theme loads
  (add-hook 'modus-themes-after-load-theme-hook
            #'bv-ui--apply-custom-faces))

;;;; Theme System

(defun bv-ui--get-theme-name (type)
  "Get modus theme name for TYPE.
TYPE should be one of: `light', `light-soft', `dark', `dark-soft'.
Returns the appropriate theme name considering deuteranopia setting."
  (cond
   ((eq type 'light)
    (if bv-ui-theme-deuteranopia
        'modus-operandi-deuteranopia
      'modus-operandi))
   ((eq type 'light-soft)
    (if bv-ui-theme-deuteranopia
        'modus-operandi-deuteranopia
      'modus-operandi-tinted))
   ((eq type 'dark)
    (if bv-ui-theme-deuteranopia
        'modus-vivendi-deuteranopia
      'modus-vivendi))
   ((eq type 'dark-soft)
    (if bv-ui-theme-deuteranopia
        'modus-vivendi-deuteranopia
      'modus-vivendi-tinted))
   (t
    (error "Unknown theme type: %s" type))))

(defun bv-ui--get-theme-for-hour (hour)
  "Get theme type for HOUR based on configuration.
HOUR should be an integer from 0 to 23.
Returns the theme type from `bv-ui-theme-switch-hours'."
  (let ((sorted-hours (bv-ui--sort-list (mapcar #'car bv-ui-theme-switch-hours) #'>)))
    (or (cdr (assoc (bv-ui--find-if (lambda (h) (<= h hour)) sorted-hours)
                    bv-ui-theme-switch-hours))
        ;; If hour is before first switch, use last theme of previous day
        (cdr (assoc (car sorted-hours) bv-ui-theme-switch-hours))
        'dark))) ; Fallback to dark if no configuration

;;;###autoload
(defun bv-ui-load-theme (theme)
  "Load THEME, disabling all others first.
THEME should be a symbol naming a theme.
Only loads if THEME is not already the current theme."
  (unless (eq theme bv-ui--current-theme)
    ;; Disable all currently enabled themes
    (when (boundp 'custom-enabled-themes)
      (mapc #'disable-theme custom-enabled-themes))
    ;; Load the new theme
    (load-theme theme t)
    (setq bv-ui--current-theme theme)
    (message "Switched to %s theme" theme)))

;;;###autoload
(defun bv-ui-switch-theme ()
  "Switch theme based on current time.
Uses the configuration in `bv-ui-theme-switch-hours' to determine
which theme should be active."
  (interactive)
  (let* ((hour (string-to-number (format-time-string "%H")))
         (type (bv-ui--get-theme-for-hour hour))
         (theme (bv-ui--get-theme-name type)))
    (bv-ui-load-theme theme)))

(defun bv-ui--cancel-theme-timers ()
  "Cancel all active theme switching timers.
This is an internal function used during cleanup."
  (dolist (timer bv-ui--theme-timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq bv-ui--theme-timers nil))

(defun bv-ui--schedule-next-switch ()
  "Schedule the next theme switch.
Calculates when the next theme change should occur based on
`bv-ui-theme-switch-hours' and schedules a timer."
  (when bv-ui-theme-auto-switch
    (let* ((current-hour (string-to-number (format-time-string "%H")))
           (current-min (string-to-number (format-time-string "%M")))
           (current-total-min (+ (* current-hour 60) current-min))
           (switch-times (mapcar (lambda (entry)
                                   (* (car entry) 60))
                                 bv-ui-theme-switch-hours))
           ;; Find next switch time
           (next-switch-min (or (bv-ui--find-if (lambda (m) (> m current-total-min))
                                                (bv-ui--sort-list switch-times #'<))
                                ;; If no switch today, use first switch tomorrow
                                (+ (* 24 60) (car (bv-ui--sort-list switch-times #'<)))))
           (delay-min (- next-switch-min current-total-min))
           (delay-sec (* delay-min 60)))

      ;; Schedule the switch
      (push (run-at-time delay-sec nil
                         (lambda ()
                           (bv-ui-switch-theme)
                           (bv-ui--schedule-next-switch)))
            bv-ui--theme-timers))))

;;;###autoload
(defun bv-ui-setup-theme-switching ()
  "Initialize automatic theme switching.
Sets up the theme for the current time and schedules future switches."
  (interactive)
  (bv-ui--cancel-theme-timers)
  (when bv-ui-theme-auto-switch
    (bv-ui-switch-theme)
    (bv-ui--schedule-next-switch)))

;; Initialize theme system after init
(add-hook 'after-init-hook #'bv-ui-setup-theme-switching)

;; Handle environment variable override
(let ((variant (getenv "EMACS_THEME_VARIANT")))
  (when (member variant '("light" "dark"))
    (add-hook 'after-init-hook
              (lambda ()
                (bv-ui-load-theme
                 (bv-ui--get-theme-name (intern variant))))
              91))) ; Run after theme setup

;;;; Interactive Commands

;;;###autoload
(defun bv-ui-toggle-theme ()
  "Toggle between light and dark themes.
If automatic switching is enabled, you must disable it first
with `bv-ui-toggle-auto-switch'."
  (interactive)
  (if bv-ui-theme-auto-switch
      (message "Disable auto-switch first with `bv-ui-toggle-auto-switch'")
    (when (fboundp 'modus-themes-toggle)
      (call-interactively 'modus-themes-toggle))))

;;;###autoload
(defun bv-ui-toggle-auto-switch ()
  "Toggle automatic theme switching.
When enabled, themes change based on time of day.
When disabled, theme remains constant."
  (interactive)
  (setq bv-ui-theme-auto-switch (not bv-ui-theme-auto-switch))
  (if bv-ui-theme-auto-switch
      (progn
        (bv-ui-setup-theme-switching)
        (message "Automatic theme switching enabled"))
    (bv-ui--cancel-theme-timers)
    (message "Automatic theme switching disabled")))

;;;###autoload
(defun bv-ui-toggle-header-line-mode ()
  "Toggle between header line and mode line.
Switches the mode line between top (header) and bottom positions."
  (interactive)
  (setq bv-ui-header-line-as-mode-line (not bv-ui-header-line-as-mode-line))
  (if bv-ui-header-line-as-mode-line
      (progn
        ;; Store current mode line if not already stored
        (unless bv-ui--original-mode-line-format
          (setq bv-ui--original-mode-line-format
                (or (default-value 'mode-line-format)
                    ;; Fallback mode line format
                    '("%e" mode-line-front-space mode-line-mule-info
                      mode-line-client mode-line-modified
                      mode-line-remote mode-line-frame-identification
                      mode-line-buffer-identification "   "
                      mode-line-position "  " mode-line-modes
                      mode-line-misc-info mode-line-end-spaces))))
        ;; Move to header line
        (setq-default header-line-format bv-ui--original-mode-line-format)
        (setq-default mode-line-format nil)
        ;; Update all buffers
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (unless (local-variable-p 'header-line-format)
              (setq header-line-format (default-value 'header-line-format)))
            (unless (local-variable-p 'mode-line-format)
              (setq mode-line-format nil))))
        (message "Mode line moved to header"))
    ;; Restore mode line
    (setq-default mode-line-format (or bv-ui--original-mode-line-format
                                       (default-value 'header-line-format)))
    (setq-default header-line-format nil)
    ;; Update all buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unless (local-variable-p 'mode-line-format)
          (setq mode-line-format (default-value 'mode-line-format)))
        (unless (local-variable-p 'header-line-format)
          (setq header-line-format nil))))
    (message "Mode line restored to bottom")))

;;;###autoload
(defun bv-ui-toggle-frame-decorations ()
  "Toggle frame decorations (title bar) for the current frame.
Note: On some systems, you may need to restart Emacs or recreate
the frame for this to take effect."
  (interactive)
  (let* ((frame (selected-frame))
         (current (frame-parameter frame 'undecorated)))
    (set-frame-parameter frame 'undecorated (not current))
    ;; Update default for new frames
    (setq bv-ui-undecorated-frame (not current))
    (if (not current)
        (progn
          (setq default-frame-alist
                (cons '(undecorated . t)
                      (assq-delete-all 'undecorated default-frame-alist)))
          (message "Frame decorations disabled (may require frame recreation)"))
      (setq default-frame-alist
            (cons '(undecorated . nil)
                  (assq-delete-all 'undecorated default-frame-alist)))
      (message "Frame decorations enabled (may require frame recreation)"))))

;; Keybindings (using toggle-map from bv-defaults if available)
(bv-with-value toggle-map map
  (define-key map "t" #'bv-ui-toggle-theme)
  (define-key map "T" #'bv-ui-toggle-auto-switch)
  (define-key map "h" #'bv-ui-toggle-header-line-mode)
  (define-key map "d" #'bv-ui-toggle-frame-decorations))

;;;; Which-Key Configuration

(use-package which-key
  :ensure nil  ; Built-in since Emacs 28
  :demand t
  :init
  ;; Set configuration before loading
  (setq which-key-idle-delay bv-ui-which-key-idle-delay
        which-key-min-display-lines 1
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25)
  :config
  (which-key-mode 1))

;;;; Nerd Icons Configuration

(use-package nerd-icons
  :ensure nil  ; Optional package
  :if (display-graphic-p)  ; Only in GUI mode
  :custom
  ;; The Nerd Font to use for display
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  ;; Customize icon colors to match theme
  (setq nerd-icons-color-icons t)
  ;; Install fonts if not present (only needs to run once)
  (unless (member nerd-icons-font-family (font-family-list))
    (when (fboundp 'nerd-icons-install-fonts)
      (nerd-icons-install-fonts t))))

;; Integration with ibuffer
(use-package nerd-icons-ibuffer
  :ensure nil  ; Optional package
  :after (nerd-icons ibuffer)
  :if (display-graphic-p)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-icon t)
  (nerd-icons-ibuffer-color-icon t)
  (nerd-icons-ibuffer-icon-size 1.0)
  (ibuffer-human-readable-size t))

;;;; Mode Line Configuration

;; Mode line with long format
(setq mode-line-compact 'long)

;; Hide minor modes with minions
(use-package minions
  :ensure nil  ; Built-in since Emacs 28
  :demand t
  :init
  (setq minions-mode-line-lighter "…"
        minions-mode-line-delimiters '("" . "")
        ;; Keep important modes visible
        minions-prominent-modes bv-ui-prominent-modes)
  :config
  (minions-mode 1))

;; Time display configuration
(setq display-time-format "%a %b %d %H:%M:%S"  ; "Sun Jun 15 13:02:52"
      display-time-24hr-format t
      display-time-default-load-average nil
      display-time-interval 1)

;; Enable time display after init
(add-hook 'after-init-hook #'display-time-mode)

;;;; Additional UI Settings

;; Highlight current line in programming modes
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Better scrolling behavior
(setq scroll-step 1
      scroll-margin 2
      scroll-conservatively 101      ; Never recenter point
      scroll-preserve-screen-position t)

;; Window management preferences
(setq split-width-threshold 160      ; Prefer horizontal splits on wide screens
      split-height-threshold nil)    ; Avoid vertical splits

;;;; UI Status Report

;;;###autoload
(defun bv-ui-status ()
  "Display current UI configuration status.
Shows theme, font, and feature settings in a dedicated buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*bv-ui-status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "UI Configuration Status\n")
      (insert "=======================\n\n")

      (insert "Theme Configuration:\n")
      (insert (format "  Current theme: %s\n" (or bv-ui--current-theme "none")))
      (insert (format "  Auto-switch: %s\n" (if bv-ui-theme-auto-switch "enabled" "disabled")))
      (insert (format "  Deuteranopia mode: %s\n" (if bv-ui-theme-deuteranopia "yes" "no")))
      (insert (format "  Heading scaling: %s\n" (if bv-ui-theme-headings-scaling "yes" "no")))
      (insert "\n  Schedule:\n")
      (dolist (entry bv-ui-theme-switch-hours)
        (insert (format "    %02d:00 - %s\n" (car entry) (cdr entry))))

      (insert "\nFont Configuration:\n")
      (insert (format "  Family: %s\n" (or bv-ui-font-family "system default")))
      (insert (format "  Size: %d points\n" bv-ui-font-size))

      (insert "\nUI Elements:\n")
      (insert (format "  Frame decorations: %s\n" (if bv-ui-undecorated-frame "hidden" "shown")))
      (insert (format "  Mode line position: %s\n" (if bv-ui-header-line-as-mode-line "top (header)" "bottom")))
      (insert (format "  Fringe width: %d pixels\n" bv-ui-fringes))
      (insert (format "  Internal margin: %d pixels\n" bv-ui-margin))

      (insert "\nActive Features:\n")
      (insert (format "  Which-key: %s (delay: %.1fs)\n"
                      (if (bound-and-true-p which-key-mode) "active" "inactive")
                      bv-ui-which-key-idle-delay))
      (insert (format "  Minions: %s\n"
                      (if (bound-and-true-p minions-mode) "active" "inactive")))
      (insert (format "  Nerd icons: %s\n"
                      (if (featurep 'nerd-icons) "loaded" "not available")))

      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

;;;; Convenience Entry Point

;;;###autoload
(defun bv-ui-setup ()
  "Set up UI configuration.
This is a convenience function that ensures all UI components are
properly initialized.  It:
- Sets up fonts
- Initializes theme switching
- Enables which-key and minions
- Configures icon support if available

Safe to call multiple times."
  (interactive)
  ;; Set up fonts
  (bv-ui-setup-fonts)
  ;; Initialize theme system
  (bv-ui-setup-theme-switching)
  ;; Ensure which-key is active
  (when (fboundp 'which-key-mode)
    (which-key-mode 1))
  ;; Ensure minions is active
  (when (fboundp 'minions-mode)
    (minions-mode 1))
  ;; Set up time display
  (when (fboundp 'display-time-mode)
    (display-time-mode 1))
  (message "UI setup complete"))

;;;; Feature Registration

(bv-register-feature 'bv-ui '(bv-core))

(provide 'bv-ui)

;;; bv-ui.el ends here
