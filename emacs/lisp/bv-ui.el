;;; bv-ui.el --- Appearance and UI configuration -*- lexical-binding: t -*-

;;; Commentary:
;; UI and appearance configuration with automatic theme switching
;; Minimal chrome, modus themes, which-key, and optional icons

;;; Code:

(require 'bv-core)

;;;; External Variable Declarations
(defvar display-time-24hr-format)
(defvar display-time-default-load-average)
(defvar modus-themes-mode-line)

;;;; Function Declarations
(declare-function modus-themes-toggle "modus-themes" ())
(declare-function modus-themes-with-colors "modus-themes" (&rest body))
(declare-function nerd-icons-install-fonts "nerd-icons" (&optional arg))
(declare-function display-time-mode "time" (&optional arg))

;;;; Custom Variables

(defgroup bv-ui nil
  "UI and appearance configuration."
  :group 'bv
  :prefix "bv-ui-")

;; Appearance settings
(bv-defcustom bv-ui-margin 8
  "Internal border width for frames."
  :type 'integer
  :group 'bv-ui)

(bv-defcustom bv-ui-fringes 8
  "Width of window fringes."
  :type 'integer
  :group 'bv-ui)

(bv-defcustom bv-ui-mode-line-padding 4
  "Padding for mode line."
  :type 'integer
  :group 'bv-ui)

(bv-defcustom bv-ui-header-line-padding 4
  "Padding for header line."
  :type 'integer
  :group 'bv-ui)

(bv-defcustom bv-ui-tab-bar-padding 4
  "Padding for tab bar."
  :type 'integer
  :group 'bv-ui)

(bv-defcustom bv-ui-header-line-as-mode-line t
  "Move mode line to header line position."
  :type 'boolean
  :group 'bv-ui)

(bv-defcustom bv-ui-undecorated-frame t
  "Remove window title bar for minimal UI."
  :type 'boolean
  :group 'bv-ui)

;; Theme settings
(bv-defcustom bv-ui-theme-auto-switch t
  "Automatically switch themes based on time of day."
  :type 'boolean
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-deuteranopia nil
  "Use deuteranopia-friendly colors (red/green color blindness)."
  :type 'boolean
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-headings-scaling nil
  "Scale headings to different sizes."
  :type 'boolean
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-switch-hours '((5 . light-soft)
                                         (10 . light)
                                         (18 . dark-soft)
                                         (23 . dark))
  "Hours and theme types for automatic switching.
Each element is (HOUR . TYPE) where TYPE is \\='light, \\='light-soft,
\\='dark, or \\='dark-soft."
  :type '(alist :key-type integer
                :value-type (choice (const light)
                                   (const light-soft)
                                   (const dark)
                                   (const dark-soft)))
  :group 'bv-ui)

;; Font settings
(bv-defcustom bv-ui-font-family nil
  "Font family to use.  Set to nil to use system default."
  :type '(choice (const :tag "System default" nil)
                 (string :tag "Font family"))
  :group 'bv-ui)

(bv-defcustom bv-ui-font-size 12
  "Font size in points."
  :type 'integer
  :group 'bv-ui)

;; Which-key settings
(bv-defcustom bv-ui-which-key-idle-delay 1.0
  "Delay before which-key popup appears."
  :type 'number
  :group 'bv-ui)

;;;; Internal Variables

(defvar bv-ui--theme-timers nil
  "List of active theme switching timers.")

(defvar bv-ui--current-theme nil
  "Currently active theme.")

(defvar bv-ui--original-mode-line-format nil
  "Store the original `mode-line-format'.")

;;;; Font Setup

(defun bv-ui-setup-fonts ()
  "Set up fonts if configured."
  (when bv-ui-font-family
    (when (find-font (font-spec :family bv-ui-font-family))
      (set-face-attribute 'default nil
                          :family bv-ui-font-family
                          :height (* bv-ui-font-size 10)))))

;; Apply font settings
(add-hook 'after-make-frame-functions
          (lambda (_) (bv-ui-setup-fonts)))
(bv-ui-setup-fonts)

;;;; Minimal UI

;; Remove UI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Fringes
(when bv-ui-fringes
  (set-fringe-mode bv-ui-fringes))

;; Window dividers
(setq window-divider-default-right-width bv-ui-margin)
(window-divider-mode 1)

;; Apply undecorated setting to new frames
(when bv-ui-undecorated-frame
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Misc settings
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 2)
              cursor-in-non-selected-windows nil)

;;;; Header Line as Mode Line

;; Define helper function before it's used
(defun bv-ui--use-header-line ()
  "Use header line instead of mode line for new buffers."
  (unless (local-variable-p 'header-line-format)
    (setq header-line-format (default-value 'header-line-format)))
  (unless (local-variable-p 'mode-line-format)
    (setq mode-line-format nil)))

(defun bv-ui--setup-header-line-mode ()
  "Set up header line as mode line if configured."
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

;; Set up header line after init
(add-hook 'after-init-hook #'bv-ui--setup-header-line-mode 90)

;;;; Modus Themes

;; Define custom faces function before it's used
(defun bv-ui--apply-custom-faces ()
  "Apply custom faces after loading theme."
  ;; Variables c, bg-main, etc. are bound by modus-themes-with-colors macro
  (with-no-warnings  ; Suppress warnings about free vars from the macro
    (modus-themes-with-colors
      (custom-set-faces
       ;; Window dividers
       `(window-divider ((,c :foreground ,bg-main)))
       `(window-divider-first-pixel ((,c :foreground ,bg-main)))
       `(window-divider-last-pixel ((,c :foreground ,bg-main)))
       `(vertical-border ((,c :foreground ,bg-main)))

       ;; Mode line
       `(mode-line ((,c :box (:line-width ,bv-ui-mode-line-padding
                              :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width ,bv-ui-mode-line-padding
                                       :color ,bg-mode-line-inactive))))

       ;; Header line
       `(header-line ((,c :box (:line-width ,bv-ui-header-line-padding
                                :color ,bg-dim))))

       ;; Tab bar
       `(tab-bar ((,c :background ,bg-dim
                      :box (:line-width ,bv-ui-tab-bar-padding
                            :color ,bg-dim))))))))

(use-package modus-themes
  :ensure nil
  :init
  ;; Theme customization
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)

  ;; Use palette overrides instead of obsolete org-blocks
  (setq modus-themes-common-palette-overrides
        `((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (bg-region bg-ochre)
          (fg-region unspecified)
          ;; Org blocks with gray background
          (bg-prose-block-contents bg-dim)
          (bg-prose-block-delimiter bg-dim)))

  ;; Mode line style (use correct variable)
  (setq modus-themes-mode-line '(borderless))

  ;; Heading scaling
  (when bv-ui-theme-headings-scaling
    (setq modus-themes-headings
          '((1 . (1.3))
            (2 . (1.2))
            (3 . (1.1))
            (t . (1.0)))))

  :config
  (add-hook 'modus-themes-after-load-theme-hook
            #'bv-ui--apply-custom-faces))

;;;; Theme System

(defun bv-ui--get-theme-name (type)
  "Get modus theme name for TYPE (\\='light, \\='light-soft, \\='dark, \\='dark-soft)."
  (pcase type
    ('light (if bv-ui-theme-deuteranopia
                'modus-operandi-deuteranopia
              'modus-operandi))
    ('light-soft (if bv-ui-theme-deuteranopia
                     'modus-operandi-deuteranopia
                   'modus-operandi-tinted))
    ('dark (if bv-ui-theme-deuteranopia
               'modus-vivendi-deuteranopia
             'modus-vivendi))
    ('dark-soft (if bv-ui-theme-deuteranopia
                    'modus-vivendi-deuteranopia
                  'modus-vivendi-tinted))
    (_ (error "Unknown theme type: %s" type))))

(defun bv-ui--get-theme-for-hour (hour)
  "Get theme type for HOUR based on configuration."
  (let ((sorted-hours (sort (mapcar #'car bv-ui-theme-switch-hours) #'>)))
    (or (cdr (assoc (cl-find-if (lambda (h) (<= h hour)) sorted-hours)
                    bv-ui-theme-switch-hours))
        ;; If hour is before first switch, use last theme of previous day
        (cdr (assoc (car sorted-hours) bv-ui-theme-switch-hours))
        'dark)))

(defun bv-ui-load-theme (theme)
  "Load THEME, disabling all others first."
  ;; Only load if not already the current theme
  (unless (eq theme bv-ui--current-theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (setq bv-ui--current-theme theme)
    (message "Switched to %s theme" theme)))

(defun bv-ui-switch-theme ()
  "Switch theme based on current time."
  (interactive)
  (let* ((hour (string-to-number (format-time-string "%H")))
         (type (bv-ui--get-theme-for-hour hour))
         (theme (bv-ui--get-theme-name type)))
    (bv-ui-load-theme theme)))

(defun bv-ui--cancel-theme-timers ()
  "Cancel all active theme switching timers."
  (dolist (timer bv-ui--theme-timers)
    (cancel-timer timer))
  (setq bv-ui--theme-timers nil))

(defun bv-ui--schedule-next-switch ()
  "Schedule the next theme switch."
  (when bv-ui-theme-auto-switch
    (let* ((current-hour (string-to-number (format-time-string "%H")))
           (current-min (string-to-number (format-time-string "%M")))
           (current-total-min (+ (* current-hour 60) current-min))
           (switch-times (mapcar (lambda (entry)
                                  (* (car entry) 60))
                                bv-ui-theme-switch-hours))
           ;; Find next switch time
           (next-switch-min (or (cl-find-if (lambda (m) (> m current-total-min))
                                           (sort switch-times #'<))
                               ;; If no switch today, use first switch tomorrow
                               (+ (* 24 60) (car (sort switch-times #'<)))))
           (delay-min (- next-switch-min current-total-min))
           (delay-sec (* delay-min 60)))

      ;; Schedule the switch
      (push (run-at-time delay-sec nil
                        (lambda ()
                          (bv-ui-switch-theme)
                          (bv-ui--schedule-next-switch)))
            bv-ui--theme-timers))))

(defun bv-ui-setup-theme-switching ()
  "Initialize automatic theme switching."
  (bv-ui--cancel-theme-timers)
  (when bv-ui-theme-auto-switch
    (bv-ui-switch-theme)
    (bv-ui--schedule-next-switch)))

;; Initialize theme system
(bv-ui-setup-theme-switching)

;;;; Interactive Commands

(defun bv-ui-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (if bv-ui-theme-auto-switch
      (message "Disable auto-switch first with `bv-ui-toggle-auto-switch'")
    (when (fboundp 'modus-themes-toggle)
      (call-interactively 'modus-themes-toggle))))

(defun bv-ui-toggle-auto-switch ()
  "Toggle automatic theme switching."
  (interactive)
  (setq bv-ui-theme-auto-switch (not bv-ui-theme-auto-switch))
  (if bv-ui-theme-auto-switch
      (progn
        (bv-ui-setup-theme-switching)
        (message "Automatic theme switching enabled"))
    (bv-ui--cancel-theme-timers)
    (message "Automatic theme switching disabled")))

(defun bv-ui-toggle-header-line-mode ()
  "Toggle between header line and mode line."
  (interactive)
  (setq bv-ui-header-line-as-mode-line (not bv-ui-header-line-as-mode-line))
  (if bv-ui-header-line-as-mode-line
      (progn
        ;; Store current mode line if not already stored
        (unless bv-ui--original-mode-line-format
          (setq bv-ui--original-mode-line-format
                (or (default-value 'mode-line-format)
                    '("%e" mode-line-front-space mode-line-mule-info
                      mode-line-client mode-line-modified
                      mode-line-remote mode-line-frame-identification
                      mode-line-buffer-identification "   "
                      mode-line-position "  " mode-line-modes
                      mode-line-misc-info mode-line-end-spaces))))
        ;; Move to header line
        (setq-default header-line-format bv-ui--original-mode-line-format)
        (setq-default mode-line-format nil)
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
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unless (local-variable-p 'mode-line-format)
          (setq mode-line-format (default-value 'mode-line-format)))
        (unless (local-variable-p 'header-line-format)
          (setq header-line-format nil))))
    (message "Mode line restored to bottom")))

(defun bv-ui-toggle-frame-decorations ()
  "Toggle frame decorations (title bar) for the current frame.
Note: On some systems, you may need to restart Emacs for this to take effect."
  (interactive)
  (let* ((frame (selected-frame))
         (current (frame-parameter frame 'undecorated)))
    (set-frame-parameter frame 'undecorated (not current))
    ;; Update default for new frames
    (setq bv-ui-undecorated-frame (not current))
    (if (not current)
        (progn
          (add-to-list 'default-frame-alist '(undecorated . t))
          (message "Frame decorations disabled (may require frame recreation)"))
      (setq default-frame-alist
            (assq-delete-all 'undecorated default-frame-alist))
      (add-to-list 'default-frame-alist '(undecorated . nil))
      (message "Frame decorations enabled (may require frame recreation)"))))

;; Keybindings
(bv-with-value toggle-map map
  (define-key map "t" #'bv-ui-toggle-theme)
  (define-key map "T" #'bv-ui-toggle-auto-switch)
  (define-key map "h" #'bv-ui-toggle-header-line-mode)
  (define-key map "d" #'bv-ui-toggle-frame-decorations))

;;;; Which-Key

(use-package which-key
  :ensure nil
  :demand t
  :config
  (setq which-key-idle-delay bv-ui-which-key-idle-delay
        which-key-min-display-lines 1
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25)
  (which-key-mode 1))

;;;; Nerd Icons

(use-package nerd-icons
  :ensure nil
  :custom
  ;; The Nerd Font to use for display
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  ;; Customize icon colors to match theme
  (setq nerd-icons-color-icons t)
  ;; This only needs to be run once
  (unless (member nerd-icons-font-family (font-family-list))
    (when (fboundp 'nerd-icons-install-fonts)
      (nerd-icons-install-fonts t))))

;; Integration with ibuffer
(use-package nerd-icons-ibuffer
  :ensure nil
  :after nerd-icons
  :demand t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-icon t)
  (nerd-icons-ibuffer-color-icon t)
  (nerd-icons-ibuffer-icon-size 1.0)
  (ibuffer-human-readable-size t))

;;;; Mode Line

;; Clean mode line
(setq mode-line-compact 'long)

;; Time in mode line
(when (boundp 'display-time-24hr-format)
  (setq display-time-24hr-format t))
(when (boundp 'display-time-default-load-average)
  (setq display-time-default-load-average nil))
(when (fboundp 'display-time-mode)
  (display-time-mode 1))

;;;; Additional UI Settings

;; Highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Better scrolling
(setq scroll-conservatively 101
      scroll-margin 2
      scroll-preserve-screen-position t)

;; Window management
(setq split-width-threshold 160
      split-height-threshold nil)

;;;; Feature Registration

(bv-register-feature 'bv-ui)

(provide 'bv-ui)
;;; bv-ui.el ends here
