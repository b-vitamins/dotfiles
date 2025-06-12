;;; bv-ui.el --- Appearance and UI configuration -*- lexical-binding: t -*-

;;; Commentary:
;; UI and appearance configuration
;; Minimal chrome, modus themes, which-key, icons support

;;; Code:

(require 'bv-core)

;;;; Custom Variables

(defgroup bv-ui nil
  "UI and appearance configuration."
  :group 'bv)

;; Appearance settings
(bv-defcustom bv-ui-margin 8
  "Internal border width for frames."
  :type 'integer
  :group 'bv-ui)

(bv-defcustom bv-ui-fringes 8
  "Width of window fringes. Set to nil to disable."
  :type '(choice integer (const nil))
  :group 'bv-ui)

(bv-defcustom bv-ui-mode-line-padding 4
  "Padding for mode line."
  :type 'number
  :group 'bv-ui)

(bv-defcustom bv-ui-header-line-padding 4
  "Padding for header line."
  :type 'number
  :group 'bv-ui)

(bv-defcustom bv-ui-tab-bar-padding 4
  "Padding for tab bar."
  :type 'number
  :group 'bv-ui)

(bv-defcustom bv-ui-header-line-as-mode-line t
  "Move mode line to header line position."
  :type 'boolean
  :group 'bv-ui)

;; Theme settings
(bv-defcustom bv-ui-theme-dark-mode nil
  "Use dark theme by default."
  :type 'boolean
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-deuteranopia t
  "Use deuteranopia-friendly colors (red/green color blindness)."
  :type 'boolean
  :group 'bv-ui)

(bv-defcustom bv-ui-theme-headings-scaling nil
  "Scale headings to different sizes."
  :type 'boolean
  :group 'bv-ui)

;; Font settings
(bv-defcustom bv-ui-font-family "SF Mono"
  "Font family to use."
  :type 'string
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

(bv-defcustom bv-ui-which-key-min-height 1
  "Minimum height of which-key popup."
  :type 'integer
  :group 'bv-ui)

;; Icons settings
(bv-defcustom bv-ui-enable-icons nil
  "Enable all-the-icons integration."
  :type 'boolean
  :group 'bv-ui)

;;;; Font Setup

(defun bv-ui--setup-fonts ()
  "Set up fonts with SF family preference."
  (when (find-font (font-spec :family bv-ui-font-family))
    (set-face-attribute 'default nil
                        :family bv-ui-font-family
                        :height (* bv-ui-font-size 10))))

;; Apply font settings after frame creation
(add-hook 'after-make-frame-functions
          (lambda (_) (bv-ui--setup-fonts)))
(bv-ui--setup-fonts)

;;;; Minimal UI Setup

(defun bv-ui--setup-minimal-chrome ()
  "Configure minimal UI chrome."
  ;; Remove UI elements
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;; Set fringe width when available
  (when (and bv-ui-fringes (fboundp 'set-fringe-style))
    (set-fringe-style bv-ui-fringes))

  ;; Window dividers
  (setq window-divider-default-right-width bv-ui-margin)
  (window-divider-mode 1)

  ;; Frame parameters
  (add-to-list 'default-frame-alist `(internal-border-width . ,bv-ui-margin))

  ;; Disable various UI elements
  (setq use-dialog-box nil
        use-file-dialog nil)

  ;; Cursor settings
  (setq-default cursor-type '(bar . 2))
  (blink-cursor-mode -1)
  (setq-default cursor-in-non-selected-windows nil)

  ;; Misc UI settings
  (setq bookmark-set-fringe-mark nil))

;; Apply minimal chrome early
(bv-ui--setup-minimal-chrome)

;;;; Mode Line Configuration

;; Move mode line to header line
(when bv-ui-header-line-as-mode-line
  (add-hook 'after-init-hook
            (lambda ()
              ;; Move mode line to header line
              (setq-default header-line-format mode-line-format)
              (setq-default mode-line-format nil)

              ;; Fix all existing buffers
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (setq header-line-format (default-value 'header-line-format))
                  (setq mode-line-format nil)))

              ;; Ensure new buffers get the header line
              (defun bv-ui--use-header-line ()
                "Use header line instead of mode line."
                (setq header-line-format (default-value 'header-line-format))
                (setq mode-line-format nil))

              (add-hook 'find-file-hook #'bv-ui--use-header-line)
              (add-hook 'after-change-major-mode-hook #'bv-ui--use-header-line))
            90))

;;;; Modus Themes Configuration

;; Define theme variations early
(defvar bv-ui--light-theme nil
  "Light theme to use.")

(defvar bv-ui--dark-theme nil
  "Dark theme to use.")

(use-package modus-themes
  :init
  ;; Theme customization
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-org-blocks 'gray-background)

  ;; Common color palette overrides
  (setq modus-themes-common-palette-overrides
        `((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (bg-region bg-ochre)
          (fg-region unspecified)))

  ;; Heading scaling if enabled
  (when bv-ui-theme-headings-scaling
    (setq modus-themes-headings
          '((1 . (1.15))
            (2 . (1.1))
            (3 . (1.1))
            (4 . (1.0))
            (5 . (1.0))
            (6 . (1.0))
            (7 . (0.9))
            (8 . (0.9)))))

  :config
  ;; Define theme variations
  (setq bv-ui--light-theme
        (if bv-ui-theme-deuteranopia
            'modus-operandi-deuteranopia
          'modus-operandi))

  (setq bv-ui--dark-theme
        (if bv-ui-theme-deuteranopia
            'modus-vivendi-deuteranopia
          'modus-vivendi))

  ;; Set theme toggle list
  (setq modus-themes-to-toggle
        (list bv-ui--light-theme bv-ui--dark-theme))

  ;; Custom faces after theme load
  (defun bv-ui--modus-themes-custom-faces ()
    "Apply custom faces after loading modus theme."
    (when (modus-themes--current-theme)
      (modus-themes-with-colors
        (custom-set-faces
         ;; Window dividers
         `(window-divider ((,c :foreground ,bg-main)))
         `(window-divider-first-pixel ((,c :foreground ,bg-main)))
         `(window-divider-last-pixel ((,c :foreground ,bg-main)))
         `(vertical-border ((,c :foreground ,bg-main)))

         ;; Tab bar
         `(tab-bar ((,c :background ,bg-dim
                        :box (:line-width ,bv-ui-tab-bar-padding
                              :color ,bg-dim
                              :style nil))))

         ;; Mode line
         `(mode-line ((,c :box (:line-width ,bv-ui-mode-line-padding
                                :color ,bg-mode-line-active))))
         `(mode-line-inactive ((,c :box (:line-width ,bv-ui-mode-line-padding
                                         :color ,bg-mode-line-inactive))))

         ;; Header line
         `(header-line ((,c :box (:line-width ,bv-ui-header-line-padding
                                  :color ,bg-dim))))))))

  ;; Apply custom faces hook
  (add-hook 'modus-themes-after-load-theme-hook
            #'bv-ui--modus-themes-custom-faces)

  ;; Load initial theme
  (load-theme (if bv-ui-theme-dark-mode
                  bv-ui--dark-theme
                bv-ui--light-theme)
              t))

;; Theme toggling
(defun bv-ui-toggle-theme ()
  "Toggle between light and dark modus themes."
  (interactive)
  (modus-themes-toggle))

(with-eval-after-load 'bv-defaults
  (define-key bv-toggle-map "t" #'bv-ui-toggle-theme))

;;;; Which Key Configuration

(use-package which-key
  :demand t
  :config
  (setq which-key-min-display-lines bv-ui-which-key-min-height
        which-key-idle-delay bv-ui-which-key-idle-delay
        which-key-ellipsis "..."
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-show-remaining-keys t
        which-key-max-display-columns nil)

  ;; Popup configuration
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25)

  (which-key-mode 1)

  ;; Keybinding
  (global-set-key (kbd "C-h C-k") 'which-key-show-top-level))

;;;; All The Icons Configuration

(when bv-ui-enable-icons
  (use-package all-the-icons
    :config
    (setq all-the-icons-scale-factor 1.0
          all-the-icons-default-adjust 0
          all-the-icons-octicon-scale-factor 0.9))

  ;; Icons in completion
  (bv-when-feature bv-completion
    (use-package all-the-icons-completion
      :after (all-the-icons marginalia)
      :config
      (all-the-icons-completion-mode)
      (add-hook 'marginalia-mode-hook
                #'all-the-icons-completion-marginalia-setup))))

;;;; Additional UI Enhancements

;; Highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Better help buffers
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-command] . helpful-command)
         ("C-h F" . helpful-function)
         ("C-h o" . helpful-at-point))
  :config
  (add-hook 'helpful-mode-hook #'visual-line-mode))

;; Clean mode line
(setq mode-line-compact 'long)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Better scrolling
(setq scroll-conservatively 101
      scroll-margin 2
      scroll-preserve-screen-position t)

;; Window management
(setq split-width-threshold 160
      split-height-threshold nil)

;;;; Feature Registration

(bv-register-feature 'bv-ui)
(bv-set-value 'modus-themes-enabled t)

;; Export theme names
(with-eval-after-load 'modus-themes
  (bv-set-value 'light-theme bv-ui--light-theme)
  (bv-set-value 'dark-theme bv-ui--dark-theme))

(provide 'bv-ui)
;;; bv-ui.el ends here
