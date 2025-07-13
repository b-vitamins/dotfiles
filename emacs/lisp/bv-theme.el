;;; bv-theme.el --- Unified theme system  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Unified theme system combining base colors, semantic faces, and theme
;; application. Supports light/dark variants with all faces derived from
;; a core set of semantic faces.

;;; Code:

(defgroup bv '()
  "Personal Emacs customizations.")

;; Base color definitions

(defvar bv-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun bv-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name bv-base-colors--defaults)))

(defcustom bv-color-foreground (bv-base-colors--get 'foreground)
  "Foreground color."
  :type 'color
  :group 'bv)

(defcustom bv-color-background (bv-base-colors--get 'background)
  "Background color."
  :type 'color
  :group 'bv)

(defcustom bv-color-highlight (bv-base-colors--get 'highlight)
  "Highlight color."
  :type 'color
  :group 'bv)

(defcustom bv-color-critical (bv-base-colors--get 'critical)
  "Critical color."
  :type 'color
  :group 'bv)

(defcustom bv-color-salient (bv-base-colors--get 'salient)
  "Salient color."
  :type 'color
  :group 'bv)

(defcustom bv-color-strong (bv-base-colors--get 'strong)
  "Strong color."
  :type 'color
  :group 'bv)

(defcustom bv-color-popout (bv-base-colors--get 'popout)
  "Popout color."
  :type 'color
  :group 'bv)

(defcustom bv-color-subtle (bv-base-colors--get 'subtle)
  "Subtle color."
  :type 'color
  :group 'bv)

(defcustom bv-color-faded (bv-base-colors--get 'faded)
  "Faded color."
  :type 'color
  :group 'bv)

;; Font settings

(defcustom bv-font-family-monospaced "Roboto Mono"
  "Name of the font-family to use.
Defaults to Roboto Mono. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'bv
  :type 'string)

(defcustom bv-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows displaying variable pitch faces when
variable-pitch-mode or mixed-pitch-mode is active.
Defaults to nil."
  :group 'bv
  :type 'string)

(defcustom bv-font-size 12
  "Default value for the font size in pt units."
  :group 'bv
  :type 'integer)

;; Theme variant

(defcustom bv-theme-var nil
  "Current theme variant ('light' or 'dark')."
  :group 'bv
  :type 'string)

;; Semantic face definitions

(defface bv-face-default nil
  "Default face is used for regular information."
  :group 'bv)

(defface bv-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'bv)

(defface bv-face-critical nil
  "Critical face is for information that requires immediate action.
High contrast face with intense background color."
  :group 'bv)

(defface bv-face-popout nil
  "Popout face is used for information that needs attention.
Uses contrasting hue to attract attention."
  :group 'bv)

(defface bv-face-strong nil
  "Strong face is used for structural elements.
Same color as default with different weight."
  :group 'bv)

(defface bv-face-salient nil
  "Salient face is used for important information.
Different hue with similar intensity to default."
  :group 'bv)

(defface bv-face-faded nil
  "Faded face is for less important information.
Same hue as default with reduced intensity."
  :group 'bv)

(defface bv-face-subtle nil
  "Subtle face is used to delineate areas.
Light background color that is barely perceptible."
  :group 'bv)

;; Helper functions

(defun bv-faces ()
  "Derive face attributes for bv-faces using bv-theme values."
  (set-face-attribute 'bv-face-default nil
                      :foreground bv-color-foreground
                      :background bv-color-background
                      :family     bv-font-family-monospaced
                      :height     (* bv-font-size 10))

  (set-face-attribute 'bv-face-critical nil
                      :foreground bv-color-foreground
                      :background bv-color-critical)

  (set-face-attribute 'bv-face-popout nil
                      :foreground bv-color-popout)

  (set-face-attribute 'bv-face-variable-pitch nil
                      :foreground (face-foreground 'bv-face-default)
                      :background (face-background 'bv-face-default)
                      :family (or bv-font-family-proportional
                                  bv-font-family-monospaced)
                      :height (* bv-font-size 10))

  (if (display-graphic-p)
      (set-face-attribute 'bv-face-strong nil
                          :foreground bv-color-strong
                          :weight 'medium)
    (set-face-attribute 'bv-face-strong nil
                        :foreground bv-color-strong
                        :weight 'bold))

  (set-face-attribute 'bv-face-salient nil
                      :foreground bv-color-salient
                      :weight 'light)

  (set-face-attribute 'bv-face-faded nil
                      :foreground bv-color-faded
                      :weight 'light)

  (set-face-attribute 'bv-face-subtle nil
                      :background bv-color-subtle))

(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (if (facep face)
      (set-face-attribute face nil
                          :foreground 'unspecified :background 'unspecified
                          :family     'unspecified :slant      'unspecified
                          :weight     'unspecified :height     'unspecified
                          :underline  'unspecified :overline   'unspecified
                          :box        'unspecified :inherit    style)))

;; Theme application functions

(defun bv-theme--basics ()
  "Derive basic Emacs faces from bv-faces."

  (set-foreground-color bv-color-foreground)
  (set-background-color bv-color-background)

  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'light
                      :family     (face-attribute 'bv-face-default :family)
                      :height     (face-attribute 'bv-face-default :height))

  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  (set-face 'bold                                     'bv-face-strong)
  (set-face 'italic                                    'bv-face-faded)
  (set-face 'bold-italic                              'bv-face-strong)
  (set-face 'region                                   'bv-face-subtle)
  (set-face 'highlight                                'bv-face-subtle)
  (set-face 'fixed-pitch-serif                       'bv-face-default)
  (set-face 'cursor                                  'bv-face-default)
  (if bv-font-family-proportional
      (set-face-attribute 'variable-pitch nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)
                          :family     (face-attribute 'bv-face-variable-pitch :family)
                          :height     (face-attribute 'bv-face-variable-pitch :height))
    (set-face 'variable-pitch                     'bv-face-default))

  (set-face-attribute 'cursor nil
                      :background (face-foreground 'bv-face-default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'bv-face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground bv-color-background)
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground bv-color-background)
  (set-face-foreground 'vertical-border bv-color-subtle)

  (set-face 'shadow                                    'bv-face-faded)
  (set-face 'success                                 'bv-face-salient)
  (set-face 'warning                                  'bv-face-popout)
  (set-face 'error                                  'bv-face-critical)
  (set-face 'match                                    'bv-face-popout)

  (set-face 'buffer-menu-buffer                       'bv-face-strong)
  (set-face 'minibuffer-prompt                        'bv-face-strong)
  (set-face 'link                                    'bv-face-salient)
  (set-face 'fringe                                    'bv-face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'bv-face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'bv-face-strong)
  (set-face 'isearch-fail                              'bv-face-faded)
  (set-face 'lazy-highlight                           'bv-face-subtle)
  (set-face 'trailing-whitespace                      'bv-face-subtle)
  (set-face 'show-paren-match                         'bv-face-popout)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'bv-face-subtle)
  (set-face 'completions-common-part                   'bv-face-faded)
  (set-face 'completions-first-difference            'bv-face-default))

(defun bv-theme--font-lock ()
  "Derive font-lock faces from bv-faces."
  (set-face 'font-lock-comment-face                    'bv-face-faded)
  (set-face 'font-lock-doc-face                        'bv-face-faded)
  (set-face 'font-lock-string-face                    'bv-face-popout)
  (set-face 'font-lock-constant-face                 'bv-face-salient)
  (set-face 'font-lock-warning-face                   'bv-face-popout)
  (set-face 'font-lock-function-name-face             'bv-face-strong)
  (set-face 'font-lock-variable-name-face             'bv-face-strong)
  (set-face 'font-lock-builtin-face                  'bv-face-salient)
  (set-face 'font-lock-type-face                     'bv-face-salient)
  (set-face 'font-lock-keyword-face                  'bv-face-salient))

(defun bv-theme--mode-line ()
  "Derive mode-line and header-line faces from bv-faces."
  (set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'bv-face-default)
                                    (face-foreground 'bv-face-default))
                      :background (face-background 'bv-face-default)
                      :underline nil
                      :overline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'bv-face-default)
                                    (face-foreground 'bv-face-default))
                      :background (face-background 'bv-face-default)
                      :underline nil
                      :overline nil
                      :inherit nil
                      :box nil)

  (set-face-attribute 'header-line nil
                      :weight 'light
                      :foreground (face-foreground 'bv-face-default)
                      :background (face-background 'bv-face-default)
                      :overline nil
                      :underline nil
                      :box nil
                      :box `(:line-width 1
                                         :color ,(face-background 'bv-face-default)
                                         :style nil)
                      :inherit nil)

  (set-face-attribute 'internal-border nil
                      :background (face-background 'bv-face-default)))

(defun bv-theme--hl-line ()
  "Derive hl-line faces from bv faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                        :background bv-color-highlight)))

(defun bv-theme--org ()
  "Derive org faces from bv faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'bv-face-faded)
    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'bv-face-faded)
    (set-face 'org-block-end-line                      'bv-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))
    (set-face 'org-checkbox                            'bv-face-faded)
    (set-face 'org-checkbox-statistics-done            'bv-face-faded)
    (set-face 'org-checkbox-statistics-todo            'bv-face-faded)
    (set-face 'org-clock-overlay                       'bv-face-faded)
    (set-face 'org-code                                'bv-face-faded)
    (set-face 'org-column                              'bv-face-faded)
    (set-face 'org-column-title                        'bv-face-faded)
    (set-face 'org-date                                'bv-face-faded)
    (set-face 'org-date-selected                       'bv-face-faded)
    (set-face 'org-default                             'bv-face-faded)
    (set-face 'org-document-info                       'bv-face-faded)
    (set-face 'org-document-info-keyword               'bv-face-faded)
    (set-face 'org-document-title                      'bv-face-faded)
    (set-face 'org-done                              'bv-face-default)
    (set-face 'org-drawer                              'bv-face-faded)
    (set-face 'org-ellipsis                            'bv-face-faded)
    (set-face 'org-footnote                            'bv-face-faded)
    (set-face 'org-formula                             'bv-face-faded)
    (set-face 'org-headline-done                       'bv-face-faded)
    (set-face 'org-latex-and-related                   'bv-face-faded)
    (set-face 'org-level-1                            'bv-face-strong)
    (set-face 'org-level-2                            'bv-face-strong)
    (set-face 'org-level-3                            'bv-face-strong)
    (set-face 'org-level-4                            'bv-face-strong)
    (set-face 'org-level-5                            'bv-face-strong)
    (set-face 'org-level-6                            'bv-face-strong)
    (set-face 'org-level-7                            'bv-face-strong)
    (set-face 'org-level-8                            'bv-face-strong)
    (set-face 'org-link                              'bv-face-salient)
    (set-face 'org-list-dt                             'bv-face-faded)
    (set-face 'org-macro                               'bv-face-faded)
    (set-face 'org-meta-line                           'bv-face-faded)
    (set-face 'org-mode-line-clock                     'bv-face-faded)
    (set-face 'org-mode-line-clock-overrun             'bv-face-faded)
    (set-face 'org-priority                            'bv-face-faded)
    (set-face 'org-property-value                      'bv-face-faded)
    (set-face 'org-quote                               'bv-face-faded)
    (set-face 'org-scheduled                           'bv-face-faded)
    (set-face 'org-scheduled-previously                'bv-face-faded)
    (set-face 'org-scheduled-today                   '(bv-face-salient
                                                       bv-face-strong))
    (set-face 'org-sexp-date                           'bv-face-faded)
    (set-face 'org-special-keyword                     'bv-face-faded)
    (set-face 'org-table                               'bv-face-faded)
    (set-face 'org-tag                                'bv-face-popout)
    (set-face 'org-tag-group                           'bv-face-faded)
    (set-face 'org-target                              'bv-face-faded)
    (set-face 'org-time-grid                           'bv-face-faded)
    (set-face 'org-todo                              'bv-face-salient)
    (set-face 'org-upcoming-deadline                 'bv-face-default)
    (set-face 'org-verbatim                           'bv-face-popout)
    (set-face 'org-verse                               'bv-face-faded)
    (set-face 'org-warning                            'bv-face-popout)))

(defun bv-theme ()
  "Derive many faces from the core bv faces."
  (bv-theme--basics)
  (bv-theme--font-lock)
  (bv-theme--mode-line)
  (bv-theme--hl-line)
  (bv-theme--org))

;; Theme variants

(defun bv-theme-set-light ()
  "Apply light theme base."
  (setq frame-background-mode    'light)
  (setq bv-color-foreground "#3a3a3a") ;; Charcoal
  (setq bv-color-background "#FFFFFF") ;; Pure white
  (setq bv-color-highlight  "#F7F7F7") ;; Off-white
  (setq bv-color-critical   "#d7875f") ;; Terracotta
  (setq bv-color-salient    "#5f87d7") ;; Cornflower blue
  (setq bv-color-strong     "#262626") ;; Dark charcoal
  (setq bv-color-popout     "#87afaf") ;; Sage green
  (setq bv-color-subtle     "#eeeeee") ;; Light gray
  (setq bv-color-faded      "#767676") ;; Medium gray
  (setq bv-theme-var "light"))

(defun bv-theme-set-dark ()
  "Apply dark theme base."
  (setq frame-background-mode     'dark)
  (setq bv-color-foreground "#dadada") ;; Platinum
  (setq bv-color-background "#1c1c1c") ;; Eerie black
  (setq bv-color-highlight  "#303030") ;; Jet
  (setq bv-color-critical   "#d7875f") ;; Terracotta
  (setq bv-color-salient    "#5f87d7") ;; Cornflower blue
  (setq bv-color-strong     "#eeeeee") ;; White smoke
  (setq bv-color-popout     "#afaf87") ;; Olive
  (setq bv-color-subtle     "#303030") ;; Jet
  (setq bv-color-faded      "#6c6c6c") ;; Dim gray
  (setq bv-theme-var "dark"))

;; Public functions

(defun bv-refresh-theme ()
  "Refresh the current theme."
  (interactive)
  (bv-faces)
  (bv-theme)
  (when (fboundp 'bv-modeline-faces)
    (bv-modeline-faces)))

(defun bv-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((string= bv-theme-var "light")
         (bv-theme-set-dark))
        ((string= bv-theme-var "dark")
         (bv-theme-set-light)))
  (bv-refresh-theme))

(provide 'bv-theme)
;;; bv-theme.el ends here