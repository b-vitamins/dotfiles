;;; bv-themes.el --- Sophisticated theme engine -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; A sophisticated theme engine supporting multiple variants through
;; a data-driven architecture with extensive customization options.
;; Inspired by the Modus themes' design principles.

;;; Code:

(require 'cl-lib)
(require 'color)

(defgroup bv-themes ()
  "Sophisticated theme system with semantic color mapping."
  :group 'faces
  :prefix "bv-themes-")

;;; User Options - Syntax Highlighting

(defcustom bv-themes-syntax nil
  "Style for syntax highlighting.
Nil means default.
Other options:
- `faint': Subtle syntax highlighting
- `intense': More vivid colors
- `monochrome': Minimal color variation"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Faint" faint)
                 (const :tag "Intense" intense)
                 (const :tag "Monochrome" monochrome))
  :group 'bv-themes)

;;; User Options - Mode Line

(defcustom bv-themes-mode-line nil
  "Style for mode line presentation.
Nil means default flat style.
Other options:
- `accented': Colored active mode line
- `padded': Extra padding
- `borderless': No borders"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Accented" accented)
                 (const :tag "Padded" padded)
                 (const :tag "Borderless" borderless))
  :group 'bv-themes)

;;; User Options - Typography

(defcustom bv-themes-headings nil
  "Alist of heading level properties.
Each level can have:
- `:height': Scaling factor
- `:weight': Font weight
- `:overline': Whether to add overline"
  :type '(alist :key-type integer
                :value-type (plist :options
                                  ((:height float)
                                   (:weight symbol)
                                   (:overline boolean))))
  :group 'bv-themes)

(defcustom bv-themes-bold-constructs nil
  "When non-nil, use bold for function names and similar constructs."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-italic-constructs nil
  "When non-nil, use italic for comments and docstrings."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-variable-pitch-ui nil
  "When non-nil, use variable pitch for UI elements."
  :type 'boolean
  :group 'bv-themes)

;;; User Options - Org Mode

(defcustom bv-themes-org-blocks nil
  "Style for Org source blocks.
Nil means default subtle background.
Other options:
- `tinted': Slightly colored backgrounds
- `rainbow': Different colors per language"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Tinted" tinted)
                 (const :tag "Rainbow" rainbow))
  :group 'bv-themes)

;;; User Options - UI

(defcustom bv-themes-ui-density 'default
  "UI element density.
Controls padding and spacing in UI elements."
  :type '(choice (const :tag "Compact" compact)
                 (const :tag "Default" default)
                 (const :tag "Comfortable" comfortable))
  :group 'bv-themes)

;;; User Options - Fonts

(defcustom bv-themes-font-family-monospaced nil
  "Monospaced font family.
If nil, uses the default monospace font."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Font family"))
  :group 'bv-themes)

(defcustom bv-themes-font-family-proportional nil
  "Proportional font family.
If nil, uses the default proportional font."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Font family"))
  :group 'bv-themes)

(defcustom bv-themes-font-size 120
  "Font size in units of 1/10 pt."
  :type 'integer
  :group 'bv-themes)

;;; User Options - Palette Overrides

(defcustom bv-themes-common-palette-overrides nil
  "Palette overrides applied to all variants.
An alist of (COLOR . VALUE) pairs where COLOR is a palette key
and VALUE is either a color string or another palette key."
  :type '(alist :key-type symbol
                :value-type (choice string symbol))
  :group 'bv-themes)

(defcustom bv-themes-light-palette-overrides nil
  "Palette overrides applied to light variant only."
  :type '(alist :key-type symbol
                :value-type (choice string symbol))
  :group 'bv-themes)

(defcustom bv-themes-dark-palette-overrides nil
  "Palette overrides applied to dark variant only."
  :type '(alist :key-type symbol
                :value-type (choice string symbol))
  :group 'bv-themes)

;;; Color Utilities

(defun bv-themes--blend (color1 color2 alpha)
  "Blend COLOR1 with COLOR2 by ALPHA (0.0-1.0).
ALPHA of 0.0 returns COLOR1, 1.0 returns COLOR2."
  (cl-destructuring-bind (r1 g1 b1) (color-name-to-rgb color1)
    (cl-destructuring-bind (r2 g2 b2) (color-name-to-rgb color2)
      (color-rgb-to-hex
       (+ (* r1 (- 1.0 alpha)) (* r2 alpha))
       (+ (* g1 (- 1.0 alpha)) (* g2 alpha))
       (+ (* b1 (- 1.0 alpha)) (* b2 alpha))))))

(defun bv-themes--lighten (color percent)
  "Lighten COLOR by PERCENT (0.0-1.0)."
  (bv-themes--blend color "#ffffff" percent))

(defun bv-themes--darken (color percent)
  "Darken COLOR by PERCENT (0.0-1.0)."
  (bv-themes--blend color "#000000" percent))

(defun bv-themes--intensify (color &optional amount)
  "Make COLOR more saturated by AMOUNT (default 0.2)."
  (let ((amount (or amount 0.2)))
    (cl-destructuring-bind (h s l) (apply #'color-rgb-to-hsl
                                          (color-name-to-rgb color))
      (color-hsl-to-hex h (min 1.0 (+ s amount)) l))))

;;; Property Computation Functions

(defun bv-themes--syntax-color (base-color palette)
  "Compute syntax color based on user settings.
BASE-COLOR is the palette key, PALETTE is the current palette."
  (let ((color (bv-themes--retrieve-palette-value base-color palette)))
    (pcase bv-themes-syntax
      ('faint
       (list :foreground (bv-themes--blend color
                                           (bv-themes--retrieve-palette-value 'bg-main palette)
                                           0.3)))
      ('intense
       (list :foreground (bv-themes--intensify color)))
      ('monochrome
       (list :foreground (bv-themes--retrieve-palette-value 'fg-main palette)
             :weight 'medium))
      (_
       (list :foreground color)))))

(defun bv-themes--heading (level palette)
  "Compute heading properties for LEVEL using PALETTE."
  (let* ((user-props (alist-get level bv-themes-headings))
         (default-heights '(1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
         (height (or (plist-get user-props :height)
                    (nth (1- level) default-heights)))
         (weight (or (plist-get user-props :weight)
                    (if (<= level 4) 'medium 'normal)))
         (overline (plist-get user-props :overline)))
    `(:foreground ,(bv-themes--retrieve-palette-value 'fg-header-strong palette)
      :height ,height
      :weight ,weight
      ,@(when overline
          `(:overline ,(bv-themes--retrieve-palette-value 'fg-dim palette))))))

(defun bv-themes--mode-line-props (which palette)
  "Compute mode line properties for WHICH (active or inactive) using PALETTE.
Since we use header-line for modeline display, make mode-line invisible."
  ;; Always use main background to make mode-line invisible
  `(:background ,(bv-themes--retrieve-palette-value 'bg-main palette)
    :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
    :box nil
    :underline nil
    :overline nil))

(defun bv-themes--org-block-bg (lang palette)
  "Compute org block background based on settings, LANG, and PALETTE."
  (pcase bv-themes-org-blocks
    ('tinted
     (bv-themes--retrieve-palette-value 'bg-dim palette))
    ('rainbow
     (let ((colors '(bg-completion bg-hover bg-highlight bg-selection)))
       (bv-themes--retrieve-palette-value
        (nth (mod (sxhash lang) (length colors)) colors)
        palette)))
    (_
     (bv-themes--retrieve-palette-value 'bg-dim palette))))

;;; Semantic faces (these are the building blocks)

(defface bv-themes-default nil
  "Base face for default text."
  :group 'bv-themes)

(defface bv-themes-strong nil
  "Face for structurally important elements."
  :group 'bv-themes)

(defface bv-themes-emphasis nil
  "Face for emphasized text."
  :group 'bv-themes)

(defface bv-themes-faded nil
  "Face for de-emphasized elements."
  :group 'bv-themes)

(defface bv-themes-subtle nil
  "Face for subtle backgrounds."
  :group 'bv-themes)

(defface bv-themes-salient nil
  "Face for elements that should stand out."
  :group 'bv-themes)

(defface bv-themes-popout nil
  "Face for attention-grabbing elements."
  :group 'bv-themes)

(defface bv-themes-critical nil
  "Face for critical information."
  :group 'bv-themes)

;;; Header line faces (for modeline)

(defface bv-themes-header-default nil
  "Default face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-strong nil
  "Strong face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-salient nil
  "Salient face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-popout nil
  "Popout face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-faded nil
  "Faded face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-critical nil
  "Critical face for the header line."
  :group 'bv-themes)

;;; Palette utilities

(defconst bv-themes-base-palette
  '((accent-0 . "#5f87d7")         ; Blue
    (accent-1 . "#87afaf")         ; Sage green
    (accent-2 . "#d7875f"))        ; Terracotta
  "Base palette with common accent colors.")

(defun bv-themes--palette-value (theme-name &optional overrides)
  "Get palette for THEME-NAME with optional OVERRIDES.
THEME-NAME should be a symbol like 'bv-light or 'bv-dark."
  (let* ((base-palette-name (intern (format "%s-palette" theme-name)))
         (base-palette (symbol-value base-palette-name))
         (variant-overrides
          (pcase theme-name
            ('bv-light bv-themes-light-palette-overrides)
            ('bv-dark bv-themes-dark-palette-overrides)
            (_ nil))))
    (append overrides
            variant-overrides
            bv-themes-common-palette-overrides
            base-palette
            bv-themes-base-palette)))

(defun bv-themes--retrieve-palette-value (color palette)
  "Recursively retrieve COLOR from PALETTE."
  (let ((value (cdr (assq color palette))))
    (cond
     ((stringp value) value)
     ((symbolp value)
      (bv-themes--retrieve-palette-value value palette))
     (t 'unspecified))))

;;; Face specifications

(defun bv-themes--face-specs (palette)
  "Generate face specifications using PALETTE."
  (let ((c '((class color) (min-colors 256)))
        (bold-constructs bv-themes-bold-constructs)
        (italic-constructs bv-themes-italic-constructs)
        (variable-pitch-ui bv-themes-variable-pitch-ui)
        (font-mono (or bv-themes-font-family-monospaced
                       (face-attribute 'default :family)))
        (font-prop (or bv-themes-font-family-proportional
                       (face-attribute 'variable-pitch :family)))
        (font-size bv-themes-font-size))
    `(;; Core semantic faces
      (bv-themes-default
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-main palette))))

      (bv-themes-strong
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-active palette)
            :weight ,(if bold-constructs 'medium 'normal))))

      (bv-themes-emphasis
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      (bv-themes-faded
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      (bv-themes-subtle
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette))))

      (bv-themes-salient
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'salient palette))))

      (bv-themes-popout
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'popout palette))))

      (bv-themes-critical
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'critical palette))))

      ;; Basic faces
      (default
       ((,c :family ,font-mono
            :height ,font-size
            :weight light
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-main palette))))

      (cursor
       ((,c :background ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (region
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-selection palette)
            :extend t)))

      (highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-highlight palette))))

      (hl-line
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette)
            :extend t)))

      ;; Font lock faces with syntax variants
      (font-lock-keyword-face
       ((,c ,@(bv-themes--syntax-color 'keyword palette)
            :weight ,(if bold-constructs 'medium 'normal))))

      (font-lock-string-face
       ((,c ,@(bv-themes--syntax-color 'string palette))))

      (font-lock-comment-face
       ((,c ,@(bv-themes--syntax-color 'comment palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      (font-lock-function-name-face
       ((,c ,@(bv-themes--syntax-color 'fnname palette))))

      (font-lock-variable-name-face
       ((,c ,@(bv-themes--syntax-color 'variable palette))))

      (font-lock-type-face
       ((,c ,@(bv-themes--syntax-color 'type palette))))

      (font-lock-constant-face
       ((,c ,@(bv-themes--syntax-color 'constant palette))))

      (font-lock-builtin-face
       ((,c ,@(bv-themes--syntax-color 'builtin palette))))

      (font-lock-preprocessor-face
       ((,c ,@(bv-themes--syntax-color 'preprocessor palette))))

      (font-lock-warning-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette)
            :weight bold)))

      ;; Mode line with variants
      (mode-line
       ((,c ,@(bv-themes--mode-line-props 'active palette))))

      (mode-line-inactive
       ((,c ,@(bv-themes--mode-line-props 'inactive palette))))

      (mode-line-emphasis
       ((,c :inherit bv-themes-strong)))

      (mode-line-highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette))))

      ;; Headings with dynamic properties
      (org-level-1 ((,c ,@(bv-themes--heading 1 palette))))
      (org-level-2 ((,c ,@(bv-themes--heading 2 palette))))
      (org-level-3 ((,c ,@(bv-themes--heading 3 palette))))
      (org-level-4 ((,c ,@(bv-themes--heading 4 palette))))
      (org-level-5 ((,c ,@(bv-themes--heading 5 palette))))
      (org-level-6 ((,c ,@(bv-themes--heading 6 palette))))
      (org-level-7 ((,c ,@(bv-themes--heading 7 palette))))
      (org-level-8 ((,c ,@(bv-themes--heading 8 palette))))

      ;; Completion faces with multiple match levels
      (orderless-match-face-0
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-completion-match-0 palette)
            :weight bold)))
      (orderless-match-face-1
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-completion-match-1 palette)
            :weight bold)))
      (orderless-match-face-2
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-completion-match-2 palette)
            :weight bold)))
      (orderless-match-face-3
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-completion-match-3 palette)
            :weight bold)))

      ;; Diff faces with proper backgrounds
      (diff-added
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-added palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-added palette)
            :extend t)))
      (diff-removed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-removed palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-removed palette)
            :extend t)))
      (diff-changed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-changed palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-changed palette)
            :extend t)))

      ;; Org blocks with dynamic backgrounds
      (org-block
       ((,c :background ,(bv-themes--org-block-bg 'default palette)
            :extend t)))
      (org-block-begin-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-alt palette)
            :extend t)))
      (org-block-end-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-alt palette)
            :extend t)))

      ;; Org elements
      (org-code
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2-faint palette))))
      (org-verbatim
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1-faint palette))))
      (org-table
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))
      (org-formula
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-faint palette))))
      (org-quote
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      ;; Org todo keywords
      (org-todo
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette)
            :weight bold)))
      (org-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette)
            :weight medium)))
      (org-headline-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      ;; Org meta
      (org-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (org-tag
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1-faint palette)
            :weight light)))
      (org-meta-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-inactive palette))))
      (org-document-title
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-header-strong palette)
            :weight bold
            :height 1.5)))
      (org-document-info
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (org-document-info-keyword
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-inactive palette))))

      ;; Links and buttons
      (link
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :underline t)))
      (link-visited
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-faint palette)
            :underline t)))

      ;; Additional standard faces...
      (fringe
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-dim palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-main palette))))

      (vertical-border
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-active palette))))

      (minibuffer-prompt
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :weight medium)))

      ;; Search and matching
      (match
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette)
            :weight bold)))
      (isearch
       ((,c :background ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :weight bold)))
      (lazy-highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion-subtle palette))))

      ;; Line numbers
      (line-number
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-inactive palette))))
      (line-number-current-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight medium)))

      ;; Parentheses
      (show-paren-match
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette)
            :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :weight bold)))
      (show-paren-mismatch
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-removed palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-removed palette))))

      ;; Package-specific faces...
      ;; Vertico
      (vertico-current
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette)
            :extend t)))

      ;; Corfu
      (corfu-current
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette)
            :extend t)))
      (corfu-default
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette))))
      (corfu-border
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-active palette))))

      ;; Consult
      (consult-file
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))
      (consult-bookmark
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))

      ;; Marginalia
      (marginalia-documentation
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      ;; Which-key
      (which-key-key-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :weight medium)))
      (which-key-separator-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (which-key-command-description-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      ;; Magit
      (magit-branch-current
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :weight bold)))
      (magit-branch-local
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (magit-branch-remote
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (magit-hash
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (magit-section-heading
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :weight medium)))

      ;; Tree-sitter
      (tree-sitter-hl-face:function
       ((,c ,@(bv-themes--syntax-color 'fnname palette))))
      (tree-sitter-hl-face:function.call
       ((,c ,@(bv-themes--syntax-color 'fnname palette))))
      (tree-sitter-hl-face:keyword
       ((,c ,@(bv-themes--syntax-color 'keyword palette))))
      (tree-sitter-hl-face:string
       ((,c ,@(bv-themes--syntax-color 'string palette))))
      (tree-sitter-hl-face:type
       ((,c ,@(bv-themes--syntax-color 'type palette))))

      ;; LSP/Eglot
      (lsp-face-highlight-textual
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette))))
      (lsp-face-highlight-read
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover-secondary palette))))
      (lsp-face-highlight-write
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette)
            :weight bold)))

      ;; Flycheck/Flymake
      (flycheck-error
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'error palette)))))
      (flycheck-warning
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'warning palette)))))
      (flycheck-info
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'info palette)))))

      ;; Company
      (company-tooltip
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette))))
      (company-tooltip-selection
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette))))
      (company-tooltip-common
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight bold)))

      ;; Org agenda
      (org-agenda-structure
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-header-strong palette)
            :weight light
            :height 1.2)))
      (org-agenda-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight regular)))
      (org-agenda-date-today
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight regular
            :underline t)))
      (org-agenda-date-weekend
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :weight regular)))

      ;; Header line faces
      (bv-themes-header-default
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-mode-line-active palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-mode-line-active palette))))
      (bv-themes-header-strong
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-header-strong palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-header-strong palette)
            :weight medium)))
      (bv-themes-header-salient
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (bv-themes-header-popout
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (bv-themes-header-critical
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'accent-2 palette))))
      (bv-themes-header-faded
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-dim palette))))

      ;; Rainbow delimiters
      (rainbow-delimiters-depth-1-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (rainbow-delimiters-depth-2-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (rainbow-delimiters-depth-3-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette))))
      (rainbow-delimiters-depth-4-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette))))
      (rainbow-delimiters-depth-5-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1-intense palette))))
      (rainbow-delimiters-depth-6-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2-intense palette))))
      (rainbow-delimiters-depth-7-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-faint palette))))
      (rainbow-delimiters-depth-8-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1-faint palette))))
      (rainbow-delimiters-depth-9-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2-faint palette)))))))

;;; Theme generation

(defmacro bv-themes-theme (name palette)
  "Generate theme NAME using PALETTE."
  (let ((theme-name name))
    `(progn
       (deftheme ,name
         ,(format "Theme variant: %s" name))

       (let* ((palette (bv-themes--palette-value ',theme-name))
              (faces (bv-themes--face-specs palette)))
         (apply #'custom-theme-set-faces ',name faces))

       (provide-theme ',name))))

;;; Interactive commands

(defvar bv-themes-variants '(bv-light bv-dark)
  "List of available theme variants.")

(defun bv-themes-load-theme (theme)
  "Load theme variant THEME."
  (interactive
   (list (intern (completing-read "Load theme: "
                                  (mapcar #'symbol-name bv-themes-variants)))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun bv-themes-toggle ()
  "Toggle between light and dark themes."
  (interactive)
  (let* ((current (cl-find-if (lambda (theme)
                                (memq theme custom-enabled-themes))
                              bv-themes-variants))
         (next (if (eq current 'bv-light) 'bv-dark 'bv-light)))
    (bv-themes-load-theme next)
    (message "Switched to %s theme" next)))

(defun bv-themes-current ()
  "Return the currently active theme variant, or nil if none active."
  (cl-find-if (lambda (theme)
                (memq theme custom-enabled-themes))
              bv-themes-variants))

(defun bv-themes-variant ()
  "Return the variant name of the current theme ('light' or 'dark')."
  (let ((current (bv-themes-current)))
    (cond
     ((eq current 'bv-light) "light")
     ((eq current 'bv-dark) "dark")
     (t nil))))

;;; Development helpers

(defun bv-themes-preview-colors ()
  "Display all current theme colors in a buffer."
  (interactive)
  (let* ((theme (bv-themes-current))
         (palette (when theme (bv-themes--palette-value theme)))
         (buf (get-buffer-create "*Theme Colors*")))
    (if (not palette)
        (message "No theme currently active")
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "Theme Colors: %s\n\n" theme))
        (insert "Color Name                  Color      Sample\n")
        (insert "─────────────────────────────────────────────\n")
        (dolist (entry palette)
          (when (stringp (cdr entry))
            (let* ((name (car entry))
                   (value (bv-themes--retrieve-palette-value name palette))
                   (sample "████████"))
              (when (stringp value)
                (insert (format "%-25s  %-9s  "
                                (symbol-name name)
                                value))
                (insert (propertize sample 'face `(:foreground ,value)))
                (insert "\n")))))
        (goto-char (point-min))
        (special-mode))
      (switch-to-buffer buf))))

(defun bv-themes-contrast-report ()
  "Generate WCAG contrast report for current theme."
  (interactive)
  (let* ((theme (bv-themes-current))
         (palette (when theme (bv-themes--palette-value theme)))
         (buf (get-buffer-create "*Theme Contrast Report*")))
    (if (not palette)
        (message "No theme currently active")
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "WCAG Contrast Report: %s\n\n" theme))
        (let ((bg-main (bv-themes--retrieve-palette-value 'bg-main palette))
              (test-colors '(fg-main fg-dim fg-active accent-0 accent-1 accent-2)))
          (insert "Testing against background: " bg-main "\n\n")
          (insert "Color        Value      Ratio  WCAG AA  WCAG AAA\n")
          (insert "────────────────────────────────────────────────\n")
          (dolist (color-name test-colors)
            (let* ((fg-color (bv-themes--retrieve-palette-value color-name palette))
                   (ratio (when (stringp fg-color)
                            (color-contrast fg-color bg-main)))
                   (aa-pass (and ratio (>= ratio 4.5)))
                   (aaa-pass (and ratio (>= ratio 7.0))))
              (when ratio
                (insert (format "%-10s  %-9s  %5.2f  %-7s  %s\n"
                                (symbol-name color-name)
                                fg-color
                                ratio
                                (if aa-pass "✓ Pass" "✗ Fail")
                                (if aaa-pass "✓ Pass" "✗ Fail")))))))
        (goto-char (point-min))
        (special-mode))
      (switch-to-buffer buf))))

;;; Provide a macro for users to evaluate colors in context

(defmacro bv-themes-with-colors (&rest body)
  "Execute BODY with current theme's colors bound.
Each color from the palette is bound as a variable."
  (declare (indent 0))
  `(let* ((current-theme (bv-themes-current))
          (palette (when current-theme
                     (bv-themes--palette-value current-theme))))
     (if (not palette)
         (error "No theme currently active")
       (let ,(cl-loop for entry in '(bg-main fg-main bg-dim fg-dim
                                     bg-alt fg-alt bg-active fg-active
                                     bg-inactive fg-inactive
                                     accent-0 accent-0-faint accent-0-intense
                                     accent-1 accent-1-faint accent-1-intense
                                     accent-2 accent-2-faint accent-2-intense)
                      collect `(,entry (bv-themes--retrieve-palette-value
                                        ',entry palette)))
         ,@body))))

(provide 'bv-themes)
;;; bv-themes.el ends here
