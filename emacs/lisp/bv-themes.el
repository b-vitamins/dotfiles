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
- `monochrome': Minimal color variation
- `rainbow': Maximum color variety
- `tinted': Slightly desaturated colors"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Faint" faint)
                 (const :tag "Intense" intense)
                 (const :tag "Monochrome" monochrome)
                 (const :tag "Rainbow" rainbow)
                 (const :tag "Tinted" tinted))
  :group 'bv-themes)

;;; User Options - Mode Line

(defcustom bv-themes-mode-line nil
  "Style for mode line presentation.
Nil means default flat style.
Other options:
- `accented': Colored active mode line
- `padded': Extra padding
- `borderless': No borders
- `gradient': Gradient effect (simulated)
- `minimal': Very subtle styling"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Accented" accented)
                 (const :tag "Padded" padded)
                 (const :tag "Borderless" borderless)
                 (const :tag "Gradient" gradient)
                 (const :tag "Minimal" minimal))
  :group 'bv-themes)

;;; User Options - Typography

(defcustom bv-themes-headings nil
  "Alist of heading level properties.
Each level can have:
- `:height': Scaling factor
- `:weight': Font weight
- `:overline': Whether to add overline
- `:style': Special styling (rainbow, gradient, monochrome)"
  :type '(alist :key-type integer
                :value-type (plist :options
                                  ((:height float)
                                   (:weight symbol)
                                   (:overline boolean)
                                   (:style symbol))))
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
- `rainbow': Different colors per language
- `zebra': Alternating backgrounds
- `minimal': No background distinction"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Tinted" tinted)
                 (const :tag "Rainbow" rainbow)
                 (const :tag "Zebra" zebra)
                 (const :tag "Minimal" minimal))
  :group 'bv-themes)

(defcustom bv-themes-org-agenda-structure nil
  "Style for org-agenda structure.
Can be `default', `rainbow', or `gradient'."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Rainbow" rainbow)
                 (const :tag "Gradient" gradient))
  :group 'bv-themes)

;;; User Options - UI

(defcustom bv-themes-ui-density 'default
  "UI element density.
Controls padding and spacing in UI elements."
  :type '(choice (const :tag "Compact" compact)
                 (const :tag "Default" default)
                 (const :tag "Comfortable" comfortable))
  :group 'bv-themes)

(defcustom bv-themes-fringes nil
  "Style for window fringes.
Can be nil (default), `subtle', `greyscale', or `invisible'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Subtle" subtle)
                 (const :tag "Greyscale" greyscale)
                 (const :tag "Invisible" invisible))
  :group 'bv-themes)

(defcustom bv-themes-completions nil
  "Style for completion interfaces.
Can be nil (default), `opinionated', `moderate', or `minimal'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Opinionated" opinionated)
                 (const :tag "Moderate" moderate)
                 (const :tag "Minimal" minimal))
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

;;; User Options - Diffs

(defcustom bv-themes-diffs nil
  "Style for diffs and version control.
Can be nil (default), `desaturated', `fg-only', `bg-only', or `deuteranopia'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Desaturated" desaturated)
                 (const :tag "Foreground only" fg-only)
                 (const :tag "Background only" bg-only)
                 (const :tag "Deuteranopia" deuteranopia))
  :group 'bv-themes)

;;; User Options - Links

(defcustom bv-themes-links nil
  "Style for links.
Can be nil (default), `neutral-underline', `faint', `no-underline'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Neutral underline" neutral-underline)
                 (const :tag "Faint" faint)
                 (const :tag "No underline" no-underline))
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

(defun bv-themes--desaturate (color &optional amount)
  "Make COLOR less saturated by AMOUNT (default 0.3)."
  (let ((amount (or amount 0.3)))
    (cl-destructuring-bind (h s l) (apply #'color-rgb-to-hsl
                                          (color-name-to-rgb color))
      (color-hsl-to-hex h (max 0.0 (- s amount)) l))))

(defun bv-themes--rotate-hue (color degrees)
  "Rotate COLOR hue by DEGREES."
  (cl-destructuring-bind (h s l) (apply #'color-rgb-to-hsl
                                        (color-name-to-rgb color))
    (color-hsl-to-hex (mod (+ h (/ degrees 360.0)) 1.0) s l)))

;;; Property Computation Functions

(defun bv-themes--syntax-color (base-color palette)
  "Compute syntax color based on user settings.
BASE-COLOR is the palette key, PALETTE is the current palette."
  (let ((color (bv-themes--retrieve-palette-value base-color palette)))
    (pcase bv-themes-syntax
      ('faint
       (list :foreground (bv-themes--blend color
                                           (bv-themes--retrieve-palette-value 'bg-main palette)
                                           0.4)))
      ('intense
       (list :foreground (bv-themes--intensify color 0.3)))
      ('monochrome
       (list :foreground (bv-themes--retrieve-palette-value 'fg-main palette)
             :weight (if (memq base-color '(keyword fnname type)) 'medium 'normal)))
      ('rainbow
       (list :foreground color
             :weight (if (memq base-color '(keyword fnname)) 'medium 'normal)))
      ('tinted
       (list :foreground (bv-themes--desaturate color 0.2)))
      (_
       (list :foreground color)))))

(defun bv-themes--heading (level palette)
  "Compute heading properties for LEVEL using PALETTE."
  (let* ((user-props (alist-get level bv-themes-headings))
         (default-heights '(1.8 1.5 1.3 1.2 1.1 1.05 1.0 1.0))
         (height (or (plist-get user-props :height)
                    (nth (1- level) default-heights)))
         (weight (or (plist-get user-props :weight)
                    (if (<= level 4) 'medium 'normal)))
         (style (plist-get user-props :style))
         (overline (plist-get user-props :overline))
         (colors '(fg-heading-1 fg-heading-2 fg-heading-3 fg-heading-4
                   fg-heading-5 fg-heading-6 fg-heading-7 fg-heading-8)))
    `(:foreground ,(bv-themes--retrieve-palette-value (nth (1- level) colors) palette)
      :height ,height
      :weight ,weight
      ,@(when overline
          `(:overline ,(bv-themes--retrieve-palette-value 'border palette)))
      ,@(when (eq style 'rainbow)
          `(:inherit ,(intern (format "bv-themes-heading-%d-rainbow" level)))))))

(defun bv-themes--mode-line-props (which palette)
  "Compute mode line properties for WHICH (active or inactive) using PALETTE."
  (let ((style bv-themes-mode-line))
    (pcase which
      ('active
       (pcase style
         ('accented
          `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-active-accent palette)
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active-accent palette)
            :box (:line-width 1 :color ,(bv-themes--retrieve-palette-value 'modeline-border-active palette))))
         ('gradient
          `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-active palette)
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active palette)
            :box (:line-width 3 :color ,(bv-themes--retrieve-palette-value 'modeline-bg-active-accent palette))))
         ('minimal
          `(:background ,(bv-themes--retrieve-palette-value 'bg-dim palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :box nil))
         ('borderless
          `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-active palette)
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active palette)
            :box nil))
         ('padded
          `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-active palette)
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active palette)
            :box (:line-width 4 :color ,(bv-themes--retrieve-palette-value 'modeline-bg-active palette))))
         (_
          `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-active palette)
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active palette)))))
      ('inactive
       `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-inactive palette)
         :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-inactive palette)
         :box nil)))))

(defun bv-themes--org-block-bg (lang palette)
  "Compute org block background based on settings, LANG, and PALETTE."
  (pcase bv-themes-org-blocks
    ('tinted
     (bv-themes--retrieve-palette-value 'bg-prose-block-contents palette))
    ('rainbow
     (let ((colors '(bg-red-nuanced bg-yellow-nuanced bg-green-nuanced
                     bg-cyan-nuanced bg-blue-nuanced bg-magenta-nuanced)))
       (bv-themes--retrieve-palette-value
        (nth (mod (sxhash lang) (length colors)) colors)
        palette)))
    ('zebra
     (if (cl-evenp (sxhash lang))
         (bv-themes--retrieve-palette-value 'bg-dim palette)
       (bv-themes--retrieve-palette-value 'bg-alt palette)))
    ('minimal
     'unspecified)
    (_
     (bv-themes--retrieve-palette-value 'bg-dim palette))))

(defun bv-themes--link-props (palette)
  "Compute link properties based on user settings and PALETTE."
  (let ((color (bv-themes--retrieve-palette-value 'fg-link palette)))
    (pcase bv-themes-links
      ('neutral-underline
       `(:foreground ,color :underline ,(bv-themes--retrieve-palette-value 'border palette)))
      ('faint
       `(:foreground ,(bv-themes--retrieve-palette-value 'fg-link-faint palette) :underline t))
      ('no-underline
       `(:foreground ,color :underline nil))
      (_
       `(:foreground ,color :underline t)))))

(defun bv-themes--completion-props (which palette)
  "Compute completion properties for WHICH match level using PALETTE."
  (let ((fg-key (intern (format "fg-completion-match-%d" which)))
        (bg-key (intern (format "bg-completion-match-%d" which))))
    (pcase bv-themes-completions
      ('opinionated
       `(:foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
         :background ,(bv-themes--retrieve-palette-value fg-key palette)
         :weight bold))
      ('moderate
       `(:foreground ,(bv-themes--retrieve-palette-value fg-key palette)
         :background ,(bv-themes--retrieve-palette-value bg-key palette)))
      ('minimal
       `(:foreground ,(bv-themes--retrieve-palette-value fg-key palette)
         :weight medium))
      (_
       `(:foreground ,(bv-themes--retrieve-palette-value fg-key palette)
         :weight bold)))))

(defun bv-themes--diff-props (which side palette)
  "Compute diff properties for WHICH (added/changed/removed) SIDE (bg/fg) using PALETTE."
  (let* ((base-bg-key (intern (format "bg-%s" which)))
         (base-fg-key (intern (format "fg-%s" which)))
         (refine-bg-key (intern (format "bg-%s-refine" which)))
         (refine-fg-key (intern (format "fg-%s-refine" which))))
    (pcase bv-themes-diffs
      ('desaturated
       (pcase side
         ('bg `(:background ,(bv-themes--desaturate
                              (bv-themes--retrieve-palette-value base-bg-key palette) 0.5)))
         ('fg `(:foreground ,(bv-themes--desaturate
                              (bv-themes--retrieve-palette-value base-fg-key palette) 0.3)))))
      ('fg-only
       (pcase side
         ('bg `(:background unspecified))
         ('fg `(:foreground ,(bv-themes--intensify
                              (bv-themes--retrieve-palette-value base-fg-key palette) 0.3)))))
      ('bg-only
       (pcase side
         ('bg `(:background ,(bv-themes--retrieve-palette-value base-bg-key palette)))
         ('fg `(:foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)))))
      ('deuteranopia
       (let ((deut-colors '((added . green) (removed . blue) (changed . yellow))))
         (pcase side
           ('bg `(:background ,(bv-themes--retrieve-palette-value
                                (intern (format "bg-%s-nuanced" (alist-get which deut-colors)))
                                palette)))
           ('fg `(:foreground ,(bv-themes--retrieve-palette-value
                                (alist-get which deut-colors)
                                palette))))))
      (_
       (pcase side
         ('bg `(:background ,(bv-themes--retrieve-palette-value base-bg-key palette)))
         ('fg `(:foreground ,(bv-themes--retrieve-palette-value base-fg-key palette))))))))

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

;;; Additional semantic faces

(defface bv-themes-accent-0 nil
  "Primary accent color face."
  :group 'bv-themes)

(defface bv-themes-accent-1 nil
  "Secondary accent color face."
  :group 'bv-themes)

(defface bv-themes-accent-2 nil
  "Tertiary accent color face."
  :group 'bv-themes)

(defface bv-themes-accent-3 nil
  "Quaternary accent color face."
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

;;; Rainbow heading faces (for special heading styles)

(dotimes (i 8)
  (let ((n (1+ i)))
    (eval `(defface ,(intern (format "bv-themes-heading-%d-rainbow" n)) nil
             ,(format "Rainbow variant of heading level %d." n)
             :group 'bv-themes))))

;;; Palette utilities

(defconst bv-themes-base-palette
  '(;; These are shared across all themes but can be overridden
    )
  "Base palette with common colors.")

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
  "Recursively retrieve COLOR from PALETTE.
Supports recursive semantic mappings like Modus themes."
  (let ((value (cdr (assq color palette)))
        (seen nil))
    ;; Handle direct unspecified values
    (when (eq value 'unspecified)
      (setq value 'unspecified))
    ;; Resolve symbolic references
    (while (and (symbolp value)
                (not (eq value 'unspecified))
                (not (memq value seen)))
      (push value seen)
      (setq value (cdr (assq value palette))))
    (cond
     ((stringp value) value)
     ((eq value 'unspecified) 'unspecified)
     ((null value) (error "Color `%s' not found in palette" color))
     (t (error "Circular reference detected for color `%s'" color)))))

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
            :weight ,(if bold-constructs 'bold 'medium))))

      (bv-themes-emphasis
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :slant ,(if italic-constructs 'italic 'normal)
            :weight ,(if bold-constructs 'bold 'medium))))

      (bv-themes-faded
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      (bv-themes-subtle
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette))))

      (bv-themes-salient
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))

      (bv-themes-popout
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))

      (bv-themes-critical
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'red-intense palette))))

      ;; Accent faces
      (bv-themes-accent-0
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (bv-themes-accent-1
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (bv-themes-accent-2
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette))))
      (bv-themes-accent-3
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-3 palette))))

      ;; Basic faces
      (default
       ((,c :family ,font-mono
            :height ,font-size
            :weight light
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-main palette))))

      (cursor
       ((,c :background ,(bv-themes--retrieve-palette-value 'cursor palette))))

      (region
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-region palette)
            ,@(let ((fg (bv-themes--retrieve-palette-value 'fg-region palette)))
                (unless (eq fg 'unspecified)
                  (list :foreground fg)))
            :extend t)))

      (highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette))))

      (hl-line
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hl-line palette)
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

      (font-lock-doc-face
       ((,c ,@(bv-themes--syntax-color 'docstring palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      (font-lock-negation-char-face
       ((,c ,@(bv-themes--syntax-color 'fnname palette))))

      (font-lock-regexp-grouping-backslash
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'rx-backslash palette))))

      (font-lock-regexp-grouping-construct
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'rx-construct palette))))

      ;; Mode line with variants
      (mode-line
       ((,c ,@(bv-themes--mode-line-props 'active palette))))

      (mode-line-inactive
       ((,c ,@(bv-themes--mode-line-props 'inactive palette))))

      (mode-line-emphasis
       ((,c :inherit bv-themes-strong)))

      (mode-line-highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover-secondary palette))))

      ;; Headings with dynamic properties
      (org-level-1 ((,c ,@(bv-themes--heading 1 palette))))
      (org-level-2 ((,c ,@(bv-themes--heading 2 palette))))
      (org-level-3 ((,c ,@(bv-themes--heading 3 palette))))
      (org-level-4 ((,c ,@(bv-themes--heading 4 palette))))
      (org-level-5 ((,c ,@(bv-themes--heading 5 palette))))
      (org-level-6 ((,c ,@(bv-themes--heading 6 palette))))
      (org-level-7 ((,c ,@(bv-themes--heading 7 palette))))
      (org-level-8 ((,c ,@(bv-themes--heading 8 palette))))

      ;; Rainbow heading styles
      ,@(cl-loop for i from 1 to 8
                 for colors = '(red-faint orange-faint yellow-faint green-faint
                                cyan-faint blue-faint purple-faint magenta-faint)
                 collect `(,(intern (format "bv-themes-heading-%d-rainbow" i))
                           ((,c :foreground ,(bv-themes--retrieve-palette-value
                                              (nth (1- i) colors) palette)))))

      ;; Completion faces with multiple match levels
      ,@(cl-loop for i from 0 to 3
                 collect `(,(intern (format "orderless-match-face-%d" i))
                           ((,c ,@(bv-themes--completion-props i palette)))))

      (completions-common-part
       ((,c ,@(bv-themes--completion-props 0 palette))))

      (completions-first-difference
       ((,c ,@(bv-themes--completion-props 1 palette))))

      ;; Diff faces with proper backgrounds
      (diff-added
       ((,c ,@(bv-themes--diff-props 'added 'bg palette)
            ,@(bv-themes--diff-props 'added 'fg palette)
            :extend t)))
      (diff-removed
       ((,c ,@(bv-themes--diff-props 'removed 'bg palette)
            ,@(bv-themes--diff-props 'removed 'fg palette)
            :extend t)))
      (diff-changed
       ((,c ,@(bv-themes--diff-props 'changed 'bg palette)
            ,@(bv-themes--diff-props 'changed 'fg palette)
            :extend t)))

      (diff-refine-added
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-added-refine palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-added-intense palette))))
      (diff-refine-removed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-removed-refine palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-removed-intense palette))))
      (diff-refine-changed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-changed-refine palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-changed-intense palette))))

      ;; Org blocks with dynamic backgrounds
      (org-block
       ((,c :background ,(bv-themes--org-block-bg 'default palette)
            :extend t)))
      (org-block-begin-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-prose-block-delimiter palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-prose-block-delimiter palette)
            :extend t)))
      (org-block-end-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-prose-block-delimiter palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-prose-block-delimiter palette)
            :extend t)))

      ;; Org elements
      (org-code
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-code palette))))
      (org-verbatim
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-verbatim palette))))
      (org-table
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-table palette))))
      (org-formula
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-macro palette))))
      (org-quote
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      ;; Org todo keywords
      (org-todo
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-todo palette)
            :weight bold)))
      (org-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-done palette)
            :weight medium)))
      (org-headline-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      ;; Org dates and scheduling
      (org-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette))))
      (org-scheduled
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-scheduled palette))))
      (org-scheduled-today
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-scheduled palette)
            :weight bold)))
      (org-scheduled-previously
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-warning palette))))
      (org-upcoming-deadline
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-deadline palette))))
      (org-deadline-announce
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-deadline palette)
            :weight bold)))
      (org-time-grid
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (org-upcoming-distant-deadline
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette))))

      ;; Org meta
      (org-tag
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-tag palette)
            :weight light)))
      (org-meta-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata palette))))
      (org-document-title
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-heading-0 palette)
            :weight bold
            :height 1.8)))
      (org-document-info
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata palette))))
      (org-document-info-keyword
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata-value palette))))

      ;; Links and buttons
      (link
       ((,c ,@(bv-themes--link-props palette))))
      (link-visited
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-link-visited palette)
            :underline t)))

      (button
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-button-active palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-button-active palette)
            :box (:line-width 1 :color ,(bv-themes--retrieve-palette-value 'border palette)))))

      ;; Additional standard faces
      (fringe
       ((,c :foreground ,(pcase bv-themes-fringes
                          ('subtle (bv-themes--retrieve-palette-value 'fringe-subtle palette))
                          ('greyscale (bv-themes--retrieve-palette-value 'fringe-greyscale palette))
                          ('invisible (bv-themes--retrieve-palette-value 'bg-main palette))
                          (_ (bv-themes--retrieve-palette-value 'fringe palette)))
            :background ,(bv-themes--retrieve-palette-value 'bg-main palette))))

      (vertical-border
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))

      (window-divider
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))
      (window-divider-first-pixel
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))
      (window-divider-last-pixel
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))

      (minibuffer-prompt
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-prompt palette)
            :weight medium)))

      ;; Search and matching
      (match
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-yellow-nuanced palette)
            :weight bold)))
      (isearch
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-yellow-intense palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight bold)))
      (lazy-highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-cyan-nuanced palette))))

      ;; Replace
      (query-replace
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-magenta-intense palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight bold)))

      ;; Line numbers
      (line-number
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-line-number-inactive palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-line-number-inactive palette))))
      (line-number-current-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-line-number-active palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-line-number-active palette)
            :weight medium)))

      ;; Parentheses
      (show-paren-match
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-paren-match palette)
            ,@(let ((fg (bv-themes--retrieve-palette-value 'fg-paren-match palette)))
                (unless (eq fg 'unspecified)
                  (list :foreground fg)))
            :weight bold)))
      (show-paren-mismatch
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-paren-mismatch palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-paren-mismatch palette))))

      ;; Whitespace mode
      (whitespace-space
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-whitespace palette))))
      (whitespace-tab
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-whitespace palette))))
      (whitespace-trailing
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-red-nuanced palette))))

      ;; Package-specific faces

      ;; Vertico
      (vertico-current
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette)
            :extend t)))
      (vertico-group-title
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'name palette)
            :weight bold)))
      (vertico-group-separator
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))

      ;; Corfu
      (corfu-current
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette)
            :extend t)))
      (corfu-default
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-popup palette))))
      (corfu-border
       ((,c :background ,(bv-themes--retrieve-palette-value 'border palette))))

      ;; Consult
      (consult-file
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'identifier palette))))
      (consult-bookmark
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (consult-async-split
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))

      ;; Marginalia
      (marginalia-documentation
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'docstring palette)
            :slant ,(if italic-constructs 'italic 'normal))))
      (marginalia-value
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'string palette))))
      (marginalia-key
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'keybind palette))))

      ;; Which-key
      (which-key-key-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'keybind palette)
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
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'identifier palette))))
      (magit-section-heading
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-heading-3 palette)
            :weight medium)))
      (magit-diff-file-heading
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight bold)))
      (magit-diff-hunk-heading
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

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
      (tree-sitter-hl-face:variable
       ((,c ,@(bv-themes--syntax-color 'variable palette))))
      (tree-sitter-hl-face:comment
       ((,c ,@(bv-themes--syntax-color 'comment palette))))
      (tree-sitter-hl-face:constant
       ((,c ,@(bv-themes--syntax-color 'constant palette))))
      (tree-sitter-hl-face:property
       ((,c ,@(bv-themes--syntax-color 'variable palette))))

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
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-err palette)))))
      (flycheck-warning
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-warning palette)))))
      (flycheck-info
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-note palette)))))

      (flymake-error
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-err palette)))))
      (flymake-warning
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-warning palette)))))
      (flymake-note
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-note palette)))))

      ;; Company
      (company-tooltip
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-popup palette))))
      (company-tooltip-selection
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette))))
      (company-tooltip-common
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight bold)))

      ;; Org agenda
      (org-agenda-structure
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-heading-0 palette)
            :weight light
            :height 1.4)))
      (org-agenda-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette)
            :weight regular)))
      (org-agenda-date-today
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-now palette)
            :weight medium
            :underline t)))
      (org-agenda-date-weekend
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-weekend palette)
            :weight regular)))
      (org-agenda-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-done palette))))
      (org-agenda-dimmed-todo-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      ;; Email
      (message-header-name
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-header-name palette)
            :weight medium)))
      (message-header-to
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-recipient palette))))
      (message-header-subject
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-subject palette)
            :weight bold)))
      (message-header-other
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-other palette))))
      (message-cited-text-1
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-0 palette))))
      (message-cited-text-2
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-1 palette))))
      (message-cited-text-3
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-2 palette))))
      (message-cited-text-4
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-3 palette))))

      ;; Header line faces (for when header-line is used as modeline)
      (bv-themes-header-default
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active palette)
            :background ,(bv-themes--retrieve-palette-value 'modeline-bg-active palette))))
      (bv-themes-header-strong
       ((,c :inherit bv-themes-header-default
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight medium)))
      (bv-themes-header-salient
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (bv-themes-header-popout
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (bv-themes-header-critical
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'bg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'red-intense palette))))
      (bv-themes-header-faded
       ((,c :inherit bv-themes-header-default
            :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      ;; Rainbow delimiters with proper scaling
      ,@(cl-loop for i from 1 to 9
                 for base-colors = '(red orange yellow green cyan blue purple magenta pink)
                 collect `(,(intern (format "rainbow-delimiters-depth-%d-face" i))
                           ((,c :foreground ,(bv-themes--retrieve-palette-value
                                              (nth (mod (1- i) (length base-colors)) base-colors)
                                              palette)))))

      ;; Tab bar
      (tab-bar
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-bar palette))))
      (tab-bar-tab
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-current palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight medium)))
      (tab-bar-tab-inactive
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-other palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      ;; Tooltip
      (tooltip
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tooltip palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      ;; Ansi colors
      (ansi-color-black
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-black palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-black palette))))
      (ansi-color-red
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-red palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-red palette))))
      (ansi-color-green
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-green palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-green palette))))
      (ansi-color-yellow
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-yellow palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-yellow palette))))
      (ansi-color-blue
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-blue palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-blue palette))))
      (ansi-color-magenta
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-magenta palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-magenta palette))))
      (ansi-color-cyan
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-cyan palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-cyan palette))))
      (ansi-color-white
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-white palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-white palette))))

      ;; Eshell
      (eshell-prompt
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-prompt palette)
            :weight bold)))
      (eshell-ls-directory
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight bold)))
      (eshell-ls-symlink
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (eshell-ls-executable
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette))))

      ;; Dired
      (dired-directory
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight bold)))
      (dired-symlink
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (dired-broken-symlink
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette)
            :weight bold))))))

;;; Theme generation

(defmacro bv-themes-theme (name palette)
  "Generate theme NAME using PALETTE."
  (let ((theme-name name))
    `(progn
       (deftheme ,name
         ,(format "BV theme variant: %s" name))

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
        (insert "Color Category                   Name                             Sample\n")
        (insert "────────────────────────────────────────────────────────────────────────\n")

        ;; Group colors by category
        (let ((categories '(("Base" . (bg-main fg-main bg-dim fg-dim bg-alt fg-alt
                                       bg-active fg-active bg-inactive fg-inactive))
                            ("Accents" . (accent-0 accent-1 accent-2 accent-3
                                          red orange yellow green cyan blue purple magenta pink))
                            ("Faint Variants" . (red-faint orange-faint yellow-faint green-faint
                                                 cyan-faint blue-faint purple-faint magenta-faint))
                            ("Intense Variants" . (red-intense orange-intense yellow-intense green-intense
                                                   cyan-intense blue-intense purple-intense magenta-intense))
                            ("Nuanced Backgrounds" . (bg-red-nuanced bg-orange-nuanced bg-yellow-nuanced
                                                      bg-green-nuanced bg-cyan-nuanced bg-blue-nuanced
                                                      bg-purple-nuanced bg-magenta-nuanced))
                            ("Syntax" . (keyword string comment fnname variable type constant
                                         builtin preprocessor docstring))
                            ("UI Elements" . (bg-completion bg-hover bg-hl-line bg-region
                                              modeline-bg-active modeline-fg-active))
                            ("Semantic Mappings" . (error warning success info prose-done prose-todo)))))
          (dolist (cat categories)
            (insert (format "\n%s:\n" (car cat)))
            (dolist (color-name (cdr cat))
              (let ((value (bv-themes--retrieve-palette-value color-name palette)))
                (when (stringp value)
                  (insert (format "  %-30s %-9s  "
                                  (symbol-name color-name)
                                  value))
                  (insert (propertize "████████" 'face `(:foreground ,value)))
                  (insert "\n"))))))

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
              (test-colors '(fg-main fg-dim fg-active
                             accent-0 accent-1 accent-2 accent-3
                             red green blue yellow cyan magenta
                             warning error success info)))
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

(defun bv-themes-list-all-faces ()
  "List all faces defined by the current theme."
  (interactive)
  (let* ((theme (bv-themes-current))
         (palette (when theme (bv-themes--palette-value theme)))
         (buf (get-buffer-create "*Theme Faces*")))
    (if (not palette)
        (message "No theme currently active")
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "Faces defined by: %s\n\n" theme))
        (let ((faces (bv-themes--face-specs palette)))
          (dolist (face-spec faces)
            (let ((face-name (car face-spec)))
              (insert (propertize (format "%s\n" face-name)
                                  'face face-name))))
          (goto-char (point-min))
          (special-mode)))
      (switch-to-buffer buf))))

;;; Preset overrides (like Modus themes)

(defvar bv-themes-preset-overrides-faint
  '((keyword . blue-faint)
    (string . green-faint)
    (comment . fg-dim)
    (fnname . magenta-faint)
    (variable . cyan-faint)
    (type . blue-faint)
    (constant . red-faint)
    (builtin . purple-faint)

    (bg-completion . bg-dim)
    (bg-hover . bg-dim)
    (bg-hl-line . bg-dim)

    (accent-0 . blue-faint)
    (accent-1 . green-faint)
    (accent-2 . orange-faint)
    (accent-3 . purple-faint))
  "Preset for very subtle, faint colors.")

(defvar bv-themes-preset-overrides-intense
  '((keyword . blue-intense)
    (string . green-intense)
    (comment . red-faint)
    (fnname . magenta-intense)
    (variable . cyan-intense)
    (type . blue-intense)
    (constant . red-intense)
    (builtin . purple-intense)

    (bg-completion . bg-blue-nuanced)
    (bg-hover . bg-yellow-nuanced)
    (bg-hl-line . bg-cyan-nuanced)

    (modeline-bg-active . bg-blue-nuanced)
    (modeline-fg-active . fg-main))
  "Preset for vivid, intense colors.")

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
                                     accent-0 accent-1 accent-2 accent-3
                                     red green blue yellow cyan magenta purple orange
                                     red-faint green-faint blue-faint yellow-faint
                                     red-intense green-intense blue-intense yellow-intense
                                     bg-red-nuanced bg-green-nuanced bg-blue-nuanced
                                     bg-yellow-nuanced bg-cyan-nuanced bg-magenta-nuanced)
                      collect `(,entry (bv-themes--retrieve-palette-value
                                        ',entry palette)))
         ,@body))))

(provide 'bv-themes)
;;; bv-themes.el ends here
