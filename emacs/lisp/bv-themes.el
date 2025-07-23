;;; bv-themes.el --- Sophisticated aesthetic theme engine -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme, aesthetics

;;; Commentary:

;; A sophisticated theme engine supporting light and dark variants through
;; a data-driven architecture with extensive customization options.
;; Prioritizes aesthetics and visual appeal over strict contrast ratios.
;; Inspired by the Modus themes' architecture and extensibility.

;;; Code:

(require 'cl-lib)
(require 'color)

(defgroup bv-themes ()
  "Sophisticated aesthetic theme system with semantic color mapping."
  :group 'faces
  :prefix "bv-themes-"
  :tag "BV Themes")

;;; User Options - Syntax Highlighting

(defcustom bv-themes-syntax nil
  "Style for syntax highlighting.
Nil means default.
Other options:
- `faint': Subtle syntax highlighting
- `intense': More vivid colors
- `monochrome': Minimal color variation
- `rainbow': Maximum color variety
- `tinted': Slightly desaturated colors
- `alt': Alternative color scheme"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Faint" faint)
                 (const :tag "Intense" intense)
                 (const :tag "Monochrome" monochrome)
                 (const :tag "Rainbow" rainbow)
                 (const :tag "Tinted" tinted)
                 (const :tag "Alternative" alt))
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
- `minimal': Very subtle styling
- `moody': Alternative modern style"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Accented" accented)
                 (const :tag "Padded" padded)
                 (const :tag "Borderless" borderless)
                 (const :tag "Gradient" gradient)
                 (const :tag "Minimal" minimal)
                 (const :tag "Moody" moody))
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

(defcustom bv-themes-bold-constructs t
  "When non-nil, use bold for function names and similar constructs.
This creates a stronger visual hierarchy by default."
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

(defcustom bv-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-weight-hierarchy 'sophisticated
  "Weight hierarchy style for syntax highlighting.
Options:
- `traditional': Minimal weight variation (mostly normal)
- `moderate': Selective use of medium and bold
- `sophisticated': Rich weight hierarchy with all available weights
- `custom': Use custom weights defined in `bv-themes-custom-weights'"
  :type '(choice (const :tag "Traditional (minimal)" traditional)
                 (const :tag "Moderate (selective)" moderate)
                 (const :tag "Sophisticated (rich)" sophisticated)
                 (const :tag "Custom" custom))
  :group 'bv-themes)

(defcustom bv-themes-custom-weights nil
  "Custom weight assignments for syntax elements.
An alist mapping syntax element types to font weights.
Example: '((keyword . bold) (function . medium) (variable . normal))"
  :type '(alist :key-type symbol
                :value-type (choice (const :tag "Thin" thin)
                                   (const :tag "Extra Light" ultralight)
                                   (const :tag "Light" light)
                                   (const :tag "Semi Light" semilight)
                                   (const :tag "Normal" normal)
                                   (const :tag "Medium" medium)
                                   (const :tag "Semi Bold" semibold)
                                   (const :tag "Bold" bold)
                                   (const :tag "Extra Bold" ultrabold)
                                   (const :tag "Black" black)))
  :group 'bv-themes)

;;; User Options - Org Mode

(defcustom bv-themes-org-blocks nil
  "Style for Org source blocks.
Nil means default subtle background.
Other options:
- `tinted': Slightly colored backgrounds
- `rainbow': Different colors per language
- `zebra': Alternating backgrounds
- `minimal': No background distinction
- `bordered': Add borders to blocks"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Tinted" tinted)
                 (const :tag "Rainbow" rainbow)
                 (const :tag "Zebra" zebra)
                 (const :tag "Minimal" minimal)
                 (const :tag "Bordered" bordered))
  :group 'bv-themes)

(defcustom bv-themes-org-agenda-structure nil
  "Style for org-agenda structure.
Can be `default', `rainbow', `gradient', or `modern'."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Rainbow" rainbow)
                 (const :tag "Gradient" gradient)
                 (const :tag "Modern" modern))
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
Can be nil (default), `subtle', `greyscale', `invisible', or `accented'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Subtle" subtle)
                 (const :tag "Greyscale" greyscale)
                 (const :tag "Invisible" invisible)
                 (const :tag "Accented" accented))
  :group 'bv-themes)

(defcustom bv-themes-hl-line nil
  "Style for line highlighting.
Can be nil (default), `intense', `accented', `underline', or `faint'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Intense" intense)
                 (const :tag "Accented" accented)
                 (const :tag "Underline" underline)
                 (const :tag "Faint" faint))
  :group 'bv-themes)


(defcustom bv-themes-region nil
  "Style for region highlighting.
Can be nil (default), `no-extend', `bg-only', or `accented'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "No extend" no-extend)
                 (const :tag "Background only" bg-only)
                 (const :tag "Accented" accented))
  :group 'bv-themes)

(defcustom bv-themes-completions nil
  "Control the style of completion user interfaces."
  :type `(alist
          :key-type symbol
          :value-type (set :tag "Style of completion" :greedy t
                          (const :tag "Bold weight" bold)
                          (const :tag "Italic" italic)
                          (const :tag "Underline" underline)))
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
Can be nil (default), `desaturated', `fg-only', `bg-only', `deuteranopia', or `unified'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Desaturated" desaturated)
                 (const :tag "Foreground only" fg-only)
                 (const :tag "Background only" bg-only)
                 (const :tag "Deuteranopia" deuteranopia)
                 (const :tag "Unified" unified))
  :group 'bv-themes)

;;; User Options - Links

(defcustom bv-themes-links nil
  "Style for links.
Can be nil (default), `neutral-underline', `faint', `no-underline', or `bold'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Neutral underline" neutral-underline)
                 (const :tag "Faint" faint)
                 (const :tag "No underline" no-underline)
                 (const :tag "Bold" bold))
  :group 'bv-themes)

;;; User Options - Markup

(defcustom bv-themes-markup nil
  "Style for markup faces (bold, italic, etc).
Can be nil (default), `intense', `faint', or `color-coded'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Intense" intense)
                 (const :tag "Faint" faint)
                 (const :tag "Color-coded" color-coded))
  :group 'bv-themes)

;;; User Options - Syntax Highlighting

(defcustom bv-themes-syntax-highlighting nil
  "Fine-grained control over syntax highlighting styles.
This is an alist where each element is (FACE-CATEGORY . PROPERTIES).
Available categories:
  - `strings': String literals styling
  - `comments': Comments and documentation
  - `functions': Function names and calls
  - `constants': Constants and literals
  - `operators': Mathematical and logical operators
  - `delimiters': Brackets, parens, and punctuation
  - `regexp': Regular expressions
  - `escapes': Escape sequences

Properties can include:
  - `bold': Use bold weight
  - `italic': Use italic slant
  - `underline': Add underline
  - `box': Add subtle box
  - `background': Use nuanced background
  - `intense': Use more vivid colors"
  :type '(alist :key-type (choice (const :tag "Strings" strings)
                                  (const :tag "Comments" comments)
                                  (const :tag "Functions" functions)
                                  (const :tag "Constants" constants)
                                  (const :tag "Operators" operators)
                                  (const :tag "Delimiters" delimiters)
                                  (const :tag "Regexps" regexp)
                                  (const :tag "Escapes" escapes))
                :value-type (set :tag "Properties"
                                 (const :tag "Bold" bold)
                                 (const :tag "Italic" italic)
                                 (const :tag "Underline" underline)
                                 (const :tag "Box" box)
                                 (const :tag "Background" background)
                                 (const :tag "Intense" intense)))
  :group 'bv-themes)

(defcustom bv-themes-paren-match 'intense
  "Style for parenthesis matching.
Options:
  - `intense': Colored background with bold (default)
  - `subtle': Colored background only
  - `bold': Bold weight only
  - `underline': Underline matched parens
  - `intense-foreground': Colored foreground with bold"
  :type '(choice (const :tag "Intense background" intense)
                 (const :tag "Subtle background" subtle)
                 (const :tag "Bold only" bold)
                 (const :tag "Underline" underline)
                 (const :tag "Intense foreground" intense-foreground))
  :group 'bv-themes)

;;; User Options - Prompts

(defcustom bv-themes-prompts nil
  "Style for prompts.
Properties can include `bold', `italic', `background', and various colors."
  :type '(set :tag "Properties" :greedy t
              (const :tag "Bold" bold)
              (const :tag "Italic" italic)
              (const :tag "Background" background))
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
      (apply #'color-rgb-to-hex
             (color-hsl-to-rgb h (min 1.0 (+ s amount)) l)))))

(defun bv-themes--desaturate (color &optional amount)
  "Make COLOR less saturated by AMOUNT (default 0.3)."
  (let ((amount (or amount 0.3)))
    (cl-destructuring-bind (h s l) (apply #'color-rgb-to-hsl
                                          (color-name-to-rgb color))
      (apply #'color-rgb-to-hex
             (color-hsl-to-rgb h (max 0.0 (- s amount)) l)))))

(defun bv-themes--rotate-hue (color degrees)
  "Rotate COLOR hue by DEGREES."
  (cl-destructuring-bind (h s l) (apply #'color-rgb-to-hsl
                                        (color-name-to-rgb color))
    (apply #'color-rgb-to-hex
           (color-hsl-to-rgb (mod (+ h (/ degrees 360.0)) 1.0) s l))))

;;; Helper functions for conditional styling

(defun bv-themes--bold-weight ()
  "Conditional use of a heavier text weight."
  (when bv-themes-bold-constructs
    (list :inherit 'bold)))

(defun bv-themes--slant ()
  "Conditional use of italics for slant attribute."
  (when bv-themes-italic-constructs
    (list :inherit 'italic)))

(defun bv-themes--fixed-pitch ()
  "Conditional use of fixed pitch font."
  (when bv-themes-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun bv-themes--variable-pitch-ui ()
  "Conditional use of variable pitch for UI elements."
  (when bv-themes-variable-pitch-ui
    (list :inherit 'variable-pitch)))

(defun bv-themes--syntax-extra-props (category palette)
  "Get extra properties for syntax CATEGORY from user preferences.
Returns a property list of additional face attributes."
  (let ((props (cdr (assq category bv-themes-syntax-highlighting)))
        result)
    (when props
      (when (memq 'bold props)
        (setq result (append result '(:weight bold))))
      (when (memq 'italic props)
        (setq result (append result '(:slant italic))))
      (when (memq 'underline props)
        (setq result (append result '(:underline t))))
      (when (memq 'box props)
        (setq result (append result
                            `(:box (:line-width -1
                                    :color ,(bv-themes--retrieve-palette-value 'border palette))))))
      (when (memq 'background props)
        (let ((bg-key (pcase category
                        ('strings 'bg-green-nuanced)
                        ('comments 'bg-cyan-nuanced)
                        ('functions 'bg-blue-nuanced)
                        ('constants 'bg-yellow-nuanced)
                        ('operators 'bg-magenta-nuanced)
                        ('delimiters 'bg-red-nuanced)
                        ('regexp 'bg-purple-nuanced)
                        ('escapes 'bg-orange-nuanced)
                        (_ 'bg-dim))))
          (setq result (append result
                              `(:background ,(bv-themes--retrieve-palette-value bg-key palette))))))
      (when (memq 'intense props)
        ;; For intense, we'll modify the color retrieval in syntax-color
        (setq result (append result '(:bv-themes-intense t)))))
    result))

;;; Property Computation Functions

(defun bv-themes--get-weight-for-element (element)
  "Get font weight for syntax ELEMENT based on hierarchy settings."
  (pcase bv-themes-weight-hierarchy
    ('traditional
     ;; Minimal weight variation - mostly normal
     (pcase element
       ('keyword (if bv-themes-bold-constructs 'medium 'normal))
       ('fnname (if bv-themes-bold-constructs 'medium 'normal))
       ('function-call 'normal)
       ('type 'normal)
       ('builtin 'normal)
       ('constant 'normal)
       ('variable 'normal)
       ('string 'normal)
       ('comment 'normal)
       ('warning 'bold)
       (_ 'normal)))
    ('moderate
     ;; Selective use of medium and bold
     (pcase element
       ('keyword 'medium)
       ('fnname 'medium)
       ('function-call 'normal)
       ('type 'medium)
       ('builtin 'normal)
       ('constant 'normal)
       ('variable 'normal)
       ('string 'normal)
       ('comment 'normal)
       ('warning 'bold)
       ('error 'bold)
       (_ 'normal)))
    ('sophisticated
     ;; Rich weight hierarchy using all available weights
     (pcase element
       ;; Primary importance - keywords control flow
       ('keyword 'semibold)
       ;; Function definitions are important structural elements
       ('fnname 'bold)
       ;; Function calls are less important than definitions
       ('function-call 'medium)
       ;; Types define structure
       ('type 'semibold)
       ;; Built-ins are important but shouldn't dominate
       ('builtin 'medium)
       ;; Constants are semantically important
       ('constant 'medium)
       ;; Variables are frequent, keep them light
       ('variable 'normal)
       ;; Variable references even lighter
       ('variable-use 'light)
       ;; Properties slightly emphasized
       ('property 'medium)
       ('property-use 'normal)
       ;; Strings are content, not structure
       ('string 'normal)
       ;; Numbers stand out slightly
       ('number 'medium)
       ;; Operators are structural
       ('operator 'semibold)
       ;; Comments are secondary
       ('comment 'light)
       ;; Doc strings slightly more important than comments
       ('docstring 'semilight)
       ;; Warnings and errors must stand out
       ('warning 'bold)
       ('error 'bold)
       ;; Preprocessor directives
       ('preprocessor 'medium)
       ;; Punctuation hierarchy
       ('bracket 'medium)      ; Structural brackets
       ('delimiter 'normal)    ; Commas, semicolons
       ('punctuation 'light)   ; Other punctuation
       ;; Regex elements
       ('regexp 'medium)
       ('escape 'medium)
       ;; Default
       (_ 'normal)))
    ('custom
     ;; Use custom weights
     (or (alist-get element bv-themes-custom-weights) 'normal))
    (_
     'normal)))

(defun bv-themes--syntax-color (base-color palette &optional category)
  "Compute syntax color based on user settings.
BASE-COLOR is the palette key, PALETTE is the current palette.
Optional CATEGORY is used for fine-grained styling preferences."
  (let* ((color (bv-themes--retrieve-palette-value base-color palette))
         (weight (bv-themes--get-weight-for-element base-color))
         (extra-props (when category (bv-themes--syntax-extra-props category palette)))
         ;; Check if intense is requested via extra props
         (intense-p (plist-get extra-props :bv-themes-intense))
         ;; Remove our internal marker
         (extra-props (cl-loop for (k v) on extra-props by #'cddr
                               unless (eq k :bv-themes-intense)
                               collect k and collect v))
         base-props)
    ;; Compute base properties
    (setq base-props
          (pcase bv-themes-syntax
            ('faint
             (list :foreground (bv-themes--blend color
                                                 (bv-themes--retrieve-palette-value 'bg-main palette)
                                                 0.4)
                   :weight weight))
            ('intense
             (list :foreground (bv-themes--intensify color 0.3)
                   :weight weight))
            ('monochrome
             (list :foreground (bv-themes--retrieve-palette-value 'fg-main palette)
                   :weight weight))
            ('rainbow
             (list :foreground color
                   :weight weight))
            ('tinted
             (list :foreground (bv-themes--desaturate color 0.2)
                   :weight weight))
            ('alt
             (let ((alt-key (intern (format "%s-alt" base-color))))
               (list :foreground (or (bv-themes--retrieve-palette-value alt-key palette) color)
                     :weight weight)))
            (_
             (list :foreground (if intense-p
                                   (bv-themes--intensify color 0.3)
                                 color)
                   :weight weight))))
    ;; Merge with extra properties, extra props take precedence
    (append extra-props base-props)))

(defun bv-themes--heading (level palette)
  "Compute heading properties for LEVEL using PALETTE."
  (let* ((user-props (alist-get level bv-themes-headings))
         (default-heights '(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0))
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
         ('moody
          `(:background ,(bv-themes--retrieve-palette-value 'modeline-bg-active-alt palette)
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-fg-active palette)
            :overline ,(bv-themes--retrieve-palette-value 'accent-0 palette)))
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
    ('bordered
     (bv-themes--retrieve-palette-value 'bg-inactive palette))
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
      ('bold
       `(:foreground ,color :underline t :weight bold))
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
      ('unified
       (pcase side
         ('bg `(:background ,(bv-themes--retrieve-palette-value 'bg-changed palette)))
         ('fg `(:foreground ,(bv-themes--retrieve-palette-value 'fg-changed palette)))))
      (_
       (pcase side
         ('bg `(:background ,(bv-themes--retrieve-palette-value base-bg-key palette)))
         ('fg `(:foreground ,(bv-themes--retrieve-palette-value base-fg-key palette))))))))

(defun bv-themes--hl-line-props (palette)
  "Compute hl-line properties based on user settings and PALETTE."
  (pcase bv-themes-hl-line
    ('intense
     `(:background ,(bv-themes--retrieve-palette-value 'bg-hl-line-intense palette) :extend t))
    ('accented
     `(:background ,(bv-themes--retrieve-palette-value 'bg-hl-line-accent palette) :extend t))
    ('underline
     `(:underline ,(bv-themes--retrieve-palette-value 'border palette) :extend t))
    ('faint
     `(:background ,(bv-themes--retrieve-palette-value 'bg-hl-line-faint palette) :extend t))
    (_
     `(:background ,(bv-themes--retrieve-palette-value 'bg-hl-line palette) :extend t))))

(defun bv-themes--paren-match-props (palette)
  "Compute paren match properties based on user settings and PALETTE."
  (pcase bv-themes-paren-match
    ('intense
     `(:background ,(bv-themes--retrieve-palette-value 'bg-paren-match-intense palette)
       :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
       :weight bold))
    ('subtle
     `(:background ,(bv-themes--retrieve-palette-value 'bg-paren-match palette)))
    ('underline
     `(:underline ,(bv-themes--retrieve-palette-value 'accent-0 palette)
       :weight bold))
    ('bold
     `(:inherit bold))
    ('intense-foreground
     `(:foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
       :weight bold))
    (_
     `(:background ,(bv-themes--retrieve-palette-value 'bg-paren-match palette)
       :foreground ,(bv-themes--retrieve-palette-value 'fg-paren-match palette)))))

(defun bv-themes--region-props (palette)
  "Compute region properties based on user settings and PALETTE."
  (pcase bv-themes-region
    ('no-extend
     `(:background ,(bv-themes--retrieve-palette-value 'bg-region palette)
       :foreground ,(bv-themes--retrieve-palette-value 'fg-region palette)
       :extend nil))
    ('bg-only
     `(:background ,(bv-themes--retrieve-palette-value 'bg-region palette)
       :extend t))
    ('accented
     `(:background ,(bv-themes--retrieve-palette-value 'bg-region-accent palette)
       :foreground ,(bv-themes--retrieve-palette-value 'fg-region palette)
       :extend t))
    (_
     `(:background ,(bv-themes--retrieve-palette-value 'bg-region palette)
       :foreground ,(bv-themes--retrieve-palette-value 'fg-region palette)
       :extend t))))

(defun bv-themes--prompt-props (palette)
  "Compute prompt properties based on user settings and PALETTE."
  (let ((properties bv-themes-prompts))
    (append
     (list :foreground (bv-themes--retrieve-palette-value 'fg-prompt palette))
     (when (memq 'bold properties)
       (list :weight 'bold))
     (when (memq 'italic properties)
       (list :slant 'italic))
     (when (memq 'background properties)
       (list :background (bv-themes--retrieve-palette-value 'bg-prompt palette))))))

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

;;; Typography helper faces

(defface bv-themes-bold nil
  "Helper face for conditional bold styling."
  :group 'bv-themes)

(defface bv-themes-slant nil
  "Helper face for conditional italic/slant styling."
  :group 'bv-themes)

(defface bv-themes-fixed-pitch nil
  "Helper face for fixed pitch text."
  :group 'bv-themes)

(defface bv-themes-variable-pitch nil
  "Helper face for variable pitch text."
  :group 'bv-themes)

(defface bv-themes-ui-variable-pitch nil
  "Helper face for variable pitch UI elements."
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

;;; Special UI faces

(defface bv-themes-mark-select nil
  "Face for selected marks."
  :group 'bv-themes)

(defface bv-themes-mark-delete nil
  "Face for deletion marks."
  :group 'bv-themes)

(defface bv-themes-mark-other nil
  "Face for other marks."
  :group 'bv-themes)

;;; Error/Warning/Note faces

(defface bv-themes-lang-error nil
  "Face for language-specific error indicators."
  :group 'bv-themes)

(defface bv-themes-lang-note nil
  "Face for language-specific note indicators."
  :group 'bv-themes)

(defface bv-themes-lang-warning nil
  "Face for language-specific warning indicators."
  :group 'bv-themes)

(defface bv-themes-prominent-error nil
  "Face for prominent error messages."
  :group 'bv-themes)

(defface bv-themes-prominent-note nil
  "Face for prominent notes."
  :group 'bv-themes)

(defface bv-themes-prominent-warning nil
  "Face for prominent warnings."
  :group 'bv-themes)

;;; Diff and version control faces

(defface bv-themes-diff-added nil
  "Face for added diff lines."
  :group 'bv-themes)

(defface bv-themes-diff-removed nil
  "Face for removed diff lines."
  :group 'bv-themes)

(defface bv-themes-diff-changed nil
  "Face for changed diff lines."
  :group 'bv-themes)

(defface bv-themes-diff-refine-added nil
  "Face for refined added diff lines."
  :group 'bv-themes)

(defface bv-themes-diff-refine-removed nil
  "Face for refined removed diff lines."
  :group 'bv-themes)

(defface bv-themes-diff-refine-changed nil
  "Face for refined changed diff lines."
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

;;; Search and highlighting faces

(defface bv-themes-search-current nil
  "Face for current search match."
  :group 'bv-themes)

(defface bv-themes-search-lazy nil
  "Face for lazy search highlighting."
  :group 'bv-themes)

(defface bv-themes-search-replace nil
  "Face for search replacement."
  :group 'bv-themes)

(defface bv-themes-search-rx-group-0 nil
  "Face for regexp group 0 in search."
  :group 'bv-themes)

(defface bv-themes-search-rx-group-1 nil
  "Face for regexp group 1 in search."
  :group 'bv-themes)

(defface bv-themes-search-rx-group-2 nil
  "Face for regexp group 2 in search."
  :group 'bv-themes)

(defface bv-themes-search-rx-group-3 nil
  "Face for regexp group 3 in search."
  :group 'bv-themes)

;;; Completion faces

(defface bv-themes-completion-match-0 nil
  "Face for first level completion match."
  :group 'bv-themes)

(defface bv-themes-completion-match-1 nil
  "Face for second level completion match."
  :group 'bv-themes)

(defface bv-themes-completion-match-2 nil
  "Face for third level completion match."
  :group 'bv-themes)

(defface bv-themes-completion-match-3 nil
  "Face for fourth level completion match."
  :group 'bv-themes)

(defface bv-themes-completion-selected nil
  "Face for selected completion item."
  :group 'bv-themes)

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
        (mixed-fonts bv-themes-mixed-fonts)
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

      ;; Typography helper faces
      (bv-themes-bold
       ((,c ,@(bv-themes--bold-weight))))

      (bv-themes-slant
       ((,c ,@(bv-themes--slant))))

      (bv-themes-fixed-pitch
       ((,c ,@(bv-themes--fixed-pitch))))

      (bv-themes-variable-pitch
       ((,c :family ,font-prop)))

      (bv-themes-ui-variable-pitch
       ((,c ,@(bv-themes--variable-pitch-ui))))

      ;; Accent faces
      (bv-themes-accent-0
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))
      (bv-themes-accent-1
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))
      (bv-themes-accent-2
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette))))
      (bv-themes-accent-3
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-3 palette))))

      ;; Mark faces
      (bv-themes-mark-select
       ((,c :inherit bold :background ,(bv-themes--retrieve-palette-value 'bg-mark-select palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-mark-select palette))))
      (bv-themes-mark-delete
       ((,c :inherit bold :background ,(bv-themes--retrieve-palette-value 'bg-mark-delete palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-mark-delete palette))))
      (bv-themes-mark-other
       ((,c :inherit bold :background ,(bv-themes--retrieve-palette-value 'bg-mark-other palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-mark-other palette))))

      ;; Error/Warning/Note faces with language-specific underlines
      (bv-themes-lang-error
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-error palette)))))
      (bv-themes-lang-note
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-note palette)))))
      (bv-themes-lang-warning
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-warning palette)))))

      (bv-themes-prominent-error
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-prominent-err palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-prominent-err palette))))
      (bv-themes-prominent-note
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-prominent-note palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-prominent-note palette))))
      (bv-themes-prominent-warning
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-prominent-warning palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-prominent-warning palette))))

      ;; Diff faces
      (bv-themes-diff-added
       ((,c ,@(bv-themes--diff-props 'added 'bg palette)
            ,@(bv-themes--diff-props 'added 'fg palette))))
      (bv-themes-diff-removed
       ((,c ,@(bv-themes--diff-props 'removed 'bg palette)
            ,@(bv-themes--diff-props 'removed 'fg palette))))
      (bv-themes-diff-changed
       ((,c ,@(bv-themes--diff-props 'changed 'bg palette)
            ,@(bv-themes--diff-props 'changed 'fg palette))))
      (bv-themes-diff-refine-added
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-added-refine palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-added-intense palette))))
      (bv-themes-diff-refine-removed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-removed-refine palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-removed-intense palette))))
      (bv-themes-diff-refine-changed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-changed-refine palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-changed-intense palette))))

      ;; Basic faces
      (default
       ((,c :family ,font-mono
            :height ,font-size
            :weight normal
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-main palette))))

      (cursor
       ((,c :background ,(bv-themes--retrieve-palette-value 'cursor palette))))

      (region
       ((,c ,@(bv-themes--region-props palette))))

      (highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette))))

      (hl-line
       ((,c ,@(bv-themes--hl-line-props palette))))

      (fringe
       ((,c :background ,(pcase bv-themes-fringes
                          ('subtle (bv-themes--retrieve-palette-value 'bg-dim palette))
                          ('greyscale (bv-themes--retrieve-palette-value 'bg-inactive palette))
                          ('invisible (bv-themes--retrieve-palette-value 'bg-main palette))
                          ('accented (bv-themes--retrieve-palette-value 'bg-accent-subtle palette))
                          (_ (bv-themes--retrieve-palette-value 'fringe palette)))
            :foreground ,(pcase bv-themes-fringes
                          ('subtle (bv-themes--retrieve-palette-value 'fringe-subtle palette))
                          ('greyscale (bv-themes--retrieve-palette-value 'fringe-greyscale palette))
                          ('invisible (bv-themes--retrieve-palette-value 'bg-main palette))
                          ('accented (bv-themes--retrieve-palette-value 'fringe-accent palette))
                          (_ (bv-themes--retrieve-palette-value 'fringe palette))))))

      (vertical-border
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))

      (window-divider
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))
      (window-divider-first-pixel
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))
      (window-divider-last-pixel
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'border palette))))

      (minibuffer-prompt
       ((,c ,@(bv-themes--prompt-props palette))))

      ;; Basic UI faces
      (error ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'error palette))))
      (warning ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))
      (success ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'success palette))))

      (shadow ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (secondary-selection ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover-secondary palette))))

      (trailing-whitespace ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space-error palette))))

      ;; Font lock faces with syntax variants and fine-grained control
      (font-lock-bracket-face
       ((,c ,@(bv-themes--syntax-color 'bracket palette 'delimiters))))

      (font-lock-builtin-face
       ((,c :inherit bv-themes-bold
            ,@(bv-themes--syntax-color 'builtin palette 'functions))))

      (font-lock-comment-delimiter-face
       ((,c :inherit font-lock-comment-face)))

      (font-lock-comment-face
       ((,c :inherit bv-themes-slant
            ,@(bv-themes--syntax-color 'comment palette 'comments))))

      (font-lock-constant-face
       ((,c ,@(bv-themes--syntax-color 'constant palette 'constants))))

      (font-lock-delimiter-face
       ((,c ,@(bv-themes--syntax-color 'delimiter palette 'delimiters))))

      (font-lock-doc-face
       ((,c :inherit bv-themes-slant
            ,@(bv-themes--syntax-color 'docstring palette 'comments))))

      (font-lock-doc-markup-face
       ((,c :inherit bv-themes-slant
            ,@(bv-themes--syntax-color 'docmarkup palette 'comments))))

      (font-lock-escape-face
       ((,c ,@(let ((props (bv-themes--syntax-color 'escape palette 'escapes)))
                ;; Make escape sequences stand out by default with slight weight increase
                (unless (assq 'escapes bv-themes-syntax-highlighting)
                  (plist-put props :weight 'medium))
                props))))

      (font-lock-function-call-face
       ((,c ,@(bv-themes--syntax-color 'function-call palette 'functions))))

      (font-lock-function-name-face
       ((,c ,@(bv-themes--syntax-color 'fnname palette 'functions))))

      (font-lock-keyword-face
       ((,c :inherit bv-themes-bold
            ,@(bv-themes--syntax-color 'keyword palette))))

      (font-lock-misc-punctuation-face
       ((,c ,@(bv-themes--syntax-color 'punctuation palette 'delimiters))))

      (font-lock-negation-char-face
       ((,c :inherit error)))

      (font-lock-number-face
       ((,c ,@(bv-themes--syntax-color 'number palette 'constants))))

      (font-lock-operator-face
       ((,c ,@(bv-themes--syntax-color 'operator palette 'operators))))

      (font-lock-preprocessor-face
       ((,c ,@(bv-themes--syntax-color 'preprocessor palette))))

      (font-lock-property-name-face
       ((,c ,@(bv-themes--syntax-color 'property palette))))

      (font-lock-property-use-face
       ((,c ,@(bv-themes--syntax-color 'property-use palette))))

      (font-lock-punctuation-face
       ((,c ,@(bv-themes--syntax-color 'punctuation palette 'delimiters))))

      (font-lock-regexp-face
       ((,c ,@(let ((props (bv-themes--syntax-color 'regexp palette 'regexp)))
                ;; Regexps benefit from slight emphasis by default
                (unless (assq 'regexp bv-themes-syntax-highlighting)
                  (plist-put props :weight 'medium))
                props))))

      (font-lock-regexp-grouping-backslash
       ((,c :inherit bv-themes-bold
            ,@(let ((props (bv-themes--syntax-color 'rx-backslash palette 'regexp)))
                ;; Grouping backslashes should really stand out
                (unless (plist-get props :background)
                  (plist-put props :foreground
                            (bv-themes--intensify
                             (plist-get props :foreground) 0.2)))
                props))))

      (font-lock-regexp-grouping-construct
       ((,c :inherit bv-themes-bold
            ,@(let ((props (bv-themes--syntax-color 'rx-construct palette 'regexp)))
                ;; Grouping constructs also need emphasis
                (unless (plist-get props :background)
                  (plist-put props :foreground
                            (bv-themes--intensify
                             (plist-get props :foreground) 0.2)))
                props))))

      (font-lock-string-face
       ((,c ,@(bv-themes--syntax-color 'string palette 'strings))))

      (font-lock-type-face
       ((,c :inherit bv-themes-bold
            ,@(bv-themes--syntax-color 'type palette))))

      (font-lock-variable-name-face
       ((,c ,@(bv-themes--syntax-color 'variable palette))))

      (font-lock-variable-use-face
       ((,c ,@(bv-themes--syntax-color 'variable-use palette))))

      (font-lock-warning-face
       ((,c :inherit bv-themes-bold
            ,@(bv-themes--syntax-color 'warning palette))))

      ;; Search and replace
      (isearch
       ((,c :inherit bv-themes-search-current)))

      (isearch-fail
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-fail palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (lazy-highlight
       ((,c :inherit bv-themes-search-lazy)))

      (match
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-match palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (query-replace
       ((,c :inherit bv-themes-search-replace)))

      ;; BV theme search faces
      (bv-themes-search-current
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-current palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (bv-themes-search-lazy
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-lazy palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (bv-themes-search-replace
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-replace palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (bv-themes-search-rx-group-0
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-rx-group-0 palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (bv-themes-search-rx-group-1
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-rx-group-1 palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (bv-themes-search-rx-group-2
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-rx-group-2 palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (bv-themes-search-rx-group-3
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-search-rx-group-3 palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      ;; Line numbers
      (line-number
       ((,c :inherit ,(if mixed-fonts '(fixed-pitch default) 'default)
            :background ,(bv-themes--retrieve-palette-value 'bg-line-number-inactive palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-line-number-inactive palette))))

      (line-number-current-line
       ((,c :inherit ,(if mixed-fonts '(fixed-pitch default) 'default)
            :background ,(bv-themes--retrieve-palette-value 'bg-line-number-active palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-line-number-active palette)
            :weight bold)))

      (line-number-major-tick
       ((,c :inherit line-number
            :foreground ,(bv-themes--retrieve-palette-value 'red palette))))

      (line-number-minor-tick
       ((,c :inherit line-number
            :foreground ,(bv-themes--retrieve-palette-value 'cyan palette))))

      ;; Mode line with variants
      (mode-line
       ((,c ,@(bv-themes--mode-line-props 'active palette)
            ,@(when variable-pitch-ui '(:inherit variable-pitch)))))

      (mode-line-inactive
       ((,c ,@(bv-themes--mode-line-props 'inactive palette)
            ,@(when variable-pitch-ui '(:inherit variable-pitch)))))

      (mode-line-emphasis
       ((,c :inherit bv-themes-strong)))

      (mode-line-highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover-secondary palette)
            :box ,(bv-themes--retrieve-palette-value 'border palette))))

      ;; Links and buttons
      (link
       ((,c ,@(bv-themes--link-props palette))))

      (link-visited
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-link-visited palette)
            :underline t)))

      (button
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-button-active palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-button-active palette)
            :box (:line-width 1 :color ,(bv-themes--retrieve-palette-value 'border palette)))))

      ;; Parentheses matching
      (show-paren-match
       ((,c ,@(bv-themes--paren-match-props palette))))

      (show-paren-mismatch
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-paren-mismatch palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-paren-mismatch palette))))

      ;; Header line
      (header-line
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-header palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-header palette)
            ,@(when variable-pitch-ui '(:inherit variable-pitch)))))

      (header-line-highlight
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :box ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      ;; Tab bar
      (tab-bar
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-bar palette))))

      (tab-bar-tab
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-current palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight medium
            :box (:line-width -2 :color ,(bv-themes--retrieve-palette-value 'bg-tab-current palette)))))

      (tab-bar-tab-inactive
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-other palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :box (:line-width -2 :color ,(bv-themes--retrieve-palette-value 'bg-tab-other palette)))))

      ;; Tab line
      (tab-line
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-bar palette)
            :height 0.95)))

      (tab-line-tab
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-current palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :box (:line-width -2 :color ,(bv-themes--retrieve-palette-value 'bg-tab-current palette)))))

      (tab-line-tab-current
       ((,c :inherit tab-line-tab :weight bold)))

      (tab-line-tab-inactive
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tab-other palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :box (:line-width -2 :color ,(bv-themes--retrieve-palette-value 'bg-tab-other palette)))))

      (tab-line-highlight
       ((,c :inherit highlight)))

      (tab-line-close-highlight
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'red palette))))

      ;; Whitespace mode
      (whitespace-space
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-space palette))))

      (whitespace-tab
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-space palette))))

      (whitespace-trailing
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space-error palette))))

      (whitespace-line
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space palette)
            :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))

      (whitespace-space-before-tab
       ((,c :inherit warning :background ,(bv-themes--retrieve-palette-value 'bg-space palette))))

      (whitespace-space-after-tab
       ((,c :inherit warning :background ,(bv-themes--retrieve-palette-value 'bg-space palette))))

      (whitespace-indentation
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-space palette))))

      (whitespace-empty
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space palette))))

      (whitespace-newline
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-space palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-space palette))))

      ;; Pulse
      (pulse-highlight-start-face
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-pulse palette))))

      ;; Fill column indicator
      (fill-column-indicator
       ((,c :height 1
            :background ,(bv-themes--retrieve-palette-value 'bg-fill-column palette)
            :foreground ,(bv-themes--retrieve-palette-value 'bg-fill-column palette))))

      ;; Tooltip
      (tooltip
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-tooltip palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      ;; Help
      (help-key-binding
       ((,c :inherit bold
            :background ,(bv-themes--retrieve-palette-value 'bg-keybind palette)
            :foreground ,(bv-themes--retrieve-palette-value 'keybind palette))))

      (help-argument-name
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'variable palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      ;; Custom
      (custom-button
       ((,c :inherit bv-themes-button)))

      (custom-button-mouse
       ((,c :inherit (highlight custom-button))))

      (custom-button-pressed
       ((,c :inherit (secondary-selection custom-button))))

      (custom-state
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))

      (custom-changed
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-changed-subtle palette))))

      (custom-face-tag
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'type palette))))

      (custom-group-tag
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'builtin palette))))

      (custom-variable-tag
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'variable palette))))

      ;; Info
      (info-node
       ((,c :inherit bold)))

      (info-title-1
       ((,c :inherit bv-themes-heading-1)))

      (info-title-2
       ((,c :inherit bv-themes-heading-2)))

      (info-title-3
       ((,c :inherit bv-themes-heading-3)))

      (info-title-4
       ((,c :inherit bv-themes-heading-4)))

      (info-menu-header
       ((,c :inherit bold)))

      (info-menu-star
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'red palette))))

      ;; Compilation
      (compilation-mode-line-run
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'modeline-warning palette))))

      (compilation-mode-line-exit
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'modeline-success palette))))

      (compilation-mode-line-fail
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'modeline-error palette))))

      (compilation-error
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'error palette))))

      (compilation-warning
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))

      (compilation-info
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'info palette))))

      (compilation-line-number
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-special-mild palette))))

      (compilation-column-number
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-special-mild palette))))

      ;; Completions
      (completions-annotations
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'docstring palette)
            :slant ,(if italic-constructs 'italic 'normal))))

      (completions-common-part
       ((,c ,@(bv-themes--completion-props 0 palette))))

      (completions-first-difference
       ((,c ,@(bv-themes--completion-props 1 palette))))

      ;; BV themes completion faces
      (bv-themes-completion-match-0
       ((,c ,@(bv-themes--completion-props 0 palette))))

      (bv-themes-completion-match-1
       ((,c ,@(bv-themes--completion-props 1 palette))))

      (bv-themes-completion-match-2
       ((,c ,@(bv-themes--completion-props 2 palette))))

      (bv-themes-completion-match-3
       ((,c ,@(bv-themes--completion-props 3 palette))))

      (bv-themes-completion-selected
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette)
            :extend t)))

      ;; Bookmark
      (bookmark-face
       ((,c :inherit success)))

      ;; Message (mail)
      (message-header-name
       ((,c :inherit bold)))

      (message-header-subject
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'mail-subject palette))))

      (message-header-to
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'mail-recipient palette))))

      (message-header-cc
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-recipient palette))))

      (message-header-other
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-other palette))))

      (message-separator
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-inactive palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))

      (message-cited-text-1
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-0 palette))))

      (message-cited-text-2
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-1 palette))))

      (message-cited-text-3
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-2 palette))))

      (message-cited-text-4
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'mail-cite-3 palette))))

      ;; Calendar and diary
      (calendar-today
       ((,c :inherit bold :underline t)))

      (calendar-weekday-header
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-weekday palette))))

      (calendar-weekend-header
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-weekend palette))))

      (calendar-month-header
       ((,c :inherit bold)))

      (diary
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette))))

      (holiday
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-holiday palette))))

      ;; Outline
      (outline-1 ((,c :inherit bv-themes-heading-1)))
      (outline-2 ((,c :inherit bv-themes-heading-2)))
      (outline-3 ((,c :inherit bv-themes-heading-3)))
      (outline-4 ((,c :inherit bv-themes-heading-4)))
      (outline-5 ((,c :inherit bv-themes-heading-5)))
      (outline-6 ((,c :inherit bv-themes-heading-6)))
      (outline-7 ((,c :inherit bv-themes-heading-7)))
      (outline-8 ((,c :inherit bv-themes-heading-8)))

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

      ;; Dired
      (dired-directory
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette))))

      (dired-symlink
       ((,c :inherit link)))

      (dired-broken-symlink
       ((,c :inherit link :foreground ,(bv-themes--retrieve-palette-value 'error palette))))

      (dired-flagged
       ((,c :inherit bv-themes-mark-delete)))

      (dired-marked
       ((,c :inherit bv-themes-mark-select)))

      (dired-header
       ((,c :inherit bold)))

      (dired-mark
       ((,c :inherit bold)))

      (dired-ignored
       ((,c :inherit shadow)))

      (dired-perm-write
       ((,c :inherit shadow)))

      ;; Eshell
      (eshell-prompt
       ((,c ,@(bv-themes--prompt-props palette))))

      (eshell-ls-directory
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight bold)))

      (eshell-ls-symlink
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1 palette))))

      (eshell-ls-executable
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette))))

      ;; Term/ansi-term
      (term-color-black
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-black palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-black palette))))

      (term-color-red
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-red palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-red palette))))

      (term-color-green
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-green palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-green palette))))

      (term-color-yellow
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-yellow palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-yellow palette))))

      (term-color-blue
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-blue palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-blue palette))))

      (term-color-magenta
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-magenta palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-magenta palette))))

      (term-color-cyan
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-cyan palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-cyan palette))))

      (term-color-white
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-white palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-white palette))))

      ;; Ansi-color
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

      (ansi-color-bright-black
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-black-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-black-bright palette))))

      (ansi-color-bright-red
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-red-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-red-bright palette))))

      (ansi-color-bright-green
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-green-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-green-bright palette))))

      (ansi-color-bright-yellow
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-yellow-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-yellow-bright palette))))

      (ansi-color-bright-blue
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-blue-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-blue-bright palette))))

      (ansi-color-bright-magenta
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-magenta-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-magenta-bright palette))))

      (ansi-color-bright-cyan
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-cyan-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-cyan-bright palette))))

      (ansi-color-bright-white
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-term-white-bright palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-term-white-bright palette))))

      ;; Package faces
      (package-name
       ((,c :inherit link)))

      (package-description
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'docstring palette))))

      (package-status-installed
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-alt palette))))

      (package-status-available
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-alt palette))))

      (package-status-new
       ((,c :inherit success)))

      (package-status-held
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))

      (package-status-disabled
       ((,c :inherit error :strike-through t)))

      (package-status-built-in
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'builtin palette))))

      (package-status-dependency
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))

      (package-status-unsigned
       ((,c :inherit error)))

      (package-status-from-source
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'type palette))))

      ;; SHR/eww
      (shr-link
       ((,c :inherit link)))

      (shr-selected-link
       ((,c :inherit bv-themes-mark-select)))

      (shr-h1
       ((,c :inherit bv-themes-heading-1)))

      (shr-h2
       ((,c :inherit bv-themes-heading-2)))

      (shr-h3
       ((,c :inherit bv-themes-heading-3)))

      (shr-h4
       ((,c :inherit bv-themes-heading-4)))

      (shr-h5
       ((,c :inherit bv-themes-heading-5)))

      (shr-h6
       ((,c :inherit bv-themes-heading-6)))

      ;; Eldoc
      (eldoc-highlight-function-argument
       ((,c :inherit bold
            :background ,(bv-themes--retrieve-palette-value 'bg-active-argument palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-active-argument palette))))

      ;; Diff mode
      (diff-header
       ((,c :inherit bold)))

      (diff-file-header
       ((,c :inherit bold)))

      (diff-hunk-header
       ((,c :inherit bold :background ,(bv-themes--retrieve-palette-value 'bg-inactive palette))))

      (diff-function
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-inactive palette))))

      (diff-added
       ((,c :inherit bv-themes-diff-added :extend t)))

      (diff-removed
       ((,c :inherit bv-themes-diff-removed :extend t)))

      (diff-changed
       ((,c :inherit bv-themes-diff-changed :extend t)))

      (diff-refine-added
       ((,c :inherit bv-themes-diff-refine-added)))

      (diff-refine-removed
       ((,c :inherit bv-themes-diff-refine-removed)))

      (diff-refine-changed
       ((,c :inherit bv-themes-diff-refine-changed)))

      (diff-indicator-added
       ((,c :inherit diff-added :foreground ,(bv-themes--retrieve-palette-value 'fg-added-intense palette))))

      (diff-indicator-removed
       ((,c :inherit diff-removed :foreground ,(bv-themes--retrieve-palette-value 'fg-removed-intense palette))))

      (diff-indicator-changed
       ((,c :inherit diff-changed :foreground ,(bv-themes--retrieve-palette-value 'fg-changed-intense palette))))

      ;; Ediff
      (ediff-current-diff-A
       ((,c :inherit bv-themes-diff-removed)))

      (ediff-current-diff-B
       ((,c :inherit bv-themes-diff-added)))

      (ediff-current-diff-C
       ((,c :inherit bv-themes-diff-changed)))

      (ediff-current-diff-Ancestor
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-region palette))))

      (ediff-fine-diff-A
       ((,c :inherit bv-themes-diff-refine-removed)))

      (ediff-fine-diff-B
       ((,c :inherit bv-themes-diff-refine-added)))

      (ediff-fine-diff-C
       ((,c :inherit bv-themes-diff-refine-changed)))

      ;; Smerge
      (smerge-lower
       ((,c :inherit diff-added)))

      (smerge-upper
       ((,c :inherit diff-removed)))

      (smerge-base
       ((,c :inherit diff-changed)))

      (smerge-markers
       ((,c :inherit diff-header)))

      ;; Flyspell
      (flyspell-incorrect
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-error palette)))))

      (flyspell-duplicate
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-warning palette)))))

      ;; Flymake comprehensive diagnostic faces
      (flymake-error
       ((,c :inherit bv-themes-lang-error)))
      (flymake-warning
       ((,c :inherit bv-themes-lang-warning)))
      (flymake-note
       ((,c :inherit bv-themes-lang-note)))

      ;; Echo area messages
      (flymake-error-echo
       ((,c :inherit error)))
      (flymake-warning-echo
       ((,c :inherit warning)))
      (flymake-note-echo
       ((,c :inherit success)))

      ;; End-of-line diagnostics
      (flymake-end-of-line-diagnostics-face
       ((,c :inherit bv-themes-slant
            :height 0.85
            :box ,(bv-themes--retrieve-palette-value 'border palette))))
      (flymake-error-echo-at-eol
       ((,c :inherit flymake-end-of-line-diagnostics-face
            :foreground ,(bv-themes--retrieve-palette-value 'error palette))))
      (flymake-warning-echo-at-eol
       ((,c :inherit flymake-end-of-line-diagnostics-face
            :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))
      (flymake-note-echo-at-eol
       ((,c :inherit flymake-end-of-line-diagnostics-face
            :foreground ,(bv-themes--retrieve-palette-value 'info palette))))

      ;; Flycheck
      (flycheck-error
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-error palette)))))

      (flycheck-warning
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-warning palette)))))

      (flycheck-info
       ((,c :underline (:style wave :color ,(bv-themes--retrieve-palette-value 'underline-note palette)))))

      ;; Completion frameworks
      (orderless-match-face-0 ((,c ,@(bv-themes--completion-props 0 palette))))
      (orderless-match-face-1 ((,c ,@(bv-themes--completion-props 1 palette))))
      (orderless-match-face-2 ((,c ,@(bv-themes--completion-props 2 palette))))
      (orderless-match-face-3 ((,c ,@(bv-themes--completion-props 3 palette))))

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

      ;; Company
      (company-tooltip
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-popup palette))))
      (company-tooltip-selection
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-completion palette))))
      (company-tooltip-common
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0 palette)
            :weight bold)))

      ;; Org mode
      (org-block
       ((,c :background ,(bv-themes--org-block-bg 'default palette)
            :extend t
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-block-begin-line
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-prose-block-delimiter palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-prose-block-delimiter palette)
            :extend t
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-block-end-line
       ((,c :inherit org-block-begin-line)))
      (org-code
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-code palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-verbatim
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-verbatim palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-table
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-table palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-formula
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-macro palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-quote
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :slant ,(if italic-constructs 'italic 'normal))))
      (org-verse
       ((,c :inherit org-quote)))
      (org-todo
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-todo palette)
            :weight bold)))
      (org-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-done palette)
            :weight medium)))
      (org-headline-done
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (org-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
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
      (org-tag
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-tag palette)
            :weight light)))
      (org-meta-line
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-document-title
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-heading-0 palette)
            :weight bold
            :height 1.1)))
      (org-document-info
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata palette))))
      (org-document-info-keyword
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata-value palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-drawer
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-footnote
       ((,c :inherit link)))
      (org-ellipsis
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))
      (org-agenda-structure
       ((,c :inherit bv-themes-heading-1)))
      (org-agenda-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette)
            :inherit bold)))
      (org-agenda-date-today
       ((,c :inherit org-agenda-date :underline t)))
      (org-agenda-date-weekend
       ((,c :inherit org-agenda-date :foreground ,(bv-themes--retrieve-palette-value 'date-weekend palette))))
      (org-checkbox
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'warning palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-priority
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-tag palette))))
      (org-special-keyword
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'prose-metadata palette)
            ,@(when mixed-fonts '(:inherit fixed-pitch)))))
      (org-sexp-date
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette))))

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


      ;; EIN (Emacs IPython Notebook) faces
      (ein:basecell-input-area-face
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette)
            :extend t)))
      (ein:cell-output-area
       (( ))) ; no styling
      (ein:cell-output-area-error
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-removed palette)
            :extend t)))
      (ein:cell-output-stderr
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-removed palette)
            :extend t)))
      (ein:markdowncell-input-area-face
       (( ))) ; no styling
      (ein:notification-tab-normal
       ((,c :underline t)))

      ;; Base error/warning/success faces
      (error
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'error palette))))
      (warning
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'warning palette))))
      (success
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'info palette))))

      ;; Eldoc
      (eldoc-highlight-function-argument
       ((,c :inherit bold
            :background ,(bv-themes--retrieve-palette-value 'bg-active-argument palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-active-argument palette))))

      ;; LSP/Eglot comprehensive faces
      (eglot-mode-line
       ((,c :inherit bv-themes-bold
            :foreground ,(bv-themes--retrieve-palette-value 'modeline-info palette))))
      (eglot-diagnostic-tag-unnecessary-face
       ((,c :inherit bv-themes-lang-note)))

      (lsp-face-highlight-textual
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette))))
      (lsp-face-highlight-read
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover-secondary palette))))
      (lsp-face-highlight-write
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-hover palette)
            :weight bold)))

      ;; LSP headerline breadcrumb
      (lsp-headerline-breadcrumb-separator-face
       ((,c :inherit shadow)))
      (lsp-headerline-breadcrumb-path-face
       ((,c :inherit font-lock-string-face)))
      (lsp-headerline-breadcrumb-path-error-face
       ((,c :inherit (lsp-headerline-breadcrumb-path-face bv-themes-lang-error))))
      (lsp-headerline-breadcrumb-path-warning-face
       ((,c :inherit (lsp-headerline-breadcrumb-path-face bv-themes-lang-warning))))
      (lsp-headerline-breadcrumb-path-info-face
       ((,c :inherit (lsp-headerline-breadcrumb-path-face bv-themes-lang-note))))
      (lsp-headerline-breadcrumb-path-hint-face
       ((,c :inherit (lsp-headerline-breadcrumb-path-face bv-themes-lang-note))))
      (lsp-headerline-breadcrumb-project-prefix-face
       ((,c :inherit bold :foreground ,(bv-themes--retrieve-palette-value 'name palette))))
      (lsp-headerline-breadcrumb-symbols-face
       ((,c :inherit (bold font-lock-doc-face))))

      ;; LSP UI elements
      (lsp-face-rename
       ((,c :inherit bv-themes-search-replace)))
      (lsp-modeline-code-actions-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'modeline-warning palette))))
      (lsp-signature-highlight-function-argument
       ((,c :inherit bold
            :background ,(bv-themes--retrieve-palette-value 'bg-active-argument palette)
            :foreground ,(bv-themes--retrieve-palette-value 'fg-active-argument palette))))

      ;; LSP-UI package (if used)
      (lsp-ui-doc-background
       ((,c :background ,(bv-themes--retrieve-palette-value 'bg-dim palette))))
      (lsp-ui-sideline-symbol
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :box (:line-width -1 :color ,(bv-themes--retrieve-palette-value 'border palette)))))
      (lsp-ui-sideline-current-symbol
       ((,c :inherit bold
            :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :box (:line-width -1 :color ,(bv-themes--retrieve-palette-value 'border palette)))))
      (lsp-ui-sideline-code-action
       ((,c :inherit lsp-modeline-code-actions-face)))
      (lsp-ui-sideline-symbol-info
       ((,c :inherit bv-themes-slant)))

      ;; Rainbow delimiters with proper scaling
      ,@(cl-loop for i from 1 to 9
                 for base-colors = '(red orange yellow green cyan blue purple magenta pink)
                 collect `(,(intern (format "rainbow-delimiters-depth-%d-face" i))
                           ((,c :foreground ,(bv-themes--retrieve-palette-value
                                              (nth (mod (1- i) (length base-colors)) base-colors)
                                              palette)))))

      ;; Nerd-icons
      (nerd-icons-blue ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue-cooler palette))))
      (nerd-icons-blue-alt ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue-warmer palette))))
      (nerd-icons-cyan ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan palette))))
      (nerd-icons-cyan-alt ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan-warmer palette))))
      (nerd-icons-dblue ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue-faint palette))))
      (nerd-icons-dcyan ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan-faint palette))))
      (nerd-icons-dgreen ((,c :foreground ,(bv-themes--retrieve-palette-value 'green-faint palette))))
      (nerd-icons-dmaroon ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-faint palette))))
      (nerd-icons-dorange ((,c :foreground ,(bv-themes--retrieve-palette-value 'red-faint palette))))
      (nerd-icons-dpink ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-faint palette))))
      (nerd-icons-dpurple ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-cooler palette))))
      (nerd-icons-dred ((,c :foreground ,(bv-themes--retrieve-palette-value 'red palette))))
      (nerd-icons-dsilver ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan-faint palette))))
      (nerd-icons-dyellow ((,c :foreground ,(bv-themes--retrieve-palette-value 'yellow-faint palette))))
      (nerd-icons-green ((,c :foreground ,(bv-themes--retrieve-palette-value 'green palette))))
      (nerd-icons-lblue ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue-cooler palette))))
      (nerd-icons-lcyan ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan palette))))
      (nerd-icons-lgreen ((,c :foreground ,(bv-themes--retrieve-palette-value 'green-warmer palette))))
      (nerd-icons-lmaroon ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-warmer palette))))
      (nerd-icons-lorange ((,c :foreground ,(bv-themes--retrieve-palette-value 'red-warmer palette))))
      (nerd-icons-lpink ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta palette))))
      (nerd-icons-lpurple ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-faint palette))))
      (nerd-icons-lred ((,c :foreground ,(bv-themes--retrieve-palette-value 'red-faint palette))))
      (nerd-icons-lsilver ((,c :foreground "gray50")))
      (nerd-icons-lyellow ((,c :foreground ,(bv-themes--retrieve-palette-value 'yellow-warmer palette))))
      (nerd-icons-maroon ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta palette))))
      (nerd-icons-orange ((,c :foreground ,(bv-themes--retrieve-palette-value 'yellow-warmer palette))))
      (nerd-icons-pink ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-warmer palette))))
      (nerd-icons-purple ((,c :foreground ,(bv-themes--retrieve-palette-value 'magenta-cooler palette))))
      (nerd-icons-purple-alt ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue-warmer palette))))
      (nerd-icons-red ((,c :foreground ,(bv-themes--retrieve-palette-value 'red palette))))
      (nerd-icons-red-alt ((,c :foreground ,(bv-themes--retrieve-palette-value 'red-cooler palette))))
      (nerd-icons-silver ((,c :foreground "gray50")))
      (nerd-icons-yellow ((,c :foreground ,(bv-themes--retrieve-palette-value 'yellow palette))))

      ;; Nerd-icons-dired
      (nerd-icons-dired-dir-face ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue palette))))

      ;; Nerd-icons-ibuffer
      (nerd-icons-ibuffer-dir-face ((,c :foreground ,(bv-themes--retrieve-palette-value 'blue palette))))
      (nerd-icons-ibuffer-file-face ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette))))
      (nerd-icons-ibuffer-mode-face ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-alt palette))))
      (nerd-icons-ibuffer-size-face ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan palette))))

      ;; Header line faces
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

      ;; Elfeed faces - RSS feed reader
      ;; Log buffer faces
      (elfeed-log-date-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette))))

      (elfeed-log-debug-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      (elfeed-log-error-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'red-intense palette)
            :weight bold)))

      (elfeed-log-info-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan palette))))

      (elfeed-log-warn-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'yellow-intense palette))))

      ;; Elfeed-score specific faces
      (elfeed-score-date-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette)
            :slant italic)))  ; Distinguish from regular log dates

      (elfeed-score-debug-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))

      (elfeed-score-error-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'red-intense palette)
            :weight bold)))

      (elfeed-score-info-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'cyan-faint palette))))

      (elfeed-score-scoring-explain-text-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :background ,(bv-themes--retrieve-palette-value 'bg-cyan-nuanced palette)
            :weight bold)))  ; Highlight matched text in explanations

      (elfeed-score-warn-level-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'yellow palette))))

      ;; Search buffer faces - the main interface
      (elfeed-search-date-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'date-common palette))))

      (elfeed-search-feed-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-1-faint palette))))  ; Subtle orange for feed names

      (elfeed-search-filter-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-3-intense palette)
            :weight bold)))  ; Purple for active filter - important UI element

      (elfeed-search-last-update-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette)
            :slant italic)))  ; Subtle timestamp info

      (elfeed-search-tag-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-2 palette))))  ; Green for tags - categorical data

      (elfeed-search-title-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-dim palette))))  ; Dimmed for read entries

      (elfeed-search-unread-count-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'accent-0-intense palette)
            :weight bold)))  ; Bright cyan for unread count - attention grabbing

      (elfeed-search-unread-title-face
       ((,c :foreground ,(bv-themes--retrieve-palette-value 'fg-main palette)
            :weight bold))))))  ; Bold and bright for unread - primary content

;;; Hooks and state

(defvar bv-themes-after-load-theme-hook nil
  "Hook run after a BV theme is loaded.
Functions in this hook can access the current theme's palette
using `bv-themes--current-theme-palette'.")

(defvar bv-themes--current-theme nil
  "The currently loaded BV theme.")

(defvar bv-themes--current-palette nil
  "The palette of the currently loaded theme.")

(defun bv-themes--current-theme-palette ()
  "Return the palette of the currently loaded theme.
Returns nil if no BV theme is loaded."
  bv-themes--current-palette)

;;; Theme generation

(defmacro bv-themes-theme (name palette)
  "Generate theme NAME using PALETTE."
  (let ((theme-name name))
    `(progn
       (deftheme ,name
         ,(format "BV theme variant: %s" name))

       (let* ((palette (bv-themes--palette-value ',theme-name))
              (faces (bv-themes--face-specs palette))
              (ansi-colors (vector
                           (bv-themes--retrieve-palette-value 'bg-term-black palette)
                           (bv-themes--retrieve-palette-value 'red palette)
                           (bv-themes--retrieve-palette-value 'green palette)
                           (bv-themes--retrieve-palette-value 'yellow palette)
                           (bv-themes--retrieve-palette-value 'blue palette)
                           (bv-themes--retrieve-palette-value 'magenta palette)
                           (bv-themes--retrieve-palette-value 'cyan palette)
                           (bv-themes--retrieve-palette-value 'bg-term-white palette))))
         (apply #'custom-theme-set-faces ',name faces)
         (custom-theme-set-variables
          ',name
          (list 'ansi-color-names-vector ansi-colors))
         ;; Set current theme state
         (setq bv-themes--current-theme ',name)
         (setq bv-themes--current-palette palette))

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
  (load-theme theme t)
  ;; Run after-load hook
  (run-hooks 'bv-themes-after-load-theme-hook))

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
                            ("Cooler Variants" . (red-cooler green-cooler blue-cooler yellow-cooler
                                                  magenta-cooler cyan-cooler))
                            ("Warmer Variants" . (red-warmer green-warmer blue-warmer yellow-warmer
                                                  magenta-warmer cyan-warmer))
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

;;; Preset overrides (inspired by Modus themes)

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

(defvar bv-themes-preset-overrides-monochrome
  '((keyword . fg-main)
    (string . fg-dim)
    (comment . fg-dim)
    (fnname . fg-main)
    (variable . fg-alt)
    (type . fg-main)
    (constant . fg-alt)
    (builtin . fg-main)
    (docstring . fg-dim)
    (preprocessor . fg-alt)

    (accent-0 . fg-main)
    (accent-1 . fg-alt)
    (accent-2 . fg-dim)
    (accent-3 . fg-active))
  "Preset for monochrome styling.")

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
