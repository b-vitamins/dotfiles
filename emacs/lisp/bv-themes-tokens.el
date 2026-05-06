;;; bv-themes-tokens.el --- BV theme token compiler -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; Theme profiles are registered by theme specification files.  This module only
;; owns profile validation and expansion into dense semantic tokens: text,
;; surfaces, syntax, diagnostics, diffs, terminal colors, and material states.

;;; Code:

(require 'cl-lib)
(require 'bv-themes-color)

(defvar bv-themes-token-profiles nil
  "Registered BV theme profile definitions.")

(defconst bv-themes-tokens-required-anchor-names
  '(bg-main bg-dim fg-main red orange yellow green teal cyan blue purple
    magenta)
  "Anchor names every BV theme profile must define.")

(defconst bv-themes-tokens-known-mix-names
  '(bg-dim-dark bg-alt bg-alt-2 bg-active bg-hover bg-hover-secondary
    bg-region bg-region-subtle bg-hl-line bg-header bg-header-strong
    bg-completion-current bg-prose-block-contents bg-markup-code fg-muted
    fg-dim fg-faint border border-subtle border-strong)
  "Known mix controls accepted by BV token profiles.")

(defconst bv-themes-tokens-known-tone-names
  '(faint subtle subtle-2 strong-chroma-scale)
  "Known tone controls accepted by BV token profiles.")

(defconst bv-themes-known-token-names
  (let ((accent-names '(red orange yellow green teal cyan blue purple magenta))
        (tokens
         '(bg-main bg-dim bg-alt bg-alt-2 bg-active bg-hover
           bg-hover-secondary bg-hl-line bg-region bg-region-subtle
           bg-selected bg-tab-bar bg-tab-current bg-tab-other bg-header
           bg-header-strong bg-tooltip bg-completion bg-completion-current
           bg-popup bg-prose-block bg-prose-block-contents bg-markup-code
           fg-main fg-muted fg-dim fg-faint fg-inverse fg-active fg-salient
           fg-special fg-special-mild fg-header fg-header-muted
           fg-header-inverse cursor border border-subtle border-strong
           accent-0 accent-1 accent-2 accent-3 accent-0-subtle
           accent-1-subtle accent-2-subtle accent-3-subtle accent-0-intense
           accent-1-intense accent-2-intense accent-3-intense success
           success-subtle success-strong warning warning-subtle
           warning-strong error error-subtle error-strong info info-subtle
           info-strong critical critical-bg critical-fg syntax-comment
           syntax-doc syntax-string syntax-regexp syntax-escape
           syntax-keyword syntax-builtin syntax-function syntax-variable
           syntax-constant syntax-number syntax-type syntax-operator
           syntax-preprocessor syntax-property syntax-bracket
           syntax-delimiter prose-heading-1 prose-heading-2 prose-heading-3
           prose-heading-4 prose-heading-5 prose-heading-6 prose-heading-7
           prose-heading-8 prose-link prose-link-visited prose-code
           prose-verbatim prose-metadata prose-metadata-value prose-table
           prose-todo prose-done bg-search fg-search bg-search-current
           fg-search-current bg-search-lazy bg-match bg-paren fg-paren
           bg-added bg-added-refine fg-added bg-removed bg-removed-refine
           fg-removed bg-changed bg-changed-refine fg-changed
           modeline-bg-active modeline-fg-active modeline-bg-inactive
           modeline-fg-inactive modeline-border-active
           modeline-border-inactive modeline-bg-accent modeline-fg-accent
           modeline-bg-critical modeline-fg-critical fg-link fg-link-faint
           fg-link-visited prompt keybind term-black term-red term-green
           term-yellow term-blue term-magenta term-cyan term-white
           term-bright-black term-bright-red term-bright-green
           term-bright-yellow term-bright-blue term-bright-magenta
           term-bright-cyan term-bright-white)))
    (append
     tokens
     (cl-loop for name in accent-names append
              (list name
                    (intern (format "%s-faint" name))
                    (intern (format "%s-subtle" name))
                    (intern (format "%s-subtle-2" name))
                    (intern (format "%s-strong" name))))
     (cl-loop for index from 16 to 255
              collect (intern (format "term-color-%d" index)))))
  "All token names produced by BV theme profiles.")

(defun bv-themes-tokens--duplicates (items)
  "Return duplicate symbols in ITEMS."
  (let (seen duplicates)
    (dolist (item items)
      (if (memq item seen)
          (cl-pushnew item duplicates)
        (push item seen)))
    (nreverse duplicates)))

(defun bv-themes-tokens--validate-anchors (theme anchors)
  "Validate THEME ANCHORS."
  (unless (listp anchors)
    (error "BV theme profile %S has invalid :anchors: %S" theme anchors))
  (when-let ((duplicates
              (bv-themes-tokens--duplicates (mapcar #'car anchors))))
    (error "BV theme profile %S has duplicate anchors: %S"
           theme duplicates))
  (dolist (required bv-themes-tokens-required-anchor-names)
    (unless (assq required anchors)
      (error "BV theme profile %S is missing anchor %S" theme required)))
  (dolist (anchor anchors)
    (pcase anchor
      (`(,name . (,l ,chroma ,hue))
       (unless (memq name bv-themes-tokens-required-anchor-names)
         (error "BV theme profile %S has unknown anchor %S" theme name))
       (unless (and (numberp l)
                    (<= 0.0 l)
                    (<= l 1.0)
                    (numberp chroma)
                    (<= 0.0 chroma)
                    (numberp hue)
                    (<= 0.0 hue)
                    (< hue 360.0))
         (error "BV theme profile %S has invalid anchor %S: %S"
                theme name (cdr anchor))))
      (_
       (error "BV theme profile %S has malformed anchor: %S"
              theme anchor)))))

(defun bv-themes-tokens--validate-control-alist
    (theme key values known-names)
  "Validate THEME control KEY VALUES against KNOWN-NAMES."
  (when values
    (unless (listp values)
      (error "BV theme profile %S has invalid %S: %S"
             theme key values))
    (when-let ((duplicates
                (bv-themes-tokens--duplicates (mapcar #'car values))))
      (error "BV theme profile %S has duplicate %S keys: %S"
             theme key duplicates))
    (dolist (entry values)
      (unless (memq (car entry) known-names)
        (error "BV theme profile %S has unknown %S key %S"
               theme key (car entry)))
      (unless (numberp (cdr entry))
        (error "BV theme profile %S has non-numeric %S value for %S"
               theme key (car entry)))
      (pcase key
        (:mixes
         (unless (<= 0.0 (cdr entry) 1.0)
           (error "BV theme profile %S has out-of-range mix %S: %S"
                  theme (car entry) (cdr entry))))
        (:tone-curve
         (if (eq (car entry) 'strong-chroma-scale)
             (unless (<= 0.5 (cdr entry) 2.0)
               (error "BV theme profile %S has out-of-range tone %S: %S"
                      theme (car entry) (cdr entry)))
           (unless (<= 0.0 (cdr entry) 1.0)
             (error "BV theme profile %S has out-of-range tone %S: %S"
                    theme (car entry) (cdr entry)))))))))

(defun bv-themes-tokens--hex-color-p (value)
  "Return non-nil if VALUE is a hex color string."
  (and (stringp value)
       (string-match-p "\\`#[[:xdigit:]]\\{3\\}\\(?:[[:xdigit:]]\\{3\\}\\)?\\'"
                       value)))

(defun bv-themes-tokens--validate-oklch
    (theme context l chroma hue)
  "Validate THEME OKLCH values for CONTEXT."
  (unless (and (numberp l)
               (<= 0.0 l)
               (<= l 1.0)
               (numberp chroma)
               (<= 0.0 chroma)
               (numberp hue)
               (<= 0.0 hue)
               (< hue 360.0))
    (error "BV theme profile %S has invalid OKLCH in %S: %S"
           theme context (list l chroma hue))))

(defun bv-themes-tokens--validate-expression (theme expression)
  "Validate token EXPRESSION for THEME."
  (cond
   ((bv-themes-tokens--hex-color-p expression)
    t)
   ((stringp expression)
    (error "BV theme profile %S has invalid color string: %S"
           theme expression))
   ((symbolp expression)
    (unless (memq expression bv-themes-known-token-names)
      (error "BV theme profile %S references unknown token %S"
             theme expression)))
   ((consp expression)
    (pcase expression
      (`(token ,name)
       (bv-themes-tokens--validate-expression theme name))
      (`(mix ,left ,right ,amount)
       (unless (and (numberp amount) (<= 0.0 amount) (<= amount 1.0))
         (error "BV theme profile %S has invalid mix amount: %S"
                theme expression))
       (bv-themes-tokens--validate-expression theme left)
       (bv-themes-tokens--validate-expression theme right))
      (`(tone ,name ,kind)
       (unless (memq (intern (format "%s-%s" name kind))
                     bv-themes-known-token-names)
         (error "BV theme profile %S references unknown tone: %S"
                theme expression)))
      (`(adjust ,source . ,plist)
       (unless (and (listp plist) (not (cl-oddp (length plist))))
         (error "BV theme profile %S has invalid adjust plist: %S"
                theme expression))
       (bv-themes-tokens--validate-expression theme source)
       (while plist
         (let ((key (pop plist))
               (value (pop plist)))
           (unless (memq key
                         '(:lightness :chroma :hue
                           :lightness-scale :chroma-scale))
             (error "BV theme profile %S has invalid adjust key %S"
                    theme key))
           (unless (numberp value)
             (error "BV theme profile %S has invalid adjust value for %S"
                    theme key)))))
      (`(oklch ,l ,chroma ,hue)
       (bv-themes-tokens--validate-oklch
        theme 'token-expression l chroma hue))
      (_
       (error "BV theme profile %S has invalid token expression: %S"
              theme expression))))
   (t
    (error "BV theme profile %S has invalid token expression: %S"
           theme expression))))

(defun bv-themes-tokens--validate-overrides (theme overrides)
  "Validate THEME token OVERRIDES."
  (when-let ((duplicates
              (bv-themes-tokens--duplicates (mapcar #'car overrides))))
    (error "BV theme profile %S has duplicate token overrides: %S"
           theme duplicates))
  (dolist (override overrides)
    (unless (memq (car override) bv-themes-known-token-names)
      (error "BV theme profile %S has unknown token override %S"
             theme (car override)))
    (bv-themes-tokens--validate-expression theme (cdr override))))

(defun bv-themes-tokens--validate-profile (theme profile)
  "Validate THEME PROFILE."
  (unless (symbolp theme)
    (error "BV theme name must be a symbol: %S" theme))
  (unless (plist-get profile :display-name)
    (error "BV theme profile %S is missing :display-name" theme))
  (unless (memq (plist-get profile :variant) '(light dark))
    (error "BV theme profile %S has invalid :variant %S"
           theme (plist-get profile :variant)))
  (bv-themes-tokens--validate-anchors
   theme
   (plist-get profile :anchors))
  (bv-themes-tokens--validate-control-alist
   theme :mixes (plist-get profile :mixes)
   bv-themes-tokens-known-mix-names)
  (bv-themes-tokens--validate-control-alist
   theme :tone-curve (plist-get profile :tone-curve)
   bv-themes-tokens-known-tone-names)
  (bv-themes-tokens--validate-overrides
   theme
   (plist-get profile :token-overrides)))

(defun bv-themes-tokens-register-profile (theme profile)
  "Register THEME with token PROFILE."
  (bv-themes-tokens--validate-profile theme profile)
  (setq bv-themes-token-profiles
        (cons (cons theme profile)
              (assq-delete-all theme bv-themes-token-profiles)))
  theme)

(defun bv-themes-tokens-profile (theme)
  "Return the token profile for THEME."
  (or (cdr (assq theme bv-themes-token-profiles))
      (error "Unknown BV theme profile: %S" theme)))

(defun bv-themes-tokens-variant (theme)
  "Return the variant symbol for THEME."
  (plist-get (bv-themes-tokens-profile theme) :variant))

(defun bv-themes-tokens-display-name (theme)
  "Return the human display name for THEME."
  (plist-get (bv-themes-tokens-profile theme) :display-name))

(defun bv-themes-tokens--intensity-scale (intensity)
  "Return chroma scale for INTENSITY."
  (pcase intensity
    ('faint 0.82)
    ('vivid 1.14)
    ('high-chroma 1.24)
    (_ 1.0)))

(defun bv-themes-tokens--anchor (profile name &optional scale)
  "Return hex color for PROFILE anchor NAME.
SCALE multiplies chroma before gamut mapping."
  (let* ((anchors (plist-get profile :anchors))
         (oklch (cdr (assq name anchors))))
    (unless oklch
      (error "Unknown BV theme anchor: %S" name))
    (cl-destructuring-bind (l chroma hue) oklch
      (bv-themes-color-oklch l (* chroma (or scale 1.0)) hue))))

(defun bv-themes-tokens--amount (profile table key default)
  "Return numeric PROFILE TABLE value for KEY, or DEFAULT."
  (or (alist-get key (plist-get profile table))
      default))

(defun bv-themes-tokens--mix (fg bg amount)
  "Return FG mixed into BG by AMOUNT."
  (bv-themes-color-mix fg bg amount))

(defun bv-themes-tokens--tone (profile color variant name bg-main)
  "Return a derived COLOR tone for PROFILE, VARIANT, and NAME."
  (pcase name
    ('faint (if (eq variant 'light)
                (bv-themes-color-mix
                 color bg-main
                 (bv-themes-tokens--amount profile :tone-curve 'faint 0.72))
              (bv-themes-color-mix
               color bg-main
               (bv-themes-tokens--amount profile :tone-curve 'faint 0.78))))
    ('subtle (if (eq variant 'light)
                 (bv-themes-color-mix
                  color bg-main
                  (bv-themes-tokens--amount profile :tone-curve 'subtle 0.16))
               (bv-themes-color-mix
                color bg-main
                (bv-themes-tokens--amount profile :tone-curve 'subtle 0.20))))
    ('subtle-2 (if (eq variant 'light)
                   (bv-themes-color-mix
                    color bg-main
                    (bv-themes-tokens--amount
                     profile :tone-curve 'subtle-2 0.24))
                 (bv-themes-color-mix
                  color bg-main
                  (bv-themes-tokens--amount
                   profile :tone-curve 'subtle-2 0.28))))
    ('strong (if (eq variant 'light)
                 (bv-themes-color-adjust-oklch color :lightness -0.05
                                               :chroma-scale
                                               (bv-themes-tokens--amount
                                                profile :tone-curve
                                                'strong-chroma-scale 1.05))
               (bv-themes-color-adjust-oklch color :lightness 0.04
                                             :chroma-scale
                                             (bv-themes-tokens--amount
                                              profile :tone-curve
                                              'strong-chroma-scale 1.05))))
    (_ color)))

(defun bv-themes-tokens--resolve-value (value tokens)
  "Resolve token expression VALUE using TOKENS."
  (cond
   ((stringp value) value)
   ((and (symbolp value) (assq value tokens))
    (cdr (assq value tokens)))
   ((symbolp value)
    (error "Unknown BV token expression symbol: %S" value))
   ((consp value)
    (pcase value
      (`(token ,name)
       (bv-themes-tokens--resolve-value name tokens))
      (`(mix ,left ,right ,amount)
       (bv-themes-color-mix
        (bv-themes-tokens--resolve-value left tokens)
        (bv-themes-tokens--resolve-value right tokens)
        amount))
      (`(tone ,name ,kind)
       (bv-themes-tokens--resolve-value
        (intern (format "%s-%s" name kind))
        tokens))
      (`(adjust ,source . ,plist)
       (apply #'bv-themes-color-adjust-oklch
              (bv-themes-tokens--resolve-value source tokens)
              plist))
      (`(oklch ,l ,chroma ,hue)
       (bv-themes-color-oklch l chroma hue))
      (_
       (error "Unknown BV token expression: %S" value))))
   (t
    (error "Invalid BV token expression: %S" value))))

(defun bv-themes-tokens--apply-overrides (tokens overrides)
  "Return TOKENS with semantic OVERRIDES applied."
  (let ((result (copy-sequence tokens))
        (expressions (copy-sequence overrides)))
    (cl-labels
        ((resolve-token
          (name stack)
          (cond
           ((memq name stack)
            (error "Cyclic BV token override reference: %S"
                   (nreverse (cons name stack))))
           ((assq name expressions)
            (let ((value (resolve-expression
                          (cdr (assq name expressions))
                          (cons name stack))))
              (setf (alist-get name result) value)
              value))
           ((assq name result)
            (cdr (assq name result)))
           (t
            (error "Unknown BV token expression symbol: %S" name))))
         (resolve-expression
          (value stack)
          (cond
           ((stringp value) value)
           ((symbolp value)
            (resolve-token value stack))
           ((consp value)
            (pcase value
              (`(token ,name)
               (resolve-expression name stack))
              (`(mix ,left ,right ,amount)
               (bv-themes-color-mix
                (resolve-expression left stack)
                (resolve-expression right stack)
                amount))
              (`(tone ,name ,kind)
               (resolve-token
                (intern (format "%s-%s" name kind))
                stack))
              (`(adjust ,source . ,plist)
               (apply #'bv-themes-color-adjust-oklch
                      (resolve-expression source stack)
                      plist))
              (`(oklch ,l ,chroma ,hue)
               (bv-themes-color-oklch l chroma hue))
              (_
               (error "Unknown BV token expression: %S" value))))
           (t
            (error "Invalid BV token expression: %S" value)))))
      (dolist (override overrides)
        (resolve-token (car override) nil)))
    result))

(defun bv-themes-tokens--rgb-hex (red green blue)
  "Return #rrggbb for RED GREEN BLUE byte values."
  (format "#%02x%02x%02x" red green blue))

(defun bv-themes-tokens--xterm-color (index)
  "Return the xterm 256-color hex value for INDEX."
  (cond
   ((< index 16)
    nil)
   ((< index 232)
    (let* ((value (- index 16))
           (levels [0 95 135 175 215 255])
           (red (/ value 36))
           (green (/ (% value 36) 6))
           (blue (% value 6)))
      (bv-themes-tokens--rgb-hex
       (aref levels red)
       (aref levels green)
       (aref levels blue))))
   ((<= index 255)
    (let ((level (+ 8 (* (- index 232) 10))))
      (bv-themes-tokens--rgb-hex level level level)))
   (t nil)))

(defun bv-themes-tokens-build (theme &optional options)
  "Build token alist for THEME using OPTIONS.
OPTIONS is a plist.  Recognized keys include `:intensity'."
  (let* ((profile (bv-themes-tokens-profile theme))
         (variant (plist-get profile :variant))
         (scale (bv-themes-tokens--intensity-scale
                 (plist-get options :intensity)))
         (bg-main (bv-themes-tokens--anchor profile 'bg-main))
         (bg-dim-anchor (bv-themes-tokens--anchor profile 'bg-dim))
         (fg-main (bv-themes-tokens--anchor profile 'fg-main))
         (red (bv-themes-tokens--anchor profile 'red scale))
         (orange (bv-themes-tokens--anchor profile 'orange scale))
         (yellow (bv-themes-tokens--anchor profile 'yellow scale))
         (green (bv-themes-tokens--anchor profile 'green scale))
         (teal (bv-themes-tokens--anchor profile 'teal scale))
         (cyan (bv-themes-tokens--anchor profile 'cyan scale))
         (blue (bv-themes-tokens--anchor profile 'blue scale))
         (purple (bv-themes-tokens--anchor profile 'purple scale))
         (magenta (bv-themes-tokens--anchor profile 'magenta scale))
         (bg-dim (if (eq variant 'light)
                     bg-dim-anchor
                   (bv-themes-tokens--mix
                    fg-main bg-main
                    (bv-themes-tokens--amount
                     profile :mixes 'bg-dim-dark 0.035))))
         (bg-alt (bv-themes-tokens--mix fg-main bg-main
                                        (bv-themes-tokens--amount
                                         profile :mixes 'bg-alt
                                         (if (eq variant 'light) 0.055 0.075))))
         (bg-alt-2 (bv-themes-tokens--mix fg-main bg-main
                                          (bv-themes-tokens--amount
                                           profile :mixes 'bg-alt-2
                                           (if (eq variant 'light) 0.085 0.115))))
         (bg-active (bv-themes-tokens--mix fg-main bg-main
                                           (bv-themes-tokens--amount
                                            profile :mixes 'bg-active
                                            (if (eq variant 'light) 0.12 0.16))))
         (bg-hover (bv-themes-tokens--mix blue bg-main
                                          (bv-themes-tokens--amount
                                           profile :mixes 'bg-hover
                                           (if (eq variant 'light) 0.08 0.13))))
         (bg-region (bv-themes-tokens--mix blue bg-main
                                           (bv-themes-tokens--amount
                                            profile :mixes 'bg-region
                                            (if (eq variant 'light) 0.24 0.32))))
         (bg-region-subtle (bv-themes-tokens--mix blue bg-main
                                                  (bv-themes-tokens--amount
                                                   profile :mixes
                                                   'bg-region-subtle
                                                   (if (eq variant 'light)
                                                       0.15
                                                     0.22))))
         (bg-hl-line (bv-themes-tokens--mix teal bg-main
                                             (bv-themes-tokens--amount
                                              profile :mixes 'bg-hl-line
                                              (if (eq variant 'light) 0.065 0.10))))
         (fg-muted (bv-themes-tokens--mix fg-main bg-main
                                          (bv-themes-tokens--amount
                                           profile :mixes 'fg-muted
                                           (if (eq variant 'light) 0.68 0.70))))
         (fg-dim (bv-themes-tokens--mix fg-main bg-main
                                        (bv-themes-tokens--amount
                                         profile :mixes 'fg-dim
                                         (if (eq variant 'light) 0.56 0.56))))
         (fg-faint (bv-themes-tokens--mix fg-main bg-main
                                          (bv-themes-tokens--amount
                                           profile :mixes 'fg-faint
                                           (if (eq variant 'light) 0.45 0.42))))
         (fg-inverse (if (eq variant 'light) "#ffffff" "#101417"))
         (on-accent (if (eq variant 'light) "#ffffff" "#101417"))
         tokens)
    (cl-labels
        ((put (name value)
              (push (cons name value) tokens))
         (tone (color name)
               (bv-themes-tokens--tone profile color variant name bg-main))
         (put-accent (name color)
           (put name color)
           (put (intern (format "%s-faint" name)) (tone color 'faint))
           (put (intern (format "%s-subtle" name)) (tone color 'subtle))
           (put (intern (format "%s-subtle-2" name)) (tone color 'subtle-2))
           (put (intern (format "%s-strong" name)) (tone color 'strong))))
      ;; Neutral text and surfaces.
      (put 'bg-main bg-main)
      (put 'bg-dim bg-dim)
      (put 'bg-alt bg-alt)
      (put 'bg-alt-2 bg-alt-2)
      (put 'bg-active bg-active)
      (put 'bg-hover bg-hover)
      (put 'bg-hover-secondary
           (bv-themes-tokens--mix purple bg-main
                                  (bv-themes-tokens--amount
                                   profile :mixes 'bg-hover-secondary
                                   (if (eq variant 'light) 0.07 0.12))))
      (put 'bg-hl-line bg-hl-line)
      (put 'bg-region bg-region)
      (put 'bg-region-subtle bg-region-subtle)
      (put 'bg-selected bg-region)
      (put 'bg-tab-bar (if (eq variant 'light) bg-dim bg-main))
      (put 'bg-tab-current bg-main)
      (put 'bg-tab-other bg-alt)
      (put 'bg-header (if (eq variant 'light)
                          (bv-themes-tokens--mix
                           fg-main bg-main
                           (bv-themes-tokens--amount
                            profile :mixes 'bg-header 0.09))
                        (bv-themes-tokens--mix
                         fg-main bg-main
                         (bv-themes-tokens--amount
                          profile :mixes 'bg-header 0.10))))
      (put 'bg-header-strong (bv-themes-tokens--mix blue bg-main
                                                     (bv-themes-tokens--amount
                                                      profile :mixes
                                                      'bg-header-strong
                                                      (if (eq variant 'light)
                                                          0.18
                                                        0.24))))
      (put 'bg-tooltip (if (eq variant 'light) "#fffffb" bg-alt-2))
      (put 'bg-completion bg-alt)
      (put 'bg-completion-current (bv-themes-tokens--mix blue bg-main
                                                          (bv-themes-tokens--amount
                                                           profile :mixes
                                                           'bg-completion-current
                                                           (if (eq variant 'light)
                                                               0.18
                                                             0.28))))
      (put 'bg-popup bg-main)
      (put 'bg-prose-block bg-alt)
      (put 'bg-prose-block-contents
           (bv-themes-tokens--mix cyan bg-main
                                  (bv-themes-tokens--amount
                                   profile :mixes 'bg-prose-block-contents
                                   (if (eq variant 'light) 0.055 0.09))))
      (put 'bg-markup-code
           (bv-themes-tokens--mix purple bg-main
                                  (bv-themes-tokens--amount
                                   profile :mixes 'bg-markup-code
                                   (if (eq variant 'light) 0.08 0.12))))
      (put 'fg-main fg-main)
      (put 'fg-muted fg-muted)
      (put 'fg-dim fg-dim)
      (put 'fg-faint fg-faint)
      (put 'fg-inverse fg-inverse)
      (put 'fg-active (if (eq variant 'light)
                          (bv-themes-color-adjust-oklch fg-main :lightness -0.04)
                        (bv-themes-color-adjust-oklch fg-main :lightness 0.02)))
      (put 'fg-salient blue)
      (put 'fg-special purple)
      (put 'fg-special-mild (tone purple 'faint))
      (put 'fg-header fg-main)
      (put 'fg-header-muted fg-dim)
      (put 'fg-header-inverse on-accent)
      (put 'cursor (if (eq variant 'light) "#17212f" "#f6f7fb"))
      (put 'border (bv-themes-tokens--mix fg-main bg-main
                                          (bv-themes-tokens--amount
                                           profile :mixes 'border
                                           (if (eq variant 'light) 0.18 0.24))))
      (put 'border-subtle (bv-themes-tokens--mix fg-main bg-main
                                                 (bv-themes-tokens--amount
                                                  profile :mixes 'border-subtle
                                                  (if (eq variant 'light) 0.11 0.16))))
      (put 'border-strong (bv-themes-tokens--mix fg-main bg-main
                                                 (bv-themes-tokens--amount
                                                  profile :mixes 'border-strong
                                                  (if (eq variant 'light) 0.32 0.40))))
      ;; Accents.
      (put-accent 'red red)
      (put-accent 'orange orange)
      (put-accent 'yellow yellow)
      (put-accent 'green green)
      (put-accent 'teal teal)
      (put-accent 'cyan cyan)
      (put-accent 'blue blue)
      (put-accent 'purple purple)
      (put-accent 'magenta magenta)
      (put 'accent-0 blue)
      (put 'accent-1 magenta)
      (put 'accent-2 teal)
      (put 'accent-3 orange)
      (put 'accent-0-subtle (tone blue 'subtle))
      (put 'accent-1-subtle (tone magenta 'subtle))
      (put 'accent-2-subtle (tone teal 'subtle))
      (put 'accent-3-subtle (tone orange 'subtle))
      (put 'accent-0-intense (tone blue 'strong))
      (put 'accent-1-intense (tone magenta 'strong))
      (put 'accent-2-intense (tone teal 'strong))
      (put 'accent-3-intense (tone orange 'strong))
      ;; Semantic states.
      (put 'success green)
      (put 'success-subtle (tone green 'subtle))
      (put 'success-strong (tone green 'strong))
      (put 'warning yellow)
      (put 'warning-subtle (tone yellow 'subtle))
      (put 'warning-strong (tone yellow 'strong))
      (put 'error red)
      (put 'error-subtle (tone red 'subtle))
      (put 'error-strong (tone red 'strong))
      (put 'info cyan)
      (put 'info-subtle (tone cyan 'subtle))
      (put 'info-strong (tone cyan 'strong))
      (put 'critical red)
      (put 'critical-bg (if (eq variant 'light)
                            (tone red 'strong)
                          (bv-themes-tokens--mix red bg-main 0.04)))
      (put 'critical-fg (if (eq variant 'light) "#ffffff" (tone red 'strong)))
      ;; Syntax.
      (put 'syntax-comment fg-dim)
      (put 'syntax-doc (if (eq variant 'light)
                           (bv-themes-tokens--mix green fg-main 0.56)
                         (bv-themes-tokens--mix green fg-main 0.68)))
      (put 'syntax-string green)
      (put 'syntax-regexp magenta)
      (put 'syntax-escape orange)
      (put 'syntax-keyword purple)
      (put 'syntax-builtin cyan)
      (put 'syntax-function blue)
      (put 'syntax-variable fg-main)
      (put 'syntax-constant magenta)
      (put 'syntax-number orange)
      (put 'syntax-type teal)
      (put 'syntax-operator (if (eq variant 'light)
                                (bv-themes-tokens--mix blue fg-main 0.60)
                              (bv-themes-tokens--mix blue fg-main 0.72)))
      (put 'syntax-preprocessor yellow)
      (put 'syntax-property cyan)
      (put 'syntax-bracket fg-muted)
      (put 'syntax-delimiter fg-muted)
      ;; Markup and prose.
      (put 'prose-heading-1 blue)
      (put 'prose-heading-2 purple)
      (put 'prose-heading-3 teal)
      (put 'prose-heading-4 magenta)
      (put 'prose-heading-5 orange)
      (put 'prose-heading-6 cyan)
      (put 'prose-heading-7 green)
      (put 'prose-heading-8 fg-main)
      (put 'prose-link blue)
      (put 'prose-link-visited purple)
      (put 'prose-code (if (eq variant 'light)
                           (bv-themes-tokens--mix purple fg-main 0.72)
                         (bv-themes-tokens--mix purple fg-main 0.84)))
      (put 'prose-verbatim cyan)
      (put 'prose-metadata fg-dim)
      (put 'prose-metadata-value magenta)
      (put 'prose-table teal)
      (put 'prose-todo red)
      (put 'prose-done green)
      ;; Search, selection, matches.
      (put 'bg-search (if (eq variant 'light)
                          (bv-themes-tokens--mix yellow bg-main 0.40)
                        (bv-themes-tokens--mix yellow bg-main 0.36)))
      (put 'fg-search fg-main)
      (put 'bg-search-current (if (eq variant 'light)
                                  (bv-themes-tokens--mix orange bg-main 0.42)
                                (bv-themes-tokens--mix orange bg-main 0.44)))
      (put 'fg-search-current fg-main)
      (put 'bg-search-lazy (bv-themes-tokens--mix cyan bg-main
                                                  (if (eq variant 'light) 0.22 0.28)))
      (put 'bg-match (bv-themes-tokens--mix green bg-main
                                             (if (eq variant 'light) 0.22 0.26)))
      (put 'bg-paren (bv-themes-tokens--mix blue bg-main
                                             (if (eq variant 'light) 0.35 0.40)))
      (put 'fg-paren fg-main)
      ;; Diffs.
      (put 'bg-added (tone green 'subtle))
      (put 'bg-added-refine (tone green 'subtle-2))
      (put 'fg-added (tone green 'strong))
      (put 'bg-removed (tone red 'subtle))
      (put 'bg-removed-refine (tone red 'subtle-2))
      (put 'fg-removed (tone red 'strong))
      (put 'bg-changed (tone yellow 'subtle))
      (put 'bg-changed-refine (tone yellow 'subtle-2))
      (put 'fg-changed (tone yellow 'strong))
      ;; Modeline/header surfaces.
      (put 'modeline-bg-active (if (eq variant 'light) "#232832" "#dfe5ea"))
      (put 'modeline-fg-active (if (eq variant 'light) "#ffffff" "#15191d"))
      (put 'modeline-bg-inactive (if (eq variant 'light) bg-alt-2 bg-alt))
      (put 'modeline-fg-inactive fg-dim)
      (put 'modeline-border-active (if (eq variant 'light) "#232832" "#dfe5ea"))
      (put 'modeline-border-inactive (alist-get 'border tokens))
      (put 'modeline-bg-accent blue)
      (put 'modeline-fg-accent on-accent)
      (put 'modeline-bg-critical red)
      (put 'modeline-fg-critical on-accent)
      ;; Links and prompts.
      (put 'fg-link blue)
      (put 'fg-link-faint (tone blue 'faint))
      (put 'fg-link-visited purple)
      (put 'prompt cyan)
      (put 'keybind magenta)
      ;; Terminal colors.
      (put 'term-black (if (eq variant 'light) "#30343d" fg-faint))
      (put 'term-red red)
      (put 'term-green green)
      (put 'term-yellow yellow)
      (put 'term-blue blue)
      (put 'term-magenta magenta)
      (put 'term-cyan cyan)
      (put 'term-white (if (eq variant 'light) fg-muted "#d9dde1"))
      (put 'term-bright-black fg-dim)
      (put 'term-bright-red (tone red 'strong))
      (put 'term-bright-green (tone green 'strong))
      (put 'term-bright-yellow (tone yellow 'strong))
      (put 'term-bright-blue (tone blue 'strong))
      (put 'term-bright-magenta (tone magenta 'strong))
      (put 'term-bright-cyan (tone cyan 'strong))
      (put 'term-bright-white fg-main)
      (dotimes (offset 240)
        (let* ((index (+ 16 offset))
               (name (intern (format "term-color-%d" index))))
          (put name (bv-themes-tokens--xterm-color index))))
      (bv-themes-tokens--apply-overrides
       (nreverse tokens)
       (plist-get profile :token-overrides)))))

(defun bv-themes-tokens-get (name tokens)
  "Return token NAME from TOKENS."
  (or (cdr (assq name tokens))
      (error "Unknown BV theme token: %S" name)))

(provide 'bv-themes-tokens)
;;; bv-themes-tokens.el ends here
