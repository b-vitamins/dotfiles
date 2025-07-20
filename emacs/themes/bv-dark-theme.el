;;; bv-dark-theme.el --- Dark theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Ink & Frost Dark Theme
;; ======================
;;
;; A sophisticated dark theme inspired by Japanese sumi-e ink paintings and
;; winter frost patterns. This theme creates a harmonious balance between cool
;; and warm tones, providing a refined coding environment that reduces eye
;; strain during extended programming sessions.
;;
;; Design Philosophy:
;; -----------------
;; • Deep charcoal backgrounds (#1c1c1c) provide a dramatic, high-contrast foundation
;; • Cool frost accents (cyan) serve as the primary highlight color
;; • Warm paper tones balance the cool palette
;; • Subtle color variations maintain visual hierarchy without distraction
;;
;; Color Scales:
;; ------------
;; Each color scale is carefully crafted with five variants:
;; • Base: The standard color for typical use
;; • Warmer: A temperature-shifted variant for contrast
;; • Cooler: A cooler variant for subtle differentiation
;; • Faint: Muted version for secondary elements
;; • Intense: High-saturation version for emphasis
;;
;; Accent System:
;; -------------
;; • Primary (cyan): Keywords, UI elements, links
;; • Secondary (orange): Constants, selections
;; • Tertiary (green): Strings, success states
;; • Quaternary (purple): Types, special syntax
;;
;; Usage:
;; ------
;; (load-theme 'bv-dark t)
;;
;; The theme is designed to work seamlessly with the bv-themes engine,
;; providing consistent styling across all major Emacs modes.

;;; Code:

(require 'bv-themes)

(defconst bv-dark-palette
  '(;; === Foundation Colors ===
    ;; Deep charcoal grays with subtle cool undertones, inspired by sumi-e
    (bg-main . "#1c1c1c")          ; Deep charcoal black
    (fg-main . "#ebebeb")          ; Slightly brighter paper white for contrast
    (bg-dim . "#161616")           ; Deeper charcoal
    (fg-dim . "#a8a8a8")           ; Muted paper gray
    (bg-alt . "#222222")           ; Alternative charcoal wash
    (fg-alt . "#f3f3f3")           ; Bright paper white

    ;; === Active/Inactive States ===
    (bg-active . "#2a2a2a")        ; Active charcoal wash
    (fg-active . "#f8f8f8")        ; Bright active
    (bg-inactive . "#1f1f1f")      ; Inactive charcoal
    (fg-inactive . "#606060")      ; Muted gray

    ;; === Complete Color Scales ===
    ;; Red scale (Vermillion ink)
    (red . "#e27878")              ; Soft vermillion
    (red-warmer . "#e88c8c")       ; Warm coral
    (red-cooler . "#d66b6b")       ; Cool vermillion
    (red-faint . "#c09999")        ; Faded ink red
    (red-intense . "#ff6b6b")      ; Bright vermillion

    ;; Orange scale (Burnt sienna)
    (orange . "#e89c72")           ; Burnt sienna
    (orange-warmer . "#f0a674")    ; Warm amber
    (orange-cooler . "#d68c62")    ; Deep sienna
    (orange-faint . "#c4a08c")     ; Faded sienna
    (orange-intense . "#ff9f45")   ; Bright amber

    ;; Yellow scale (Gold leaf)
    (yellow . "#e7c787")           ; Gold leaf
    (yellow-warmer . "#f0d492")    ; Bright gold
    (yellow-cooler . "#deba7a")    ; Deep gold
    (yellow-faint . "#d4c4a0")     ; Faded gold
    (yellow-intense . "#ffd866")   ; Brilliant gold

    ;; Green scale (Jade & moss)
    (green . "#87c792")            ; Jade green
    (green-warmer . "#95d5a0")     ; Spring jade
    (green-cooler . "#79b984")     ; Deep jade
    (green-faint . "#a0c4a8")      ; Pale jade
    (green-intense . "#66d982")    ; Brilliant jade

    ;; Cyan scale (Frost & ice) - slightly brighter for dark background
    (cyan . "#8fd4f4")             ; Frost blue
    (cyan-warmer . "#9de0f6")      ; Light frost
    (cyan-cooler . "#81c6e6")      ; Deep frost
    (cyan-faint . "#afd9e9")       ; Pale frost
    (cyan-intense . "#5ddcff")     ; Brilliant ice

    ;; Blue scale (Winter sky)
    (blue . "#7aa2f7")             ; Winter sky
    (blue-warmer . "#8bb0ff")      ; Warm sky
    (blue-cooler . "#6994e9")      ; Deep sky
    (blue-faint . "#9db4f5")       ; Pale sky
    (blue-intense . "#5d86ff")     ; Electric sky

    ;; Purple scale (Wisteria)
    (purple . "#ad8ee6")           ; Wisteria purple
    (purple-warmer . "#bb9af7")    ; Warm wisteria
    (purple-cooler . "#9f80d8")    ; Deep wisteria
    (purple-faint . "#c0a8e8")     ; Pale wisteria
    (purple-intense . "#9d7cd8")   ; Vivid wisteria

    ;; Magenta scale (Plum blossom)
    (magenta . "#e799b0")          ; Plum blossom
    (magenta-warmer . "#f0a3ba")   ; Warm plum
    (magenta-cooler . "#d98da4")   ; Deep plum
    (magenta-faint . "#ddb0c0")    ; Pale plum
    (magenta-intense . "#ff7aa2")  ; Bright plum

    ;; Pink scale (Cherry blossom)
    (pink . "#f7a8c8")             ; Cherry blossom
    (pink-warmer . "#ffb3d2")      ; Warm sakura
    (pink-cooler . "#e99dbd")      ; Cool sakura
    (pink-faint . "#f5c5d5")       ; Pale sakura
    (pink-intense . "#ff91c5")     ; Bright sakura

    ;; === Accent Mappings (semantic) ===
    (accent-0 . cyan)              ; Primary accent - frost
    (accent-1 . orange)            ; Secondary accent - sienna
    (accent-2 . green)             ; Tertiary accent - jade
    (accent-3 . purple)            ; Quaternary accent - wisteria

    ;; Accent intensity variants
    (accent-0-intense . cyan-intense)
    (accent-1-intense . orange-intense)
    (accent-2-intense . green-intense)
    (accent-3-intense . purple-intense)
    (accent-0-faint . cyan-faint)
    (accent-1-faint . orange-faint)
    (accent-2-faint . green-faint)
    (accent-3-faint . purple-faint)

    ;; === Nuanced/Subtle Backgrounds ===
    (bg-red-nuanced . "#1f1819")
    (bg-orange-nuanced . "#1f1a17")
    (bg-yellow-nuanced . "#1f1b17")
    (bg-green-nuanced . "#171f19")
    (bg-cyan-nuanced . "#171f1f")
    (bg-blue-nuanced . "#17191f")
    (bg-purple-nuanced . "#1b171f")
    (bg-magenta-nuanced . "#1f171b")

    ;; === Intense Backgrounds ===
    (bg-red-intense . "#2c1f1f")
    (bg-orange-intense . "#2c231f")
    (bg-yellow-intense . "#2c271f")
    (bg-green-intense . "#1f2c23")
    (bg-cyan-intense . "#1f2c2c")
    (bg-blue-intense . "#1f232c")
    (bg-purple-intense . "#271f2c")
    (bg-magenta-intense . "#2c1f27")

    ;; === Syntax Highlighting Semantic Mappings ===
    (keyword . cyan)               ; Control flow keywords - frost
    (builtin . purple)             ; Built-in functions - wisteria
    (string . green)               ; String literals - jade
    (docstring . green-faint)      ; Documentation
    (comment . fg-inactive)        ; Comments
    (constant . orange)            ; Constants - sienna
    (fnname . blue)                ; Function names - sky
    (variable . yellow)            ; Variables - gold
    (type . purple-warmer)         ; Type names - wisteria
    (preprocessor . orange-faint)  ; Preprocessor directives
    (rx-construct . purple)        ; Regex constructs
    (rx-backslash . pink-warmer)   ; Regex backslashes
    (number . orange-cooler)       ; Numbers
    (operator . cyan-cooler)       ; Operators
    (property . yellow-warmer)     ; Properties
    (punctuation . fg-dim)         ; Punctuation
    (bracket . fg-main)            ; Brackets
    (delimiter . fg-dim)           ; Delimiters
    (escape . red-faint)           ; Escape sequences

    ;; === UI Elements ===
    ;; Headers/Headings
    (fg-heading-0 . cyan-intense)
    (fg-heading-1 . fg-main)
    (fg-heading-2 . yellow)
    (fg-heading-3 . fg-alt)
    (fg-heading-4 . blue)
    (fg-heading-5 . green)
    (fg-heading-6 . purple)
    (fg-heading-7 . orange)
    (fg-heading-8 . fg-dim)

    ;; Mode line
    (modeline-bg-active . "#272727")
    (modeline-fg-active . "#ebebeb")
    (modeline-bg-active-accent . cyan)
    (modeline-fg-active-accent . "#1c1c1c")
    (modeline-bg-active-alt . "#222222")
    (modeline-bg-inactive . "#1f1f1f")
    (modeline-fg-inactive . "#606060")
    (modeline-border-active . "#323232")
    (modeline-border-inactive . "#222222")
    (modeline-err . red-intense)
    (modeline-error . red-intense)
    (modeline-warning . yellow-intense)
    (modeline-info . cyan-intense)
    (modeline-success . green-intense)

    ;; === Interactive Elements ===
    (bg-hover . "#272727")         ; Hover state
    (bg-hover-secondary . "#2a2a2a") ; Secondary hover
    (bg-hl-line . "#1f1f1f")       ; Current line highlight
    (bg-hl-line-intense . "#272727") ; Intense line highlight
    (bg-hl-line-accent . "#22222a") ; Accent line highlight
    (bg-hl-line-faint . "#1e1e1e") ; Faint line highlight
    (bg-region . "#2c2c37")        ; Selection
    (fg-region . unspecified)      ; Let foreground show through
    (bg-accent-subtle . "#22222a") ; Subtle accent background

    ;; === Completion & Search ===
    (bg-completion . "#272727")    ; Completion selection
    (bg-search-current . "#3c3c47") ; Current search match
    (bg-search-fail . "#37272a")   ; Failed search
    (bg-search-lazy . "#2a2a2a")   ; Lazy highlight
    (bg-match . "#273237")         ; General match
    (bg-search-replace . "#32273a") ; Replace highlight

    ;; Completion match levels
    (fg-completion-match-0 . cyan-intense)
    (fg-completion-match-1 . orange-intense)
    (fg-completion-match-2 . green-intense)
    (fg-completion-match-3 . purple-intense)
    (bg-completion-match-0 . bg-cyan-nuanced)
    (bg-completion-match-1 . bg-orange-nuanced)
    (bg-completion-match-2 . bg-green-nuanced)
    (bg-completion-match-3 . bg-purple-nuanced)

    ;; === Diffs & Version Control ===
    (bg-added . "#1f2c23")         ; Dark green
    (bg-added-faint . "#171f19")   ; Very dark green
    (bg-added-refine . "#273727")  ; Refined green
    (bg-added-fringe . "#87c792")  ; Fringe indicator
    (fg-added . "#87c792")         ; Light green
    (fg-added-intense . "#66d982") ; Intense green

    (bg-removed . "#2c1f1f")       ; Dark red
    (bg-removed-faint . "#1f1819") ; Very dark red
    (bg-removed-refine . "#372727") ; Refined red
    (bg-removed-fringe . "#e27878") ; Fringe indicator
    (fg-removed . "#e27878")       ; Light red
    (fg-removed-intense . "#ff6b6b") ; Intense red

    (bg-changed . "#2c271f")       ; Dark yellow
    (bg-changed-faint . "#1f1b17") ; Very dark yellow
    (bg-changed-refine . "#373227") ; Refined yellow
    (bg-changed-fringe . "#e7c787") ; Fringe indicator
    (fg-changed . "#e7c787")       ; Light yellow
    (fg-changed-intense . "#ffd866") ; Intense yellow
    (bg-changed-subtle . "#1f1b17") ; Subtle changed

    (bg-diff-context . "#161616")  ; Context lines

    ;; === Status Indicators ===
    (info . cyan)                  ; Information
    (success . green)              ; Success states
    (warning . yellow)             ; Warnings
    (error . red)                  ; Errors

    ;; === Special Purpose ===
    (bg-popup . "#1f1f1f")         ; Popup backgrounds
    (bg-tooltip . "#272727")       ; Tooltip background
    (bg-tab-bar . "#161616")       ; Tab bar background
    (bg-tab-current . "#1c1c1c")   ; Current tab
    (bg-tab-other . "#222222")     ; Other tabs
    (bg-space . "#161616")         ; Space background
    (fg-space . "#323232")         ; Space foreground
    (bg-space-error . "#2c1f1f")   ; Space error background
    (bg-pulse . "#272727")         ; Pulse highlight
    (bg-fill-column . "#161616")   ; Fill column indicator
    (bg-header . "#222222")        ; Header background
    (fg-header . "#f8f8f8")        ; Header foreground
    (bg-keybind . "#272727")       ; Keybind background

    ;; Mark selections (for dired, etc.)
    (bg-mark-select . "#272727")   ; Selected mark background
    (fg-mark-select . "#f8f8f8")   ; Selected mark foreground
    (bg-mark-delete . "#2c1f1f")   ; Delete mark background
    (fg-mark-delete . "#f8f8f8")   ; Delete mark foreground
    (bg-mark-other . "#2c271f")    ; Other mark background
    (fg-mark-other . "#f8f8f8")    ; Other mark foreground

    ;; Active argument (for eldoc)
    (bg-active-argument . "#2c271f") ; Yellow-tinted background
    (fg-active-argument . "#f8f8f8") ; Light foreground

    ;; Paren matching
    (bg-paren-match . "#2c2c37")   ; Matching paren
    (bg-paren-match-intense . "#373747") ; Intense matching paren
    (fg-paren-match . unspecified) ; Use existing foreground
    (bg-paren-expression . "#32273a") ; Expression highlight
    (bg-paren-mismatch . bg-red-intense) ; Mismatch
    (fg-paren-mismatch . "#f8f8f8") ; Light on red

    ;; === Links ===
    (fg-link . cyan)
    (fg-link-faint . cyan-faint)
    (fg-link-visited . purple-faint)
    (underline-link . unspecified)
    (underline-link-visited . unspecified)

    ;; === Code/Prose Elements ===
    (prose-code . cyan-faint)
    (prose-macro . purple-faint)
    (prose-verbatim . orange-faint)
    (prose-table . fg-main)
    (prose-tag . magenta-faint)
    (prose-todo . red)
    (prose-done . green)
    (prose-metadata . fg-dim)
    (prose-metadata-value . fg-alt)

    ;; Prose block delimiters
    (bg-prose-block-delimiter . bg-dim)
    (fg-prose-block-delimiter . fg-dim)
    (bg-prose-block-contents . bg-dim)

    ;; === Org Agenda ===
    (date-common . cyan)
    (date-deadline . red)
    (date-event . blue)
    (date-holiday . magenta)
    (date-now . green-intense)
    (date-scheduled . yellow)
    (date-weekday . fg-main)
    (date-weekend . orange-faint)
    (date-warning . yellow-intense)

    ;; === Mail/Messages ===
    (mail-cite-0 . blue-faint)
    (mail-cite-1 . yellow-faint)
    (mail-cite-2 . green-faint)
    (mail-cite-3 . red-faint)
    (mail-header-name . cyan-faint)
    (mail-recipient . blue)
    (mail-subject . magenta)
    (mail-other . fg-dim)
    (mail-part . orange-faint)

    ;; === Line Numbers ===
    (fg-line-number-active . fg-main)
    (bg-line-number-active . unspecified)
    (fg-line-number-inactive . fg-inactive)
    (bg-line-number-inactive . unspecified)

    ;; === Underlines ===
    (underline-err . red)
    (underline-error . red)          ; Alias for consistency
    (underline-warning . yellow)
    (underline-note . cyan)

    ;; === Buttons ===
    (fg-button-active . fg-main)
    (bg-button-active . bg-active)
    (fg-button-inactive . fg-dim)
    (bg-button-inactive . bg-inactive)

    ;; === Miscellaneous ===
    (cursor . cyan)                ; Frost cursor
    (prompt . cyan-intense)        ; Used by fg-prompt
    (fg-prompt . prompt)
    (hl-todo . red-intense)
    (keybind . cyan-intense)
    (name . purple)                ; Names (buffers, files, etc.)
    (identifier . cyan-faint)      ; Identifiers
    (border . "#323232")          ; General border color
    (fringe . bg-dim)             ; Fringe color
    (fringe-subtle . "#161616")   ; Subtle fringe
    (fringe-greyscale . "#222222") ; Greyscale fringe
    (fringe-accent . cyan-faint)  ; Accent fringe
    (shadow . "#00000090")        ; Shadow color (transparent black)
    (fg-whitespace . "#323232")   ; Whitespace indicators
    (fg-special-mild . cyan-faint) ; Special mild foreground

    ;; === Terminal Colors ===
    (bg-term-black . "#1c1c1c")
    (fg-term-black . "#161616")
    (bg-term-red . red)
    (fg-term-red . red)
    (bg-term-green . green)
    (fg-term-green . green)
    (bg-term-yellow . yellow)
    (fg-term-yellow . yellow)
    (bg-term-blue . blue)
    (fg-term-blue . blue)
    (bg-term-magenta . magenta)
    (fg-term-magenta . magenta)
    (bg-term-cyan . cyan)
    (fg-term-cyan . cyan)
    (bg-term-white . "#ebebeb")
    (fg-term-white . "#f3f3f3")

    ;; Bright terminal colors
    (bg-term-black-bright . "#222222")
    (fg-term-black-bright . "#222222")
    (bg-term-red-bright . red-intense)
    (fg-term-red-bright . red-intense)
    (bg-term-green-bright . green-intense)
    (fg-term-green-bright . green-intense)
    (bg-term-yellow-bright . yellow-intense)
    (fg-term-yellow-bright . yellow-intense)
    (bg-term-blue-bright . blue-intense)
    (fg-term-blue-bright . blue-intense)
    (bg-term-magenta-bright . magenta-intense)
    (fg-term-magenta-bright . magenta-intense)
    (bg-term-cyan-bright . cyan-intense)
    (fg-term-cyan-bright . cyan-intense)
    (bg-term-white-bright . "#f8f8f8")
    (fg-term-white-bright . "#f8f8f8"))
  "Ink & Frost Dark - A sophisticated palette inspired by Japanese sumi-e
ink paintings and winter frost. Deep charcoal backgrounds with subtle cool undertones
are balanced by cool frost accents and warm paper tones, creating a refined
coding environment that reduces eye strain while maintaining visual interest.")

(bv-themes-theme bv-dark bv-dark-palette)

(provide 'bv-dark-theme)
;;; bv-dark-theme.el ends here
