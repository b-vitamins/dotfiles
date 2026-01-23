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
;; • Rich charcoal backgrounds (#24262b) provide a refined foundation
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
;; • Primary (blue): Functions, UI elements, links
;; • Secondary (purple): Keywords, types, special syntax
;; • Tertiary (green): Strings, success states
;; • Quaternary (orange): Constants, numbers, selections
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
    ;; Cinematic charcoal inspired by Claude
    (bg-main . "#24262b")          ; Rich charcoal-black (cool undertone)
    (fg-main . "#c0c8d2")          ; Crisp paper-gray
    (bg-dim . "#1f2126")           ; Deeper charcoal
    (fg-dim . "#9aa2ad")           ; Muted gray-blue
    (bg-alt . "#2b2e34")           ; Alt surface
    (fg-alt . "#d0d6e0")           ; Bright foreground

    ;; === Active/Inactive States ===
    (bg-active . "#343740")        ; Active surface
    (fg-active . "#e0e5ef")        ; Active foreground
    (bg-inactive . "#212329")      ; Inactive surface
    (fg-inactive . "#858b95")      ; Inactive foreground

    ;; === Complete Color Scales ===
    ;; Red scale (Vermillion)
    (red . "#e06c75")              ; Vermillion
    (red-warmer . "#e06c8b")       ; Warm vermillion
    (red-cooler . "#e06c5f")       ; Cool vermillion
    (red-faint . "#be737a")        ; Faded ink red
    (red-intense . "#ff5f6b")      ; Intense vermillion

    ;; Orange scale (Sienna)
    (orange . "#997451")           ; Warm tan
    (orange-warmer . "#9d7753")    ; Warm tan (warmer)
    (orange-cooler . "#8a6a4b")    ; Warm tan (cooler)
    (orange-faint . "#7f6044")     ; Faded tan
    (orange-intense . "#b98a5c")   ; Intense tan

    ;; Yellow scale (Gold leaf)
    (yellow . "#c8b07a")           ; Gold leaf
    (yellow-warmer . "#c4a870")    ; Warm gold
    (yellow-cooler . "#d0b887")    ; Cool gold
    (yellow-faint . "#a89a78")     ; Faded gold
    (yellow-intense . "#d1a850")   ; Brilliant gold

    ;; Green scale (Jade & moss)
    (green . "#73905e")            ; Moss green
    (green-warmer . "#789761")     ; Warm moss
    (green-cooler . "#6a8357")     ; Cool moss
    (green-faint . "#637a52")      ; Faded moss
    (green-intense . "#98c078")    ; Lush green

    ;; Cyan scale (Ice)
    (cyan . "#4b8b9a")             ; Muted teal
    (cyan-warmer . "#4f93a6")      ; Warm teal
    (cyan-cooler . "#4a7f96")      ; Cool teal
    (cyan-faint . "#5c7f88")       ; Pale teal
    (cyan-intense . "#52aebb")     ; Intense teal

    ;; Blue scale (Winter sky)
    (blue . "#528ab7")             ; Azure
    (blue-warmer . "#4f83ae")      ; Soft azure
    (blue-cooler . "#53678c")      ; Steel blue
    (blue-faint . "#3b4751")       ; Blue-gray
    (blue-intense . "#5494c7")     ; Sky blue

    ;; Purple scale (Wisteria)
    (purple . "#915ea0")           ; Muted wisteria
    (purple-warmer . "#9961a9")    ; Warm wisteria
    (purple-cooler . "#885996")    ; Cool wisteria
    (purple-faint . "#6f4a7d")     ; Pale wisteria
    (purple-intense . "#a06bb0")   ; Intense wisteria

    ;; Magenta scale (Plum blossom)
    (magenta . "#a05a7f")          ; Plum
    (magenta-warmer . "#a05a8f")   ; Warm plum
    (magenta-cooler . "#a05a6f")   ; Cool plum
    (magenta-faint . "#8a6a7a")    ; Pale plum
    (magenta-intense . "#b06b90")  ; Bright plum

    ;; Pink scale (Cherry blossom)
    (pink . "#c07aa5")             ; Cherry blossom
    (pink-warmer . "#c88ab0")      ; Warm sakura
    (pink-cooler . "#b96a9a")      ; Cool sakura
    (pink-faint . "#a88a9a")       ; Pale sakura
    (pink-intense . "#d08ab8")     ; Bright sakura

    ;; === Accent Mappings (semantic) ===
    (accent-0 . blue)              ; Primary accent - sky
    (accent-1 . purple)            ; Secondary accent - wisteria
    (accent-2 . green)             ; Tertiary accent - jade
    (accent-3 . orange)            ; Quaternary accent - sienna

    ;; Accent intensity variants
    (accent-0-intense . blue-intense)
    (accent-1-intense . purple-intense)
    (accent-2-intense . green-intense)
    (accent-3-intense . orange-intense)
    (accent-0-faint . blue-faint)
    (accent-1-faint . purple-faint)
    (accent-2-faint . green-faint)
    (accent-3-faint . orange-faint)

	    ;; === Syntax Highlighting Semantic Mappings ===
	    (keyword . purple)             ; Control flow keywords
	    (builtin . cyan)               ; Built-in functions
	    (string . green-intense)       ; String literals
	    (docstring . green)            ; Documentation
	    (docmarkup . green-warmer)     ; Documentation markup
	    (comment . fg-inactive)        ; Comments
	    (constant . cyan)              ; Constants
	    (fnname . blue-intense)        ; Function names
	    (function-call . blue)         ; Function calls
	    (variable . red)               ; Variable definitions
	    (variable-use . fg-main)       ; Variable references
	    (type . orange)               ; Type names
	    (preprocessor . orange-intense) ; Preprocessor directives
	    (rx-construct . purple-intense) ; Regex constructs
	    (rx-backslash . pink-intense)  ; Regex backslashes
	    (number . orange)              ; Numbers
	    (operator . fg-main)           ; Operators
	    (property . fg-main)           ; Properties
	    (property-use . fg-main)       ; Property references
	    (punctuation . fg-dim)         ; Punctuation
	    (bracket . fg-main)            ; Brackets
	    (delimiter . fg-dim)           ; Delimiters
	    (escape . red-intense)         ; Escape sequences
	    (regexp . purple-intense)      ; Regular expressions

	    ;; === UI Elements ===
	    ;; Headers/Headings
	    (fg-heading-0 . blue-intense)
	    (fg-heading-1 . fg-main)
	    (fg-heading-2 . yellow)
	    (fg-heading-3 . fg-alt)
	    (fg-heading-4 . blue)
	    (fg-heading-5 . green)
	    (fg-heading-6 . purple)
    (fg-heading-7 . orange)
    (fg-heading-8 . fg-dim)

	    ;; Mode line
	    (modeline-bg-active . bg-active)
	    (modeline-fg-active . fg-active)
	    (modeline-bg-active-accent . accent-0)
	    (modeline-fg-active-accent . bg-main)
	    (modeline-bg-active-alt . bg-alt)
	    (modeline-bg-inactive . bg-inactive)
	    (modeline-fg-inactive . fg-inactive)
	    (modeline-border-active . border)
	    (modeline-border-inactive . bg-dim)
	    (modeline-err . red)
	    (modeline-error . red)
	    (modeline-warning . yellow)
	    (modeline-info . cyan)
	    (modeline-success . green)

	    ;; === Interactive Elements ===
	    (bg-hover . bg-alt)            ; Hover state
	    (bg-hover-secondary . bg-active) ; Secondary hover
	    (bg-hl-line-faint . bg-inactive) ; Faint line highlight
	    (fg-region . unspecified)      ; Let foreground show through

	    ;; === Completion & Search ===
	    (bg-search-current . bg-yellow-intense) ; Current search match
	    (bg-search-fail . bg-red-intense) ; Failed search
	    (bg-search-lazy . bg-alt)      ; Lazy highlight
	    (bg-match . bg-cyan-intense)   ; General match
	    (bg-search-replace . bg-purple-intense) ; Replace highlight

    ;; Search regexp groups
    (bg-search-rx-group-0 . bg-magenta-nuanced)
    (bg-search-rx-group-1 . bg-cyan-nuanced)
    (bg-search-rx-group-2 . bg-yellow-nuanced)
    (bg-search-rx-group-3 . bg-green-nuanced)

	    ;; Completion match levels
	    (fg-completion-match-0 . blue-intense)
	    (fg-completion-match-1 . purple-intense)
	    (fg-completion-match-2 . green-intense)
	    (fg-completion-match-3 . orange-intense)
	    (bg-completion-match-0 . bg-blue-nuanced)
	    (bg-completion-match-1 . bg-purple-nuanced)
	    (bg-completion-match-2 . bg-green-nuanced)
	    (bg-completion-match-3 . bg-orange-nuanced)

	    ;; === Diffs & Version Control ===
	    (bg-added . bg-green-intense)  ; Added lines
	    (bg-added-faint . bg-green-nuanced) ; Very dark green
	    (bg-added-refine . "#515f46")  ; Refined green
	    (bg-added-fringe . green)      ; Fringe indicator
	    (fg-added . green)             ; Added foreground
	    (fg-added-intense . green-intense) ; Intense foreground

	    (bg-removed . bg-red-intense)  ; Removed lines
	    (bg-removed-faint . bg-red-nuanced) ; Very dark red
	    (bg-removed-refine . "#684345") ; Refined red
	    (bg-removed-fringe . red)      ; Fringe indicator
	    (fg-removed . red)             ; Removed foreground
	    (fg-removed-intense . red-intense) ; Intense foreground

	    (bg-changed . bg-yellow-intense) ; Changed lines
	    (bg-changed-faint . bg-yellow-nuanced) ; Very dark yellow
	    (bg-changed-refine . "#6a5e47") ; Refined yellow
	    (bg-changed-fringe . yellow)    ; Fringe indicator
	    (fg-changed . yellow)           ; Changed foreground
	    (fg-changed-intense . yellow-intense) ; Intense foreground
	    (bg-changed-subtle . bg-yellow-nuanced) ; Subtle changed

	    (bg-diff-context . bg-main)    ; Context lines

    ;; === Status Indicators ===
    (info . cyan)                  ; Information
    (success . green)              ; Success states
    (warning . yellow)             ; Warnings
    (error . red)                  ; Errors

    ;; Prominent status backgrounds
    (bg-prominent-err . bg-red-intense)
    (fg-prominent-err . fg-main)
    (bg-prominent-note . bg-cyan-intense)
    (fg-prominent-note . fg-main)
    (bg-prominent-warning . bg-yellow-intense)
    (fg-prominent-warning . fg-main)

	    ;; === Special Purpose ===
	    (bg-popup . bg-alt)            ; Popup backgrounds
	    (bg-tooltip . bg-active)       ; Tooltip background
	    (bg-tab-bar . bg-dim)          ; Tab bar background
	    (bg-tab-current . bg-main)     ; Current tab
	    (bg-tab-other . bg-alt)        ; Other tabs
	    (bg-space . bg-dim)            ; Space background
	    (fg-space . border)            ; Space foreground
	    (bg-space-error . bg-red-nuanced) ; Space error background
	    (bg-pulse . bg-yellow-intense) ; Pulse highlight
	    (bg-fill-column . bg-dim)      ; Fill column indicator
	    (bg-header . bg-main)          ; Header background (match window/bg)
	    (fg-header . fg-alt)           ; Header foreground
	    (bg-keybind . bg-blue-nuanced) ; Keybind background

	    ;; Mark selections (for dired, etc.)
	    (bg-mark-select . bg-blue-nuanced) ; Selected mark background
	    (fg-mark-select . fg-alt)      ; Selected mark foreground
	    (bg-mark-delete . bg-red-nuanced) ; Delete mark background
	    (fg-mark-delete . fg-alt)      ; Delete mark foreground
	    (bg-mark-other . bg-yellow-nuanced) ; Other mark background
	    (fg-mark-other . fg-alt)       ; Other mark foreground

	    ;; Active argument (for eldoc)
	    (bg-active-argument . bg-yellow-nuanced) ; Yellow-tinted background
	    (fg-active-argument . fg-alt)  ; Foreground

	    ;; Paren matching
	    (bg-paren-match . bg-cyan-nuanced) ; Matching paren
	    (bg-paren-match-intense . bg-cyan-intense) ; Intense matching paren
	    (fg-paren-match . unspecified) ; Use existing foreground
	    (bg-paren-expression . bg-purple-nuanced) ; Expression highlight
	    (bg-paren-mismatch . bg-red-intense) ; Mismatch
	    (fg-paren-mismatch . fg-alt)   ; Light on red

	    ;; === Links ===
	    (fg-link . accent-0)
	    (fg-link-faint . blue-faint)
	    (fg-link-visited . purple)
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
		    (cursor . accent-0)            ; Cursor
		    (prompt . accent-3-intense)    ; Used by fg-prompt (warm)
		    (fg-prompt . prompt)
		    (hl-todo . red-intense)
		    (keybind . accent-0)
	    (name . purple)                ; Names (buffers, files, etc.)
	    (identifier . cyan-faint)      ; Identifiers
	    (border . "#31343c")          ; General border color
		    (fringe . bg-main)            ; Fringe color (no separator line)
		    (fringe-subtle . bg-dim)      ; Subtle fringe
		    (fringe-greyscale . bg-alt)   ; Greyscale fringe
		    (fringe-accent . blue-faint)  ; Accent fringe
	    (shadow . "#00000090")        ; Shadow color (transparent black)
	    (fg-whitespace . border)      ; Whitespace indicators
	    (fg-special-mild . cyan-faint) ; Special mild foreground

	    ;; === Terminal Colors ===
	    (bg-term-black . bg-alt)
	    (fg-term-black . bg-alt)
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
	    (bg-term-white . fg-alt)
	    (fg-term-white . fg-alt)

	    ;; Bright terminal colors
	    (bg-term-black-bright . border)
	    (fg-term-black-bright . border)
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
	    (bg-term-white-bright . fg-active)
	    (fg-term-white-bright . fg-active))
  "Ink & Frost Dark - A sophisticated palette inspired by Japanese sumi-e
ink paintings and winter frost. Deep charcoal backgrounds with subtle cool undertones
are balanced by cool frost accents and warm paper tones, creating a refined
coding environment that reduces eye strain while maintaining visual interest.")

(bv-themes-theme bv-dark bv-dark-palette)

(provide 'bv-dark-theme)
;;; bv-dark-theme.el ends here
