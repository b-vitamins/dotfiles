;;; bv-light-theme.el --- Light theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Ink & Frost Light Theme
;; =======================
;;
;; A sophisticated light theme inspired by Japanese sumi-e ink on washi paper.
;; This theme creates a calm, focused environment with pure paper whites and
;; carefully chosen ink colors that provide visual hierarchy without overwhelming
;; the senses.
;;
;; Design Philosophy:
;; -----------------
;; • Pure paper whites (#fbfbfd) minimize eye strain in bright environments
;; • Deep ink colors provide excellent contrast and readability
;; • Cool primary accents (frost blue) create a calm atmosphere
;; • Warm secondary tones add visual interest without distraction
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
;; (load-theme 'bv-light t)
;;
;; The theme is designed to work seamlessly with the bv-themes engine,
;; providing consistent styling across all major Emacs modes.

;;; Code:

(require 'bv-themes)

(defconst bv-light-palette
  '(;; === Foundation Colors ===
    ;; Pure paper whites with subtle warmth, inspired by washi paper
    (bg-main . "#fbfbfd")          ; Pure paper white
    (fg-main . "#383a42")          ; Deep ink
    (bg-dim . "#f3f4f6")           ; Light wash
    (fg-dim . "#6c6f77")           ; Muted ink
    (bg-alt . "#eaeaed")           ; Alternative wash
    (fg-alt . "#282c34")           ; Dark ink

    ;; === Active/Inactive States ===
    (bg-active . "#e0e0e5")        ; Active wash
    (fg-active . "#282c34")        ; Dark ink
    (bg-inactive . "#f0f0f3")      ; Inactive light
    (fg-inactive . "#8a8d96")      ; Muted ink

    ;; === Complete Color Scales ===
    ;; Red scale (Vermillion ink)
    (red . "#e45649")              ; Vermillion
    (red-warmer . "#e44958")       ; Warm vermillion
    (red-cooler . "#e47249")       ; Cool vermillion
    (red-faint . "#d3938d")        ; Faded ink red
    (red-intense . "#f23726")      ; Intense vermillion

    ;; Orange scale (Burnt sienna)
    (orange . "#da8548")           ; Burnt sienna
    (orange-warmer . "#da6b48")    ; Warm sienna
    (orange-cooler . "#da9f48")    ; Cool sienna
    (orange-faint . "#cca589")     ; Faded sienna
    (orange-intense . "#e77727")   ; Bright sienna

    ;; Yellow scale (Gold leaf)
    (yellow . "#c18401")           ; Gold leaf
    (yellow-warmer . "#c16101")    ; Warm gold
    (yellow-cooler . "#c1a707")    ; Cool gold
    (yellow-faint . "#c39532")     ; Faded gold
    (yellow-intense . "#ae7600")   ; Intense gold

    ;; Green scale (Jade & moss)
    (green . "#50a14f")            ; Jade green
    (green-warmer . "#5fa14f")     ; Warm jade
    (green-cooler . "#4fa15d")     ; Cool jade
    (green-faint . "#7ca87b")      ; Pale jade
    (green-intense . "#429b41")    ; Intense jade

    ;; Cyan scale (Frost & ice)
    (cyan . "#0184bc")             ; Frost blue
    (cyan-warmer . "#01a6bc")      ; Warm frost
    (cyan-cooler . "#0162bc")      ; Cool frost
    (cyan-faint . "#3195bf")       ; Pale frost
    (cyan-intense . "#0076a9")     ; Intense ice

    ;; Blue scale (Winter sky)
    (blue . "#4078f2")             ; Winter sky
    (blue-warmer . "#4098f2")      ; Warm sky
    (blue-cooler . "#4058f2")      ; Cool sky
    (blue-faint . "#8aa4db")       ; Pale sky
    (blue-intense . "#1f65ff")     ; Electric sky

    ;; Purple scale (Wisteria)
    (purple . "#a626a4")           ; Wisteria
    (purple-warmer . "#9126a6")    ; Warm wisteria
    (purple-cooler . "#a6268d")    ; Cool wisteria
    (purple-faint . "#b050ae")     ; Pale wisteria
    (purple-intense . "#a1179f")   ; Intense wisteria

    ;; Magenta scale (Plum blossom)
    (magenta . "#b751b6")          ; Plum blossom
    (magenta-warmer . "#a651b7")   ; Warm plum
    (magenta-cooler . "#b751a4")   ; Cool plum
    (magenta-faint . "#b685b5")    ; Pale plum
    (magenta-intense . "#b63db5")  ; Bright plum

    ;; Pink scale (Cherry blossom)
    (pink . "#ca1243")             ; Cherry blossom
    (pink-warmer . "#ca1264")      ; Warm sakura
    (pink-cooler . "#ca1222")      ; Cool sakura
    (pink-faint . "#c34c6b")       ; Pale sakura
    (pink-intense . "#c80035")     ; Bright sakura

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
	    (string . green)               ; String literals
	    (docstring . green-faint)      ; Documentation
	    (docmarkup . green-warmer)     ; Documentation markup
	    (comment . fg-inactive)        ; Comments
	    (constant . cyan)              ; Constants
	    (fnname . blue)                ; Function names
	    (function-call . blue)         ; Function calls
	    (variable . red)               ; Variable definitions
	    (variable-use . fg-main)       ; Variable references
	    (type . yellow)                ; Type names
	    (preprocessor . orange)        ; Preprocessor directives
	    (rx-construct . purple)        ; Regex constructs
	    (rx-backslash . pink-warmer)   ; Regex backslashes
	    (number . yellow)              ; Numbers
	    (operator . fg-dim)            ; Operators
	    (property . fg-main)           ; Properties
	    (property-use . fg-main)       ; Property references
	    (punctuation . fg-dim)         ; Punctuation
	    (bracket . fg-main)            ; Brackets
	    (delimiter . fg-dim)           ; Delimiters
	    (escape . red-faint)           ; Escape sequences
	    (regexp . purple)              ; Regular expressions

	    ;; === UI Elements ===
	    ;; Headers/Headings
	    (fg-heading-0 . blue-intense)
	    (fg-heading-1 . fg-main)
	    (fg-heading-2 . yellow-intense)
	    (fg-heading-3 . fg-alt)
	    (fg-heading-4 . blue-intense)
    (fg-heading-5 . green-intense)
    (fg-heading-6 . purple-intense)
    (fg-heading-7 . orange-intense)
    (fg-heading-8 . fg-dim)

	    ;; Mode line
	    (modeline-bg-active . "#e5e5ea")
	    (modeline-fg-active . fg-alt)
	    (modeline-bg-active-accent . accent-0)
	    (modeline-fg-active-accent . "#fbfbfd")
	    (modeline-bg-active-alt . "#eaeaed")
	    (modeline-bg-inactive . "#f0f0f3")
	    (modeline-fg-inactive . fg-inactive)
	    (modeline-border-active . "#d0d0d5")
	    (modeline-border-inactive . "#e0e0e5")
	    (modeline-err . red)
	    (modeline-error . red)
    (modeline-warning . yellow)
    (modeline-info . cyan)
    (modeline-success . green)

	    ;; === Interactive Elements ===
	    (bg-hover . "#f0f0f3")         ; Hover state
	    (bg-hover-secondary . "#e8e8eb") ; Secondary hover
	    (bg-hl-line-faint . bg-dim)    ; Faint line highlight
	    (fg-region . unspecified)      ; Let foreground show through

	    ;; === Completion & Search ===
	    (bg-search-current . bg-yellow-intense) ; Current search match
	    (bg-search-fail . bg-red-intense) ; Failed search
	    (bg-search-lazy . bg-dim)      ; Lazy highlight
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
	    (bg-added-faint . bg-green-nuanced) ; Very light green
	    (bg-added-refine . "#d5e7d7")  ; Refined green
	    (bg-added-fringe . green)      ; Fringe indicator
	    (fg-added . green)             ; Added foreground
	    (fg-added-intense . green-intense) ; Intense foreground

	    (bg-removed . bg-red-intense)  ; Removed lines
	    (bg-removed-faint . bg-red-nuanced) ; Very light red
	    (bg-removed-refine . "#f6d7d5") ; Refined red
	    (bg-removed-fringe . red)      ; Fringe indicator
	    (fg-removed . red)             ; Removed foreground
	    (fg-removed-intense . red-intense) ; Intense foreground

	    (bg-changed . bg-yellow-intense) ; Changed lines
	    (bg-changed-faint . bg-yellow-nuanced) ; Very light yellow
	    (bg-changed-refine . "#eee1c6") ; Refined yellow
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
	    (bg-popup . bg-dim)            ; Popup backgrounds
	    (bg-tooltip . "#fbfbfd")       ; Tooltip background
	    (bg-tab-bar . "#eaeaed")       ; Tab bar background
	    (bg-tab-current . "#fbfbfd")   ; Current tab
	    (bg-tab-other . bg-dim)        ; Other tabs
	    (bg-space . "#fbfbfd")         ; Space background
	    (fg-space . "#e0e0e5")         ; Space foreground
	    (bg-space-error . bg-red-intense) ; Space error background
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
	    (fg-active-argument . fg-alt)  ; Dark foreground

	    ;; Paren matching
	    (bg-paren-match . bg-cyan-intense) ; Matching paren
	    (bg-paren-match-intense . "#c4e1ef") ; Intense matching paren
	    (fg-paren-match . unspecified) ; Use existing foreground
	    (bg-paren-expression . bg-purple-intense) ; Expression highlight
	    (bg-paren-mismatch . bg-red-intense) ; Mismatch
	    (fg-paren-mismatch . fg-alt)   ; Dark on red

	    ;; === Links ===
	    (fg-link . accent-0)
	    (fg-link-faint . blue-faint)
	    (fg-link-visited . purple)
	    (underline-link . unspecified)
	    (underline-link-visited . unspecified)

    ;; === Code/Prose Elements ===
    (prose-code . magenta-warmer)
    (prose-macro . purple)
    (prose-verbatim . cyan-warmer)
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
    (date-common . blue)
    (date-deadline . red)
    (date-event . purple)
    (date-holiday . magenta)
    (date-now . green-intense)
    (date-scheduled . yellow-warmer)
    (date-weekday . fg-main)
    (date-weekend . red-faint)
    (date-warning . orange)

    ;; === Mail/Messages ===
    (mail-cite-0 . blue-warmer)
    (mail-cite-1 . orange-warmer)
    (mail-cite-2 . green-warmer)
    (mail-cite-3 . purple-warmer)
    (mail-header-name . blue-faint)
    (mail-recipient . blue-intense)
    (mail-subject . red-warmer)
    (mail-other . fg-dim)
    (mail-part . orange)

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
	    (identifier . cyan-warmer)     ; Identifiers
	    (border . "#d0d0d5")          ; General border color
		    (fringe . bg-main)            ; Fringe color (no separator line)
		    (fringe-subtle . "#fbfbfd")   ; Subtle fringe
		    (fringe-greyscale . "#e0e0e5") ; Greyscale fringe
		    (fringe-accent . blue-faint)  ; Accent fringe
	    (shadow . "#282c3430")        ; Shadow color (transparent dark)
	    (fg-whitespace . "#e0e0e5")   ; Whitespace indicators
	    (fg-special-mild . cyan-faint) ; Special mild foreground

	    ;; === Terminal Colors ===
	    (bg-term-black . fg-alt)
	    (fg-term-black . fg-alt)
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
	    (bg-term-white . "#e5e5ea")
	    (fg-term-white . "#e5e5ea")

	    ;; Bright terminal colors
	    (bg-term-black-bright . fg-dim)
	    (fg-term-black-bright . fg-dim)
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
    (bg-term-white-bright . "#fbfbfd")
    (fg-term-white-bright . "#fbfbfd"))
  "Ink & Frost Light - A sophisticated palette inspired by Japanese sumi-e
ink on washi paper. Pure paper whites with subtle blue-gray undertones
provide a calm base, while carefully chosen ink colors create visual
hierarchy without overwhelming the senses. Perfect for extended coding
sessions in bright environments.")

(bv-themes-theme bv-light bv-light-palette)

(provide 'bv-light-theme)
;;; bv-light-theme.el ends here
