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
;; • Primary (cyan): Keywords, UI elements, links
;; • Secondary (orange): Constants, selections
;; • Tertiary (green): Strings, success states
;; • Quaternary (purple): Types, special syntax
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
    (fg-main . "#2a2d3a")          ; Deep ink blue-black
    (bg-dim . "#f4f4f7")           ; Light wash
    (fg-dim . "#4a4d5a")           ; Medium ink
    (bg-alt . "#eaeaed")           ; Alternative wash
    (fg-alt . "#1a1d2a")           ; Dark ink

    ;; === Active/Inactive States ===
    (bg-active . "#e0e0e5")        ; Active wash
    (fg-active . "#0a0d1a")        ; Very dark ink
    (bg-inactive . "#f0f0f3")      ; Inactive light
    (fg-inactive . "#6a6d7a")      ; Muted ink

    ;; === Complete Color Scales ===
    ;; Red scale (Vermillion ink)
    (red . "#d73a3a")              ; Deep vermillion
    (red-warmer . "#e54545")       ; Warm flame
    (red-cooler . "#c73030")       ; Cool crimson
    (red-faint . "#d06060")        ; Soft coral
    (red-intense . "#cc2222")      ; Intense ruby

    ;; Orange scale (Burnt sienna)
    (orange . "#d77815")           ; Burnt sienna
    (orange-warmer . "#e88520")    ; Warm amber
    (orange-cooler . "#c76d10")    ; Deep sienna
    (orange-faint . "#d49050")     ; Faded sienna
    (orange-intense . "#cc6600")   ; Bright amber

    ;; Yellow scale (Gold leaf)
    (yellow . "#bf9000")           ; Gold leaf
    (yellow-warmer . "#cfa000")    ; Bright gold
    (yellow-cooler . "#af8000")    ; Deep gold
    (yellow-faint . "#bfa040")     ; Faded gold
    (yellow-intense . "#aa7700")   ; Brilliant gold

    ;; Green scale (Jade & moss)
    (green . "#4a7c4e")            ; Jade green
    (green-warmer . "#5a8c5e")     ; Spring jade
    (green-cooler . "#3a6c3e")     ; Deep jade
    (green-faint . "#6a9c6e")      ; Pale jade
    (green-intense . "#2a6c2e")    ; Brilliant jade

    ;; Cyan scale (Frost & ice)
    (cyan . "#0087af")             ; Frost blue
    (cyan-warmer . "#0097bf")      ; Light frost
    (cyan-cooler . "#00779f")      ; Deep frost
    (cyan-faint . "#4097af")       ; Pale frost
    (cyan-intense . "#0077aa")     ; Brilliant ice

    ;; Blue scale (Winter sky)
    (blue . "#4a6cc3")             ; Winter sky
    (blue-warmer . "#5a7cd3")      ; Warm sky
    (blue-cooler . "#3a5cb3")      ; Deep sky
    (blue-faint . "#6a8cc3")       ; Pale sky
    (blue-intense . "#2a4cb3")     ; Electric sky

    ;; Purple scale (Wisteria)
    (purple . "#8959a8")           ; Wisteria purple
    (purple-warmer . "#9969b8")    ; Warm wisteria
    (purple-cooler . "#794998")    ; Deep wisteria
    (purple-faint . "#9979b8")     ; Pale wisteria
    (purple-intense . "#6939a8")   ; Vivid wisteria

    ;; Magenta scale (Plum blossom)
    (magenta . "#c54b8c")          ; Plum blossom
    (magenta-warmer . "#d55b9c")   ; Warm plum
    (magenta-cooler . "#b53b7c")   ; Deep plum
    (magenta-faint . "#c56b9c")    ; Pale plum
    (magenta-intense . "#b52b7c")  ; Bright plum

    ;; Pink scale (Cherry blossom)
    (pink . "#d5669c")             ; Cherry blossom
    (pink-warmer . "#e576ac")      ; Warm sakura
    (pink-cooler . "#c5568c")      ; Cool sakura
    (pink-faint . "#d586ac")       ; Pale sakura
    (pink-intense . "#c5468c")     ; Bright sakura

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
    (bg-red-nuanced . "#fef5f5")
    (bg-orange-nuanced . "#fef8f0")
    (bg-yellow-nuanced . "#fefbf0")
    (bg-green-nuanced . "#f5fef5")
    (bg-cyan-nuanced . "#f0fefe")
    (bg-blue-nuanced . "#f5f5fe")
    (bg-purple-nuanced . "#fef5fe")
    (bg-magenta-nuanced . "#fef5f8")

    ;; === Intense Backgrounds ===
    (bg-red-intense . "#fde0e0")
    (bg-orange-intense . "#fde8d8")
    (bg-yellow-intense . "#fdf0d8")
    (bg-green-intense . "#e5f5e5")
    (bg-cyan-intense . "#d8f5f5")
    (bg-blue-intense . "#e0e8fd")
    (bg-purple-intense . "#f0e0fd")
    (bg-magenta-intense . "#fde0f0")

    ;; === Syntax Highlighting Semantic Mappings ===
    (keyword . cyan-intense)       ; Control flow keywords - frost
    (builtin . purple)             ; Built-in functions - wisteria
    (string . green)               ; String literals - jade
    (docstring . green-faint)      ; Documentation
    (comment . fg-inactive)        ; Comments
    (constant . orange)            ; Constants - sienna
    (fnname . blue)                ; Function names - sky
    (variable . yellow-intense)    ; Variables - gold
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
    (fg-heading-2 . yellow-intense)
    (fg-heading-3 . fg-alt)
    (fg-heading-4 . blue-intense)
    (fg-heading-5 . green-intense)
    (fg-heading-6 . purple-intense)
    (fg-heading-7 . orange-intense)
    (fg-heading-8 . fg-dim)

    ;; Mode line
    (modeline-bg-active . "#e5e5ea")
    (modeline-fg-active . "#1a1d2a")
    (modeline-bg-active-accent . cyan)
    (modeline-fg-active-accent . "#fbfbfd")
    (modeline-bg-active-alt . "#eaeaed")
    (modeline-bg-inactive . "#f0f0f3")
    (modeline-fg-inactive . "#6a6d7a")
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
    (bg-hl-line . "#f4f4f7")       ; Current line highlight
    (bg-hl-line-intense . "#eaeaed") ; Intense line highlight
    (bg-hl-line-accent . "#e8f0f8") ; Accent line highlight
    (bg-hl-line-faint . "#f8f8fa") ; Faint line highlight
    (bg-region . "#d8e0e8")        ; Selection
    (fg-region . unspecified)      ; Let foreground show through
    (bg-accent-subtle . "#e8f0f8") ; Subtle accent background

    ;; === Completion & Search ===
    (bg-completion . "#e8f0f8")    ; Completion selection
    (bg-search-current . "#fdf0d8") ; Current search match
    (bg-search-fail . "#fde0e0")   ; Failed search
    (bg-search-lazy . "#e8e8eb")   ; Lazy highlight
    (bg-match . "#d8f5f5")         ; General match
    (bg-search-replace . "#f0e0fd") ; Replace highlight

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
    (bg-added . "#e5f5e5")         ; Light green
    (bg-added-faint . "#f0faf0")   ; Very light green
    (bg-added-refine . "#d0e8d0")  ; Refined green
    (bg-added-fringe . "#4a7c4e")  ; Fringe indicator
    (fg-added . "#2a5a2e")         ; Dark green
    (fg-added-intense . "#1a4a1e") ; Very dark green

    (bg-removed . "#fde0e0")       ; Light red
    (bg-removed-faint . "#fef5f5") ; Very light red
    (bg-removed-refine . "#f0d0d0") ; Refined red
    (bg-removed-fringe . "#d73a3a") ; Fringe indicator
    (fg-removed . "#7a2a2a")       ; Dark red
    (fg-removed-intense . "#6a1a1a") ; Very dark red

    (bg-changed . "#fdf0d8")       ; Light yellow
    (bg-changed-faint . "#fefbf0") ; Very light yellow
    (bg-changed-refine . "#f0e5c8") ; Refined yellow
    (bg-changed-fringe . "#bf9000") ; Fringe indicator
    (fg-changed . "#6a5a00")       ; Dark yellow/brown
    (fg-changed-intense . "#5a4a00") ; Very dark brown
    (bg-changed-subtle . "#fefbf0") ; Subtle changed

    (bg-diff-context . "#fbfbfd")  ; Context lines

    ;; === Status Indicators ===
    (info . cyan)                  ; Information
    (success . green)              ; Success states
    (warning . yellow)             ; Warnings
    (error . red)                  ; Errors

    ;; === Special Purpose ===
    (bg-popup . "#f4f4f7")         ; Popup backgrounds
    (bg-tooltip . "#fbfbfd")       ; Tooltip background
    (bg-tab-bar . "#eaeaed")       ; Tab bar background
    (bg-tab-current . "#fbfbfd")   ; Current tab
    (bg-tab-other . "#f4f4f7")     ; Other tabs
    (bg-space . "#fbfbfd")         ; Space background
    (fg-space . "#e0e0e5")         ; Space foreground
    (bg-space-error . "#fde0e0")   ; Space error background
    (bg-pulse . "#e8f0f8")         ; Pulse highlight
    (bg-fill-column . "#f4f4f7")   ; Fill column indicator
    (bg-header . "#eaeaed")        ; Header background
    (fg-header . "#0a0d1a")        ; Header foreground
    (bg-keybind . "#e8f0f8")       ; Keybind background

    ;; Mark selections (for dired, etc.)
    (bg-mark-select . "#e8f0f8")   ; Selected mark background
    (fg-mark-select . "#0a0d1a")   ; Selected mark foreground
    (bg-mark-delete . "#fde0e0")   ; Delete mark background
    (fg-mark-delete . "#0a0d1a")   ; Delete mark foreground
    (bg-mark-other . "#fdf0d8")    ; Other mark background
    (fg-mark-other . "#0a0d1a")    ; Other mark foreground

    ;; Active argument (for eldoc)
    (bg-active-argument . "#fdf0d8") ; Yellow-tinted background
    (fg-active-argument . "#0a0d1a") ; Dark foreground

    ;; Paren matching
    (bg-paren-match . "#d8e0e8")   ; Matching paren
    (bg-paren-match-intense . "#c8d0d8") ; Intense matching paren
    (fg-paren-match . unspecified) ; Use existing foreground
    (bg-paren-expression . "#f0e0fd") ; Expression highlight
    (bg-paren-mismatch . bg-red-intense) ; Mismatch
    (fg-paren-mismatch . "#0a0d1a") ; Dark on red

    ;; === Links ===
    (fg-link . cyan-intense)
    (fg-link-faint . cyan)
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
    (cursor . cyan-intense)        ; Frost cursor
    (prompt . cyan-intense)        ; Used by fg-prompt
    (fg-prompt . prompt)
    (hl-todo . red-intense)
    (keybind . cyan-intense)
    (name . purple)                ; Names (buffers, files, etc.)
    (identifier . cyan-warmer)     ; Identifiers
    (border . "#d0d0d5")          ; General border color
    (fringe . bg-dim)             ; Fringe color
    (fringe-subtle . "#fbfbfd")   ; Subtle fringe
    (fringe-greyscale . "#e0e0e5") ; Greyscale fringe
    (fringe-accent . cyan-faint)  ; Accent fringe
    (shadow . "#2a2d3a30")        ; Shadow color (transparent dark)
    (fg-whitespace . "#e0e0e5")   ; Whitespace indicators
    (fg-special-mild . cyan-faint) ; Special mild foreground

    ;; === Terminal Colors ===
    (bg-term-black . "#2a2d3a")
    (fg-term-black . "#2a2d3a")
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
    (bg-term-white . "#e0e0e5")
    (fg-term-white . "#e0e0e5")

    ;; Bright terminal colors
    (bg-term-black-bright . "#4a4d5a")
    (fg-term-black-bright . "#4a4d5a")
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
