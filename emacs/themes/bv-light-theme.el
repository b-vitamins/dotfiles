;;; bv-light-theme.el --- Light theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Light variant theme with a modern Nord-inspired aesthetic.
;; Features a sophisticated color system with warmer/cooler/faint/intense variants.

;;; Code:

(require 'bv-themes)

(defconst bv-light-palette
  '(;; === Foundation Colors ===
    ;; Main backgrounds and foregrounds (Nord Snow Storm)
    (bg-main . "#eceff4")          ; Nord6 - Snow Storm
    (fg-main . "#2e3440")          ; Nord0 - Polar Night
    (bg-dim . "#e5e9f0")           ; Nord5 - Lighter Snow Storm
    (fg-dim . "#4c566a")           ; Nord3 - Polar Night
    (bg-alt . "#d8dee9")           ; Nord4 - Snow Storm
    (fg-alt . "#434c5e")           ; Nord2 - Polar Night

    ;; === Active/Inactive States ===
    (bg-active . "#d8dee9")        ; Active elements
    (fg-active . "#2e3440")        ; Dark on active
    (bg-inactive . "#e5e9f0")      ; Inactive elements
    (fg-inactive . "#4c566a")      ; Dim on inactive

    ;; === Complete Color Scales ===
    ;; Red scale (Nord Aurora)
    (red . "#bf616a")              ; Nord11 - Aurora Red
    (red-warmer . "#d08770")       ; Nord12 - Aurora Orange tint
    (red-cooler . "#b85965")       ; Cooler red
    (red-faint . "#d08a90")        ; Faint red
    (red-intense . "#a54e56")      ; Intense red

    ;; Orange scale (Nord Aurora)
    (orange . "#d08770")           ; Nord12 - Aurora Orange
    (orange-warmer . "#e4935a")    ; Warmer orange
    (orange-cooler . "#c97965")    ; Cooler orange
    (orange-faint . "#e0a890")     ; Faint orange
    (orange-intense . "#b96e50")   ; Intense orange

    ;; Yellow scale (Nord Aurora)
    (yellow . "#ebcb8b")           ; Nord13 - Aurora Yellow
    (yellow-warmer . "#f4d79e")    ; Warmer yellow
    (yellow-cooler . "#e3bf7e")    ; Cooler yellow
    (yellow-faint . "#f2ddb0")     ; Faint yellow
    (yellow-intense . "#d9b263")   ; Intense yellow

    ;; Green scale (Nord Aurora)
    (green . "#a3be8c")            ; Nord14 - Aurora Green
    (green-warmer . "#b1c89d")     ; Warmer green
    (green-cooler . "#96b77f")     ; Cooler green
    (green-faint . "#bfd0ad")      ; Faint green
    (green-intense . "#8fa876")    ; Intense green

    ;; Cyan scale (Nord Frost)
    (cyan . "#88c0d0")             ; Nord8 - Frost Cyan
    (cyan-warmer . "#81a1c1")      ; Nord9 tint
    (cyan-cooler . "#8fbcbb")      ; Nord7 - Frost Green-Cyan
    (cyan-faint . "#a3d0db")       ; Faint cyan
    (cyan-intense . "#6fa7ba")     ; Intense cyan

    ;; Blue scale (Nord Frost - primary accent)
    (blue . "#5e81ac")             ; Nord10 - Frost Deep Blue
    (blue-warmer . "#81a1c1")      ; Nord9 - Frost Light Blue
    (blue-cooler . "#5d7a9a")      ; Cooler blue
    (blue-faint . "#8fa9c4")       ; Faint blue
    (blue-intense . "#4c6e93")     ; Intense blue

    ;; Purple scale (Nord Aurora)
    (purple . "#b48ead")           ; Nord15 - Aurora Purple
    (purple-warmer . "#c79dbd")    ; Warmer purple
    (purple-cooler . "#a983a2")    ; Cooler purple
    (purple-faint . "#cab0c5")     ; Faint purple
    (purple-intense . "#9f7897")   ; Intense purple

    ;; Magenta scale (Nord Aurora blend)
    (magenta . "#bf616a")          ; Nord11 based
    (magenta-warmer . "#d08770")   ; Nord12 tint
    (magenta-cooler . "#b85570")   ; Cooler magenta
    (magenta-faint . "#d08a95")    ; Faint magenta
    (magenta-intense . "#a54e5a")  ; Intense magenta

    ;; Pink scale (Nord Aurora soft)
    (pink . "#d8a3b6")             ; Soft Aurora Pink
    (pink-warmer . "#e5b0c0")      ; Warmer pink
    (pink-cooler . "#cc97ab")      ; Cooler pink
    (pink-faint . "#e6c5d0")       ; Faint pink
    (pink-intense . "#c18a9e")     ; Intense pink

    ;; === Accent Mappings (semantic) ===
    (accent-0 . blue)              ; Primary accent
    (accent-1 . green)             ; Secondary accent
    (accent-2 . orange)            ; Tertiary accent
    (accent-3 . purple)            ; Quaternary accent

    ;; Accent intensity variants
    (accent-0-intense . blue-intense)
    (accent-1-intense . green-intense)
    (accent-2-intense . orange-intense)
    (accent-3-intense . purple-intense)
    (accent-0-faint . blue-faint)
    (accent-1-faint . green-faint)
    (accent-2-faint . orange-faint)
    (accent-3-faint . purple-faint)

    ;; === Nuanced/Subtle Backgrounds ===
    (bg-red-nuanced . "#f8f0f0")
    (bg-orange-nuanced . "#f8f2ec")
    (bg-yellow-nuanced . "#f8f6ec")
    (bg-green-nuanced . "#f0f6ec")
    (bg-cyan-nuanced . "#ecf4f6")
    (bg-blue-nuanced . "#ecf0f6")
    (bg-purple-nuanced . "#f2ecf6")
    (bg-magenta-nuanced . "#f6ecf0")

    ;; === Intense Backgrounds ===
    (bg-red-intense . "#f0d5d8")
    (bg-orange-intense . "#f0ddd5")
    (bg-yellow-intense . "#f0e8d5")
    (bg-green-intense . "#dde8d5")
    (bg-cyan-intense . "#d5e8ed")
    (bg-blue-intense . "#d5dded")
    (bg-purple-intense . "#e5d5ed")
    (bg-magenta-intense . "#edd5df")

    ;; === Syntax Highlighting Semantic Mappings ===
    (keyword . blue)               ; Control flow keywords
    (builtin . purple)             ; Built-in functions
    (string . green)               ; String literals
    (docstring . green-faint)      ; Documentation
    (comment . fg-inactive)        ; Comments
    (constant . orange)            ; Constants
    (fnname . cyan)                ; Function names
    (variable . blue-warmer)       ; Variables
    (type . purple-warmer)         ; Type names
    (preprocessor . orange-faint)  ; Preprocessor directives
    (rx-construct . purple)        ; Regex constructs
    (rx-backslash . pink-warmer)   ; Regex backslashes
    (number . orange-cooler)       ; Numbers
    (operator . blue-cooler)       ; Operators
    (property . cyan-warmer)       ; Properties
    (punctuation . fg-dim)         ; Punctuation
    (bracket . fg-main)            ; Brackets
    (delimiter . fg-dim)           ; Delimiters
    (escape . red-faint)           ; Escape sequences

    ;; === UI Elements ===
    ;; Headers/Headings
    (fg-heading-0 . blue-warmer)
    (fg-heading-1 . fg-main)
    (fg-heading-2 . orange-warmer)
    (fg-heading-3 . fg-alt)
    (fg-heading-4 . purple)
    (fg-heading-5 . green-warmer)
    (fg-heading-6 . red)
    (fg-heading-7 . cyan-warmer)
    (fg-heading-8 . fg-dim)

    ;; Mode line
    (modeline-bg-active . "#d8dee9")
    (modeline-fg-active . "#2e3440")
    (modeline-bg-active-accent . blue)
    (modeline-fg-active-accent . "#eceff4")
    (modeline-bg-active-alt . "#e5e9f0")
    (modeline-bg-inactive . "#e5e9f0")
    (modeline-fg-inactive . "#4c566a")
    (modeline-border-active . "#a8b4c0")
    (modeline-border-inactive . "#d8dee9")
    (modeline-err . red)
    (modeline-error . red)
    (modeline-warning . orange)
    (modeline-info . blue)
    (modeline-success . green)

    ;; === Interactive Elements ===
    (bg-hover . "#e5e9f0")         ; Hover state
    (bg-hover-secondary . "#d8e1ec") ; Secondary hover (blue tint)
    (bg-hl-line . "#e5e9f0")       ; Current line highlight
    (bg-hl-line-intense . "#d8dee9") ; Intense line highlight
    (bg-hl-line-accent . "#d5e1ec") ; Accent line highlight
    (bg-hl-line-faint . "#eceff4") ; Faint line highlight
    (bg-region . "#c2d0e0")        ; Selection (Nord tint)
    (fg-region . unspecified)      ; Let foreground show through
    (bg-accent-subtle . "#d5e1ec") ; Subtle accent background

    ;; === Completion & Search ===
    (bg-completion . "#d5e1ec")    ; Completion selection (blue tint)
    (bg-search-current . "#f0e8d5") ; Current search match
    (bg-search-fail . "#f0d5d8")   ; Failed search
    (bg-search-lazy . "#e5e8ed")   ; Lazy highlight
    (bg-match . "#d5e8ed")         ; General match
    (bg-search-replace . "#e5d5ed") ; Replace highlight

    ;; Completion match levels
    (fg-completion-match-0 . blue-intense)
    (fg-completion-match-1 . magenta-intense)
    (fg-completion-match-2 . orange-intense)
    (fg-completion-match-3 . green-intense)
    (bg-completion-match-0 . bg-blue-nuanced)
    (bg-completion-match-1 . bg-magenta-nuanced)
    (bg-completion-match-2 . bg-orange-nuanced)
    (bg-completion-match-3 . bg-green-nuanced)

    ;; === Diffs & Version Control ===
    (bg-added . "#dde8d5")         ; Light green
    (bg-added-faint . "#ecf2e8")   ; Very light green
    (bg-added-refine . "#c5dbc0")  ; Refined green
    (bg-added-fringe . "#a3be8c")  ; Fringe indicator
    (fg-added . "#4c6640")         ; Dark green
    (fg-added-intense . "#3a5030") ; Very dark green

    (bg-removed . "#f0d5d8")       ; Light red
    (bg-removed-faint . "#f8e8ea") ; Very light red
    (bg-removed-refine . "#e5c0c5") ; Refined red
    (bg-removed-fringe . "#bf616a") ; Fringe indicator
    (fg-removed . "#734045")       ; Dark red
    (fg-removed-intense . "#5a3035") ; Very dark red

    (bg-changed . "#f0e8d5")       ; Light yellow
    (bg-changed-faint . "#f8f2e8") ; Very light yellow
    (bg-changed-refine . "#e5dbc0") ; Refined yellow
    (bg-changed-fringe . "#ebcb8b") ; Fringe indicator
    (fg-changed . "#73664d")       ; Dark yellow/brown
    (fg-changed-intense . "#5a503a") ; Very dark brown
    (bg-changed-subtle . "#f8f6ec") ; Subtle changed

    (bg-diff-context . "#eceff4")  ; Context lines

    ;; === Status Indicators ===
    (info . blue)                  ; Information
    (success . green)              ; Success states
    (warning . orange)             ; Warnings
    (error . red)                  ; Errors

    ;; === Special Purpose ===
    (bg-popup . "#e5e9f0")         ; Popup backgrounds
    (bg-tooltip . "#f8f6ec")       ; Tooltip background (light yellow)
    (bg-tab-bar . "#d8dee9")       ; Tab bar background
    (bg-tab-current . "#eceff4")   ; Current tab
    (bg-tab-other . "#e5e9f0")     ; Other tabs
    (bg-space . "#eceff4")         ; Space background
    (fg-space . "#d8dee9")         ; Space foreground
    (bg-space-error . "#f0d5d8")   ; Space error background
    (bg-pulse . "#d5e1ec")         ; Pulse highlight
    (bg-fill-column . "#e5e9f0")   ; Fill column indicator
    (bg-header . "#d8dee9")        ; Header background
    (fg-header . "#2e3440")        ; Header foreground
    (bg-keybind . "#d5e1ec")       ; Keybind background

    ;; Mark selections (for dired, etc.)
    (bg-mark-select . "#d5e1ec")   ; Selected mark background
    (fg-mark-select . "#2e3440")   ; Selected mark foreground
    (bg-mark-delete . "#f0d5d8")   ; Delete mark background
    (fg-mark-delete . "#2e3440")   ; Delete mark foreground
    (bg-mark-other . "#f0e8d5")    ; Other mark background
    (fg-mark-other . "#2e3440")    ; Other mark foreground

    ;; Active argument (for eldoc)
    (bg-active-argument . "#f0e8d5") ; Yellow-tinted background
    (fg-active-argument . "#2e3440") ; Dark foreground

    ;; Paren matching
    (bg-paren-match . "#c2d0e0")   ; Matching paren
    (bg-paren-match-intense . "#a8c4e0") ; Intense matching paren
    (fg-paren-match . unspecified) ; Use existing foreground
    (bg-paren-expression . "#e5d5ed") ; Expression highlight
    (bg-paren-mismatch . bg-red-intense) ; Mismatch
    (fg-paren-mismatch . "#2e3440") ; Dark on red

    ;; === Links ===
    (fg-link . blue)
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
    (underline-warning . orange)
    (underline-note . blue)

    ;; === Buttons ===
    (fg-button-active . fg-main)
    (bg-button-active . bg-active)
    (fg-button-inactive . fg-dim)
    (bg-button-inactive . bg-inactive)

    ;; === Miscellaneous ===
    (cursor . fg-main)
    (prompt . blue-intense)        ; Used by fg-prompt
    (fg-prompt . prompt)
    (hl-todo . red-intense)
    (keybind . blue-intense)
    (name . purple)                ; Names (buffers, files, etc.)
    (identifier . cyan-warmer)     ; Identifiers
    (border . "#d8dee9")          ; General border color
    (fringe . bg-dim)             ; Fringe color
    (fringe-subtle . "#eceff4")   ; Subtle fringe
    (fringe-greyscale . "#d8dee9") ; Greyscale fringe
    (fringe-accent . blue-faint)   ; Accent fringe
    (shadow . "#2e344030")        ; Shadow color (transparent dark)
    (fg-whitespace . "#d8dee9")   ; Whitespace indicators
    (fg-special-mild . blue-faint) ; Special mild foreground

    ;; === Terminal Colors ===
    (bg-term-black . "#3b4252")
    (fg-term-black . "#3b4252")
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
    (bg-term-white . "#d8dee9")
    (fg-term-white . "#d8dee9")

    ;; Bright terminal colors
    (bg-term-black-bright . "#4c566a")
    (fg-term-black-bright . "#4c566a")
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
    (bg-term-white-bright . "#eceff4")
    (fg-term-white-bright . "#eceff4")

    ;; === Legacy/Compatibility (for smooth migration) ===
    (foreground . fg-main)
    (background . bg-main)
    (strong . fg-active)
    (emphasis . fg-main)
    (faded . fg-dim)
    (salient . accent-0)
    (popout . accent-1)
    (critical . red)
    (subtle . bg-dim)
    (highlight . bg-hover)
    (mode-line-fg . bg-main))  ; For legacy modeline
  "Comprehensive color palette for light theme with Nord aesthetic.
Modern, clean design with sophisticated color scales and semantic mappings.")

(bv-themes-theme bv-light bv-light-palette)

(provide 'bv-light-theme)
;;; bv-light-theme.el ends here