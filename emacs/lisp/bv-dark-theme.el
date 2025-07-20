;;; bv-dark-theme.el --- Dark theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Dark variant theme with a modern Nord-inspired aesthetic.
;; Features a sophisticated color system with warmer/cooler/faint/intense variants.

;;; Code:

(require 'bv-themes)

(defconst bv-dark-palette
  '(;; === Foundation Colors ===
    ;; Main backgrounds and foregrounds (Nord Polar Night)
    (bg-main . "#2e3440")          ; Nord0 - Polar Night
    (fg-main . "#d8dee9")          ; Nord4 - Snow Storm
    (bg-dim . "#3b4252")           ; Nord1 - Polar Night
    (fg-dim . "#a8b4c0")           ; Dimmed Snow Storm
    (bg-alt . "#434c5e")           ; Nord2 - Polar Night
    (fg-alt . "#e5e9f0")           ; Nord5 - Snow Storm

    ;; === Active/Inactive States ===
    (bg-active . "#4c566a")        ; Nord3 - Polar Night
    (fg-active . "#eceff4")        ; Nord6 - Snow Storm
    (bg-inactive . "#3b4252")      ; Nord1 - Polar Night
    (fg-inactive . "#7b88a1")      ; Muted Snow Storm

    ;; === Complete Color Scales ===
    ;; Red scale (Nord Aurora)
    (red . "#bf616a")              ; Nord11 - Aurora Red
    (red-warmer . "#d08770")       ; Nord12 - Aurora Orange tint
    (red-cooler . "#b85560")       ; Cooler red
    (red-faint . "#a85460")        ; Faint red
    (red-intense . "#e06c75")      ; Intense red

    ;; Orange scale (Nord Aurora)
    (orange . "#d08770")           ; Nord12 - Aurora Orange
    (orange-warmer . "#e59056")    ; Warmer orange
    (orange-cooler . "#c67260")    ; Cooler orange
    (orange-faint . "#b86e5a")     ; Faint orange
    (orange-intense . "#e89a7a")   ; Intense orange

    ;; Yellow scale (Nord Aurora)
    (yellow . "#ebcb8b")           ; Nord13 - Aurora Yellow
    (yellow-warmer . "#f2d196")    ; Warmer yellow
    (yellow-cooler . "#e3c17e")    ; Cooler yellow
    (yellow-faint . "#d4b174")     ; Faint yellow
    (yellow-intense . "#f7da9e")   ; Intense yellow

    ;; Green scale (Nord Aurora)
    (green . "#a3be8c")            ; Nord14 - Aurora Green
    (green-warmer . "#b1c896")     ; Warmer green
    (green-cooler . "#96b47f")     ; Cooler green
    (green-faint . "#8faa7b")      ; Faint green
    (green-intense . "#aecf96")    ; Intense green

    ;; Cyan scale (Nord Frost)
    (cyan . "#88c0d0")             ; Nord8 - Frost Cyan
    (cyan-warmer . "#81a1c1")      ; Nord9 tint
    (cyan-cooler . "#8fbcbb")      ; Nord7 - Frost Green-Cyan
    (cyan-faint . "#7db1c3")       ; Faint cyan
    (cyan-intense . "#8fcfe0")     ; Intense cyan

    ;; Blue scale (Nord Frost - primary accent)
    (blue . "#81a1c1")             ; Nord9 - Frost Light Blue
    (blue-warmer . "#5e81ac")      ; Nord10 - Frost Deep Blue
    (blue-cooler . "#7d98b8")      ; Cooler blue
    (blue-faint . "#6d8eb3")       ; Faint blue
    (blue-intense . "#88aacf")     ; Intense blue

    ;; Purple scale (Nord Aurora)
    (purple . "#b48ead")           ; Nord15 - Aurora Purple
    (purple-warmer . "#c896b8")    ; Warmer purple
    (purple-cooler . "#a886a5")    ; Cooler purple
    (purple-faint . "#9e7f9b")     ; Faint purple
    (purple-intense . "#bf9ab8")   ; Intense purple

    ;; Magenta scale (Nord Aurora blend)
    (magenta . "#bf616a")          ; Nord11 based
    (magenta-warmer . "#d08770")   ; Nord12 tint
    (magenta-cooler . "#b85560")   ; Cooler magenta
    (magenta-faint . "#a85460")    ; Faint magenta
    (magenta-intense . "#e06c75")  ; Intense magenta

    ;; Pink scale (Nord Aurora soft)
    (pink . "#d8a3b6")             ; Soft Aurora Pink
    (pink-warmer . "#e5afc0")      ; Warmer pink
    (pink-cooler . "#cc97ab")      ; Cooler pink
    (pink-faint . "#c195a8")       ; Faint pink
    (pink-intense . "#ebb0c4")     ; Intense pink

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
    (bg-red-nuanced . "#3a2a2d")
    (bg-orange-nuanced . "#3a302a")
    (bg-yellow-nuanced . "#3a362a")
    (bg-green-nuanced . "#2d3a2f")
    (bg-cyan-nuanced . "#2a363a")
    (bg-blue-nuanced . "#2a303a")
    (bg-purple-nuanced . "#342a3a")
    (bg-magenta-nuanced . "#3a2a34")

    ;; === Intense Backgrounds ===
    (bg-red-intense . "#4d3035")
    (bg-orange-intense . "#4d3a30")
    (bg-yellow-intense . "#4d4330")
    (bg-green-intense . "#304d36")
    (bg-cyan-intense . "#304d4d")
    (bg-blue-intense . "#30384d")
    (bg-purple-intense . "#41304d")
    (bg-magenta-intense . "#4d3042")

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
    (fg-heading-0 . cyan-intense)
    (fg-heading-1 . fg-main)
    (fg-heading-2 . yellow-faint)
    (fg-heading-3 . fg-alt)
    (fg-heading-4 . purple)
    (fg-heading-5 . green-faint)
    (fg-heading-6 . red-faint)
    (fg-heading-7 . cyan-warmer)
    (fg-heading-8 . fg-dim)

    ;; Mode line
    (modeline-bg-active . "#4c566a")
    (modeline-fg-active . "#eceff4")
    (modeline-bg-active-accent . blue)
    (modeline-fg-active-accent . "#2e3440")
    (modeline-bg-active-alt . "#434c5e")
    (modeline-bg-inactive . "#3b4252")
    (modeline-fg-inactive . "#7b88a1")
    (modeline-border-active . "#5e6779")
    (modeline-border-inactive . "#434c5e")
    (modeline-err . red-intense)
    (modeline-error . red-intense)
    (modeline-warning . yellow-intense)
    (modeline-info . blue-intense)
    (modeline-success . green-intense)

    ;; === Interactive Elements ===
    (bg-hover . "#434c5e")         ; Hover state
    (bg-hover-secondary . "#3e4a5e") ; Secondary hover (blue tint)
    (bg-hl-line . "#3b4252")       ; Current line highlight
    (bg-hl-line-intense . "#434c5e") ; Intense line highlight
    (bg-hl-line-accent . "#3e4a5e") ; Accent line highlight
    (bg-hl-line-faint . "#373e4c") ; Faint line highlight
    (bg-region . "#434c5e")        ; Selection (Nord tint)
    (fg-region . unspecified)      ; Let foreground show through
    (bg-accent-subtle . "#3e4a5e") ; Subtle accent background

    ;; === Completion & Search ===
    (bg-completion . "#3e4a5e")    ; Completion selection (blue tint)
    (bg-search-current . "#4d4330") ; Current search match
    (bg-search-fail . "#4d3035")   ; Failed search
    (bg-search-lazy . "#3e4753")   ; Lazy highlight
    (bg-match . "#304d4d")         ; General match
    (bg-search-replace . "#41304d") ; Replace highlight

    ;; Completion match levels
    (fg-completion-match-0 . blue-intense)
    (fg-completion-match-1 . magenta-intense)
    (fg-completion-match-2 . cyan-intense)
    (fg-completion-match-3 . green-intense)
    (bg-completion-match-0 . bg-blue-nuanced)
    (bg-completion-match-1 . bg-magenta-nuanced)
    (bg-completion-match-2 . bg-cyan-nuanced)
    (bg-completion-match-3 . bg-green-nuanced)

    ;; === Diffs & Version Control ===
    (bg-added . "#304d36")         ; Dark green
    (bg-added-faint . "#2d3a2f")   ; Very dark green
    (bg-added-refine . "#3a5940")  ; Refined green
    (bg-added-fringe . "#a3be8c")  ; Fringe indicator
    (fg-added . "#a3be8c")         ; Light green
    (fg-added-intense . "#aecf96") ; Intense green

    (bg-removed . "#4d3035")       ; Dark red
    (bg-removed-faint . "#3a2a2d") ; Very dark red
    (bg-removed-refine . "#5a3a3f") ; Refined red
    (bg-removed-fringe . "#bf616a") ; Fringe indicator
    (fg-removed . "#bf616a")       ; Light red
    (fg-removed-intense . "#e06c75") ; Intense red

    (bg-changed . "#4d4330")       ; Dark yellow
    (bg-changed-faint . "#3a362a") ; Very dark yellow
    (bg-changed-refine . "#5a4f3a") ; Refined yellow
    (bg-changed-fringe . "#ebcb8b") ; Fringe indicator
    (fg-changed . "#ebcb8b")       ; Light yellow
    (fg-changed-intense . "#f7da9e") ; Intense yellow
    (bg-changed-subtle . "#3a362a") ; Subtle changed

    (bg-diff-context . "#3b4252")  ; Context lines

    ;; === Status Indicators ===
    (info . blue)                  ; Information
    (success . green)              ; Success states
    (warning . yellow)             ; Warnings
    (error . red)                  ; Errors

    ;; === Special Purpose ===
    (bg-popup . "#3b4252")         ; Popup backgrounds
    (bg-tooltip . "#434c5e")       ; Tooltip background
    (bg-tab-bar . "#3b4252")       ; Tab bar background
    (bg-tab-current . "#2e3440")   ; Current tab
    (bg-tab-other . "#434c5e")     ; Other tabs
    (bg-space . "#3b4252")         ; Space background
    (fg-space . "#4c566a")         ; Space foreground
    (bg-space-error . "#4d3035")   ; Space error background
    (bg-pulse . "#3e4a5e")         ; Pulse highlight
    (bg-fill-column . "#3b4252")   ; Fill column indicator
    (bg-header . "#434c5e")        ; Header background
    (fg-header . "#eceff4")        ; Header foreground
    (bg-keybind . "#3e4a5e")       ; Keybind background

    ;; Mark selections (for dired, etc.)
    (bg-mark-select . "#3e4a5e")   ; Selected mark background
    (fg-mark-select . "#eceff4")   ; Selected mark foreground
    (bg-mark-delete . "#4d3035")   ; Delete mark background
    (fg-mark-delete . "#eceff4")   ; Delete mark foreground
    (bg-mark-other . "#4d4330")    ; Other mark background
    (fg-mark-other . "#eceff4")    ; Other mark foreground

    ;; Active argument (for eldoc)
    (bg-active-argument . "#4d4330") ; Yellow-tinted background
    (fg-active-argument . "#eceff4") ; Light foreground

    ;; Paren matching
    (bg-paren-match . "#4c566a")   ; Matching paren
    (bg-paren-match-intense . "#5e6779") ; Intense matching paren
    (fg-paren-match . unspecified) ; Use existing foreground
    (bg-paren-expression . "#41304d") ; Expression highlight
    (bg-paren-mismatch . bg-red-intense) ; Mismatch
    (fg-paren-mismatch . "#eceff4") ; Light on red

    ;; === Links ===
    (fg-link . blue)
    (fg-link-faint . blue-faint)
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
    (name . magenta)               ; Names (buffers, files, etc.)
    (identifier . cyan-faint)      ; Identifiers
    (border . "#4c566a")          ; General border color
    (fringe . bg-dim)             ; Fringe color
    (fringe-subtle . "#3b4252")   ; Subtle fringe
    (fringe-greyscale . "#4c566a") ; Greyscale fringe
    (fringe-accent . blue-faint)   ; Accent fringe
    (shadow . "#00000080")        ; Shadow color (transparent black)
    (fg-whitespace . "#4c566a")   ; Whitespace indicators
    (fg-special-mild . blue-faint) ; Special mild foreground

    ;; === Terminal Colors ===
    (bg-term-black . "#2e3440")
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
    (fg-term-white . "#eceff4")

    ;; Bright terminal colors
    (bg-term-black-bright . "#434c5e")
    (fg-term-black-bright . "#434c5e")
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
    (critical . red-intense)
    (subtle . bg-dim)
    (highlight . bg-hover)
    (mode-line-fg . fg-main))  ; For legacy modeline
  "Comprehensive color palette for dark theme with Nord aesthetic.
Modern, clean design with sophisticated color scales and semantic mappings.")

(bv-themes-theme bv-dark bv-dark-palette)

(provide 'bv-dark-theme)
;;; bv-dark-theme.el ends here