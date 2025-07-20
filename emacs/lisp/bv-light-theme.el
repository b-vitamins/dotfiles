;;; bv-light-theme.el --- Light theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Light variant theme with an extensive semantic color palette.
;; Features a sophisticated color system inspired by Modus themes.

;;; Code:

(require 'bv-themes)

(defconst bv-light-palette
  '(;; === Foundation Colors ===
    ;; Main backgrounds and foregrounds
    (bg-main . "#ffffff")          ; Pure white
    (fg-main . "#2e3436")          ; Charcoal
    (bg-dim . "#f8f8f8")           ; Cultured
    (fg-dim . "#595959")           ; Dim gray
    (bg-alt . "#f0f0f0")           ; White smoke
    (fg-alt . "#505050")           ; Davy's gray

    ;; === Active/Inactive States ===
    (bg-active . "#d3d3d3")        ; Light gray
    (fg-active . "#0a0a0a")        ; Smoky black
    (bg-inactive . "#fafafa")      ; Magnolia
    (fg-inactive . "#999999")      ; Spanish gray

    ;; === Complete Color Scales ===
    ;; Red scale
    (red . "#d73a49")              ; Carmine
    (red-warmer . "#cb2431")       ; Fire engine red
    (red-cooler . "#e36069")       ; Light carmine pink
    (red-faint . "#e79498")        ; Ruddy pink
    (red-intense . "#b91c27")      ; Firebrick

    ;; Orange scale
    (orange . "#e36209")           ; Spanish orange
    (orange-warmer . "#d15704")    ; Tenne
    (orange-cooler . "#fb8500")    ; Dark orange
    (orange-faint . "#f4a460")     ; Sandy brown
    (orange-intense . "#cc5500")   ; Burnt orange

    ;; Yellow scale
    (yellow . "#dbab09")           ; Gold
    (yellow-warmer . "#b08500")    ; Dark goldenrod
    (yellow-cooler . "#f9c74f")    ; Maize
    (yellow-faint . "#f8d568")     ; Light gold
    (yellow-intense . "#aa8500")   ; Harvest gold

    ;; Green scale
    (green . "#28a745")            ; Green
    (green-warmer . "#22863a")     ; Forest green
    (green-cooler . "#43d058")     ; Emerald
    (green-faint . "#81c995")      ; Light green
    (green-intense . "#1e7e34")    ; La Salle green

    ;; Cyan scale
    (cyan . "#17a2b8")             ; Light sea green
    (cyan-warmer . "#138496")      ; Teal
    (cyan-cooler . "#20c3db")      ; Robin egg blue
    (cyan-faint . "#7dd3e0")       ; Sky blue
    (cyan-intense . "#117a8b")     ; Metallic seaweed

    ;; Blue scale (primary accent)
    (blue . "#0366d6")             ; Bright blue
    (blue-warmer . "#024fb5")      ; Cobalt blue
    (blue-cooler . "#2188ff")      ; Dodger blue
    (blue-faint . "#79b8ff")       ; French sky blue
    (blue-intense . "#0047ab")     ; Absolute zero

    ;; Purple scale
    (purple . "#6f42c1")           ; Blue violet
    (purple-warmer . "#5a32a3")    ; Grape
    (purple-cooler . "#8b5cf6")    ; Medium slate blue
    (purple-faint . "#b8a5d6")     ; Wisteria
    (purple-intense . "#4c2889")   ; Indigo

    ;; Magenta scale
    (magenta . "#ea4aaa")          ; Raspberry pink
    (magenta-warmer . "#dd2d8f")   ; Deep cerise
    (magenta-cooler . "#f364c3")   ; Orchid pink
    (magenta-faint . "#f8b5d4")    ; Lavender pink
    (magenta-intense . "#c71585")  ; Medium violet red

    ;; Pink scale
    (pink . "#f692ce")             ; Lavender rose
    (pink-warmer . "#f364c3")      ; Orchid pink
    (pink-cooler . "#f9c2e4")      ; Classic rose
    (pink-faint . "#fdd5e9")       ; Piggy pink
    (pink-intense . "#ff1493")     ; Deep pink

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
    (bg-red-nuanced . "#fff0f0")
    (bg-orange-nuanced . "#fff5e6")
    (bg-yellow-nuanced . "#fffae6")
    (bg-green-nuanced . "#eefff2")
    (bg-cyan-nuanced . "#e6feff")
    (bg-blue-nuanced . "#e6f2ff")
    (bg-purple-nuanced . "#f5efff")
    (bg-magenta-nuanced . "#ffeffa")

    ;; === Intense Backgrounds ===
    (bg-red-intense . "#ffdddd")
    (bg-orange-intense . "#ffe5cc")
    (bg-yellow-intense . "#fff3cc")
    (bg-green-intense . "#ddffdd")
    (bg-cyan-intense . "#ccffff")
    (bg-blue-intense . "#ddeeff")
    (bg-purple-intense . "#eeddff")
    (bg-magenta-intense . "#ffddee")

    ;; === Syntax Highlighting Semantic Mappings ===
    (keyword . blue)               ; Control flow keywords
    (builtin . purple)             ; Built-in functions
    (string . green)               ; String literals
    (docstring . green-faint)      ; Documentation
    (comment . fg-inactive)        ; Comments
    (constant . orange)            ; Constants
    (fnname . magenta)             ; Function names
    (variable . cyan-warmer)       ; Variables
    (type . blue-warmer)           ; Type names
    (preprocessor . orange-faint)  ; Preprocessor directives
    (rx-construct . purple)        ; Regex constructs
    (rx-backslash . pink-warmer)   ; Regex backslashes

    ;; === UI Elements ===
    ;; Headers/Headings
    (fg-heading-0 . blue-warmer)
    (fg-heading-1 . fg-main)
    (fg-heading-2 . orange-warmer)
    (fg-heading-3 . fg-alt)
    (fg-heading-4 . magenta)
    (fg-heading-5 . green-warmer)
    (fg-heading-6 . red)
    (fg-heading-7 . cyan-warmer)
    (fg-heading-8 . fg-dim)

    ;; Mode line
    (modeline-bg-active . "#c8c8c8")
    (modeline-fg-active . "#000000")
    (modeline-bg-active-accent . blue)
    (modeline-fg-active-accent . "#ffffff")
    (modeline-bg-inactive . "#e6e6e6")
    (modeline-fg-inactive . "#606060")
    (modeline-border-active . "#9a9a9a")
    (modeline-border-inactive . "#d0d0d0")
    (modeline-err . red)
    (modeline-warning . orange)
    (modeline-info . blue)

    ;; === Interactive Elements ===
    (bg-hover . "#e8e8e8")         ; Hover state
    (bg-hover-secondary . "#e0e8f0") ; Secondary hover (blue tint)
    (bg-hl-line . "#f0f0f0")       ; Current line highlight
    (bg-region . "#b8d4ff")        ; Selection (blue tint)
    (fg-region . unspecified)      ; Let foreground show through

    ;; === Completion & Search ===
    (bg-completion . "#d5e5ff")    ; Completion selection (blue tint)
    (bg-yellow-nuanced . "#fff8dc") ; Search highlights (cornsilk)
    (bg-yellow-intense . "#ffeb99") ; Active search
    (bg-cyan-nuanced . "#e0f8f8")  ; Secondary matches
    (bg-magenta-nuanced . "#ffe0ff") ; Replace highlight
    (bg-magenta-intense . "#ffb3ff") ; Active replace

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
    (bg-added . "#d5f5d5")         ; Light green
    (bg-added-faint . "#e5ffe5")   ; Very light green
    (bg-added-refine . "#b5e5b5")  ; Refined green
    (bg-added-fringe . "#50c050")  ; Fringe indicator
    (fg-added . "#005000")         ; Dark green
    (fg-added-intense . "#003000") ; Very dark green

    (bg-removed . "#ffd5d5")       ; Light red
    (bg-removed-faint . "#ffe5e5") ; Very light red
    (bg-removed-refine . "#ffb5b5") ; Refined red
    (bg-removed-fringe . "#d05050") ; Fringe indicator
    (fg-removed . "#800000")       ; Dark red
    (fg-removed-intense . "#600000") ; Very dark red

    (bg-changed . "#fff5d5")       ; Light yellow
    (bg-changed-faint . "#ffffe5") ; Very light yellow
    (bg-changed-refine . "#ffe5b5") ; Refined yellow
    (bg-changed-fringe . "#c0a050") ; Fringe indicator
    (fg-changed . "#605000")       ; Dark yellow/brown
    (fg-changed-intense . "#503000") ; Very dark brown

    (bg-diff-context . "#f5f5f5")  ; Context lines

    ;; === Status Indicators ===
    (info . blue)                  ; Information
    (success . green)              ; Success states
    (warning . orange)             ; Warnings
    (error . red)                  ; Errors

    ;; === Special Purpose ===
    (bg-popup . "#f5f5f5")         ; Popup backgrounds
    (bg-tooltip . "#ffffdc")       ; Tooltip background (light yellow)
    (bg-tab-bar . "#e0e0e0")       ; Tab bar background
    (bg-tab-current . "#ffffff")   ; Current tab
    (bg-tab-other . "#cccccc")     ; Other tabs

    ;; Paren matching
    (bg-paren-match . "#a0d0ff")   ; Matching paren
    (fg-paren-match . unspecified) ; Use existing foreground
    (bg-paren-expression . "#f0e0ff") ; Expression highlight
    (bg-paren-mismatch . bg-red-intense) ; Mismatch
    (fg-paren-mismatch . "#000000") ; Black on red

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
    (border . "#d0d0d0")          ; General border color
    (fringe . bg-dim)             ; Fringe color
    (fringe-subtle . "#f0f0f0")   ; Subtle fringe
    (fringe-greyscale . "#cccccc") ; Greyscale fringe
    (shadow . "#00000020")        ; Shadow color (transparent black)
    (fg-whitespace . "#d0d0d0")   ; Whitespace indicators

    ;; === Terminal Colors ===
    (bg-term-black . "#2e3436")
    (fg-term-black . "#2e3436")
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
    (bg-term-white . "#eeeeec")
    (fg-term-white . "#eeeeec")

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
  "Comprehensive color palette for light theme.
Follows Modus themes' approach with named colors and semantic mappings.")

(bv-themes-theme bv-light bv-light-palette)

(provide 'bv-light-theme)
;;; bv-light-theme.el ends here
