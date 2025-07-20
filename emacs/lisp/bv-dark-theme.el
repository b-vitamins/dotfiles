;;; bv-dark-theme.el --- Dark theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Dark variant theme with an extensive semantic color palette.
;; Features a sophisticated color system inspired by Modus themes.

;;; Code:

(require 'bv-themes)

(defconst bv-dark-palette
  '(;; === Foundation Colors ===
    ;; Main backgrounds and foregrounds
    (bg-main . "#1c1c1c")          ; Eerie black
    (fg-main . "#dadada")          ; Platinum
    (bg-dim . "#242424")           ; Slightly lighter than main
    (fg-dim . "#a8a8a8")           ; Slightly darker than main
    (bg-alt . "#2a2a2a")           ; Alternative background
    (fg-alt . "#c0c0c0")           ; Alternative foreground

    ;; === Active/Inactive States ===
    (bg-active . "#404040")        ; Active elements
    (fg-active . "#eeeeee")        ; White smoke
    (bg-inactive . "#202020")      ; Very subtle dark
    (fg-inactive . "#6c6c6c")      ; Dim gray

    ;; === Complete Color Scales ===
    ;; Red scale
    (red . "#ff6b6b")              ; Bright red
    (red-warmer . "#ff5454")       ; Warmer red
    (red-cooler . "#ff8787")       ; Cooler red
    (red-faint . "#cc5555")        ; Faint red
    (red-intense . "#ff3030")      ; Intense red

    ;; Orange scale
    (orange . "#ff9f40")           ; Bright orange
    (orange-warmer . "#ff8c00")    ; Warmer orange
    (orange-cooler . "#ffb366")    ; Cooler orange
    (orange-faint . "#cc7f33")     ; Faint orange
    (orange-intense . "#ff7f00")   ; Intense orange

    ;; Yellow scale
    (yellow . "#ffd93d")           ; Bright yellow
    (yellow-warmer . "#ffcc00")    ; Warmer yellow
    (yellow-cooler . "#ffe566")    ; Cooler yellow
    (yellow-faint . "#ccae31")     ; Faint yellow
    (yellow-intense . "#ffc700")   ; Intense yellow

    ;; Green scale
    (green . "#6bcf7f")            ; Bright green
    (green-warmer . "#4fc65b")     ; Warmer green
    (green-cooler . "#87d896")     ; Cooler green
    (green-faint . "#55a666")      ; Faint green
    (green-intense . "#2fc947")    ; Intense green

    ;; Cyan scale
    (cyan . "#4dd0e1")             ; Bright cyan
    (cyan-warmer . "#26c6da")      ; Warmer cyan
    (cyan-cooler . "#73dae7")      ; Cooler cyan
    (cyan-faint . "#3da7b4")       ; Faint cyan
    (cyan-intense . "#00bcd4")     ; Intense cyan

    ;; Blue scale (primary accent)
    (blue . "#5f87d7")             ; Cornflower blue
    (blue-warmer . "#4a72c2")      ; Warmer blue
    (blue-cooler . "#7097e7")      ; Cooler blue
    (blue-faint . "#4c6fb0")       ; Faint blue
    (blue-intense . "#3a5fcd")     ; Intense blue

    ;; Purple scale
    (purple . "#b19cd9")           ; Bright purple
    (purple-warmer . "#9f7ec7")    ; Warmer purple
    (purple-cooler . "#c3b3e1")    ; Cooler purple
    (purple-faint . "#8e7aae")     ; Faint purple
    (purple-intense . "#8b69c6")   ; Intense purple

    ;; Magenta scale
    (magenta . "#ff79c6")          ; Bright magenta
    (magenta-warmer . "#ff5cbb")   ; Warmer magenta
    (magenta-cooler . "#ff96d1")   ; Cooler magenta
    (magenta-faint . "#cc609e")    ; Faint magenta
    (magenta-intense . "#ff1493")  ; Intense magenta

    ;; Pink scale
    (pink . "#ffb3d9")             ; Bright pink
    (pink-warmer . "#ff99cc")      ; Warmer pink
    (pink-cooler . "#ffcce6")      ; Cooler pink
    (pink-faint . "#cc8fae")       ; Faint pink
    (pink-intense . "#ff69b4")     ; Intense pink

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
    (bg-red-nuanced . "#2d1a1a")
    (bg-orange-nuanced . "#2d231a")
    (bg-yellow-nuanced . "#2d291a")
    (bg-green-nuanced . "#1a2d1f")
    (bg-cyan-nuanced . "#1a2a2d")
    (bg-blue-nuanced . "#1a1f2d")
    (bg-purple-nuanced . "#241a2d")
    (bg-magenta-nuanced . "#2d1a27")

    ;; === Intense Backgrounds ===
    (bg-red-intense . "#5a2020")
    (bg-orange-intense . "#5a3520")
    (bg-yellow-intense . "#5a4a20")
    (bg-green-intense . "#205a30")
    (bg-cyan-intense . "#205a5a")
    (bg-blue-intense . "#20305a")
    (bg-purple-intense . "#35205a")
    (bg-magenta-intense . "#5a2045")

    ;; === Syntax Highlighting Semantic Mappings ===
    (keyword . blue)               ; Control flow keywords
    (builtin . purple)             ; Built-in functions
    (string . green)               ; String literals
    (docstring . green-faint)      ; Documentation
    (comment . fg-inactive)        ; Comments
    (constant . orange)            ; Constants
    (fnname . cyan)                ; Function names
    (variable . yellow-faint)      ; Variables
    (type . blue-intense)          ; Type names
    (preprocessor . orange-faint)  ; Preprocessor directives
    (rx-construct . magenta)       ; Regex constructs
    (rx-backslash . pink)          ; Regex backslashes

    ;; === UI Elements ===
    ;; Headers/Headings
    (fg-heading-0 . cyan-intense)
    (fg-heading-1 . fg-main)
    (fg-heading-2 . yellow-faint)
    (fg-heading-3 . fg-alt)
    (fg-heading-4 . magenta)
    (fg-heading-5 . green-faint)
    (fg-heading-6 . red-faint)
    (fg-heading-7 . cyan-warmer)
    (fg-heading-8 . fg-dim)

    ;; Mode line
    (modeline-bg-active . "#3a3a3a")
    (modeline-fg-active . "#f0f0f0")
    (modeline-bg-active-accent . blue)
    (modeline-fg-active-accent . "#ffffff")
    (modeline-bg-inactive . "#2a2a2a")
    (modeline-fg-inactive . "#808080")
    (modeline-border-active . "#5a5a5a")
    (modeline-border-inactive . "#3a3a3a")
    (modeline-err . red-intense)
    (modeline-warning . yellow-intense)
    (modeline-info . blue-intense)

    ;; === Interactive Elements ===
    (bg-hover . "#333333")         ; Hover state
    (bg-hover-secondary . "#3a3a4a") ; Secondary hover (with blue tint)
    (bg-hl-line . "#2f3849")       ; Current line highlight
    (bg-region . "#4a4a5a")        ; Selection (slight purple tint)
    (fg-region . unspecified)      ; Let foreground show through

    ;; === Completion & Search ===
    (bg-completion . "#2a3a5a")    ; Completion selection (blue tint)
    (bg-yellow-nuanced . "#3a3520") ; Search highlights
    (bg-yellow-intense . "#5a5020") ; Active search
    (bg-cyan-nuanced . "#203a3a")  ; Secondary matches
    (bg-magenta-nuanced . "#3a203a") ; Replace highlight
    (bg-magenta-intense . "#5a2050") ; Active replace

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
    (bg-added . "#1a3a1a")         ; Dark green
    (bg-added-faint . "#152915")   ; Very dark green
    (bg-added-refine . "#2a5a2a")  ; Refined green
    (bg-added-fringe . "#40a040")  ; Fringe indicator
    (fg-added . "#80d080")         ; Light green
    (fg-added-intense . "#60c060") ; Intense green

    (bg-removed . "#3a1a1a")       ; Dark red
    (bg-removed-faint . "#291515") ; Very dark red
    (bg-removed-refine . "#5a2a2a") ; Refined red
    (bg-removed-fringe . "#d04040") ; Fringe indicator
    (fg-removed . "#ff9090")       ; Light red
    (fg-removed-intense . "#ff7070") ; Intense red

    (bg-changed . "#3a3a1a")       ; Dark yellow
    (bg-changed-faint . "#292915") ; Very dark yellow
    (bg-changed-refine . "#5a5a2a") ; Refined yellow
    (bg-changed-fringe . "#b0a040") ; Fringe indicator
    (fg-changed . "#d0c050")       ; Light yellow
    (fg-changed-intense . "#c0b040") ; Intense yellow

    (bg-diff-context . "#1f1f1f")  ; Context lines

    ;; === Status Indicators ===
    (info . blue)                  ; Information
    (success . green)              ; Success states
    (warning . yellow)             ; Warnings
    (error . red)                  ; Errors

    ;; === Special Purpose ===
    (bg-popup . "#2f2f2f")         ; Popup backgrounds
    (bg-tooltip . "#3a3a3a")       ; Tooltip background
    (bg-tab-bar . "#2a2a2a")       ; Tab bar background
    (bg-tab-current . "#1c1c1c")   ; Current tab
    (bg-tab-other . "#353535")     ; Other tabs

    ;; Paren matching
    (bg-paren-match . "#2f5f7f")   ; Matching paren
    (fg-paren-match . unspecified) ; Use existing foreground
    (bg-paren-expression . "#3a2a4a") ; Expression highlight
    (bg-paren-mismatch . bg-red-intense) ; Mismatch
    (fg-paren-mismatch . "#ffffff") ; White on red

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
    (border . "#3a3a3a")          ; General border color
    (fringe . bg-dim)             ; Fringe color
    (fringe-subtle . "#2a2a2a")   ; Subtle fringe
    (fringe-greyscale . "#4a4a4a") ; Greyscale fringe
    (shadow . "#00000080")        ; Shadow color (transparent black)
    (fg-whitespace . "#3a3a3a")   ; Whitespace indicators

    ;; === Terminal Colors ===
    (bg-term-black . "#000000")
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
    (bg-term-white . "#d3d7cf")
    (fg-term-white . "#eeeeec")

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
  "Comprehensive color palette for dark theme.
Follows Modus themes' approach with named colors and semantic mappings.")

(bv-themes-theme bv-dark bv-dark-palette)

(provide 'bv-dark-theme)
;;; bv-dark-theme.el ends here
