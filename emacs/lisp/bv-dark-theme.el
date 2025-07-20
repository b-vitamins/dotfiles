;;; bv-dark-theme.el --- Dark theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Dark variant theme with an extensive semantic color palette.

;;; Code:

(require 'bv-themes)

(defconst bv-dark-palette
  '(;; === Foundation Colors ===
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

    ;; === Accent Colors with Variants ===
    ;; Blue (primary accent)
    (accent-0 . "#5f87d7")         ; Cornflower blue
    (accent-0-faint . "#4a72c2")   ; Darker blue
    (accent-0-intense . "#7097e7") ; Brighter blue

    ;; Olive (secondary accent) - adjusted for dark theme
    (accent-1 . "#afaf87")         ; Olive
    (accent-1-faint . "#9a9a72")   ; Darker olive
    (accent-1-intense . "#c4c49c") ; Brighter olive

    ;; Terracotta (tertiary accent)
    (accent-2 . "#d7875f")         ; Terracotta
    (accent-2-faint . "#c2724a")   ; Darker terracotta
    (accent-2-intense . "#ec9c74") ; Brighter terracotta

    ;; === Semantic Mappings ===
    (salient . accent-0)           ; Links, keywords
    (popout . accent-1)            ; Strings, special elements
    (critical . accent-2)          ; Errors, warnings

    ;; === UI Elements ===
    (bg-header . bg-dim)
    (fg-header . fg-main)
    (bg-header-strong . bg-active)
    (fg-header-strong . fg-active)

    (bg-mode-line-active . bg-active)
    (fg-mode-line-active . fg-active)
    (bg-mode-line-inactive . bg-dim)
    (fg-mode-line-inactive . fg-dim)

    ;; === Interactive Elements ===
    (bg-hover . "#333333")         ; Hover state
    (bg-hover-secondary . "#2d2d2d") ; Secondary hover
    (bg-selection . "#3a3a5a")     ; Selection highlight (slight purple tint)
    (bg-highlight . "#4a3a2a")     ; Search/match highlight (warm brown)

    ;; === Completion & Search ===
    (bg-completion . "#2a3a5a")    ; Completion selection (blue tint)
    (bg-completion-subtle . "#252535") ; Subtle completion background
    (fg-completion-match-0 . accent-0-intense)
    (fg-completion-match-1 . accent-1-intense)
    (fg-completion-match-2 . accent-2-intense)
    (fg-completion-match-3 . accent-0) ; Cycles back

    ;; === Diffs & Version Control ===
    (bg-added . "#1a3a1a")         ; Dark green
    (fg-added . "#70c070")         ; Light green
    (bg-removed . "#3a1a1a")       ; Dark red
    (fg-removed . "#ff7070")       ; Light red
    (bg-changed . "#3a3a1a")       ; Dark yellow
    (fg-changed . "#c0b070")       ; Light yellow

    ;; === Code Semantic Colors ===
    (keyword . accent-0)           ; Control flow keywords
    (builtin . accent-0-faint)     ; Built-in functions
    (string . accent-1)            ; String literals
    (doc . fg-dim)                 ; Documentation/docstrings
    (comment . fg-inactive)        ; Comments
    (constant . accent-2)          ; Constants
    (fnname . fg-active)           ; Function names
    (variable . fg-main)           ; Variables
    (type . accent-0-intense)      ; Type names
    (preprocessor . accent-2-faint) ; Preprocessor directives

    ;; === Status Indicators ===
    (info . accent-0)              ; Information
    (success . accent-1)           ; Success states
    (warning . accent-2)           ; Warnings
    (error . accent-2-intense)     ; Errors

    ;; === Additional UI Colors ===
    (border . bg-active)           ; Border color
    (shadow . "#00000080")         ; Shadow color (transparent black)

    ;; === Special Purpose ===
    (bg-org-block . bg-dim)        ; Org source blocks
    (bg-org-block-begin . bg-alt)  ; Org block delimiters

    ;; === Terminal Colors ===
    (term-black . "#000000")
    (term-red . "#cd3131")
    (term-green . "#0dbc79")
    (term-yellow . "#e5e510")
    (term-blue . "#2472c8")
    (term-magenta . "#bc3fbc")
    (term-cyan . "#11a8cd")
    (term-white . "#e5e5e5")

    ;; === Legacy Compatibility ===
    ;; Keep these for backward compatibility
    (foreground . fg-main)
    (background . bg-main)
    (strong . fg-active)
    (faded . fg-inactive)
    (subtle . bg-dim)
    (highlight . bg-highlight)
    (mode-line-fg . fg-main))      ; For legacy modeline
  "Comprehensive color palette for dark theme.")

(bv-themes-theme bv-dark bv-dark-palette)

(provide 'bv-dark-theme)
;;; bv-dark-theme.el ends here
