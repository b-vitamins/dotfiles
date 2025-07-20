;;; bv-light-theme.el --- Light theme -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Light variant theme with an extensive semantic color palette.

;;; Code:

(require 'bv-themes)

(defconst bv-light-palette
  '(;; === Foundation Colors ===
    (bg-main . "#ffffff")          ; Pure white
    (fg-main . "#3a3a3a")          ; Charcoal
    (bg-dim . "#f8f8f8")           ; Slightly darker than main
    (fg-dim . "#626262")           ; Slightly lighter than main
    (bg-alt . "#f0f0f0")           ; Alternative background
    (fg-alt . "#505050")           ; Alternative foreground

    ;; === Active/Inactive States ===
    (bg-active . "#e0e0e0")        ; Active elements
    (fg-active . "#0a0a0a")        ; Near black for strong contrast
    (bg-inactive . "#fafafa")      ; Very subtle gray
    (fg-inactive . "#999999")      ; Light gray

    ;; === Accent Colors with Variants ===
    ;; Blue (primary accent)
    (accent-0 . "#5f87d7")         ; Cornflower blue
    (accent-0-faint . "#8fa7e7")   ; Lighter blue
    (accent-0-intense . "#4068c0") ; Deeper blue

    ;; Sage green (secondary accent)
    (accent-1 . "#87afaf")         ; Sage green
    (accent-1-faint . "#a7cfcf")   ; Lighter sage
    (accent-1-intense . "#679f9f") ; Deeper sage

    ;; Terracotta (tertiary accent)
    (accent-2 . "#d7875f")         ; Terracotta
    (accent-2-faint . "#e7a78f")   ; Lighter terracotta
    (accent-2-intense . "#c7653f") ; Deeper terracotta

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
    (bg-hover . "#e8e8e8")         ; Hover state
    (bg-hover-secondary . "#f0f0f0") ; Secondary hover
    (bg-selection . "#ddd7f0")     ; Selection highlight (slight purple tint)
    (bg-highlight . "#fff3da")     ; Search/match highlight (warm yellow)

    ;; === Completion & Search ===
    (bg-completion . "#e0e7ff")    ; Completion selection (blue tint)
    (bg-completion-subtle . "#f0f4ff") ; Subtle completion background
    (fg-completion-match-0 . accent-0-intense)
    (fg-completion-match-1 . accent-1-intense)
    (fg-completion-match-2 . accent-2-intense)
    (fg-completion-match-3 . accent-0) ; Cycles back

    ;; === Diffs & Version Control ===
    (bg-added . "#d0f0d0")         ; Light green
    (fg-added . "#005000")         ; Dark green
    (bg-removed . "#ffd0d0")       ; Light red
    (fg-removed . "#7f0000")       ; Dark red
    (bg-changed . "#fcefcf")       ; Light yellow
    (fg-changed . "#553000")       ; Dark brown

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
    (shadow . "#00000020")         ; Shadow color (transparent black)

    ;; === Special Purpose ===
    (bg-org-block . bg-dim)        ; Org source blocks
    (bg-org-block-begin . bg-alt)  ; Org block delimiters

    ;; === Terminal Colors ===
    (term-black . "#000000")
    (term-red . "#cd0000")
    (term-green . "#00cd00")
    (term-yellow . "#cdcd00")
    (term-blue . "#0000ee")
    (term-magenta . "#cd00cd")
    (term-cyan . "#00cdcd")
    (term-white . "#e5e5e5")

    ;; === Legacy Compatibility ===
    ;; Keep these for backward compatibility
    (foreground . fg-main)
    (background . bg-main)
    (strong . fg-active)
    (faded . fg-dim)
    (subtle . bg-dim)
    (highlight . bg-highlight)
    (mode-line-fg . bg-main))      ; For legacy modeline
  "Comprehensive color palette for light theme.")

(bv-themes-theme bv-light bv-light-palette)

(provide 'bv-light-theme)
;;; bv-light-theme.el ends here
