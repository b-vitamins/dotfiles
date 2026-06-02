# Emacs Configuration Customization Guide

Configuration customization without modifying source files. All settings use
defcustom variables stored in `custom-file` (defaults to `custom.el` inside
`user-emacs-directory`, typically `~/.config/emacs/custom.el` with this setup).

## Quick Start

Add customizations to your `custom-file` (usually `~/.config/emacs/custom.el`):

```elisp
(custom-set-variables
 '(bv-fonts-default-family "JetBrains Mono")
 '(bv-fonts-default-size 140)
 '(bv-themes-toggle-themes '(bv-light bv-dark))
 '(bv-themes-intensity 'balanced)
 '(bv-org-slipbox-directory "~/org/myslipbox"))
```

Reload: `M-x eval-buffer` in custom.el or restart Emacs.

## Core Configuration Variables

### Fonts

**bv-fonts-default-family** (string)
- Default: "Iosevka Term"
- Monospaced font for code editing

**bv-fonts-default-size** (integer)
- Default: 120
- Font size in tenths of points (120 = 12pt)

**bv-fonts-variable-family** (string)
- Default: "IBM Plex Sans"
- Variable-width font for UI elements

**bv-fonts-serif-family** (string)
- Default: "Source Serif 4"
- Serif font for reading modes

**bv-fonts-enable-ligatures** (boolean)
- Default: t
- Enable programming ligatures

**bv-fonts-display-overrides** (alist)
- Default: explicit monitor-name keyed `:size` values
- Per-display font sizes for GUI frames

Example:
```elisp
(custom-set-variables
 '(bv-fonts-default-family "Source Code Pro")
 '(bv-fonts-default-size 130)
 '(bv-fonts-enable-ligatures nil))
```

### Theme System

The current theme system is DSL-authored. Broad runtime behavior is controlled
with defcustoms; detailed appearance belongs in theme specifications such as
`emacs/themes/bv-light-theme.el` and `emacs/themes/bv-dark-theme.el`.

**bv-themes-default-theme** (symbol or nil)
- Default: nil
- Theme used by inspection commands when no BV theme is active

**bv-themes-toggle-themes** (list of symbols)
- Default: nil
- Ordered theme pair used by `bv-themes-toggle`

**bv-themes-theme-directories** (list of directories)
- Default: repository `emacs/themes/`
- Directories searched for `*-theme.el` BV theme specifications

**bv-themes-intensity** (symbol)
- Default: balanced
- Options: faint, balanced, vivid, high-chroma
- Global chroma scale applied while compiling theme tokens

**bv-themes-bold-constructs** (boolean)
- Default: t
- Allow roles to use stronger weights where the active theme specifies them

**bv-themes-italic-constructs** (boolean)
- Default: t
- Allow italic roles, mainly comments and documentation

**bv-themes-no-underlines** (boolean)
- Default: t
- Remove underlines after loading a BV theme

**bv-themes-audit-on-load** (boolean)
- Default: nil
- Run the policy-aware BV audit whenever a BV theme is enabled

**bv-themes-font-family-monospaced** (string or nil)
- Default: nil
- Monospaced family used when compiling theme typography

**bv-themes-font-family-proportional** (string or nil)
- Default: nil
- Proportional family used when compiling theme typography

**bv-themes-font-size** (integer)
- Default: 120
- Default face height in tenths of points used during theme compilation

Theme example:
```elisp
(custom-set-variables
 '(bv-themes-toggle-themes '(bv-light bv-dark))
 '(bv-themes-default-theme 'bv-dark)
 '(bv-themes-intensity 'balanced)
 '(bv-themes-no-underlines t))
```

Theme authoring example:

```elisp
;; Place a file named bv-custom-theme.el in a directory listed in
;; `bv-themes-theme-directories`.
(bv-themes-define-theme bv-custom
  (metadata
   :display-name "BV Custom"
   :summary "Personal BV theme profile."
   :tags '(dark personal)
   :family 'personal
   :version 1)
  (variant dark :polarity -1 :modes '(gui tty))
  (anchors
   (neutral
    (bg-main (oklch 0.2686 0.0097 268.3))
    (bg-dim (oklch 0.2478 0.0099 268.3))
    (fg-main (oklch 0.8296 0.0165 253.9)))
   (state
    (red (oklch 0.6709 0.1448 17.0))
    (orange (oklch 0.5871 0.0679 64.3))
    (yellow (oklch 0.7653 0.0764 86.6))
    (green (oklch 0.6179 0.0793 132.8)))
   (accent
    (teal (oklch 0.6259 0.0745 218.1))
    (cyan (oklch 0.6000 0.0694 214.7))
    (blue (oklch 0.6136 0.0907 244.3))
    (purple (oklch 0.5624 0.1140 318.3))
    (magenta (oklch 0.5598 0.1028 348.0))))
  (policy
   :underlines nil
   :contrast-target 'aesthetic
   :terminal-colors 256
   :gamut 'srgb))
```

### Circadian (Automatic Theme Switching)

**bv-circadian-latitude** (float)
- Default: 12.9716 (Bangalore)
- Geographic latitude for sunrise/sunset

**bv-circadian-longitude** (float)
- Default: 77.5946 (Bangalore)
- Geographic longitude for sunrise/sunset

**bv-circadian-enabled-p** (boolean)
- Default: t
- Enable automatic light/dark switching

**bv-circadian-transition-time** (integer)
- Default: 30
- Minutes for gradual theme transition

Location example:
```elisp
(custom-set-variables
 '(bv-circadian-latitude 40.7128)    ; New York City
 '(bv-circadian-longitude -74.0060)
 '(bv-circadian-transition-time 15))
```

## Module-Specific Variables

### Org-Slipbox (Knowledge Management)

**bv-org-slipbox-directory** (string)
- Default: "~/org/myslipbox"
- Root directory for the slipbox store

**bv-org-slipbox-notes-directory** (string)
- Default: "notes/"
- Relative subdirectory for ordinary slipbox notes

**bv-org-slipbox-show-backlinks** (boolean)
- Default: t
- Display backlink counts in slipbox completions

### Org LaTeX

**bv-org-latex-auto-scale** (boolean)
- Default: t
- Follow the configured per-display preview scale

**bv-org-latex-display-overrides** (alist)
- Default: monitor-name keyed plist values
- Explicit per-display `:scale` settings for Org/OFLP previews

### Completion System

**bv-completion-auto-modes** (list)
- Default: '(prog-mode text-mode)
- Modes with automatic completion

**bv-consult-ripgrep-or-line-limit** (integer)
- Default: 300000
- Line count threshold for ripgrep vs line search

**bv-consult-enable-xdg-recent** (boolean)
- Default: nil
- Use XDG recent files list

### File Operations

**bv-tramp-default-method** (string)
- Default: "ssh"
- Default remote access protocol

**bv-tramp-ssh-config-file** (string)
- Default: "~/.ssh/config"
- SSH configuration file location

### Media and External Tools

**bv-emms-music-dir** (string)
- Default: "~/Music"
- Music library directory

**bv-ytdl-downloads-dir** (string)
- Default: "~/Downloads"
- YouTube download location

**bv-mpv-seek-step** (integer)
- Default: 3
- Seconds to seek forward/backward

### System Integration

**bv-weather-location** (string)
- Default: "Bangalore"
- City for weather display

**bv-weather-format** (string)
- Default: "%c %t"
- Weather display format string

**bv-pulseaudio-volume-step** (string)
- Default: "5%"
- Volume adjustment increment

**bv-power-loginctl-path** (string)
- Default: "loginctl"
- Path to loginctl command

### Development Tools

**bv-geiser-default-implementation** (symbol)
- Default: 'guile
- Scheme implementation for evaluation

**bv-guix-directory** (string)
- Default: "~/projects/guix"
- Local Guix source directory

**bv-gptel-default-model** (string)
- Default: "gpt-4"
- Default AI model for gptel

**bv-ellama-default-provider** (string)
- Default: "local"
- Local LLM provider

### Icons and Visual Elements

**bv-nerd-icons-font-family** (string or nil)
- Default: nil
- Explicit icon font override; nil follows `bv-fonts-icon-family`

**bv-nerd-icons-scale-factor** (float)
- Default: 1.0
- Icon size multiplier

**bv-nerd-icons-style** (symbol)
- Default: semantic
- Options: semantic, muted, monochrome, native
- Color policy for BV-owned icon roles

**bv-nerd-icons-completion-category-policy** (alist)
- Default: files/projects/notes keep icons; command and symbol surfaces require
  wide completion windows
- Controls icon density by completion category

**bv-nerd-icons-role-alist** (alist)
- Central role registry for BV-owned icon choices

## Common Customization Scenarios

### Large Display Setup
```elisp
(custom-set-variables
 '(bv-fonts-default-size 140)
 '(bv-themes-font-size 140)
 '(bv-org-latex-default-scale 1.4))
```

### Minimal Appearance
```elisp
(custom-set-variables
 '(bv-themes-intensity 'faint)
 '(bv-themes-bold-constructs nil)
 '(bv-themes-italic-constructs nil)
 '(bv-nerd-icons-style 'muted))
```

### Research-Focused
```elisp
(custom-set-variables
 '(bv-org-slipbox-directory "~/research/slipbox")
 '(bv-fonts-serif-family "Crimson Text")
 '(bv-themes-font-family-proportional "Crimson Text"))
```

### Performance Tuning
```elisp
(custom-set-variables
 '(bv-consult-ripgrep-or-line-limit 100000)
 '(bv-themes-audit-on-load nil)
 '(bv-nerd-icons-style 'monochrome))
```

### Dark Theme Preference
```elisp
(custom-set-variables
 '(bv-circadian-enabled-p nil))

;; In init.el after theme loading:
(bv-themes-load-theme 'bv-dark)
```

## Module Loading Control

Modules are loaded via `require` calls in `emacs/init.el`. Disable a module by
commenting out its `require` line and restarting Emacs.

## Configuration File Locations

**Main Settings**: `custom-file` (defaults to `custom.el` in `user-emacs-directory`)
**History**: `history` in `user-emacs-directory` (savehist)
**Backups**: `backups/` in `user-emacs-directory`
**Auto-saves**: `auto-saves/` in `user-emacs-directory`

## Debugging Customizations

Check variable values:
```elisp
M-x describe-variable RET bv-fonts-default-family RET
```

Test theme changes:
```elisp
M-x bv-themes-load-theme RET bv-light RET
```

Validate custom.el syntax:
```elisp
M-x check-parens
```

Reset to defaults:
```elisp
(custom-set-variables
 '(bv-variable-name (custom--standard-value 'bv-variable-name)))
```

## Variable Index

### Core
- bv-circadian-enabled-p
- bv-circadian-latitude
- bv-circadian-longitude
- bv-fonts-default-family
- bv-fonts-default-size

### Development
- bv-completion-auto-modes
- bv-geiser-default-implementation
- bv-guix-directory
- bv-tramp-default-method

### Media
- bv-emms-music-dir
- bv-mpv-seek-step
- bv-ytdl-downloads-dir

### Organization
- bv-org-directory
- bv-org-latex-display-overrides
- bv-org-slipbox-directory
- bv-org-slipbox-notes-directory
- bv-org-slipbox-show-backlinks

### System
- bv-nerd-icons-font-family
- bv-nerd-icons-scale-factor
- bv-nerd-icons-style
- bv-nerd-icons-completion-category-policy
- bv-nerd-icons-role-alist
- bv-power-loginctl-path
- bv-pulseaudio-volume-step
- bv-weather-location

### Theme Engine
- bv-themes-audit-on-load
- bv-themes-bold-constructs
- bv-themes-default-theme
- bv-themes-font-family-monospaced
- bv-themes-font-family-proportional
- bv-themes-font-size
- bv-themes-intensity
- bv-themes-italic-constructs
- bv-themes-no-underlines
- bv-themes-theme-directories
- bv-themes-toggle-themes
- bv-themes-variants

All variables support standard Emacs customization interface via `M-x customize-group RET bv RET`.
