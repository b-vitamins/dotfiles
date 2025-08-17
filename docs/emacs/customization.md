# Emacs Configuration Customization Guide

Configuration customization without modifying source files. All settings use defcustom variables stored in `~/.emacs.d/custom.el`.

## Quick Start

Add customizations to your `~/.emacs.d/custom.el` file:

```elisp
(custom-set-variables
 '(bv-fonts-default-family "JetBrains Mono")
 '(bv-fonts-default-size 140)
 '(bv-themes-syntax 'alt-syntax)
 '(bv-org-roam-directory "~/notes"))
```

Reload: `M-x eval-buffer` in custom.el or restart Emacs.

## Core Configuration Variables

### Fonts

**bv-fonts-default-family** (string)
- Default: "Fira Code"
- Monospaced font for code editing

**bv-fonts-default-size** (integer)
- Default: 120
- Font size in tenths of points (120 = 12pt)

**bv-fonts-variable-family** (string)
- Default: "IBM Plex Sans"
- Variable-width font for UI elements

**bv-fonts-serif-family** (string)
- Default: "ET Book"
- Serif font for reading modes

**bv-fonts-enable-ligatures** (boolean)
- Default: t
- Enable programming ligatures

Example:
```elisp
(custom-set-variables
 '(bv-fonts-default-family "Source Code Pro")
 '(bv-fonts-default-size 130)
 '(bv-fonts-enable-ligatures nil))
```

### Theme System

**bv-themes-syntax** (symbol)
- Default: nil
- Options: nil, alt-syntax, deuteranopia, tritanopia
- Controls syntax highlighting intensity

**bv-themes-mode-line** (symbol)
- Default: nil
- Options: nil, borderless, accented, moody, padded
- Mode line appearance

**bv-themes-headings** (symbol)
- Default: nil
- Options: nil, monochrome, rainbow, highlight, no-bold
- Org/markdown heading styles

**bv-themes-bold-constructs** (boolean)
- Default: t
- Bold keywords and constructs

**bv-themes-italic-constructs** (boolean)
- Default: nil
- Italic comments and strings

**bv-themes-variable-pitch-ui** (boolean)
- Default: nil
- Variable-width fonts in UI

**bv-themes-org-blocks** (symbol)
- Default: nil
- Options: nil, gray-background, tinted-background
- Org code block styling

**bv-themes-completions** (symbol)
- Default: nil
- Options: nil, opinionated, moderate
- Completion interface styling

**bv-themes-paren-match** (symbol)
- Default: 'intense
- Options: subtle-warm, intense, subtle-cool
- Parentheses matching highlight

Theme example:
```elisp
(custom-set-variables
 '(bv-themes-syntax 'alt-syntax)
 '(bv-themes-mode-line 'borderless)
 '(bv-themes-headings 'rainbow)
 '(bv-themes-org-blocks 'tinted-background))
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

### Org-Roam (Knowledge Management)

**bv-org-roam-directory** (string)
- Default: "~/documents/slipbox/slips"
- Main directory for roam notes

**bv-org-roam-show-backlinks** (boolean)
- Default: t
- Display backlinks in roam buffers

### Org LaTeX

**bv-org-latex-scale** (float)
- Default: 2.0
- Preview image scaling factor

**bv-org-latex-auto-scale** (boolean)
- Default: t
- Automatic DPI-based scaling

**bv-org-latex-base-dpi** (integer)
- Default: 96
- Reference DPI for scaling calculations

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

**bv-nerd-icons-font-family** (string)
- Default: "Symbols Nerd Font Mono"
- Icon font family

**bv-nerd-icons-scale-factor** (float)
- Default: 1.0
- Icon size multiplier

**bv-nerd-icons-color-icons** (boolean)
- Default: t
- Enable colored icons

## Common Customization Scenarios

### Large Display Setup
```elisp
(custom-set-variables
 '(bv-fonts-default-size 140)
 '(bv-themes-ui-density 'compact)
 '(bv-org-latex-scale 2.5))
```

### Minimal Appearance
```elisp
(custom-set-variables
 '(bv-themes-mode-line 'borderless)
 '(bv-themes-fringes 'subtle)
 '(bv-themes-syntax nil)
 '(bv-nerd-icons-color-icons nil))
```

### Research-Focused
```elisp
(custom-set-variables
 '(bv-org-roam-directory "~/research/notes")
 '(bv-themes-headings 'rainbow)
 '(bv-themes-org-blocks 'tinted-background)
 '(bv-fonts-serif-family "Crimson Text"))
```

### Performance Tuning
```elisp
(custom-set-variables
 '(bv-consult-ripgrep-or-line-limit 100000)
 '(bv-themes-syntax nil)
 '(bv-nerd-icons-color-icons nil))
```

### Dark Theme Preference
```elisp
(custom-set-variables
 '(bv-circadian-enabled-p nil))

;; In init.el after theme loading:
(bv-themes-load-theme 'bv-dark)
```

## Module Loading Control

Modules load automatically via idle timers. Disable by removing require statements in init.el:

**Immediate Loading** (0s):
- bv-defaults, bv-fonts, bv-themes, bv-modeline

**Quick Loading** (0.1s):
- bv-completion, bv-git, bv-org

**Delayed Loading** (1.0s+):
- bv-org-roam, bv-citation, language modes

## Configuration File Locations

**Main Settings**: `~/.emacs.d/custom.el`
**Theme Cache**: `~/.cache/emacs/themes/`
**Org Data**: `~/.local/share/emacs/org/`
**TRAMP History**: `~/.emacs.d/tramp-history`

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
- bv-themes-syntax

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
- bv-org-latex-scale
- bv-org-roam-directory
- bv-org-roam-show-backlinks

### System
- bv-nerd-icons-font-family
- bv-power-loginctl-path
- bv-pulseaudio-volume-step
- bv-weather-location

### Theme Engine
- bv-themes-bold-constructs
- bv-themes-completions
- bv-themes-headings
- bv-themes-mode-line
- bv-themes-org-blocks
- bv-themes-paren-match
- bv-themes-variable-pitch-ui

All variables support standard Emacs customization interface via `M-x customize-group RET bv RET`.