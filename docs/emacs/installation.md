# Emacs Installation Guide

## Prerequisites

### System Requirements
- Emacs 30.0+ with native compilation
- Git 2.25+
- Guix package manager (recommended) or system package manager

### Required Fonts
```bash
# Using Guix
guix install font-fira-code font-ibm-plex nerd-fonts

# Alternative: manual installation
# Download from:
# - https://github.com/tonsky/FiraCode
# - https://github.com/IBM/plex
# - https://www.nerdfonts.com/
```

### Optional Dependencies
- `ripgrep` - Fast searching (required for Consult ripgrep)
- `fd` - File finding (optional enhancement)
- `pandoc` - Document conversion
- `graphviz` - Graph visualization
- Language servers (see Language Support section)

## Installation Steps

### 1. Clone Configuration
```bash
# Configuration is part of dotfiles
cd ~/projects/dotfiles
./setup.sh --machine $(hostname)
```

### 2. First Launch
```bash
# Start Emacs
emacs

# Wait for package installation (first run only)
# Check *Messages* buffer for progress
```

### 3. Verify Installation
```elisp
;; Run these commands with M-:
(featurep 'bv-core)          ; Should return t
(featurep 'bv-themes)        ; Should return t
```

## Language Server Setup

### Python
```bash
# Ruff LSP (recommended)
guix install python-ruff

# Alternative: Pyright
npm install -g pyright
```

### Rust
```bash
# Rust analyzer
rustup component add rust-analyzer
```

### JavaScript/TypeScript
```bash
# TypeScript language server
npm install -g typescript typescript-language-server
```

### C/C++
```bash
# Clangd
guix install clang-toolchain
```

### Go
```bash
# Gopls
go install golang.org/x/tools/gopls@latest
```

## Troubleshooting

### Fonts Not Displaying
```bash
# Refresh font cache
fc-cache -fv

# Verify fonts installed
fc-list | grep -i fira
fc-list | grep -i nerd
```

### Slow Startup
```elisp
;; Profile startup
M-x esup

;; Check load times
(emacs-init-time)
```

### Package Installation Issues
```elisp
;; Refresh package list
M-x package-refresh-contents

;; Install missing packages manually
M-x package-install RET package-name RET
```

### Native Compilation Warnings
```elisp
;; Suppress warnings (add to early-init.el)
(setq native-comp-async-report-warnings-errors 'silent)
```

## Post-Installation

### Set Location (for Circadian)
```elisp
;; Add to custom.el
(custom-set-variables
 '(bv-circadian-latitude 12.9716)   ; Your latitude
 '(bv-circadian-longitude 77.5946)) ; Your longitude
```

### Configure Org-roam
```elisp
;; Set slip box location
(custom-set-variables
 '(bv-org-roam-directory "~/documents/slipbox/slips"))

;; Initialize database
M-x org-roam-db-sync
```

### Enable Optional Features
```elisp
;; Example: Enable keycast mode
(keycast-mode 1)

;; Example: Enable global git-gutter
(global-git-gutter-mode 1)
```

## Validation

### Check Core Features
1. Open file: `C-x C-f` (should show Vertico)
2. Search buffer: `C-s` (should show Consult)
3. Git status: `C-x g` (should open Magit)
4. Complete code: Type partial word and wait (should show Corfu)

### Verify Theme System
```elisp
;; Toggle theme
M-x modus-themes-toggle

;; Check circadian
M-x bv-circadian-status
```

## Next Steps

1. Review [Workflow Guide](workflow-guide.md) for usage patterns
2. Customize via [Customization Guide](customization.md)
3. Learn [Keybindings](keybindings.md)
4. Explore [Module Documentation](modules/)