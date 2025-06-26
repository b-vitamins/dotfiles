# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a comprehensive dotfiles repository for a GNU/Linux development environment using GNU Guix as the system configuration manager and Emacs as the primary editor. The repository follows a modular, declarative configuration approach.

## Key Context

- **User**: Ayan Das (b), graduate student at Indian Institute of Science
- **Machines**: 
  - `mileva`: AMD Ryzen 9 5900X workstation with RTX 3060, 64GB RAM (primary)
  - `spärck`: ThinkPad laptop with 32GB RAM (mobile)
- **Preferences**: Direct communication, minimal comments, research-driven approach
- **Git**: Conventional commits, no GPG signing required

## Common Development Commands

### Initial Setup
```bash
# Clone and setup for a new machine
git clone https://github.com/b-vitamins/dotfiles.git ~/projects/dotfiles
cd ~/projects/dotfiles
./setup.sh --machine <hostname>  # Create configuration symlinks

# For dry-run (preview changes):
./setup.sh --dry-run --machine <hostname>
```

### Guix System Management
```bash
# Reconfigure system (requires sudo)
sudo guix system reconfigure ~/projects/dotfiles/guix/machines/$(hostname).scm

# Update user packages from manifests
guix package -m ~/projects/dotfiles/guix/manifests/core-manifest.scm
guix package -m ~/projects/dotfiles/guix/manifests/development-manifest.scm
guix package -m ~/projects/dotfiles/guix/manifests/emacs-manifest.scm

# Style formatting for Scheme files  
guix style -f <file.scm>
```

### Emacs Development
```elisp
;; Run all tests
M-x ert RET t RET

;; Run specific test suite
M-x load-file RET ~/projects/dotfiles/emacs/lisp/test/bv-ui-tests.el
M-x bv-ui-run-tests

;; Reload configuration module
M-x eval-buffer (in the module file)
```

### Testing
- Emacs tests are in `emacs/lisp/test/` using ERT framework
- Test naming convention: `bv-module-test-feature-aspect`
- Run individual test: `M-x ert RET test-name RET`

## High-Level Architecture

### Module System Design

#### Emacs Configuration (`emacs/`)
The Emacs configuration implements a custom module system with:

1. **Core Infrastructure** (`bv-core.el`):
   - Feature registration with dependency tracking
   - Centralized configuration value storage (`bv-config-values`)
   - XDG-compliant path management
   - Lazy loading utilities with idle timers

2. **Module Loading Strategy**:
   - Immediate: Core modules (defaults, UI, navigation)
   - 0.1s idle: Productivity tools (completion, git, org)
   - 1.0s idle: Research tools (org-roam, citation, language modes)
   - On-demand: Language-specific configurations

3. **Key Module Categories**:
   - `bv-defaults.el`: Base Emacs configuration
   - `bv-ui.el`: Theme, fonts, visual elements
   - `bv-completion.el`: Vertico/Corfu completion system
   - `bv-development.el`: Programming tools and LSP
   - `bv-org.el`: Org-mode and knowledge management
   - `bv-lang-*.el`: Language-specific configurations

#### Guix Configuration (`guix/`)
The Guix system configuration uses:

1. **Machine-Specific Configs**: `machines/{hostname}.scm`
   - Each machine has its own system declaration
   - Shared modules for common functionality

2. **Manifest Organization**: `manifests/`
   - Purpose-specific package collections
   - Language-specific development environments
   - Separated by concern (core, development, emacs, etc.)

3. **Custom Services**: `modules/`
   - Desktop environment configurations
   - Hardware-specific settings (nvidia)
   - Container and virtualization support

### Configuration Flow

1. **System Bootstrap**:
   ```
   Guix System Config → Package Installation → Service Activation
   ```

2. **User Environment**:
   ```
   Shell (Zsh) → Direnv → Emacs Server → Application Configs
   ```

3. **Development Workflow**:
   ```
   Project Directory → Direnv Environment → LSP/Tools → Emacs Integration
   ```

### Key Design Patterns

1. **Declarative Configuration**: Everything is code (Scheme/Elisp)
2. **Lazy Loading**: Performance optimization through deferred loading
3. **Feature Detection**: Runtime capability checking with fallbacks
4. **Modular Activation**: Components can be enabled/disabled independently
5. **Configuration Inheritance**: Machine-specific overrides of base configs

### Integration Points

- **Path Management**: Centralized through Guix profiles and bv-core paths
- **Theme Consistency**: Shared Modus themes across Emacs and terminal
- **Service Discovery**: mDNS for cross-machine communication
- **Development Environments**: Direnv + Guix for reproducible environments

## Important Conventions

1. **Commit Messages**: Use conventional commit format (feat:, fix:, docs:, etc.)
2. **Code Style**: Run `./scripts/style.sh` for Scheme files
3. **Testing**: Add tests for new Emacs functionality in `emacs/lisp/test/`
4. **Documentation**: Update relevant README files when adding features
5. **Machine-Specific**: Test changes on target machine before committing

## Current Machines

- `mileva`: Primary development workstation (64GB RAM, RTX 3060, multiple NVMe drives)
- `spärck`: ThinkPad laptop (32GB RAM, optimized for battery life)
- Machine detection via hostname in setup scripts

## Code Style Guidelines

### Emacs Lisp
- Use `bv-` prefix for all custom functions and variables
- Declare external variables and functions to avoid warnings
- Prefer `when` over single-branch `if`
- Use `defcustom` with `:group` for user-facing variables

### Scheme/Guix
- Always run `guix style -f` before committing
- Use meaningful variable names (avoid abbreviations)
- Group related imports together
- Comment non-obvious service configurations

### Shell/Zsh
- Use modern command alternatives (eza, bat, ripgrep)
- Keep aliases short and memorable
- Functions should be POSIX-compatible where possible
- Use `(( ))` for arithmetic, `[[ ]]` for conditionals

### General
- No trailing whitespace
- Consistent indentation (spaces, not tabs)
- Keep lines under 100 characters when reasonable
- Descriptive commit messages following conventional format