# Emacs Configuration Documentation

A research-oriented Emacs environment with 40+ custom modules, optimized for performance and aesthetics.

## Overview

This configuration implements a modular architecture inspired by nano-emacs, emphasizing clean design, performance optimization, and research workflows. Built for Emacs 30+ with native compilation support.

## Quick Start

### First Time Setup

```bash
# Install required fonts
guix install font-fira-code font-ibm-plex nerd-fonts

# Launch Emacs
emacs

# Install language servers (as needed)
M-x eglot-install-server
```

### Essential Commands

| Command | Key | Description |
|---------|-----|-------------|
| Find file | `C-x C-f` | Enhanced with Vertico completion |
| Switch buffer | `C-x b` | Consult-buffer with preview |
| Search project | `C-c p R` | Ripgrep search in project |
| Git status | `C-x g` | Open Magit |
| Capture note | `C-c n c` | Org-roam capture |

### Key Concepts

1. **Vertico/Consult** - Modern completion system replacing Helm/Ivy
2. **Embark** - Contextual actions on any target
3. **Corfu** - In-buffer completion replacing Company
4. **Org-roam** - Zettelkasten note-taking system
5. **Eglot** - Built-in LSP client

## Documentation Guide

### For New Users
1. Start with [Workflow Guide](workflow-guide.md) - Learn efficient patterns
2. Review [Keybindings](keybindings.md) - Complete reference
3. Explore [Customization](customization.md) - Make it yours

### For Power Users
1. [Module Reference](modules/) - Deep dive into features
2. [Recipes](recipes/) - Advanced configurations
3. [Contributing](contributing.md) - Extend the system

### Quick References
- [All Keybindings](keybindings.md)
- [All Variables](customization.md#variable-index)
- [Module Matrix](modules/README.md#feature-matrix)

## Architecture

### Module System

```
┌─────────────────────────────────────────────┐
│                  init.el                     │
│            (Linear module loading)           │
└────────────────┬───────────────────────────┘
                 │
    ┌────────────┴────────────┐
    │                         │
┌───▼──────┐          ┌──────▼──────┐
│  Core    │          │  Features   │
│ Modules  │          │  Modules    │
├──────────┤          ├─────────────┤
│ layout   │          │ completion  │
│ themes   │          │ org-mode    │
│ defaults │          │ development │
│ fonts    │          │ languages   │
└──────────┘          └─────────────┘
```

### Loading Strategy

1. **Immediate**: Core UI and defaults
2. **Idle (0.1s)**: Completion and navigation
3. **Idle (1.0s)**: Development tools
4. **On-demand**: Language modes and specialized tools

### Design Principles

- **Modularity**: Self-contained features with clear boundaries
- **Performance**: Lazy loading and startup optimization
- **Aesthetics**: Sophisticated theme system with visual consistency
- **Integration**: Tools work together seamlessly
- **Simplicity**: No complex dependency management

## Feature Highlights

### Research Workflow
- **Org-roam**: Slip-box method for knowledge management
- **Citations**: Academic reference management with Citar
- **LaTeX**: High-DPI preview and comprehensive export

### Development Environment
- **LSP Support**: Automatic language server integration
- **Project Management**: Intelligent project detection
- **Git Integration**: Comprehensive Magit workflow

### User Experience
- **Smart Completion**: Context-aware with orderless matching
- **Visual Excellence**: Modus themes with extensive customization
- **Efficient Navigation**: Ace-window, Avy, and Consult

## Directory Structure

```
docs/emacs/
├── README.md              # This file
├── installation.md        # Setup guide
├── keybindings.md        # Complete key reference
├── customization.md      # Configuration options
├── workflow-guide.md     # Practical patterns
├── troubleshooting.md    # Common issues
├── contributing.md       # Development guide
├── modules/              # Module documentation
│   ├── core.md
│   ├── completion.md
│   ├── development.md
│   ├── org.md
│   └── ...
└── recipes/              # Configuration examples
    ├── research-workflow.md
    ├── development-setup.md
    └── ...
```

## Getting Help

1. **Built-in Help**: `C-h` commands work everywhere
2. **Which-key**: Shows available commands after prefix
3. **Documentation**: Comprehensive guides in this directory
4. **Source Code**: Well-commented elisp files

## Performance

- **Startup Time**: ~0.3s with full configuration
- **Native Compilation**: Enabled for all packages
- **Lazy Loading**: Deferred loading for heavy features
- **Optimizations**: GC tuning, process output handling

## Philosophy

This configuration embodies:

1. **Aesthetics Matter**: Beautiful tools inspire better work
2. **Keyboard Driven**: Mouse optional, keyboard optimal
3. **Context Preservation**: Stay in Emacs, maintain flow
4. **Progressive Disclosure**: Simple defaults, deep customization
5. **Research First**: Optimized for thinking and writing

---

Next: [Installation Guide](installation.md) | [Workflow Guide](workflow-guide.md) | [Keybindings](keybindings.md)