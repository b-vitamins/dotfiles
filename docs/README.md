# Dotfiles Documentation

Comprehensive documentation for all components of the dotfiles repository.

## System Configuration

### Core Components
- [Emacs Configuration](emacs/) - Research-oriented editor with 40+ custom modules
- [Guix System](guix/) - Declarative GNU/Linux configuration
- [Shell Environment](shell/zsh.md) - Zsh setup, aliases, and productivity tools

### Command-Line Utilities

#### Audio/Video Processing
- [transcode](bin/transcode.md) - Audio format conversion with resume capability
- [remux](bin/remux.md) - Video container conversion without re-encoding

#### Research Tools
- [harvester](bin/harvester.md) - ArXiv paper aggregation and organization

## Quick Navigation

### Emacs Documentation
- [Overview & Architecture](emacs/README.md)
- [Installation Guide](emacs/installation.md)
- [Keybinding Reference](emacs/keybindings.md)
- [Customization Guide](emacs/customization.md)
- [Workflow Patterns](emacs/workflow-guide.md)

### System Administration
- [Guix System Guide](guix/README.md)
- [Machine Configurations](guix/README.md#machines)
- [Package Management](guix/README.md#package-management)

### Development Tools
- [Shell Configuration](shell/zsh.md)
- [Git Aliases & Workflows](shell/zsh.md#git-integration)
- [Development Environments](shell/zsh.md#development-environments)

## Essential Reference

### Command-Line Utilities

| Tool | Purpose | Common Usage |
|------|---------|--------------|
| `transcode` | Convert audio formats | `transcode -i /music/flac --from .flac --to .mp3` |
| `remux` | Change video containers | `remux -i /videos --from .m2ts --to .mkv` |
| `harvester` | Download papers | `harvester --categories cs.LG cs.AI` |

### Key Features

- **Modular Design**: Each component is self-contained and configurable
- **Resume Capability**: Audio/video tools support resuming interrupted jobs
- **Performance**: Parallel processing and optimized defaults
- **Integration**: Components work together seamlessly

## Getting Started

1. **New to this setup?** Start with the [Shell Environment](shell/zsh.md)
2. **Setting up Emacs?** Follow the [Installation Guide](emacs/installation.md)
3. **Managing the system?** Read the [Guix Documentation](guix/README.md)
4. **Need specific tools?** Check the utility guides in [bin/](bin/)