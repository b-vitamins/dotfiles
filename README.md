# dotfiles

> Screaming silence hides  
> Bespeckled infinite worlds  
> Chaos tamed, reborn  

A comprehensive GNU/Linux environment using Guix System, modern tools, and Emacs at its core.

## Quick Start

```bash
# Clone and setup
git clone https://github.com/b-vitamins/dotfiles.git ~/projects/dotfiles
cd ~/projects/dotfiles
./setup.sh --machine $(hostname)

# System configuration (choose your machine)
sudo guix system reconfigure ~/projects/dotfiles/guix/machines/mileva.scm  # Workstation
sudo guix system reconfigure ~/projects/dotfiles/guix/machines/spärck.scm  # Laptop
```

## Essential Commands

```bash
# Daily workflow
z <dir>        # Jump to directory (zoxide)
e              # List files (eza with icons)
b <file>       # View file (bat with syntax)
rg <pattern>   # Search content (ripgrep)

# Git essentials  
gs             # Status
gd             # Diff
ga             # Add
gc             # Commit
glog           # Pretty log

# Guix essentials
gsh            # guix shell (temporary environment)
gpi            # guix package (install/remove)
gpl            # guix pull (update)
gsy            # sudo guix system (reconfigure)

# Development
mk             # make -j (parallel build)
venv myenv     # Create Python virtualenv
activate       # Source venv/bin/activate
```

## Key Features

- **Machines**: `mileva` (workstation), `spärck` (laptop) - fully reproducible configs
- **Shell**: Zsh with modern prompt, FZF integration, smart completions
- **Editor**: Emacs 30+ with LSP, tree-sitter, custom modules
- **Tools**: Modern CLI replacements (eza, bat, ripgrep, fd, delta)
- **Security**: GPG for SSH auth, git commit signing, encrypted backups
- **Containers**: Docker + OCI services for databases and ML tools

## Directory Structure

```
dotfiles/
├── guix/           # System configurations
│   ├── machines/   # Machine-specific configs
│   └── modules/    # Custom Guix modules
├── emacs/          # Emacs configuration
│   ├── init.el     # Entry point
│   └── lisp/       # Custom modules (bv-*.el)
├── zsh/            # Shell configuration
├── git/            # Git config and hooks
├── keys/           # Public keys (SSH, GPG)
└── setup.sh        # Installation script
```

## Documentation

- [Emacs Configuration](emacs/README.md) - Modular Emacs setup with 40+ custom modules
- [Guix System](guix/README.md) - Machine configurations and custom services
- [Shell Environment](zsh/README.md) - Zsh configuration and productivity tools
- [Development Setup](docs/development.md) - Language servers, tools, workflows

## Common Tasks

### System Updates

```bash
# Update everything
guix pull && sudo guix system reconfigure ~/projects/dotfiles/guix/machines/$(hostname).scm

# Update user packages only
guix pull && guix package -u

# Clean old generations
guix gc -d 1m  # Delete generations older than 1 month
```

### Environment Management

```bash
# Project-specific environment
echo 'use guix' > .envrc  # In project root
direnv allow

# Temporary shell
guix shell python python-numpy python-pandas

# From manifest
guix shell -m manifest.scm
```

### Emacs Operations

```bash
# Start daemon
emacs --daemon

# Connect client
emacsclient -c  # GUI
emacsclient -t  # Terminal

# Reload config
M-x eval-buffer  # In init.el
```

## Notes

Mileva Marić (1875 – 1948) was among the first women admitted to ETH Zürich’s physics program. Her surviving transcripts reveal strong performance in physics and mathematics, and her letters show that she discussed physics with her classmate—and later husband—Albert Einstein.

Karen Spärck Jones (1935 – 2007) was a British computer scientist whose 1972 paper “A statistical interpretation of term specificity and its application in retrieval” introduced inverse document frequency (IDF), the weighting scheme that underpins modern search engines; she went on to advance automatic language processing, machine-readable thesauri, and the theory of information retrieval.

Konrad Zuse (1910 – 1995) was a German engineer whose Z3 (1941) is widely regarded as the first fully automatic, programmable computer; he later designed the Z4, proposed the high-level language Plankalkül, and launched one of the earliest software companies.

David D. Clark (b. 1944) is an American computer scientist who, as chief protocol architect for the 1980s Internet, guided the evolution of TCP/IP, co-authored formative IETF RFCs, and articulated the “end-to-end” and robustness principles that still shape network design.

## License

GPL-3.0 - Free as in freedom.

---

*Built with Guix System*  
