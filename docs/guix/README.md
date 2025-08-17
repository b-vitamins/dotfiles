# Guix System Configuration

Declarative system configuration for GNU/Linux using Guix.

## Overview

This configuration provides reproducible system definitions for multiple machines, with shared modules for common functionality.

## Machines

### mileva (Workstation)
- AMD Ryzen 9 5900X (24 threads)
- 64GB RAM
- NVIDIA RTX 3060
- Primary development machine

### sparck (Laptop)
- ThinkPad configuration
- 32GB RAM
- Battery optimization
- Mobile development

## Quick Start

### System Reconfiguration
```bash
# Update system
sudo guix system reconfigure ~/projects/dotfiles/guix/machines/$(hostname).scm

# Test changes (dry run)
sudo guix system reconfigure --dry-run ~/projects/dotfiles/guix/machines/$(hostname).scm
```

### Package Management
```bash
# Update user packages
guix pull
guix package -u

# Install from manifest
guix package -m ~/projects/dotfiles/guix/manifests/core-manifest.scm

# Temporary environment
guix shell python python-numpy python-pandas
```

## Directory Structure

```
guix/
├── channels.scm           # Package channels configuration
├── machines/              # Machine-specific configs
│   ├── mileva.scm        # Workstation
│   └── sparck.scm        # Laptop
├── modules/              # Shared service modules
│   ├── desktop.scm       # Desktop environment
│   ├── nvidia.scm        # NVIDIA driver support
│   └── containers.scm    # Docker/Podman setup
└── manifests/            # Package collections
    ├── core-manifest.scm      # Essential packages
    ├── development-manifest.scm # Dev tools
    └── emacs-manifest.scm     # Emacs packages
```

## Configuration Structure

### Machine Definition
Each machine configuration includes:
- Bootloader setup
- File system layout
- Network configuration
- User accounts
- System services
- Package selection

### Service Modules
Reusable service configurations:
- Desktop environments (GNOME/KDE)
- Development tools
- Container runtimes
- Database services
- Hardware-specific settings

## Common Tasks

### System Updates
```bash
# Full system update
guix pull && sudo guix system reconfigure ~/projects/dotfiles/guix/machines/$(hostname).scm

# Update only user packages
guix pull && guix package -u
```

### Generation Management
```bash
# List system generations
sudo guix system list-generations

# Switch to previous generation
sudo guix system roll-back

# Delete old generations (older than 1 month)
sudo guix system delete-generations 1m

# Garbage collection
guix gc -d 1m
```

### Package Search
```bash
# Search available packages
guix search tensorflow

# Show package details
guix show python-numpy

# List installed packages
guix package -I
```

### Development Environments
```bash
# Python project
echo 'use guix python python-numpy python-pandas' > .envrc
direnv allow

# Custom manifest
cat > manifest.scm << EOF
(specifications->manifest
 '("python"
   "python-numpy"
   "python-pandas"))
EOF
guix shell -m manifest.scm
```

## Service Configuration

### Desktop Services
- GNOME desktop with Wayland
- NetworkManager for connectivity
- CUPS for printing
- PulseAudio for sound
- Bluetooth support

### Development Services
- Docker daemon
- PostgreSQL
- Redis
- Nginx

### System Services
- OpenSSH server
- mDNS/Avahi
- Earlyoom (OOM prevention)
- Thermald (thermal management)

## Hardware Support

### NVIDIA Configuration
For machines with NVIDIA GPUs:
```scheme
(use-modules (nongnu packages nvidia))
(use-service-modules nvidia)

(operating-system
  ;; ...
  (kernel linux)
  (kernel-loadable-modules (list nvidia-driver))
  (services
    (cons* (service nvidia-service-type)
           ;; other services
           )))
```

### Laptop Optimizations
- TLP for power management
- Backlight control
- Touchpad configuration
- Suspend/hibernate support

## Troubleshooting

### Boot Issues
```bash
# Boot previous generation from GRUB menu
# Select "GNU system, old configurations"

# Repair from live USB
guix system init /path/to/config.scm /mnt
```

### Package Conflicts
```bash
# Show package dependencies
guix graph package-name | dot -Tpdf > graph.pdf

# Force specific version
guix install package-name@version
```

### Service Debugging
```bash
# View service logs
sudo herd status
sudo journalctl -u service-name

# Restart service
sudo herd restart service-name
```

## Best Practices

1. **Version Control**: Keep configurations in git
2. **Test Changes**: Use `--dry-run` before reconfiguring
3. **Regular Cleanup**: Remove old generations monthly
4. **Manifest Usage**: Define project dependencies explicitly
5. **Channel Pinning**: Lock channel versions for reproducibility

## Resources

- [Guix Manual](https://guix.gnu.org/manual/)
- [Guix Cookbook](https://guix.gnu.org/cookbook/)
- [Nonguix Channel](https://gitlab.com/nonguix/nonguix) (for proprietary drivers)