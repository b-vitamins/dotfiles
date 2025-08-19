# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a comprehensive dotfiles repository for a GNU/Linux development environment using GNU Guix as the system configuration manager and Emacs as the primary editor. The repository follows a modular, declarative configuration approach.

## Infrastructure Integration

@~/.claude/instructions/guix-workflow.md
@~/.claude/instructions/code-quality.md
@~/.claude/instructions/agent-chains.md
@~/.claude/instructions/system-administration.md

## Key Context

- **User**: Ayan Das (b), graduate student at Indian Institute of Science
- **Machines**: 
  - `mileva`: AMD Ryzen 9 5900X workstation with RTX 3060, 64GB RAM (primary)
  - `sparck`: ThinkPad laptop with 32GB RAM (mobile)
- **Preferences**: Direct communication, minimal comments, research-driven approach
- **Git**: Conventional commits, no GPG signing required

## Proactive Agent Usage

### Critical Maintenance Agents (Run Automatically)
- **security-secret-scanner**: Scan before every commit for API keys, credentials
- **git-commit-formatter**: Ensure conventional commit format compliance
- **config-validator**: Validate Guix scheme files and shell scripts
- **guix-manifest-updater**: Keep package manifests current and consistent

### Code Quality Agents (Use Proactively)
- **elisp-formatter**: Format all Emacs Lisp files before commits
- **shell-script-linter**: Validate shell scripts with ShellCheck
- **scheme-style-checker**: Ensure Guix configuration follows style guide
- **dependency-patch-updater**: Safe dependency updates before breaking changes

### System Administration Chains
- **system-config-validator** → **guix-reconfigure-helper** → **service-status-checker**
- **manifest-dependency-checker** → **package-conflict-resolver**
- **dotfiles-sync-validator** → **cross-machine-diff-analyzer**

## Dotfiles Agent Chains

### System Reconfiguration Workflow
1. **config-validator** - Validate all Guix scheme files
2. **security-secret-scanner** - Check for exposed secrets
3. **guix-manifest-updater** - Update package manifests
4. **system-config-validator** - Full system config validation
5. **guix-reconfigure-helper** - Guided reconfiguration process

### Emacs Configuration Updates
1. **elisp-formatter** - Format all .el files
2. **emacs-config-tester** - Test configuration loading
3. **elisp-package-checker** - Validate package dependencies
4. **emacs-performance-analyzer** - Check startup times and resource usage

### Shell Environment Updates
1. **shell-script-linter** - Validate all shell scripts
2. **zsh-config-validator** - Check Zsh configuration syntax
3. **alias-conflict-detector** - Find conflicting aliases/functions
4. **environment-variable-checker** - Validate PATH and other env vars

### Cross-Machine Synchronization
1. **dotfiles-sync-validator** - Check sync status across machines
2. **machine-config-differ** - Compare configurations between machines
3. **hardware-specific-checker** - Validate hardware-specific configs
4. **service-compatibility-checker** - Ensure services work on target machine

## Quality Control Pipeline

### Pre-Commit Checklist (Mandatory)
```bash
# 1. Security scan
security-secret-scanner --path .

# 2. Validate Guix configurations
config-validator --type guix --recursive guix/

# 3. Format Scheme files
guix style -f $(find guix/ -name "*.scm")

# 4. Lint shell scripts
shell-script-linter --recursive scripts/

# 5. Format Emacs Lisp
elisp-formatter --recursive emacs/lisp/

# 6. Test Emacs configuration
emacs-config-tester --dry-run

# 7. Git commit formatting
git-commit-formatter --validate-message
```

### System Deployment Pipeline
```bash
# 1. Full configuration validation
system-config-validator --machine $(hostname)

# 2. Dependency conflict checking
manifest-dependency-checker --manifests guix/manifests/

# 3. Backup current system state
guix-system-backup --create pre-reconfigure-$(date +%Y%m%d)

# 4. Dry-run reconfiguration
sudo guix system reconfigure --dry-run guix/machines/$(hostname).scm

# 5. Actual reconfiguration
guix-reconfigure-helper --machine $(hostname)
```

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

# System rollback if needed
sudo guix system roll-back

# List system generations
sudo guix system list-generations

# Delete old generations (keep last 5)
sudo guix system delete-generations 5d
```

### Manifest Management
```bash
# Update all manifests with latest versions
guix-manifest-updater --path guix/manifests/ --update-policy safe

# Check for security updates
guix pull && guix package --list-available | grep security

# Export current profile to manifest
guix package --export-manifest > current-profile.scm

# Install from specific manifest with dry-run
guix package -m manifest.scm --dry-run
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

;; Check configuration performance
M-x emacs-startup-profiler

;; Validate package configuration
M-x package-lint-current-buffer
```

### Testing
- Emacs tests are in `emacs/lisp/test/` using ERT framework
- Test naming convention: `bv-module-test-feature-aspect`
- Run individual test: `M-x ert RET test-name RET`
- System tests: `./scripts/test-system.sh --machine <hostname>`

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

## Advanced Workflows

### Cross-Machine Configuration Sync
```bash
# Compare configurations between machines
machine-config-differ --source mileva --target sparck

# Sync specific configuration modules
dotfiles-sync-validator --modules emacs,zsh --target sparck

# Test configuration on different hardware
hardware-specific-checker --config guix/machines/sparck.scm --target mileva
```

### Performance Optimization
```bash
# Analyze Emacs startup performance
emacs-performance-analyzer --profile startup --iterations 10

# Optimize Guix system performance
guix-system-optimizer --target desktop --memory 64GB

# Check service resource usage
service-performance-checker --services network,desktop,development
```

### Backup and Recovery
```bash
# Create full system state backup
guix-system-backup --create full-$(hostname)-$(date +%Y%m%d)

# Export all user package profiles
for manifest in guix/manifests/*.scm; do
    guix package --export-manifest -p ~/.guix-profile > "backup-$(basename $manifest)"
done

# Restore configuration from backup
guix-system-restore --backup full-mileva-20240115 --target sparck
```

## Important Conventions

1. **Commit Messages**: Use conventional commit format (feat:, fix:, docs:, etc.)
2. **Code Style**: Run `./scripts/style.sh` for Scheme files
3. **Testing**: Add tests for new Emacs functionality in `emacs/lisp/test/`
4. **Documentation**: Update relevant README files when adding features
5. **Machine-Specific**: Test changes on target machine before committing
6. **Agent Usage**: Always run security-secret-scanner before commits
7. **Configuration Validation**: Use config-validator before system reconfiguration

## Git Commit Guidelines

**CRITICAL**: Before committing, ALWAYS:
1. Run `security-secret-scanner --path .` to check for secrets
2. Run `git status` to review what will be committed
3. Use `git add <specific-files>` instead of `git add -A` or `git add .`
4. If you see many unrelated files staged (especially emacs/), STOP and unstage them
5. Only commit files you explicitly modified for the current task
6. Run `git-commit-formatter --validate-message` before committing
7. When in doubt, commit files one at a time

**Common Pitfall**: The emacs/ directory often has many uncommitted changes from ongoing work. Never accidentally commit these unless specifically working on emacs configuration.

## Current Machines

- `mileva`: Primary development workstation (64GB RAM, RTX 3060, multiple NVMe drives)
- `sparck`: ThinkPad laptop (32GB RAM, optimized for battery life)
- Machine detection via hostname in setup scripts

## Code Style Guidelines

### Emacs Lisp
- Use `bv-` prefix for all custom functions and variables
- Declare external variables and functions to avoid warnings
- Prefer `when` over single-branch `if`
- Use `defcustom` with `:group` for user-facing variables
- Run `elisp-formatter` before committing
- Use `emacs-config-tester` to validate changes

### Scheme/Guix
- Always run `guix style -f` before committing
- Use meaningful variable names (avoid abbreviations)
- Group related imports together
- Comment non-obvious service configurations
- Validate with `config-validator --type guix`
- Use `scheme-style-checker` for consistency

### Shell/Zsh
- Use modern command alternatives (eza, bat, ripgrep)
- Keep aliases short and memorable
- Functions should be POSIX-compatible where possible
- Use `(( ))` for arithmetic, `[[ ]]` for conditionals
- Run `shell-script-linter` on all scripts
- Validate with `zsh-config-validator`

### General
- No trailing whitespace
- Consistent indentation (spaces, not tabs)
- Keep lines under 100 characters when reasonable
- Descriptive commit messages following conventional format
- Always use agents for quality control
- Run full pipeline before system reconfiguration

## Troubleshooting

### Common Issues
1. **Guix reconfiguration fails**: Use `config-validator` to check syntax
2. **Package conflicts**: Run `manifest-dependency-checker`
3. **Emacs startup slow**: Use `emacs-performance-analyzer`
4. **Cross-machine sync issues**: Use `dotfiles-sync-validator`

### Recovery Procedures
1. **System rollback**: `sudo guix system roll-back`
2. **Profile rollback**: `guix package --roll-back`
3. **Emacs config reset**: Load minimal configuration and rebuild
4. **Full system restore**: Use backup created by `guix-system-backup`