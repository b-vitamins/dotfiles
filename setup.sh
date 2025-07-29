#!/usr/bin/env bash
# setup.sh - Dotfiles installation and configuration manager
#
# This script manages the installation of configuration files from this
# dotfiles repository to their appropriate locations in the home directory.
# It follows XDG Base Directory specifications and supports machine-specific
# configurations.

set -euo pipefail

# Script metadata
readonly SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
readonly DOTFILES_DIR="$SCRIPT_DIR"

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly BOLD='\033[1m'
readonly RESET='\033[0m'

# Default configuration
DRY_RUN=false
FORCE=false
VERBOSE=false
MACHINE=""
GUIX_ONLY=false
SKIP_TEMPLATES=false
SKIP_HOOKS=false

# -----------------------------------------------------------------------------
# Utility functions
# -----------------------------------------------------------------------------

log() {
    local level="$1"
    shift
    local message="$*"
    local color=""

    case "$level" in
        ERROR)   color="$RED" ;;
        SUCCESS) color="$GREEN" ;;
        WARNING) color="$YELLOW" ;;
        INFO)    color="$BLUE" ;;
        *)       color="$RESET" ;;
    esac

    printf "%b%b[%s]%b %s\n" "$color" "$BOLD" "$level" "$RESET" "$message" >&2
}

die() {
    log ERROR "$@"
    exit 1
}

debug() {
    if [[ "$VERBOSE" == true ]]; then
        log INFO "$@"
    fi
}

confirm() {
    local prompt="$1"
    local response

    if [[ "$FORCE" == true ]]; then
        return 0
    fi

    printf "%b%b[?]%b %s [y/N] " "$YELLOW" "$BOLD" "$RESET" "$prompt"
    read -r response
    [[ "$response" =~ ^[Yy]$ ]]
}

# -----------------------------------------------------------------------------
# Usage and help
# -----------------------------------------------------------------------------

usage() {
    cat << EOF
${BOLD}NAME${RESET}
    $SCRIPT_NAME - Dotfiles installation and configuration manager

${BOLD}SYNOPSIS${RESET}
    $SCRIPT_NAME [OPTIONS]

${BOLD}DESCRIPTION${RESET}
    Installs configuration files from this dotfiles repository to their
    appropriate locations. Supports dry-run mode, machine-specific configs,
    and selective installation.

${BOLD}OPTIONS${RESET}
    -d, --dry-run
        Show what would be done without making changes

    -f, --force
        Overwrite existing files without prompting

    -v, --verbose
        Enable verbose output

    -m, --machine HOSTNAME
        Use configuration for specified machine (default: current hostname)

    -g, --guix-only
        Only install Guix-related configurations

    -s, --skip-templates
        Skip Emacs template regeneration

    -k, --skip-hooks
        Skip Git hooks installation

    -h, --help
        Display this help message

${BOLD}EXAMPLES${RESET}
    # Preview changes without applying them
    $SCRIPT_NAME --dry-run

    # Force installation, overwriting existing files
    $SCRIPT_NAME --force

    # Install only Guix configurations for a specific machine
    $SCRIPT_NAME --guix-only --machine mileva

${BOLD}SUPPORTED CONFIGURATIONS${RESET}
    Managed by this script:
    - Emacs (init files, lisp modules, templates)
    - Guix System (channels.scm only)
    - qBittorrent configuration (if present)
    - Git hooks installation

    Managed by Guix home configuration:
    - Shell (zsh configuration)
    - Git (config, ignore patterns)
    - Terminal (alacritty)
    - Media (mpv with shaders)
    - Development tools (grobid via containers)

EOF
}

# -----------------------------------------------------------------------------
# Argument parsing
# -----------------------------------------------------------------------------

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -d|--dry-run)
                DRY_RUN=true
                shift
                ;;
            -f|--force)
                FORCE=true
                shift
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -m|--machine)
                MACHINE="$2"
                shift 2
                ;;
            -g|--guix-only)
                GUIX_ONLY=true
                shift
                ;;
            -s|--skip-templates)
                SKIP_TEMPLATES=true
                shift
                ;;
            -k|--skip-hooks)
                SKIP_HOOKS=true
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                die "Unknown option: $1\nRun '$SCRIPT_NAME --help' for usage"
                ;;
        esac
    done
}

# -----------------------------------------------------------------------------
# Configuration detection
# -----------------------------------------------------------------------------

detect_environment() {
    # Determine hostname
    if [[ -z "$MACHINE" ]]; then
        MACHINE="$(hostname)"
        debug "Detected hostname: $MACHINE"
    else
        log INFO "Using specified machine: $MACHINE"
    fi

    # Check if we're in a Git repository
    if [[ -d "$DOTFILES_DIR/.git" ]]; then
        debug "Git repository detected"
        readonly IS_GIT_REPO=true
    else
        debug "Not a Git repository"
        readonly IS_GIT_REPO=false
    fi

    # Check for Emacs
    if command -v emacs >/dev/null 2>&1; then
        debug "Emacs found: $(command -v emacs)"
        readonly HAS_EMACS=true
    else
        log WARNING "Emacs not found - template generation will be skipped"
        readonly HAS_EMACS=false
    fi
}

# -----------------------------------------------------------------------------
# Link definitions
# -----------------------------------------------------------------------------

declare -A LINKS
declare -A COPIES

define_links() {
    # Only add non-Guix links if not in guix-only mode
    if [[ "$GUIX_ONLY" == false ]]; then
        # Emacs configuration - ONLY symlink these as they're not in Guix config
        LINKS["$DOTFILES_DIR/emacs/init.el"]="$HOME/.config/emacs/init.el"
        LINKS["$DOTFILES_DIR/emacs/early-init.el"]="$HOME/.config/emacs/early-init.el"
        LINKS["$DOTFILES_DIR/emacs/lisp"]="$HOME/.config/emacs/lisp"
        LINKS["$DOTFILES_DIR/emacs/setup"]="$HOME/.config/emacs/setup"
        LINKS["$DOTFILES_DIR/emacs/templates"]="$HOME/.config/emacs/templates"
        LINKS["$DOTFILES_DIR/emacs/themes"]="$HOME/.config/emacs/themes"
        LINKS["$DOTFILES_DIR/emacs/elfeed.score"]="$HOME/.config/emacs/elfeed.score"

        # Note: The following are handled by Guix home configuration:
        # - Shell (zsh/zshrc, zsh/zshenv)
        # - Git (git/gitconfig, git/gitignore)
        # - Terminal (alacritty/alacritty.toml)
        # - Media (mpv/mpv.conf, mpv/input.conf, mpv/shaders)
        # - Development tools (grobid via oci-container)
        # DO NOT create symlinks for these!

        # qBittorrent is not managed by Guix, so we handle it here
        # We copy instead of symlink because qBittorrent modifies config files
        if [[ -d "$DOTFILES_DIR/qBittorrent" ]]; then
            COPIES["$DOTFILES_DIR/qBittorrent"]="$HOME/.config/qBittorrent"
        fi
    fi

    # Guix configuration (always included unless machine not found)
    if [[ -d "$DOTFILES_DIR/guix" ]]; then
        if [[ -f "$DOTFILES_DIR/guix/machines/$MACHINE.scm" ]]; then
            LINKS["$DOTFILES_DIR/guix/channels.scm"]="$HOME/.config/guix/channels.scm"
            LINKS["$DOTFILES_DIR/guix/machines/$MACHINE.scm"]="$HOME/.config/guix/config.scm"
        else
            log WARNING "No Guix configuration found for machine: $MACHINE"
            log INFO "Available machines:"
            for machine in "$DOTFILES_DIR"/guix/machines/*.scm; do
                [[ -f "$machine" ]] && echo "  - $(basename "$machine" .scm)"
            done
        fi
    fi
}

# -----------------------------------------------------------------------------
# Template generation
# -----------------------------------------------------------------------------

regenerate_templates() {
    local templates_org="$DOTFILES_DIR/emacs/templates.org"

    if [[ "$SKIP_TEMPLATES" == true ]]; then
        debug "Skipping template regeneration (--skip-templates)"
        return 0
    fi

    if [[ ! -f "$templates_org" ]]; then
        log WARNING "templates.org not found - skipping template generation"
        return 0
    fi

    if [[ "$HAS_EMACS" != true ]]; then
        log WARNING "Emacs not available - skipping template generation"
        return 0
    fi

    log INFO "Regenerating Emacs templates from templates.org..."

    if [[ "$DRY_RUN" == true ]]; then
        echo "  Would regenerate templates from: $templates_org"
    else
        # Suppress profile warnings but show other output
        emacs -Q --batch \
            --eval "(require 'org)" \
            --eval "(org-babel-tangle-file \"$templates_org\")" \
            2>&1 | grep -v "^/etc/profile" || true

        if [[ ${PIPESTATUS[0]} -eq 0 ]]; then
            log SUCCESS "Templates regenerated successfully"
        else
            log WARNING "Template regeneration completed with warnings"
        fi
    fi
}

# -----------------------------------------------------------------------------
# Symlink creation
# -----------------------------------------------------------------------------

create_symlink() {
    local source="$1"
    local target="$2"
    local target_dir

    # Check if source exists
    if [[ ! -e "$source" ]]; then
        log ERROR "Source does not exist: $source"
        return 1
    fi

    # Create parent directory if needed
    target_dir="$(dirname "$target")"
    if [[ ! -d "$target_dir" ]]; then
        debug "Creating directory: $target_dir"
        if [[ "$DRY_RUN" == false ]]; then
            mkdir -p "$target_dir" || {
                log ERROR "Failed to create directory: $target_dir"
                return 1
            }
        fi
    fi

    # Handle existing target
    if [[ -e "$target" || -L "$target" ]]; then
        if [[ "$(readlink -f "$target")" == "$(readlink -f "$source")" ]]; then
            debug "Link already correct: $target"
            return 0
        fi

        if [[ "$FORCE" == true ]]; then
            log WARNING "Removing existing target: $target"
            if [[ "$DRY_RUN" == false ]]; then
                rm -rf "$target"
            fi
        else
            if confirm "Overwrite existing file/link: $target?"; then
                if [[ "$DRY_RUN" == false ]]; then
                    rm -rf "$target"
                fi
            else
                log INFO "Skipping: $target"
                return 0
            fi
        fi
    fi

    # Create symlink
    if [[ "$DRY_RUN" == true ]]; then
        echo "  Would create: $source -> $target"
    else
        ln -sf "$source" "$target" && {
            log SUCCESS "Created: $source -> $target"
        } || {
            log ERROR "Failed to create symlink: $target"
            return 1
        }
    fi
}

copy_files() {
    local source="$1"
    local target="$2"
    local target_dir

    # Check if source exists
    if [[ ! -e "$source" ]]; then
        log ERROR "Source does not exist: $source"
        return 1
    fi

    # Create parent directory if needed
    target_dir="$(dirname "$target")"
    if [[ ! -d "$target_dir" ]]; then
        debug "Creating directory: $target_dir"
        if [[ "$DRY_RUN" == false ]]; then
            mkdir -p "$target_dir" || {
                log ERROR "Failed to create directory: $target_dir"
                return 1
            }
        fi
    fi

    # Handle existing target
    if [[ -e "$target" ]]; then
        if [[ "$FORCE" == true ]]; then
            log WARNING "Removing existing target: $target"
            if [[ "$DRY_RUN" == false ]]; then
                rm -rf "$target"
            fi
        else
            log INFO "Target already exists, skipping copy: $target"
            return 0
        fi
    fi

    # Copy files/directory
    if [[ "$DRY_RUN" == true ]]; then
        echo "  Would copy: $source -> $target"
    else
        cp -r "$source" "$target" && {
            log SUCCESS "Copied: $source -> $target"
        } || {
            log ERROR "Failed to copy: $target"
            return 1
        }
    fi
}

# -----------------------------------------------------------------------------
# Git hooks installation
# -----------------------------------------------------------------------------

install_git_hooks() {
    if [[ "$SKIP_HOOKS" == true ]]; then
        debug "Skipping Git hooks installation (--skip-hooks)"
        return 0
    fi

    if [[ "$IS_GIT_REPO" != true ]]; then
        debug "Not a Git repository - skipping hooks"
        return 0
    fi

    local hooks_script="$DOTFILES_DIR/git/hooks/install.sh"
    if [[ ! -f "$hooks_script" ]]; then
        log WARNING "Git hooks install script not found"
        return 0
    fi

    log INFO "Installing Git hooks..."

    if [[ "$DRY_RUN" == true ]]; then
        echo "  Would run: $hooks_script"
    else
        if "$hooks_script"; then
            log SUCCESS "Git hooks installed successfully"
        else
            log WARNING "Git hooks installation failed"
        fi
    fi
}

# -----------------------------------------------------------------------------
# Main execution
# -----------------------------------------------------------------------------

main() {
    log INFO "Dotfiles Setup Script"
    echo "====================="

    # Parse command line arguments
    parse_arguments "$@"

    # Validate conflicting options
    if [[ "$DRY_RUN" == true && "$FORCE" == true ]]; then
        die "Cannot use --dry-run and --force together"
    fi

    # Detect environment
    detect_environment

    # Define configuration links
    define_links

    # Show what we're about to do
    if [[ "$DRY_RUN" == true ]]; then
        log INFO "Running in DRY-RUN mode - no changes will be made"
    fi

    if [[ "$GUIX_ONLY" == true ]]; then
        log INFO "Installing Guix configurations only"
    else
        log INFO "Installing all configurations"
    fi

    echo

    # Regenerate templates if applicable
    if [[ "$GUIX_ONLY" == false ]]; then
        regenerate_templates
        echo
    fi

    # Create symlinks
    log INFO "Creating configuration symlinks..."
    local failed=0
    local created=0
    local copied=0
    local skipped=0

    for source in "${!LINKS[@]}"; do
        if create_symlink "$source" "${LINKS[$source]}"; then
            created=$((created + 1))
        else
            failed=$((failed + 1))
        fi
    done

    # Copy files (for applications that modify their config)
    if [[ ${#COPIES[@]} -gt 0 ]]; then
        log INFO "Copying configuration files..."
        for source in "${!COPIES[@]}"; do
            if copy_files "$source" "${COPIES[$source]}"; then
                copied=$((copied + 1))
            else
                failed=$((failed + 1))
            fi
        done
    fi

    echo

    # Install Git hooks if applicable
    if [[ "$GUIX_ONLY" == false ]]; then
        install_git_hooks
        echo
    fi

    # Summary
    log INFO "Setup Summary"
    echo "============="
    echo "  Created: $created symlinks"
    [[ $copied -gt 0 ]] && echo "  Copied:  $copied directories"
    [[ $failed -gt 0 ]] && echo "  Failed:  $failed operations"
    [[ "$DRY_RUN" == true ]] && echo "  (DRY RUN - no actual changes made)"
    echo

    if [[ $failed -eq 0 ]]; then
        log SUCCESS "Setup completed successfully!"

        # Post-setup advice
        if [[ "$GUIX_ONLY" == false ]]; then
            echo
            log INFO "Next steps:"
            echo "  1. Restart Emacs to load new configuration"
            echo "  2. Source ~/.zshrc or start a new shell"
            echo "  3. Run 'guix pull' to update channels"
        fi
    else
        die "Setup completed with errors"
    fi
}

# Run main function
main "$@"
