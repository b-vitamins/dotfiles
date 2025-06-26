#!/bin/bash
# Install git hooks for the dotfiles repository

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Get the script directory
HOOKS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HOOKS_DIR/../.." && pwd)"
GIT_HOOKS_DIR="$REPO_ROOT/.git/hooks"

echo "Installing git hooks for dotfiles repository..."

# Check if we're in a git repository
if [ ! -d "$REPO_ROOT/.git" ]; then
    echo -e "${RED}Error: Not in a git repository${NC}" >&2
    exit 1
fi

# Create hooks directory if it doesn't exist
mkdir -p "$GIT_HOOKS_DIR"

# Install hooks
install_hook() {
    local hook_name=$1
    local source_file="$HOOKS_DIR/$hook_name"
    local target_file="$GIT_HOOKS_DIR/$hook_name"
    
    if [ -f "$source_file" ]; then
        if [ -f "$target_file" ] && [ ! -L "$target_file" ]; then
            echo -e "${YELLOW}Warning: $hook_name already exists and is not a symlink${NC}"
            echo -n "Backup and replace? [y/N] "
            read -r response
            if [[ "$response" =~ ^[Yy]$ ]]; then
                mv "$target_file" "$target_file.backup.$(date +%Y%m%d%H%M%S)"
                echo -e "Backed up existing hook to $target_file.backup.*"
            else
                echo -e "Skipping $hook_name"
                return
            fi
        fi
        
        # Create symlink
        ln -sf "$source_file" "$target_file"
        echo -e "${GREEN}✓ Installed $hook_name${NC}"
    fi
}

# Install all hooks
install_hook "commit-msg"
install_hook "pre-commit"

echo -e "\n${GREEN}Git hooks installed successfully!${NC}"
echo -e "\nInstalled hooks:"
echo -e "  - commit-msg: Enforces conventional commit format"
echo -e "  - pre-commit: Checks for code quality issues"
echo -e "\nTo skip hooks temporarily, use: git commit --no-verify"