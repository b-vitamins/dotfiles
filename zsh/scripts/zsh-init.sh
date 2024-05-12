#!/bin/sh

# Base directory for Zsh related files relative to the script's directory
script_dir="$(cd "$(dirname "$0")" && pwd)"
ZSH_BASE_DIR="$script_dir/.."

# Destination directory in the home configuration
ZSH_CONFIG_DIR="$HOME/.config/zsh"

# Ensure the configuration directory exists
if [ ! -d "$ZSH_CONFIG_DIR" ]; then
    mkdir -p "$ZSH_CONFIG_DIR"
fi

# List of configuration files to copy
config_files=("lscolors.sh" "zshrc" "zprofile")

# Copy configuration files
for file in "${config_files[@]}"; do
    cp -f "$ZSH_BASE_DIR/$file" "$ZSH_CONFIG_DIR/$file"
done

# Check if the functions directory exists and copy it recursively
functions_dir="$ZSH_BASE_DIR/functions"
if [ -d "$functions_dir" ]; then
    # Ensure the target functions directory exists
    if [ ! -d "$ZSH_CONFIG_DIR/functions" ]; then
        mkdir -p "$ZSH_CONFIG_DIR/functions"
    fi
    # Copy all function files
    cp -R "$functions_dir/"* "$ZSH_CONFIG_DIR/functions/"
fi

# Set up .zshrc in the home directory
echo "source $ZSH_CONFIG_DIR/zshrc" > "$HOME/.zshrc"

# Set up .zprofile in the home directory
echo "source $ZSH_CONFIG_DIR/zprofile" > "$HOME/.zprofile"

echo "Zsh configuration has been successfully set up."
