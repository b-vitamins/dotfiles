#!/bin/sh

# Base directory for Emacs related files relative to the script's directory
script_dir="$(cd "$(dirname "$0")" && pwd)"
EMACS_BASE_DIR="$script_dir/../"

# Destination directory in the home configuration
EMACS_CONFIG_DIR="$HOME/.config/emacs"

# Remove the old .emacs.d directory if it exists to avoid conflicts
if [ -d "$HOME/.emacs.d" ]; then
    echo "Removing existing .emacs.d directory to avoid configuration conflicts."
    rm -rf "$HOME/.emacs.d"
fi

# Ensure the configuration directory exists
mkdir -p "$EMACS_CONFIG_DIR"

# Function to copy files and directories with care
setup_emacs_environment() {
    # List of items to copy: files and directories
    items_to_copy=("init.el" "early-init.el" "lisp" "setup")

    # Copy each item to the configuration directory
    for item in "${items_to_copy[@]}"; do
        local source_path="$EMACS_BASE_DIR/$item"
        local dest_path="$EMACS_CONFIG_DIR/$item"
        
        # Check if it's a directory or file and copy accordingly
        if [ -d "$source_path" ]; then
            # Create destination directory if it doesn't exist
            mkdir -p "$dest_path"
            # Copy directory contents, avoiding overwrites and deletions
            for src_file in "$source_path/"*; do
                local dest_file="$dest_path/$(basename "$src_file")"
                cp -a "$src_file" "$dest_path"
            done
        elif [ -f "$source_path" ]; then
            cp -a "$source_path" "$dest_path"
        fi
    done
}

# Invoke the function to set up the Emacs environment
setup_emacs_environment

echo "Emacs configuration has been successfully set up."

