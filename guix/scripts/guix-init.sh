#!/bin/sh

# Establish the base directory relative to the script's location
base_dir="$(cd "$(dirname "$0")" && pwd)"
guix_source_dir="$base_dir/../"

# Define the destination directory for the Guix configuration
guix_config_dir="$HOME/.config/guix"

# Copies essential Guix files
sync_essential_files() {
    local essentials=("config.scm" "channels.scm" "manifests" "keys")

    for essential in "${essentials[@]}"; do
        local source_path="$guix_source_dir/$essential"
        local target_path="$guix_config_dir/$essential"
        
        if [ -d "$source_path" ]; then
            mkdir -p "$target_path"
            cp -a "$source_path/"* "$target_path/"
        else
            cp -a "$source_path" "$target_path"
        fi
    done
}

# Create configuration directory if it doesn't exist
mkdir -p "$guix_config_dir"

# Execute the sync functions
sync_essential_files

# Change directory to home
cd "$HOME"