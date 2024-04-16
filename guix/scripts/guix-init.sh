#!/bin/sh

# Establish the base directory relative to the script's location
base_dir="$(cd "$(dirname "$0")" && pwd)"
guix_source_dir="$base_dir/../"

# Define the destination directory for the Guix configuration
guix_config_dir="$HOME/.config/guix"

# Copies essential Guix files except channels.scm
sync_essential_files() {
    local essentials=("config.scm" "manifests" "keys")

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

# Manages the channels.scm file and decides if guix pull is needed
manage_channels_update() {
    local source_channels="$guix_source_dir/channels.scm"
    local target_channels="$guix_config_dir/channels.scm"
    
    if [ ! -f "$target_channels" ]; then
        # No channels.scm at target, copy and mark for guix pull
        cp -a "$source_channels" "$target_channels"
        should_pull_update=true
    else
        # channels.scm exists, compare with the source
        if ! cmp -s "$source_channels" "$target_channels"; then
            cp -a "$source_channels" "$target_channels"
            should_pull_update=true
        elif [ ! -d "$guix_config_dir/current" ]; then
            # No current build folder, trigger guix pull
            should_pull_update=true
        fi
    fi
}

# Create configuration directory if it doesn't exist
mkdir -p "$guix_config_dir"

# Default to no pull unless conditions require it
should_pull_update=false
if [ ! -d "$guix_config_dir/current" ]; then
    should_pull_update=true
fi

# Execute the sync functions
sync_essential_files
manage_channels_update

# Change directory to home
cd "$HOME"

# Execute guix pull if necessary
if $should_pull_update; then
    if guix pull; then
        echo "Guix has been successfully updated and the environment is set up."
    else
        echo "Failed to update Guix. Please check your setup and try again."
    fi
else
    echo "No updates are necessary. Your Guix environment is up-to-date."
fi

