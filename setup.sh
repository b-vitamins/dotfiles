#!/bin/bash

# Function to display usage information
usage() {
    echo "Usage: $0 [--dry-run] [--force]"
    echo "  --dry-run      Simulate the script without making any changes."
    echo "  --force        Remove existing files before creating symlinks."
    exit 1
}

# Parse command-line arguments
DRY_RUN=false
FORCE=false

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --dry-run) DRY_RUN=true; shift ;;
        --force) FORCE=true; shift ;;
        *) usage ;;
    esac
done

# Handle conflicting flags
if [ "$DRY_RUN" = true ] && [ "$FORCE" = true ]; then
    echo "Error: --dry-run and --force cannot be used together."
    exit 1
fi

# Get the hostname
HOSTNAME=$(hostname)

# Define the base directory for dotfiles
DOTFILES_DIR=~/projects/dotfiles

# Define source and target pairs
declare -A links=(
    ["$DOTFILES_DIR/emacs/init.el"]="~/.config/emacs/init.el"
    ["$DOTFILES_DIR/emacs/early-init.el"]="~/.config/emacs/early-init.el"
    ["$DOTFILES_DIR/emacs/lisp"]="~/.config/emacs/lisp"
    ["$DOTFILES_DIR/emacs/setup"]="~/.config/emacs/setup"
    ["$DOTFILES_DIR/alacritty/alacritty.toml"]="~/.config/alacritty/alacritty.toml"
    ["$DOTFILES_DIR/.gitconfig"]="~/.gitconfig"
)

# Add hostname-dependent Guix symlinks
GUIX_MACHINE_DIR="$DOTFILES_DIR/guix/machines/$HOSTNAME"
if [ -d "$GUIX_MACHINE_DIR" ]; then
    links["$GUIX_MACHINE_DIR/config.scm"]="~/.config/guix/config.scm"
    links["$GUIX_MACHINE_DIR/home-config.scm"]="~/.config/guix/home-config.scm"
    links["$DOTFILES_DIR/keys"]="~/.config/guix/keys"
else
    echo "Warning: No configuration found for hostname '$HOSTNAME'. Skipping Guix links."
fi

# Function to update snippets
update_snippets() {
    local local_base_dir="$HOME/.config/emacs/snippets"
    local temp_dir="/tmp/snippets"

    # Define repositories and their respective directories to sync
    local repos=(
        "https://github.com/b-vitamins/latex-snippets.git master org-mode"
    )

    # Prepare the temporary directory
    mkdir -p "$temp_dir"

    # Process each repository
    for repo in "${repos[@]}"; do
        IFS=' ' read -r repo_url branch modes <<< "$repo"

        # Clone or pull the repository into the temp directory
        local repo_name=$(basename "$repo_url" .git)
        local repo_path="$temp_dir/$repo_name"

        if [ ! -d "$repo_path" ]; then
            echo "Cloning repository '$repo_url'..."
            if [ "$DRY_RUN" = false ]; then
                git clone --branch "$branch" "$repo_url" "$repo_path"
            else
                echo " (would clone: $repo_url)"
            fi
        else
            echo "Updating repository '$repo_name'..."
            if [ "$DRY_RUN" = false ]; then
                (cd "$repo_path" && git pull origin "$branch")
            else
                echo " (would update: $repo_name)"
            fi
        fi

        # Copy each specified mode directory
        for mode in ${modes}; do
            local source_path="$repo_path/$mode"
            local dest_path="$local_base_dir/$mode"
            mkdir -p "$dest_path"
            
            echo "Copying snippets from '$source_path' to '$dest_path'..."
            if [ "$DRY_RUN" = false ]; then
                find "$source_path" -type f | while read -r file; do
                    local dest_file="${dest_path}/${file#$source_path/}"
                    cp -a "$file" "$dest_file"
                done
            else
                find "$source_path" -type f | while read -r file; do
                    local dest_file="${dest_path}/${file#$source_path/}"
                    echo " (would copy: $file -> $dest_file)"
                done
            fi
        done
    done

    # Cleanup the temporary directory
    if [ "$DRY_RUN" = false ]; then
        rm -rf "$temp_dir"
    else
        echo " (would remove temporary directory: $temp_dir)"
    fi

    echo "All specified snippet modes have been updated."
}

# Run the update snippets function only if not in dry run
if [ "$DRY_RUN" = false ]; then
    echo "Configuring the repository..."
    echo "Updating snippets..."
    update_snippets
else
    echo "Configuring the repository (dry run)..."
    echo " (snippets would be updated)"
fi

# Create symlinks
for src in "${!links[@]}"; do
    target=${links[$src]}
    
    # Expand ~ to the home directory
    target_expanded=$(eval echo "$target")

    # Check if source file/directory exists
    if [ ! -e "$src" ]; then
        echo "Error: Source '$src' does not exist. Skipping."
        continue
    fi
    
    # Create target directory if it doesn't exist
    target_dir="$(dirname "$target_expanded")"
    if [ ! -d "$target_dir" ]; then
        echo "Warning: Target directory '$target_dir' does not exist. Creating it."
        if [ "$DRY_RUN" = false ]; then
            mkdir -p "$target_dir" || { echo "Error: Failed to create directory '$target_dir'. Exiting."; exit 1; }
        else
            echo " (would create directory: $target_dir)"
        fi
    fi
    
    # Check if the target already exists
    if [ -e "$target_expanded" ]; then
        echo "Warning: Target '$target_expanded' already exists."
        if [ "$FORCE" = true ]; then
            echo "Removing existing target before creating symlink."
            if [ "$DRY_RUN" = false ]; then
                rm -rf "$target_expanded"
            else
                echo " (would remove: $target_expanded)"
            fi
        else
            if [ "$DRY_RUN" = false ]; then
                echo "Skipping symlink creation."
                continue
            else
                echo " (would be skipped in dry run)"
            fi
        fi
    fi
    
    # Create the symlink
    if [ "$DRY_RUN" = false ]; then
        ln -sf "$src" "$target_expanded" && echo "Created symlink: $src -> $target_expanded" || { echo "Error: Failed to create symlink."; exit 1; }
    else
        echo " (would create symlink: $src -> $target_expanded)"
    fi
done

echo "Symlink setup complete."
