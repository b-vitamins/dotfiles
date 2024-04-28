#!/bin/sh

# Configuration and source directories
guix_config_dir="$HOME/.config/guix"
source_manifest_dir="$(cd "$(dirname "$0")" && pwd)/../manifests"

# Check for necessary directories and abort if not found
if [ ! -d "$guix_config_dir/current" ] || [ ! -d "$guix_config_dir" ]; then
    echo "The Guix profile or current environment is not properly set up. Aborting."
    exit 1
fi

# Function to verify installation of packages from manifest
verify_manifest_installation() {
    local manifests_path="$guix_config_dir/manifests"
    local all_packages_installed=true

    for manifest in "$manifests_path/"*; do
        while read -r package; do
            if ! guix package --list-installed | grep -q "$package"; then
                echo "Package $package from manifest $manifest is not installed."
                all_packages_installed=false
                break 2
            fi
        done < "$manifest"
    done

    echo $all_packages_installed
}

# Function to sync manifests and detect changes
sync_manifests() {
    local changes_detected=false
    local target_manifest_dir="$guix_config_dir/manifests"

    if [ ! -d "$target_manifest_dir" ]; then
        # Manifests directory does not exist, copy everything
        mkdir -p "$target_manifest_dir"
        cp -a "$source_manifest_dir/"* "$target_manifest_dir/"
        changes_detected=true
    else
        # Check for new or modified files
        for file in "$source_manifest_dir/"*; do
            local filename=$(basename "$file")
            local target_file="$target_manifest_dir/$filename"
            if [ ! -f "$target_file" ] || ! cmp -s "$file" "$target_file"; then
                cp -a "$file" "$target_file"
                changes_detected=true
            fi
        done
    fi

    echo $changes_detected
}

# Sync manifests and check for changes
manifests_updated=$(sync_manifests)

# Verify if all packages in manifests are installed
all_packages_installed=$(verify_manifest_installation)

# Construct and execute the package installation command if needed
if $manifests_updated || ! $all_packages_installed; then
    cd "$HOME"
		pull_cmd="guix pull"
    install_cmd="guix package"
    
    # Build the command by appending each manifest
    for manifest in "$guix_config_dir/manifests/"*; do
        install_cmd="$install_cmd -m $manifest"
    done
    
    install_cmd="$install_cmd -c  12"
    
    # Execute the constructed command
		echo "Running: $pull_cmd"
    eval "$pull_cmd"
    echo "Running command to install packages: $install_cmd"
    eval "$install_cmd"
else
    echo "No updates to manifests detected and all packages are installed. No installation needed."
fi
