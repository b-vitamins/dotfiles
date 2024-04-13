#!/bin/sh

# Get the full path to the directory where this script is located
script_dir="$(cd "$(dirname "$0")" && pwd)"
# Set the base directory for snippets relative to the script's directory
base_directory="$script_dir/../snippets"

# List of repositories and their branches. Format: "url branch"
repositories=(
    "https://github.com/b-vitamins/latex-snippets.git master"
)

# Define the modes you expect to manage as snippets (add more as needed)
modes=("org-mode")

# Function to pull or add a specific mode's snippets from a given repository
pull_mode_snippets() {
    local repo_details="$1"
    local mode=$2
    local repo_url=$(echo $repo_details | cut -d ' ' -f 1)
    local branch=$(echo $repo_details | cut -d ' ' -f 2)
    local target_directory="$base_directory/$mode"

    # Ensure the target directory exists
    if [ ! -d "$target_directory" ]; then
        echo "Creating directory $target_directory"
        mkdir -p "$target_directory"
    fi

    # Clean any untracked files and directories to avoid conflicts
    git clean -fdx

    # Stash any existing changes, including untracked files
    git stash push --include-untracked -m "Stash changes before subtree operation"

    # Attempt to add the subtree if it does not exist
    echo "Attempting to add subtree for $mode..."
    git subtree add --prefix="$target_directory" $repo_url $branch --squash --prefix="$mode" 2>/dev/null

    # Always attempt to pull updates
    echo "Pulling updates for $mode from $repo_url branch $branch into $target_directory..."
    git subtree pull --prefix="$target_directory" $repo_url $branch --squash --prefix="$mode"

    # Pop the stashed changes
    git stash pop
}

# Loop through all repositories and modes, then pull updates
for repo_details in "${repositories[@]}"; do
    for mode in "${modes[@]}"; do
        pull_mode_snippets "$repo_details" $mode
    done
done

echo "All snippets have been updated."
