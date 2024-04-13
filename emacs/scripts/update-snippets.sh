#!/bin/sh

# Base directory for snippets relative to the script's directory
script_dir="$(cd "$(dirname "$0")" && pwd)"
LOCAL_BASE_DIR="$script_dir/../snippets"

# Temporary directory for cloning
TEMP_DIR="/tmp/snippets"

# Define repositories and their respective directories to sync
REPOS=(
    "https://github.com/b-vitamins/latex-snippets.git master org-mode"
)

# Function to clone, pull, and copy snippets
update_snippets() {
    local repo_url=$1
    local branch=$2
    shift 2 # Remove the first two arguments (repo URL and branch)
    local modes=("$@")

    # Clone or pull the repository into the temp directory
    local repo_name=$(basename "$repo_url" .git)
    local repo_path="$TEMP_DIR/$repo_name"
    if [ ! -d "$repo_path" ]; then
        git clone --branch "$branch" "$repo_url" "$repo_path"
    else
        (cd "$repo_path" && git pull origin "$branch")
    fi

    # Copy each specified mode directory
    for mode in "${modes[@]}"; do
        local source_path="$repo_path/$mode"
        local dest_path="$LOCAL_BASE_DIR/$mode"
        mkdir -p "$dest_path"
        
        # Copy contents using cp, updating existing files
        find "$source_path" -type f | while read file; do
            local dest_file="${dest_path}/${file#$source_path/}"
            cp -a "$file" "$dest_file"
        done
    done
}

# Prepare the temporary directory
mkdir -p "$TEMP_DIR"

# Process each repository
for repo in "${REPOS[@]}"; do
    IFS=' ' read -r repo_url branch modes <<< "$repo"
    update_snippets $repo_url $branch ${modes}
done

# Cleanup the temporary directory
rm -rf "$TEMP_DIR"

echo "All specified snippet modes have been updated."
