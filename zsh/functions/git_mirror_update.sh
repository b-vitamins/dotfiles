# Function to update all Git repository mirrors
function git_mirror_update {
  local base_dir="$HOME/library/mirrors/repos"

  for repo in "$base_dir"/*.tar.gz; do
    local repo_dir="${repo%.tar.gz}"
    mkdir -p "$repo_dir"
    tar -xzf "$repo" -C "$repo_dir" --strip-components=1

    echo "Updating mirror for $(basename "$repo_dir")..."
    git -C "$repo_dir" fetch --all

    tar -czf "$repo" -C "$base_dir" "$(basename "$repo_dir")"
    rm -rf "$repo_dir"
    echo "Updated and recompressed $(basename "$repo_dir")"
  done
}
