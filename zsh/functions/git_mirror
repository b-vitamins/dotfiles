# Function to mirror and compress a Git repository
function git_mirror {
  if [ -z "$1" ]; then
    echo "Usage: git-mirror <repository URL>"
    return 1
  fi

  local repo_url="$1"
  local base_dir="$HOME/library/mirrors/repos"
  local repo_name=$(basename -s .git "$repo_url")
  local target_dir="${base_dir}/${repo_name}.git"
  local archive="${target_dir}.tar.gz"

  mkdir -p "$base_dir"

  if [ -d "$target_dir" ]; then
    echo "Mirror already exists for $repo_url at $target_dir"
  elif [ -f "$archive" ]; then
    echo "Mirror already archived for $repo_url at $archive"
  else {
    git clone --mirror "$repo_url" "$target_dir" && echo "Mirrored $repo_url to $target_dir"
    tar -czf "$archive" -C "$base_dir" "$(basename "$target_dir")"
    rm -rf "$target_dir"
    echo "Compressed and stored $repo_url at ${archive}"
  fi
}
