# Function to generate repository names for completion from compressed archives
function _git_checkout_repos {
  local base_dir="$HOME/library/mirrors/repos"
  reply=($(ls -1 "$base_dir"/*.tar.gz | sed 's|^.*/||' | sed 's/\.git\.tar\.gz$//'))
}
