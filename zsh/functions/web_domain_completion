# Function to generate domain names for completion from compressed website archives
function _web_checkout_domains {
  local base_dir="$HOME/library/mirrors/websites"
  # List only .tar.gz files, remove the extension, and extract the domain name
  reply=($(ls -1 "$base_dir"/*.tar.gz | sed 's|^.*/||' | sed 's/\.tar\.gz$//'))
}
