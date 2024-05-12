# Function to checkout an archived website mirror to ~/library/mirrors/checkout
function web_checkout {
  local domain="$1"
  local base_dir="$HOME/library/mirrors/websites"
  local checkout_dir="$HOME/library/mirrors/checkout/$domain"
  local target_archive="${base_dir}/${domain}.tar.gz"

  if [ -z "$domain" ]; then
    echo "Usage: web-mirror-checkout <domain>"
    return 1
  fi

  if [ ! -f "$target_archive" ]; then
    echo "Archived website not found for $domain"
    return 1
  fi

  # Create the checkout directory if it doesn't exist
  mkdir -p "$checkout_dir"

  # Extract the archived website to the checkout directory
  tar -xzf "$target_archive" -C "$HOME/library/mirrors/checkout"

  echo "Website checked out to $checkout_dir"
}
