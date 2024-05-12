# Function to mirror a website to ~/library/mirrors/websites
function web_mirror {
  if [ -z "$1" ]; then
    echo "Usage: web-mirror <URL>"
    return 1
  fi

  local url="$1"
  local base_dir="$HOME/library/mirrors/websites"
  local domain=$(echo "$url" | awk -F/ '{print $3}')
  local temp_dir="${base_dir}/${domain}"
  local target_archive="${base_dir}/${domain}.tar.gz"

  # Check if the archive already exists
  if [ -f "$target_archive" ]; then
    echo "Website already archived for $url at $target_archive"
    return 0
  fi

  # Create temporary directory if it doesn't exist
  mkdir -p "$temp_dir"

  # Use wget to mirror the website with specified options
  wget -r -p -np -k -c -N -P "$temp_dir" "$url"

  # Archive the website content
  tar -czf "$target_archive" -C "$base_dir" "$domain"

  # Remove the temporary directory after archiving
  rm -rf "$temp_dir"

  echo "Website archived at $target_archive"
}
