# Function to update an archived website mirror
function web_mirror_update {
  local base_dir="$HOME/library/mirrors/websites"

  if [ ! -d "$base_dir" ]; then
    echo "The directory $base_dir does not exist. Please ensure your mirrored websites are stored there."
    return 1
  fi

  # Iterate through all .tar.gz archives and update their contents
  for archive in "$base_dir"/*.tar.gz; do
    local domain=$(basename "$archive" .tar.gz)
    local temp_dir="${base_dir}/${domain}"

    echo "Updating mirror for $domain..."

    # Extract the current archive contents to a temporary directory
    tar -xzf "$archive" -C "$base_dir"

    # Use wget to update the website in the temporary directory
    wget -r -p -np -k -c -N -P "$temp_dir" "http://$domain"

    # Repack the updated directory back into the archive
    tar -czf "$archive" -C "$base_dir" "$domain"

    # Remove the temporary directory after updating
    rm -rf "$temp_dir"
  done
}
