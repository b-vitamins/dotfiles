#!/bin/sh

# Base directory for Alacritty related files relative to the script's directory
script_dir="$(cd "$(dirname "$0")" && pwd)"
ALACRITTY_BASE_DIR="$script_dir/.."

# Destination directory in the home configuration
ALACRITTY_CONFIG_DIR="$HOME/.config/alacritty"

# Ensure the configuration directory exists
if [ ! -d "$ALACRITTY_CONFIG_DIR" ]; then
    mkdir -p "$ALACRITTY_CONFIG_DIR"
fi

# Copy the alacritty.toml configuration file
cp -f "$ALACRITTY_BASE_DIR/alacritty.toml" "$ALACRITTY_CONFIG_DIR/alacritty.toml"

echo "Alacritty configuration has been successfully set up."
