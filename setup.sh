#!/bin/sh
# Setup script for initial configuration after cloning the repository

# Path to the update snippets script
UPDATE_SNIPPETS_SCRIPT="./emacs/scripts/update-snippets.sh"

echo "Configuring the repository..."
# Ensure the update snippets script is executable
chmod +x $UPDATE_SNIPPETS_SCRIPT

echo "Updating snippets..."
# Execute the update snippets script
$UPDATE_SNIPPETS_SCRIPT

echo "Repository setup complete. Snippets have been updated."
