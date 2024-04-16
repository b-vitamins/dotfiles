#!/bin/sh

# Save the current working directory
original_dir=$(pwd)

# Move to the base Guix directory which is the parent of the current script's directory
cd "$(dirname "$0")/.."

# Find all .scm files and apply guix style -f to each
find . -type f -name "*.scm" -exec guix style -f {} \;

# Return to the original working directory
cd "$original_dir"

echo "All Scheme files have been formatted with guix style."

