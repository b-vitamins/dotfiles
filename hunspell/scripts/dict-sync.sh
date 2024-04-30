#!/bin/sh

repo_dict="$(dirname "$(dirname "$0")")/my-dict"
local_dict="$HOME/.config/hunspell/my-dict"

# Check if the local dictionary exists
if [ ! -f "$local_dict" ]; then
    echo "Local dictionary does not exist, creating one."
    touch "$local_dict"
fi

# Sync 
cp -u "$local_dict" "$repo_dict" # Update the repo dictionary with local changes if it's newer
cp -u "$repo_dict" "$local_dict" # Update the local dictionary with repo changes if it's newer

# Navigate to the repository directory
cd "$(dirname "$repo_dict")"

# Add changes to git
git add "$(basename "$repo_dict")"

# Check if there are changes to commit
if git diff-index --quiet HEAD --; then
    echo "No changes to commit."
else
    git commit -m "Update personal dictionary"
    echo "Committed the changes to the repository."
fi

echo "Sync complete."
