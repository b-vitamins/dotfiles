#!/bin/sh
# Generate *.qbtheme file from existing files (config.json, stylesheet.qss, and resources.qrc).
# This is a qBittorrent theme bundler.
# https://github.com/mahdimirzade/qbittorrent

TARGET=${1:-dracula.qbtheme}

# Check if required files exist
if [ ! -f config.json ]; then
    echo "Error: config.json not found."
    exit 1
fi

if [ ! -f stylesheet.qss ]; then
    echo "Error: stylesheet.qss not found."
    exit 1
fi

if [ ! -f resources.qrc ]; then
    echo "Error: resources.qrc not found."
    exit 1
fi

# Generate the .qbtheme file using rcc
rcc resources.qrc -o "$TARGET" -binary

if [ $? -eq 0 ]; then
    echo "Theme file '$TARGET' created successfully."
else
    echo "Error: Failed to create theme file."
    exit 1
fi
