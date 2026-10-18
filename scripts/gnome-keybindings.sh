#!/bin/sh

set -eu

keybindings_only=false
case "${1:-}" in
    --keybindings-only) keybindings_only=true ;;
    '') ;;
    --help|-h)
        echo "Usage: $0 [--keybindings-only]"
        echo "Use --keybindings-only to leave theme, power and default applications alone."
        exit 0
        ;;
    *) echo "Unknown argument: $1" >&2; exit 2 ;;
esac
if [ "$#" -gt 1 ]; then
    echo "Usage: $0 [--keybindings-only]" >&2
    exit 2
fi

cd "$(dirname "$0")"

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Require the shortcut tool and all input files before changing settings.
keybindings_dir="../gnome/keybindings"
if ! command_exists dconf; then
    echo "Error: 'dconf' is required to apply GNOME keybindings." >&2
    exit 1
fi
for config in gnome-media-keys gnome-custom-keybindings gnome-shell-keybindings \
              gnome-mutter-keybindings gnome-desktop-wm-keybindings; do
    if [ ! -r "$keybindings_dir/$config.dconf" ]; then
        echo "Error: Configuration file not readable: $keybindings_dir/$config.dconf" >&2
        exit 1
    fi
done

apply_keybindings() {
    dconf load "$2" < "$1"
    echo "Keybindings loaded from $1"
}

apply_keybindings "${keybindings_dir}/gnome-media-keys.dconf" "/org/gnome/settings-daemon/plugins/media-keys/"
# The dump contains both [/] and [custom-keybindings/customN] sections, so
# load it at the media-keys root, not inside custom-keybindings again.
apply_keybindings "${keybindings_dir}/gnome-custom-keybindings.dconf" "/org/gnome/settings-daemon/plugins/media-keys/"
apply_keybindings "${keybindings_dir}/gnome-shell-keybindings.dconf" "/org/gnome/shell/keybindings/"
apply_keybindings "${keybindings_dir}/gnome-mutter-keybindings.dconf" "/org/gnome/mutter/keybindings/"
apply_keybindings "${keybindings_dir}/gnome-desktop-wm-keybindings.dconf" "/org/gnome/desktop/wm/keybindings/"
echo "GNOME keybindings applied."

if [ "$keybindings_only" = true ]; then
    exit 0
fi

# Set the interface to Dark theme using gsettings if available
if command_exists gsettings; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
    gsettings set org.gnome.desktop.session idle-delay 0
    gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type 'nothing'
    gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-battery-type 'nothing'
    echo "gsettings configurations applied."
else
    echo "Warning: 'gsettings' is not installed. Skipping theme and power settings."
fi

# Set default applications using xdg-settings if available
if command_exists xdg-settings; then
    xdg-settings set default-web-browser firefox.desktop
    xdg-settings set default-url-scheme-handler video vlc.desktop
    xdg-settings set default-url-scheme-handler audio vlc.desktop
    xdg-settings set default-url-scheme-handler image eog.desktop
    echo "xdg-settings configurations applied."
else
    echo "Warning: 'xdg-settings' is not installed. Skipping default applications settings."
fi

echo "Setup completed with available tools."

