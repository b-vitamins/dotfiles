#!/bin/sh

# Navigate to the script's directory
cd "$(dirname "$0")"

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

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

# Base path for keybindings relative to the script's directory
keybindings_dir="../gnome/keybindings"

# Apply keybindings from stored dconf files if dconf is available
if command_exists dconf; then
    apply_keybindings() {
        local path="$1"
        local schema="$2"
        if [ -f "$path" ]; then
            dconf load "$schema" < "$path"
            echo "Keybindings loaded from $path"
        else
            echo "Warning: Configuration file not found: $path"
        fi
    }

    # Load keybindings
    apply_keybindings "${keybindings_dir}/gnome-media-keys.dconf" "/org/gnome/settings-daemon/plugins/media-keys/"
    apply_keybindings "${keybindings_dir}/gnome-custom-keybindings.dconf" "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/"
    apply_keybindings "${keybindings_dir}/gnome-shell-keybindings.dconf" "/org/gnome/shell/keybindings/"
    apply_keybindings "${keybindings_dir}/gnome-mutter-keybindings.dconf" "/org/gnome/mutter/keybindings/"
    apply_keybindings "${keybindings_dir}/gnome-desktop-wm-keybindings.dconf" "/org/gnome/desktop/wm/keybindings/"
    echo "dconf configurations applied."
else
    echo "Warning: 'dconf' is not installed. Skipping keybindings settings."
fi

echo "Setup completed with available tools."

