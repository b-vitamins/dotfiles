#!/bin/bash

# Main function to call setup tasks
main() {
    echo "Starting system setup..."
    run_command sudo apt update
    echo "Upgrading system..."
    run_command sudo apt upgrade -y
    echo "Done."

    # Install utilities and perform upgrade
    install_utilities

    # Set up user directories
    configure_user_dirs

    # Configure UFW for security and internet sharing
    configure_ufw

    # Set up Syncthing
    setup_syncthing

    # Set up CUPS
    setup_cups

    # Download Brother printer driver after setting up user directories
    download_printer_driver

    # Set up Internet Connection Sharing
    setup_internet_sharing

    # Configure SSH
    configure_ssh

    # Set up Zsh configuration files
    setup_zsh_config_files

    # Change default shell to Zsh
    change_default_shell

    # Set up SSH keys from pass
    setup_ssh_keys

    echo "System setup complete!"
}

# Suppress output of successful commands unless there's an error
run_command() {
    "$@" &> /dev/null
    if [ $? -ne 0 ]; then
        echo "Error occurred while running: $*"
        exit 1
    fi
}

# Function to install basic utilities and update system
install_utilities() {
    echo "Installing utilities..."

    # Update repository and install common utilities and wallpapers
    run_command sudo apt install -y \
        htop emacs coreutils net-tools curl git iputils-ping unzip \
        build-essential software-properties-common iftop iotop \
        gnupg pinentry-curses pass gnome-tweaks gnome-shell-extensions gnome-backgrounds \
        ssh alacritty gsettings-desktop-schemas xdg-utils dconf-cli dconf-editor ufw zsh

    echo "Done."
}

# Function to configure and refresh user directories
configure_user_dirs() {
    echo "Configuring user directories..."

    USER_DIRS="$HOME/.config/user-dirs.dirs"
    mkdir -p "$HOME/.config"

    # Create and configure custom directories
    mkdir -p "$HOME/desktop" "$HOME/library/documents" "$HOME/library/music" \
        "$HOME/library/pictures" "$HOME/library/public" "$HOME/library/templates" \
        "$HOME/library/videos" "$HOME/downloads"

    # Remove default directories
    rm -rf "$HOME/Documents" "$HOME/Downloads" "$HOME/Music" "$HOME/Pictures" \
           "$HOME/Public" "$HOME/Templates" "$HOME/Videos"

    # Remove the default Emacs directory
    rm -rf "$HOME/.emacs.d"

    # Write the new user-dirs configuration
    cat > "$USER_DIRS" <<EOL
XDG_DESKTOP_DIR="\$HOME/desktop"
XDG_DOCUMENTS_DIR="\$HOME/library/documents"
XDG_DOWNLOAD_DIR="\$HOME/downloads"
XDG_MUSIC_DIR="\$HOME/library/music"
XDG_PICTURES_DIR="\$HOME/library/pictures"
XDG_PUBLICSHARE_DIR="\$HOME/library/public"
XDG_TEMPLATES_DIR="\$HOME/library/templates"
XDG_VIDEOS_DIR="\$HOME/library/videos"
EOL

    run_command xdg-user-dirs-update

    echo "Done."
}

# Function to configure UFW firewall rules
configure_ufw() {
    echo "Configuring UFW firewall..."

    # Enable UFW if not already enabled
    sudo ufw status | grep -q inactive && run_command sudo ufw enable

    # Default policies: deny all incoming traffic and allow all outgoing traffic
    run_command sudo ufw default deny incoming   # Blocks any unsolicited incoming traffic
    run_command sudo ufw default allow outgoing  # Allows all outgoing traffic from the system

    # Allow DHCP traffic on wlan0 for obtaining an IP address via Wi-Fi
    run_command sudo ufw allow in on wlan0 to any port 67:68 proto udp
    # This rule permits the system to communicate with a DHCP server over ports 67 (server) and 68 (client), which is essential for obtaining an IP address via Wi-Fi.

    # Allow Internet sharing via eth0 (Ethernet interface)
    run_command sudo ufw allow in on eth0    # Allow incoming traffic on Ethernet (local network)
    run_command sudo ufw allow out on eth0   # Allow outgoing traffic on Ethernet (local network)

    # Allow access to the Syncthing web interface on port 8384 over Ethernet but block it on Wi-Fi
    run_command sudo ufw allow in on eth0 to any port 8384
    run_command sudo ufw deny in on wlan0 to any port 8384
    # Syncthing’s web interface is accessible over Ethernet but denied on Wi-Fi for added security.

    # Allow access to the CUPS web interface on port 631 over Ethernet but block it on Wi-Fi
    run_command sudo ufw allow in on eth0 to any port 631
    run_command sudo ufw deny in on wlan0 to any port 631
    # Similarly, CUPS (printer service) web interface is restricted to Ethernet for security.

    # Deny all other incoming traffic on wlan0
    run_command sudo ufw deny in on wlan0 from any
    # This blocks any other unsolicited incoming traffic on the Wi-Fi interface (wlan0).

    # Reload UFW to apply the changes
    run_command sudo ufw reload

    echo "Done."
}

# Function to set up Syncthing
setup_syncthing() {
    echo "Installing and setting up Syncthing..."
    run_command sudo apt install -y syncthing
    run_command loginctl enable-linger $(whoami)
    run_command systemctl --user enable syncthing.service
    run_command systemctl --user start syncthing.service

    # Check if Syncthing is running without halting the script
    systemctl --user is-active syncthing.service &> /dev/null || echo "Syncthing failed to start."

    echo "Done."
}

# Function to set up CUPS printing service
setup_cups() {
    echo "Installing and setting up CUPS..."
    run_command sudo apt install -y cups
    run_command sudo systemctl enable cups
    run_command sudo systemctl start cups

    # Check if CUPS is running
    systemctl is-active cups &> /dev/null || echo "CUPS failed to start."

    echo "Done."
}

# Function to download Brother printer driver
download_printer_driver() {
    DRIVER_URL="https://github.com/b-vitamins/brother-hlt4000dw-driver/releases/download/v1.0.0/hlt4000dw-driver_v1.0.0.deb"
    DRIVER_FILE="$HOME/downloads/hlt4000dw-driver_v1.0.0.deb"

    echo "Downloading Brother HL-T4000DW printer driver..."
    run_command wget -O "$DRIVER_FILE" "$DRIVER_URL"

    echo "Done."
}

# Function to set up WLAN to Ethernet Bridge
setup_internet_sharing() {
    WIFI_INTERFACE="wlan0"
    ETHERNET_INTERFACE="eth0"

    echo "Setting up WLAN to Ethernet Bridge..."

    # Enable IP forwarding, which allows the system to forward packets between interfaces
    run_command sudo sysctl -w net.ipv4.ip_forward=1
    echo "net.ipv4.ip_forward=1" | sudo tee -a /etc/sysctl.conf &> /dev/null
    # This enables IP forwarding on the system, allowing it to route traffic between interfaces.

    # Set up iptables for NAT (Network Address Translation)
    run_command sudo iptables -F  # Flush all existing iptables rules
    run_command sudo iptables -t nat -F  # Flush all existing NAT rules
    # This clears any existing iptables and NAT rules to ensure clean configuration.

    # Configure NAT so that traffic from the local network (eth0) is routed through the Wi-Fi connection (wlan0)
    run_command sudo iptables -t nat -A POSTROUTING -o $WIFI_INTERFACE -j MASQUERADE
    # This rule allows IP masquerading, so that any traffic coming from the Ethernet network (eth0) can be routed out via the Wi-Fi interface (wlan0).

    # Allow established and related connections from Wi-Fi to Ethernet
    run_command sudo iptables -A FORWARD -i $WIFI_INTERFACE -o $ETHERNET_INTERFACE -m state --state RELATED,ESTABLISHED -j ACCEPT
    # This ensures that established connections from the Wi-Fi interface can forward packets to the Ethernet interface.

    # Allow new outgoing connections from Ethernet to Wi-Fi
    run_command sudo iptables -A FORWARD -i $ETHERNET_INTERFACE -o $WIFI_INTERFACE -j ACCEPT
    # This rule forwards new connections from the Ethernet interface to the Wi-Fi interface.

    # Save iptables rules so they persist across reboots
    run_command sudo apt install -y iptables-persistent
    run_command sudo netfilter-persistent save
    # This ensures the iptables rules are saved and will persist after reboot.

    echo "Done."
}

# Function to configure SSH
configure_ssh() {
    echo "Configuring SSH..."

    # Install OpenSSH server
    run_command sudo apt install -y openssh-server

    # Update SSH configuration to disable password authentication and enable public key authentication
    sudo sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config
    sudo sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/' /etc/ssh/sshd_config
    sudo sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin no/' /etc/ssh/sshd_config

    # Restart SSH service
    run_command sudo systemctl restart ssh

    # Check if SSH is running
    systemctl is-active ssh &> /dev/null || echo "SSH failed to start."

    echo "Done."
}

# Function to set up Zsh configuration files
setup_zsh_config_files() {
    echo "Setting up Zsh configuration files..."

    # Ensure ~/.config/zsh directory exists
    mkdir -p "$HOME/.config/zsh"

    # Write ~/.zshenv
    cat > "$HOME/.zshenv" <<EOL
export ZDOTDIR=\${XDG_CONFIG_HOME:-\$HOME/.config}/zsh
[[ -f \$ZDOTDIR/.zshenv ]] && source \$ZDOTDIR/.zshenv
EOL

    # Write ~/.config/zsh/.zshenv
    cat > "$HOME/.config/zsh/.zshenv" <<EOL
export PROMPT="%F{#ff6c6b}カオス%f %F{#98be65}%2~%f %F{#51afef}>%f "
export RPROMPT="%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f"
export GUILE_AUTO_COMPILE="0"
export HISTFILE="\$HOME/.zhistory"
export HISTSIZE="10000"
export SAVEHIST="10000"
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=4"
EOL

    # Write ~/.config/zsh/.zshrc
    cat > "$HOME/.config/zsh/.zshrc" <<EOL
autoload -Uz add-zsh-hook

autoload -Uz compinit
compinit -u

autoload -Uz colors && colors

setopt NO_BEEP EXTENDED_GLOB NO_CLOBBER SHARE_HISTORY HIST_IGNORE_DUPS HIST_IGNORE_SPACE INC_APPEND_HISTORY

function update_clock_precmd {
    RPROMPT='%F{#88c0d0}%D{%d %b}%f %F{#ECBE7B}%D{%l:%M:%S}%f'
}
add-zsh-hook precmd update_clock_precmd

TRAPALRM() {
    zle reset-prompt
}
TMOUT=1

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ll='ls -lh'
alias la='ls -A'
EOL

    # Write ~/.config/zsh/.zprofile
    cat > "$HOME/.config/zsh/.zprofile" <<EOL
# Ensure that the environment variables and paths are set correctly for login shells
export PATH="\$HOME/bin:\$PATH"

# Start ssh-agent if it's not already running
if [ -z "\$SSH_AGENT_PID" ]; then
  eval "\$(ssh-agent -s)"
fi

# Set locale if necessary
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Source .zshrc to load interactive shell settings
[[ -f \$ZDOTDIR/.zshrc ]] && source \$ZDOTDIR/.zshrc
EOL

    echo "Done."
}

# Function to change the default shell to Zsh
change_default_shell() {
    echo "Changing default shell to Zsh..."

    # Install Zsh if not already installed
    run_command sudo apt install -y zsh

    # Change the default shell to Zsh for the current user (allowing user input)
    chsh -s $(which zsh)

    echo "Done."

}

# Function to set up SSH keys and authorized_keys using pass
setup_ssh_keys() {
    echo "Setting up SSH keys..."

    # Create the .ssh directory if it doesn't exist
    rm -rf "$HOME/.ssh"
    mkdir -p "$HOME/.ssh"

    # Retrieve SSH keys from pass and set correct permissions
    pass keys/ssh/freydis/id_ed25519 > "$HOME/.ssh/id_ed25519"
    chmod 600 "$HOME/.ssh/id_ed25519"

    pass keys/ssh/freydis/id_ed25519.pub > "$HOME/.ssh/id_ed25519.pub"
    chmod 644 "$HOME/.ssh/id_ed25519.pub"

    # Append other public keys to authorized_keys
    pass keys/ssh/ragnar/id_ed25519.pub >> "$HOME/.ssh/authorized_keys"
    pass keys/ssh/leif/id_ed25519.pub >> "$HOME/.ssh/authorized_keys"
    pass keys/ssh/bjorn/id_ed25519.pub >> "$HOME/.ssh/authorized_keys"
    chmod 600 "$HOME/.ssh/authorized_keys"

    echo "Done."
}

# Things to do before running this script:
# 1) Run `sudo apt install gnupg pinentry-curses pass`
# 2) Setup GnuPG by importing GPG keys
# 3) Setup pass by adding $HOME/.password-store

# Run the main function
main

# Things to do after running this script:
# 1) Install the Brother HL-T4000DW printer driver
# 2) Run the gnome-keybindings.sh script
# 3) Copy emacs and alacritty directories to $HOME/.config/
# 4) Setup syncthing