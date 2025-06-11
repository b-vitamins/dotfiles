#!/bin/bash
set -euo pipefail

# Setup development environment for dotfiles repository

# This script installs required packages and optional tools for working with the
# configuration repository. It is intended to be run once during environment
# provisioning while internet access is available.

# Variables
APT_PACKAGES=(\
    git gnupg pass stow \
    build-essential \
    dconf-cli gsettings-desktop-schemas xdg-utils \
    python3 python3-pip pyside2-tools qttools5-dev-tools \
    xvfb emacs
)

GUIX_VERSION="1.4.0"

main() {
    echo "Updating package lists..."
    sudo apt-get update -y
    echo "Installing packages: ${APT_PACKAGES[*]}"
    sudo apt-get install -y "${APT_PACKAGES[@]}"

    install_guix

    echo "Environment setup complete." 
}

install_guix() {
    if command -v guix >/dev/null 2>&1; then
        echo "Guix already installed. Skipping."
        return
    fi
    echo "Installing Guix ${GUIX_VERSION}..."
    tmpdir=$(mktemp -d)
    pushd "$tmpdir" >/dev/null
    wget -q "https://ftp.gnu.org/gnu/guix/guix-binary-${GUIX_VERSION}.x86_64-linux.tar.xz"
    tar -xf "guix-binary-${GUIX_VERSION}.x86_64-linux.tar.xz"
    sudo ./install
    popd >/dev/null
    rm -rf "$tmpdir"
}

main "$@"
