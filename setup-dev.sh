#!/bin/bash
set -euo pipefail

# Setup development environment for dotfiles repository

# This script installs required packages and optional tools for working with the
# configuration repository. It is intended to be run once during environment
# provisioning while internet access is available.

# Variables
CORE_PACKAGES=(git gnupg pass stow build-essential \
    dconf-cli gsettings-desktop-schemas xdg-utils \
    python3 python3-pip)

EXTRA_PACKAGES=(pyside2-tools qttools5-dev-tools xvfb emacs)

APT_INSTALL_OPTS=(--yes --no-install-recommends)

GUIX_VERSION="1.4.0"

usage() {
    cat <<EOF
Usage: $0 [--minimal] [--help]

  --minimal   Install a smaller package set
  --help      Show this help message
EOF
}

main() {
    local minimal=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            --minimal) minimal=true ; shift ;;
            --help) usage ; exit 0 ;;
            *) echo "Unknown option: $1"; usage; exit 1 ;;
        esac
    done

    echo "Updating package lists..."
    sudo apt-get update -y

    local packages=("${CORE_PACKAGES[@]}")
    if [[ $minimal == false ]]; then
        packages+=("${EXTRA_PACKAGES[@]}")
    fi

    echo "Installing packages: ${packages[*]}"
    sudo apt-get install "${APT_INSTALL_OPTS[@]}" "${packages[@]}"

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
