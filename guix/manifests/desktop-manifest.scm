;; -*- mode: scheme; -*-
;; desktop-manifest.scm
(use-modules (nongnu packages messaging)
             (gnu packages nicotine)
             (gnu packages syncthing)
             (gnu packages kodi)
             (gnu packages music)
             (gnu packages bittorrent)
             (gnu packages video)
             (gnu packages audio)
             (gnu packages mail)
             (gnu packages gnuzilla)
             (gnu packages password-utils)
             (gnu packages disk))

(specifications->manifest '("signal-desktop" ;Secure messaging application
                            "nicotine+" ;Graphical client for the Soulseek file sharing network
                            "deluge" ;Lightweight, Free Software, cross-platform BitTorrent client
                            "audacious" ;Open source audio player that plays your music how you want it
                            "syncthing-gtk" ;GTK3 & Python GUI and notification area icon for Syncthing
                            "icedove-wayland" ;Free email application that's easy to set up and customize
                            "qbittorrent" ;Bittorrent client built with Qt and Libtorrent-rasterbar
                            "libreoffice" ;LibreOffice is a powerful office suite
                            "kodi" ;Award-winning free and open source home theater/media center software
                            "keepassxc" ;Free and open-source password manager
                            ))
