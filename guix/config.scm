;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu system nss)
             (guix utils))
(use-modules (nongnu packages linux)
             (nongnu packages mozilla))

(use-service-modules desktop
                     xorg
                     ssh
                     dbus
                     vpn
                     networking
                     syncthing
                     sound)

(use-service-modules docker virtualization spice linux admin)

(use-package-modules certs
                     gnome
                     shells
                     terminals
                     base
                     linux)

(use-package-modules imagemagick fonts fontutils)
(use-package-modules version-control audio video)
(use-package-modules xdisorg gnome-xyz avahi)

(operating-system
  (host-name "lagertha")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")
  (kernel linux)
  (firmware (list linux-firmware))
  (kernel-arguments '("quiet"))
  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)
                (theme (grub-theme (inherit (grub-theme))
                                   (gfxmode (list "1920x1080" "auto"))))))

  (file-systems (append (list (file-system
                                (device (file-system-label "my-root"))
                                (mount-point "/")
                                (type "btrfs"))
                              (file-system
                                (device (uuid "BA6A-E3CB"
                                              'fat))
                                (mount-point "/boot/efi")
                                (type "vfat"))) %base-file-systems))

  (users (cons (user-account
                 (name "b")
                 (comment "B Vitamins")
                 (group "users")
                 (home-directory "/home/b")
                 (shell (file-append zsh "/bin/zsh"))
                 (supplementary-groups '("wheel" "netdev"
                                         "kvm"
                                         "lp"
                                         "audio"
                                         "docker"
                                         "kvm"
                                         "video"))) %base-user-accounts))

  (packages (append (list bluez
                          bluez-alsa
                          coreutils
                          gvfs
                          avahi
                          alacritty
                          firefox
                          git
                          gnome-tweaks
                          gnome-boxes
                          vlc
                          mpv
                          yt-dlp
                          font-dejavu
                          font-iosevka-comfy
                          font-google-noto
                          font-google-noto-serif-cjk
                          font-google-noto-sans-cjk
                          fontconfig
                          imagemagick
                          ffmpeg) %base-packages))

  (services
   (if (target-x86-64?)
       (append (list (service gnome-desktop-service-type)
                     (service file-database-service-type)
                     (service package-database-service-type)
                     (service bluetooth-service-type)
                     (service openssh-service-type)
                     (service bitmask-service-type)
                     (service block-facebook-hosts-service-type)
                     (service docker-service-type)
                     (service libvirt-service-type
                              (libvirt-configuration (unix-sock-group
                                                      "libvirt")
                                                     (tls-port "16555")))
                     (service rasdaemon-service-type)
                     (service earlyoom-service-type)
                     (service zram-device-service-type)
                     (service spice-vdagent-service-type)
                     (service inputattach-service-type)
                     (service nftables-service-type)
                     (service syncthing-service-type
                              (syncthing-configuration (user "b")))
                     (service pam-limits-service-type
                              (list (pam-limits-entry "@realtime"
                                                      'both
                                                      'rtprio 99)
                                    (pam-limits-entry "@realtime"
                                                      'both
                                                      'nice -19)
                                    (pam-limits-entry "@realtime"
                                                      'both
                                                      'memlock
                                                      'unlimited)
                                    (pam-limits-entry "*"
                                                      'both
                                                      'nofile 500000)))
                     (service screen-locker-service-type
                              (screen-locker-configuration (name "xlockmore")
                                                           (program (file-append
                                                                     xlockmore
                                                                     "/bin/xlockmore"))
                                                           (using-pam? #t)
                                                           (using-setuid? #f)))
                     (set-xorg-configuration
                      (xorg-configuration (keyboard-layout keyboard-layout))))
               (modify-services %desktop-services
                 (guix-service-type config =>
                                    (guix-configuration (inherit config)
                                                        (authorize-key? #t)
                                                        (authorized-keys (append
                                                                          (list
                                                                           (local-file
                                                                            "keys/nonguix-signing-key.pub")
                                                                           (local-file
                                                                            "keys/myguix-signing-key.pub"))
                                                                          %default-authorized-guix-keys))
                                                        (use-substitutes? #t)
                                                        (substitute-urls (append
                                                                          (list
                                                                           "https://substitutes.nonguix.org"
                                                                           "substitutes.myguix.org")
                                                                          %default-substitute-urls))
                                                        (discover? #t)
                                                        (tmpdir "/tmp")))
                 (dbus-root-service-type config =>
                                         (dbus-configuration (inherit config)
                                                             (services (list
                                                                        bluez-alsa))))
                 (gdm-service-type config =>
                                   (gdm-configuration (inherit config)
                                                      (auto-login? #f)
                                                      (default-user "b")
                                                      (auto-suspend? #f)
                                                      (gnome-shell-assets (list
                                                                           gitg
                                                                           gnome-shell-extension-dash-to-dock
                                                                           flat-remix-icon-theme
                                                                           flat-remix-gtk-theme
                                                                           flat-remix-gnome-theme
                                                                           bibata-cursor-theme))
                                                      (wayland? #t)))))))
  (name-service-switch %mdns-host-lookup-nss))
