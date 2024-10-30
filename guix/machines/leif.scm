;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu home)
             (gnu home services mcron)
             (gnu home services ssh)
             (gnu home services desktop)
             (gnu home services media)
             (gnu home services music)
             (gnu home services dict)
             (gnu home services sound)
             (gnu home services syncthing)
             (gnu home services shells)
             (gnu home services pm)
             (gnu system)
             (gnu services)
             (gnu system shadow)
             (gnu system nss)
             (gnu system install)
             (gnu services avahi)
             (gnu services cups)
             (gnu services ssh)
             (gnu services desktop)
             (gnu services xorg)
             (gnu services networking)
             (gnu services sysctl)
             (guix gexp)
             (myguix packages base)
             (myguix services mcron)
             (myguix home)
             (myguix home services emacs)
             (myguix services desktop)
             (myguix system install)
             (myguix packages linux)
             (myguix system linux-initrd)
             (srfi srfi-1))

(define %my-home-config
  (home-environment
    (packages (append %system-core-packages
                      %compression-tools-packages
                      %filesystem-management-packages
                      %terminal-tools-packages
                      %network-tools-packages
                      %development-tools-packages
                      %rust-development-packages
                      %python-development-packages
                      %guile-development-packages
                      %perl-development-packages
                      %language-support-packages
                      %system-monitoring-packages
                      %security-tools-packages
                      %media-tools-packages
                      %desktop-environment-packages
                      %document-formatting-packages))

    (services
     (append (list
              ;; Home Emacs Service
              (service my-home-emacs-service-type)
              ;; Power Management Home Services
              (service home-batsignal-service-type)
              ;; Scheduled User’s Job Execution
              (service home-mcron-service-type
                       (home-mcron-configuration (jobs (list
                                                        %garbage-collector-job))))

              ;; Secure Shell
              (service home-openssh-service-type
                       (home-openssh-configuration (hosts (list (openssh-host (name
                                                                               "github.com")

                                                                              
                                                                              (user
                                                                               "git")

                                                                              
                                                                              (identity-file
                                                                               "~/.ssh/id_ed25519"))
                                                                (openssh-host (name
                                                                               "ci.myguix.bvits.in")

                                                                              
                                                                              (user
                                                                               "b")

                                                                              
                                                                              (port
                                                                               2123)

                                                                              
                                                                              (identity-file
                                                                               "~/.ssh/id_ed25519"))))
                                                   (authorized-keys (list (local-file
                                                                           "../../keys/ssh/ragnar.pub")
                                                                          (local-file
                                                                           "../../keys/ssh/freydis.pub")
                                                                          (local-file
                                                                           "../../keys/ssh/bjorn.pub")))
                                                   (add-keys-to-agent
                                                    "confirm")))

              ;; Desktop Home Services
              (service home-dbus-service-type)

              ;; Sound Home Services
              (service home-pipewire-service-type)

              ;; Media Home Services
              ;; (service home-kodi-service-type)
              
              ;; Networking Home Services
              (service home-syncthing-service-type)

              ;; Miscellaneous Home Services
              (service home-beets-service-type
                       (home-beets-configuration (directory "/home/b/music"))))
             %my-home-services))))

(operating-system
  (host-name "leif")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (firmware (list linux-firmware sof-firmware))
  (initrd microcode-initrd)

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)
                (theme (grub-theme (inherit (grub-theme))))))

  (file-systems (append (list (file-system
                                (device (file-system-label "my-root"))
                                (mount-point "/")
                                (type "ext4"))
                              (file-system
                                (device (uuid "763C-E5EC"
                                              'fat32))
                                (mount-point "/boot/efi")
                                (type "vfat"))) %base-file-systems))

  (swap-devices (list (swap-space
                        (target (uuid "d9d26205-fab2-4061-82b6-4c231a429b68")))))

  (users (cons (user-account
                 (name "b")
                 (comment "Ayan")
                 (group "users")
                 (home-directory "/home/b")
                 (shell (file-append (specification->package "zsh") "/bin/zsh"))
                 (supplementary-groups '("adbusers" "wheel" "netdev"
                                         "realtime" "audio" "video")))
               %base-user-accounts))

  (groups (cons (user-group
                  (system? #t)
                  (name "realtime")) %base-groups))

  (packages (append (list (specification->package "font-dejavu")
                          (specification->package "font-iosevka-comfy")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-serif-cjk")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "fontconfig")
                          (specification->package "pinentry")) %base-packages))

  (services
   (append (list
            ;; Desktop Environment
            (service gnome-desktop-service-type)
            (set-xorg-configuration
             (xorg-configuration (keyboard-layout keyboard-layout)))

            ;; Printing Services
            (service cups-service-type
                     (cups-configuration (web-interface? #t)
                                         (extensions (list (specification->package
                                                            "cups-filters")
                                                           (specification->package
                                                            "brlaser")
                                                           (specification->package
                                                            "foomatic-filters")))))

            ;; Networking Setup
            (service network-manager-service-type
                     (network-manager-configuration (vpn-plugins (list (specification->package
                                                                        "network-manager-openvpn")
                                                                       (specification->package
                                                                        "network-manager-openconnect")))))
            (service wpa-supplicant-service-type)
            (simple-service 'network-manager-applet profile-service-type
                            (list (specification->package
                                   "network-manager-applet")))
            (service modem-manager-service-type)
            (service usb-modeswitch-service-type)

            ;; Guix Services
            (service guix-home-service-type
                     `(("b" ,%my-home-config)))

            ;; Networking Services
            (service avahi-service-type)
            (service nftables-service-type)
            (service ntp-service-type)
            (service openssh-service-type)

            ;; Miscellaneous Services
            (service sysctl-service-type
                     (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1"))
                                                      %default-sysctl-settings)))))
           %my-desktop-services))
  (name-service-switch %mdns-host-lookup-nss))
