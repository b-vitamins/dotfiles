;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu system)
             (gnu system nss)
             (gnu system install)
             (gnu services avahi)
             (gnu services cups)
             (gnu services ssh)
             (gnu services desktop)
             (gnu services xorg)
             (gnu services networking)
             (gnu services sysctl)
             (myguix services desktop)
             (myguix system install)
             (myguix packages linux)
             (myguix system linux-initrd)
             (srfi srfi-1))

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
                          (specification->package "pinentry"))
                    %base-packages))

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
