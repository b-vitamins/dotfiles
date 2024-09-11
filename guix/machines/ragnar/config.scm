;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu system)
             (gnu system nss)
             (gnu system install)
             (gnu services avahi)
             (gnu services cups)
             (gnu services desktop)
             (gnu services xorg)
             (gnu services docker)
             (gnu services networking)
             (gnu services docker)
             (gnu services virtualization)
             (gnu services spice)
             (gnu services linux)
             (gnu services ssh)
             (gnu services sysctl)
             (myguix services desktop)
             (myguix services nvidia)
             (myguix services oci-containers)
             (myguix system install)
             (myguix packages linux)
             (myguix packages nvidia)
             (myguix system linux-initrd)
             (srfi srfi-1))

(define %rclone-path
  ;; Use assoc-ref to get the store path to rclone, then append "/bin/rclone"
  (string-append (assoc-ref %build-inputs "rclone") "/bin/rclone"))

(operating-system
  (host-name "ragnar")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (kernel-arguments (list "modprobe.blacklist=nouveau" "nvidia_drm.modeset=1"))
  (firmware (list linux-firmware))
  (initrd microcode-initrd)

  (kernel-loadable-modules (list (specification->package
                                  "v4l2loopback-linux-module")))

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
                                (type "btrfs"))
                              (file-system
                                (device (uuid "925A-AE22"
                                              'fat))
                                (mount-point "/boot/efi")
                                (type "vfat"))) %base-file-systems))

  (users (cons (user-account
                 (name "b")
                 (comment "Ayan")
                 (group "users")
                 (home-directory "/home/b")
                 (shell (file-append (specification->package "zsh") "/bin/zsh"))
                 (supplementary-groups '("adbusers" "wheel"
                                         "netdev"
                                         "kvm"
                                         "tty"
                                         "libvirt"
                                         "input"
                                         "docker"
                                         "realtime"
                                         "lp"
                                         "audio"
                                         "video"))) %base-user-accounts))

  (groups (cons (user-group
                  (system? #t)
                  (name "realtime")) %base-groups))

  (packages (append (list (specification->package "font-dejavu")
                          (specification->package "font-iosevka-comfy")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-serif-cjk")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "fontconfig"))
                    %base-packages))

  (services
   (append (list
            ;; Desktop Environment
            (service gnome-desktop-service-type)
            (service nvidia-service-type)
            (set-xorg-configuration
             (xorg-configuration (keyboard-layout keyboard-layout)
                                 (modules (cons nvda %default-xorg-modules))
                                 (drivers '("nvidia"))))

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
                                                                        "network-manager-openvpn")))))
            (service wpa-supplicant-service-type)
            (simple-service 'network-manager-applet profile-service-type
                            (list (specification->package
                                   "network-manager-applet")))
            (service modem-manager-service-type)
            (service usb-modeswitch-service-type)
            (service openssh-service-type)

            ;; Networking Services
            (service avahi-service-type)
            (service nftables-service-type)
            (service ntp-service-type)

            ;; VPN Services
            (service bitmask-service-type)

            ;; Virtualization Services
            (service libvirt-service-type
                     (libvirt-configuration (tls-port "16555")))

            ;; Linux Services
            (service earlyoom-service-type)
            (service zram-device-service-type)

            ;; Miscellaneous Services
            (service sysctl-service-type
                     (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1")
                                                               ("vm.max_map_count" . "262144"))
                                                      %default-sysctl-settings))))
            (service spice-vdagent-service-type)
            (service inputattach-service-type)
            (service containerd-service-type)
            (service docker-service-type)
            (service oci-container-service-type
                     (list oci-grobid-service-type
                           oci-meilisearch-service-type
                           oci-weaviate-service-type oci-neo4j-service-type)))
           %my-desktop-services))
  (name-service-switch %mdns-host-lookup-nss))
