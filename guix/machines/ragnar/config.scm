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
             (gnu services file-sharing)
             (gnu services docker)
             (gnu services virtualization)
             (gnu services spice)
             (gnu services linux)
             (gnu services vpn)
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

(define %my-nftables-ruleset
  (plain-file "nftables.conf"
   "# A simple and safe firewall
table inet filter {
  chain input {
    type filter hook input priority 0; policy drop;

    # early drop of invalid connections
    ct state invalid drop

    # allow established/related connections
    ct state { established, related } accept

    # allow from loopback
    iif lo accept
    # drop connections to lo not coming from lo
    iif != lo ip daddr 127.0.0.1/8 drop
    iif != lo ip6 daddr ::1/128 drop

    # allow icmp
    ip protocol icmp accept
    ip6 nexthdr icmpv6 accept

    # allow ssh
    tcp dport ssh accept

    # allow traffic from the local LAN (10.42.0.0/24)
    ip saddr 10.42.0.0/24 accept

    # allow Nicotine+ (Soulseek) client traffic (port range 2234-2239)
    tcp dport { 2234-2239 } accept
    udp dport { 2234-2239 } accept

    # allow OpenVPN traffic on ports 80 and 1194 (both TCP and UDP)
    tcp dport { 80, 1194 } accept
    udp dport { 80, 1194 } accept

    # reject everything else
    reject with icmpx type port-unreachable
  }

  chain forward {
    type filter hook forward priority 0; policy drop;

    # allow forwarding of traffic from the local LAN (10.42.0.0/24) to the Wi-Fi interface (wlp37s0)
    ip saddr 10.42.0.0/24 oifname wlp37s0 accept

    # allow forwarding of traffic from Wi-Fi to the local LAN
    ip daddr 10.42.0.0/24 iifname wlp37s0 accept

    # Allow forwarding of established and related connections
    ct state { established, related } accept
  }

  chain output {
    type filter hook output priority 0; policy accept;
  }
}

# NAT
table ip nat {
  chain postrouting {
    type nat hook postrouting priority 100; policy accept;

    # Masquerade traffic from the local LAN (10.42.0.0/24) through the Wi-Fi interface (wlp37s0)
    ip saddr 10.42.0.0/24 oifname wlp37s0 masquerade
  }
}
"))

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
                                (device (uuid "B224-27F3"
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
                                         "transmission"
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
                                                                        "network-manager-openvpn")
                                                                       (specification->package
                                                                        "network-manager-openconnect")))))
            (service wpa-supplicant-service-type)
            (simple-service 'network-manager-applet profile-service-type
                            (list (specification->package
                                   "network-manager-applet")))
            (service modem-manager-service-type)
            (service usb-modeswitch-service-type)
            (service openssh-service-type)

            ;; Networking Services
            (service avahi-service-type)
            (service nftables-service-type
                     (nftables-configuration (ruleset %my-nftables-ruleset)))
            (service ntp-service-type)

            ;; File Sharing Services
            (service transmission-daemon-service-type
                     (transmission-daemon-configuration
                      ;; Restrict access to the RPC ("control") interface for user "b"
                      (rpc-authentication-required? #t)
                      (rpc-username "b")
                      (rpc-password
                       "{9b8548746166356d4f2b168a067ffec74073ada1NoxgzcwS")

                      ;; Accept requests from localhost and the subnet 10.42.*.*
                      (rpc-whitelist-enabled? #t)
                      (rpc-whitelist '("127.0.0.1" "::1" "10.42.*.*"))

                      ;; Regular (modest) speeds during the day
                      (speed-limit-down-enabled? #t)
                      (speed-limit-down (* 1024 1024 1)) ;1 MB/s download speed
                      (speed-limit-up-enabled? #t)
                      (speed-limit-up (* 1024 200)) ;200 KB/s upload speed
                      
                      ;; High-speed downloads during the night
                      (alt-speed-enabled? #t)
                      (alt-speed-down (* 1024 1024 20)) ;20 MB/s download speed
                      (alt-speed-up (* 1024 2048)) ;2 MB/s upload speed
                      
                      ;; Schedule the alternative speeds for nighttime hours
                      (alt-speed-time-enabled? #t)
                      (alt-speed-time-day 'all) ;Every day
                      (alt-speed-time-begin 1320) ;Start at 10:00 PM (1320 minutes)
                      (alt-speed-time-end 480) ;End at 8:00 AM (480 minutes)
                      
                      ;; Store downloads in the default directory
                      (download-dir "/var/lib/transmission-daemon/downloads")

                      ;; Enable incomplete directory to hold incomplete downloads
                      (incomplete-dir-enabled? #t)
                      (incomplete-dir
                       "/var/lib/transmission-daemon/incomplete")

                      ;; Enable uTP for reduced network impact
                      (utp-enabled? #t)

                      ;; Enable encryption for secure peer connections
                      (encryption prefer-encrypted-connections)

                      ;; Set peer limits for faster downloads
                      (peer-limit-global 1000)
                      (peer-limit-per-torrent 200)

                      ;; Enable Distributed Hash Table (DHT) and Peer Exchange (PEX)
                      (dht-enabled? #t)
                      (pex-enabled? #t)

                      ;; Watch directory for .torrent files and automatically add them
                      (watch-dir-enabled? #t)
                      (watch-dir "/var/lib/transmission-daemon/watch") ;Specify your watch directory path
                      
                      ;; Automatically delete .torrent files after processing
                      (trash-original-torrent-files? #t)))

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
