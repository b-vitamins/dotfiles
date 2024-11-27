(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services dict)
             (gnu home services mcron)
             (gnu home services media)
             (gnu home services music)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu home services syncthing)
             (gnu packages networking)
             (gnu packages sync)
             (gnu packages video)
             (gnu packages gnome)
             (gnu services)
             (gnu services avahi)
             (gnu services cups)
             (gnu services databases)
             (gnu services dbus)
             (gnu services desktop)
             (gnu services docker)
             (gnu services docker)
             (gnu services file-sharing)
             (gnu services guix)
             (gnu services linux)
             (gnu services networking)
             (gnu services spice)
             (gnu services ssh)
             (gnu services sysctl)
             (gnu services virtualization)
             (gnu services cuirass)
             (gnu services xorg)
             (gnu system)
             (gnu system install)
             (gnu system linux-initrd)
             (gnu system nss)
             (gnu system shadow)
             (guix packages)
             (guix deprecation)
             (guix gexp)
             (myguix home)
             (myguix home services emacs)
             (myguix packages base)
             (myguix packages linux)
             (myguix packages nvidia)
             (myguix packages python-pqrs)
             (myguix packages video)
             (myguix services desktop)
             (myguix services mcron)
             (myguix services nvidia)
             (myguix services oci-containers)
             (myguix system install)
             (myguix system linux-initrd)
             (srfi srfi-1))

(define %my-home-config
  (home-environment
    (packages (append %media-consumption-packages
                      %audio-conversion-tools-packages
                      %video-conversion-tools-packages
                      %document-authoring-packages
                      %document-manipulation-packages
                      %file-transfer-tools-packages
                      %p2p-file-sharing-packages
                      %guile-development-packages
                      %rust-development-packages
                      %python-development-packages))

    (services
     (append (list
              ;; Home Emacs Service
              (service my-home-emacs-service-type)
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
  (host-name "ragnar")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (kernel-arguments (list "modprobe.blacklist=nouveau" "nvidia_drm.modeset=1"))
  (kernel-loadable-modules (list (specification->package
                                  "v4l2loopback-linux-module")))
  (firmware (list linux-firmware))
  (initrd microcode-initrd)

  (keyboard-layout (keyboard-layout "us"
                                    "altgr-intl"
                                    #:model "apple"
                                    #:options '("ctrl:nocaps")))

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
                                         "lp"
                                         "audio"
                                         "video"))) %base-user-accounts))

  (groups (cons (user-group
                  (system? #t)
                  (name "realtime")) %base-groups))

  (packages (append %system-core-packages
                    %nvidia-core-packages
                    %cuda-accelerated-packages
                    %secret-mgmt-packages
                    %bluetooth-packages
                    %sound-system-packages
                    %search-and-index-packages
                    %terminal-tools-packages
                    %general-purpose-fonts
                    %google-fonts
                    %cjk-fonts
                    %iosevka-fonts
                    %monospace-fonts
                    %document-fonts
                    %desktop-utilities-packages
                    %version-control-packages
                    %network-utilities-packages
                    %compression-tools-packages
                    %build-system-packages
                    %basic-filesystem-tools
                    %diagnostic-and-maintenance-tools
                    %ssd-tools
                    %base-packages))
  (services
   (append (list (service gnome-desktop-service-type)
                 (service nvidia-service-type
                          (nvidia-configuration (driver nvda-recommended)
                                                (firmware
                                                 nvidia-firmware-recommended)
                                                (module
                                                 nvidia-module-recommended)))
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout)
                                      (modules (cons nvda-recommended
                                                     %default-xorg-modules))
                                      (drivers '("nvidia"))))

                 ;; Printing Services
                 (service cups-service-type
                          (cups-configuration (web-interface? #t)))

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
                 (service nftables-service-type)
                 (service ntp-service-type)

                 ;; Database Services
                 (service mysql-service-type)
                 (service redis-service-type)

                 ;; Desktop Services
                 (simple-service 'blueman dbus-root-service-type
                                 (list blueman))

                 ;; Virtualization Services
                 (service libvirt-service-type
                          (libvirt-configuration (tls-port "16555")))

                 ;; Linux Services
                 (service earlyoom-service-type)
                 (service zram-device-service-type)

                 ;; Guix Services
                 (service guix-home-service-type
                          `(("b" ,%my-home-config)))

                 ;; CI Services
                 (service cuirass-remote-worker-service-type
                          (cuirass-remote-worker-configuration (private-key
                                                                "/etc/guix/signing-key.sec")
                                                               (public-key
                                                                "/etc/guix/signing-key.pub")
                                                               (server
                                                                "5.75.139.97:5555")
                                                               (substitute-urls '
                                                                ("https://ci.guix.gnu.org"
                                                                 "https://substitutes.myguix.bvits.in"))
                                                               (systems '("x86_64-linux"))
                                                               (workers 4)))

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
                                oci-weaviate-service-type
                                oci-neo4j-service-type)))
           (modify-services %my-desktop-services
             (guix-service-type config =>
                                (guix-configuration (inherit config)
                                                    (authorized-keys (append
                                                                      %default-authorized-guix-keys
                                                                      (list (local-file
                                                                             "../../keys/guix/helga.pub")
                                                                            (local-file
                                                                             "../../keys/guix/floki.pub")))))))))
  (name-service-switch %mdns-host-lookup-nss))
