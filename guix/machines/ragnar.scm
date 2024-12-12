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
    (packages (append
               ;; Document bundles
               %document-conversion-packages
               %document-production-packages
               ;; Media and graphic bundles
               %media-packages
               %graphics-packages
               ;; Audio bundles
               %audio-conversion-packages
               %audio-production-packages
               ;; Video bundles
               %video-conversion-packages
               %video-production-packages
               ;; Development bundles
               %guile-packages
               %rust-packages
               %python-packages
               %perl-packages
               ;; Search and Index bundles
               %search-packages
               %opencog-packages))

    (services
     (append (list
              ;; Home Emacs Service
              (service my-home-emacs-service-type)
              ;; Scheduled User’s Job Execution
              (service home-mcron-service-type
                       (home-mcron-configuration (jobs (list
                                                        %garbage-collector-job))))
              ;; Home Files Service
              (simple-service 'my-home-files-service home-files-service-type
                              `((".gitconfig" ,(local-file "../../gitconfig"))))
              ;; Config Files Service
              (simple-service 'my-config-files-service
                              home-xdg-configuration-files-service-type
                              `(("alacritty/alacritty.toml" ,(local-file
                                                              "../../alacritty/alacritty.toml"))
                                ("mpv/input.conf" ,(local-file
                                                    "../../mpv/input.conf"))
                                ("mpv/mpv.conf" ,(local-file
                                                  "../../mpv/mpv.conf"))))

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
  (kernel-arguments (list "modprobe.blacklist=nouveau,amdgpu,radeon"
                          "nvidia_drm.modeset=1"
                          "nvidia.NVreg_EnableGpuFirmware=1"))
  (kernel-loadable-modules (list (specification->package
                                  "v4l2loopback-linux-module")))
  (firmware (list linux-firmware))
  (initrd microcode-initrd)

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps")))

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
                                (device (file-system-label "my-data"))
                                (mount-point "/data")
                                (type "ext4"))
                              (file-system
                                (device (uuid "4426-CA38"
                                              'fat32))
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

  (packages (append
             ;; Essential bundles
             %core-packages
             %monitoring-packages
             %versioning-packages
             %compression-packages
             %network-packages
             ;; Desktop bundles
             %desktop-packages
             %audio-packages
             %bluetooth-packages
             ;; File system bundles
             %basic-filesystem-packages
             %advanced-filesystem-packages
             %remote-filesystem-packages
             %file-transfer-packages
             ;; Development packages
             %development-packages
             %cuda-packages
             %tree-sitter-packages
             ;; Font bundles
             %general-fonts
             %document-fonts
             %adobe-fonts
             %google-fonts
             %fira-fonts
             %iosevka-fonts
             %monospace-fonts
             %sans-fonts
             %serif-fonts
             %cjk-fonts
             %unicode-fonts
             %base-packages))
  (services
   (append (list (service gnome-desktop-service-type)
                 (service nvidia-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout)
                                      (modules (cons nvda
                                                     %default-xorg-modules))
                                      (drivers '("nvidia"))))

                 ;; Printing Services
                 (service cups-service-type
                          (cups-configuration (web-interface? #t)))

                 ;; Networking Services
                 (service openssh-service-type)
                 (service nftables-service-type)

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
                                oci-neo4j-service-type))) %my-desktop-services)))
