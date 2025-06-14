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
             (gnu packages display-managers)
             (gnu packages fonts)
             (gnu packages linux)
             (gnu packages networking)
             (gnu packages sync)
             (gnu packages video)
             (gnu packages gnome)
             (gnu packages gnome-xyz)
             (gnu services)
             (gnu services avahi)
             (gnu services cuirass)
             (gnu services cups)
             (gnu services databases)
             (gnu services dbus)
             (gnu services desktop)
             (gnu services docker)
             (gnu services file-sharing)
             (gnu services guix)
             (gnu services linux)
             (gnu services networking)
             (gnu services sddm)
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
             (myguix utils)
             (myguix home services emacs)
             (myguix packages base)
             (myguix packages linux)
             (myguix packages nvidia)
             (myguix packages productivity)
             (myguix packages python-pqrs)
             (myguix packages video)
             (myguix services desktop)
             (myguix services mcron)
             (myguix services nvidia)
             (myguix services oci-containers)
             (myguix system install)
             (myguix system linux-initrd)
             (srfi srfi-1)
             (ice-9 match))

(define %pg-pass-file
  "/var/lib/postgresql/airflow.pwd")
(define pg-pass
  (read-secret %pg-pass-file))

(define (extract-propagated-inputs package)
  ;; Drop input labels.  Attempt to support outputs.
  (map (match-lambda
         ((_ (? package? pkg))
          pkg)
         ((_ (? package? pkg) output)
          (list pkg output)))
       (package-propagated-inputs package)))

(define %my-home-config
  (home-environment
    (packages (append
               ;; Media and graphic bundles
               %media-packages
               %graphics-packages
               ;; Audio bundles
               %audio-conversion-packages
               %audio-production-packages
               ;; Video bundles
               %video-conversion-packages
               %video-production-packages))

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
                              `((".gitconfig" ,(local-file
                                                "../../git/gitconfig"))
                                (".gitignore" ,(local-file
                                                "../../git/gitignore"))
                                (".gitattributes" ,(local-file
                                                    "../../git/gitattributes"))))
              ;; Config Files Service
              (simple-service 'my-config-files-service
                              home-xdg-configuration-files-service-type
                              `(("alacritty/alacritty.toml" ,(local-file
                                                              "../../alacritty/alacritty.toml"))
                                ("mpv/input.conf" ,(local-file
                                                    "../../mpv/input.conf"))
                                ("mpv/mpv.conf" ,(local-file
                                                  "../../mpv/mpv.conf"))
                                ("mpv/shaders" ,(local-file
                                                 "../../mpv/shaders"
                                                 #:recursive? #t))))

              ;; Secure Shell
              (service home-openssh-service-type
                       (home-openssh-configuration (hosts (list (openssh-host (name
                                                                               "github.com")


                                                                              (user
                                                                               "git")


                                                                              (identity-file
                                                                               "~/.ssh/id_ed25519"))))
                                                   (authorized-keys (list (local-file
                                                                           "../../keys/ssh/ragnar.pub")))
                                                   (add-keys-to-agent
                                                    "confirm")))

              ;; Desktop Home Services
              (service home-dbus-service-type)

              ;; Sound Home Services
              (service home-pipewire-service-type)

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
  (kernel-arguments (append '("modprobe.blacklist=nouveau"
                              "nvidia_drm.modeset=1" "nvidia_drm.fbdev=1")
                            %default-kernel-arguments))
  (kernel-loadable-modules (list v4l2loopback-linux-module nvidia-module))
  (firmware (list linux-firmware))
  (initrd (lambda (file-systems . rest)
            (apply microcode-initrd
                   file-systems
                   #:initrd base-initrd
                   #:microcode-packages (list amd-microcode intel-microcode)
                   #:linux-modules '("nvidia" "nvidia_modeset"
                                     "nvidia_peermem" "nvidia_uvm"
                                     "nvidia_drm")
                   rest)))

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

  (packages (append %compression-packages %rust-packages
                    (map replace-mesa
                         (append
                          ;; Essential bundles
                          %core-packages
                          %monitoring-packages
                          %versioning-packages
                          %network-packages
                          ;; Desktop bundles
                          %desktop-packages
                          %audio-packages
                          %bluetooth-packages
                          %my-gnome-shell-assets
                          ;; File system bundles
                          %basic-filesystem-packages
                          %advanced-filesystem-packages
                          %remote-filesystem-packages
                          %file-transfer-packages
                          ;; Development packages
                          %development-packages
                          %cuda-packages
                          %guile-packages
                          %python-packages
                          %perl-packages
                          %tree-sitter-packages
                          ;; Document bundles
                          %document-conversion-packages
                          %document-production-packages
                          ;; Font bundles
                          %general-fonts
                          %document-fonts
                          %adobe-fonts
                          %apple-fonts
                          %google-fonts
                          %fira-fonts
                          %iosevka-fonts
                          %monospace-fonts
                          %microsoft-fonts
                          %sans-fonts
                          %serif-fonts
                          %cjk-fonts
                          %unicode-fonts
                          %base-packages))))
  (services
   (append (list (service gnome-desktop-service-type
                          (gnome-desktop-configuration (core-services (map
                                                                       replace-mesa
                                                                       (extract-propagated-inputs
                                                                        gnome-meta-core-services)))
                                                       (shell (map
                                                               replace-mesa
                                                               (extract-propagated-inputs
                                                                gnome-meta-core-shell)))
                                                       (utilities (map
                                                                   replace-mesa
                                                                   (extract-propagated-inputs
                                                                    gnome-meta-core-utilities)))
                                                       (extra-packages (map
                                                                        replace-mesa
                                                                        (extract-propagated-inputs
                                                                         gnome-essential-extras)))))
                 (service gdm-service-type
                          (gdm-configuration (gdm (replace-mesa gdm))
                                             (gnome-shell-assets (map
                                                                  replace-mesa
                                                                  %my-gnome-shell-assets))
                                             (xorg-configuration (xorg-configuration
                                                                  (modules (cons
                                                                            nvda
                                                                            %default-xorg-modules))
                                                                  (drivers '("nvidia"))))))
                 (service nvidia-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout)
                                      (modules (cons nvda
                                                     %default-xorg-modules))
                                      (drivers '("nvidia"))))
                 (service upower-service-type
                          (upower-configuration (upower (replace-mesa upower))))

                 (service gvfs-service-type
                          (gvfs-configuration (gvfs (replace-mesa gvfs))))

                 ;; Printing Services
                 (service cups-service-type
                          (cups-configuration (web-interface? #t)))

                 ;; Networking Services
                 (service openssh-service-type)
                 (service nftables-service-type)
                 (service network-manager-service-type
                          (network-manager-configuration (network-manager (replace-mesa
                                                                           network-manager))
                                                         (vpn-plugins (map
                                                                       replace-mesa
                                                                       (list
                                                                        network-manager-openvpn
                                                                        network-manager-openconnect)))))

                 ;; Database Services
                 (service redis-service-type)
                 (service postgresql-service-type
                          (postgresql-configuration (postgresql (specification->package
                                                                 "postgresql"))
                                                    (config-file (postgresql-config-file
                                                                  (hba-file (plain-file
                                                                             "pg_hba.conf"
                                                                             "
local   all         postgres               peer
"))))))

                 (service postgresql-role-service-type
                          (postgresql-role-configuration (roles (list (postgresql-role
                                                                       (name
                                                                        "airflow")
                                                                       (password-file
                                                                        %pg-pass-file)
                                                                       (permissions '
                                                                        (createdb
                                                                         login))
                                                                       (create-database?
                                                                        #t))))))

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
                 (service docker-service-type
                          (docker-configuration (config-file (local-file
                                                              "../files/daemon.json"))))
                 (service oci-container-service-type
                          (list oci-meilisearch-service-type
                                oci-grobid-service-type oci-neo4j-service-type
                                oci-qdrant-service-type oci-minio-service-type)))
           (modify-services %my-desktop-services
             (delete gvfs-service-type)
             (delete upower-service-type)
             (delete gdm-service-type)
             (delete network-manager-service-type))))
  (name-service-switch %mdns-host-lookup-nss))
