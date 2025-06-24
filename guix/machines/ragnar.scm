(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services dict)
             (gnu home services gnupg)
             (gnu home services mcron)
             (gnu home services media)
             (gnu home services music)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu home services syncthing)
             (gnu home services xdg)
             (gnu packages display-managers)
             (gnu packages fonts)
             (gnu packages gnupg)
             (gnu packages linux)
             (gnu packages networking)
             (gnu packages sync)
             (gnu packages video)
             (gnu packages gnome)
             (gnu packages gnome-xyz)
             (gnu packages shellutils)
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

(define-public %default-dotguile
  (plain-file "guile" "(use-modules (ice-9 readline)
                           (ice-9 colorized))
(activate-readline)
(activate-colorized)"))

(define-public %default-gdbinit
  (plain-file "gdbinit" "set history save on
set history filename ~/.local/state/gdb_history
set history size 10000"))

(define-public %default-nanorc
  (plain-file "nanorc" "set positionlog
set historylog
set suspendable"))

;;; Home configuration
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
     (list
      ;; Start with the base services from myguix
      (service home-files-service-type
               `((".guile" ,%default-dotguile)))

      (service home-xdg-configuration-files-service-type
               `(("gdb/gdbinit" ,%default-gdbinit)
                 ("nano/nanorc" ,%default-nanorc)))

      (service home-xdg-user-directories-service-type
               (home-xdg-user-directories-configuration (desktop
                                                         "$HOME/desktop")
                                                        (documents
                                                         "$HOME/documents")
                                                        (download
                                                         "$HOME/downloads")
                                                        (music "$HOME/music")
                                                        (pictures
                                                         "$HOME/pictures")
                                                        (publicshare
                                                         "$HOME/public")
                                                        (templates
                                                         "$HOME/templates")
                                                        (videos "$HOME/videos")))
      (service my-home-emacs-service-type)

      (service home-inputrc-service-type
               (home-inputrc-configuration (key-bindings `(("Control-l" . "clear-screen")
                                                           ("TAB" . "menu-complete")))
                                           (variables `(("bell-style" . "visible")
                                                        ("editing-mode" . "emacs")
                                                        ("show-all-if-ambiguous" . #t)
                                                        ("mark-symlinked-directories" . #t)
                                                        ("visible-stats" . #t)
                                                        ("colored-stats" . #t)
                                                        ("colored-completion-prefix" . #t)
                                                        ("menu-complete-display-prefix" . #t)))))

      ;; Scheduled User's Job Execution
      (service home-mcron-service-type
               (home-mcron-configuration (jobs (list %garbage-collector-job))))

      ;; Configuration files
      ;; Using simple-service to extend the existing home-files-service-type
      (simple-service 'ragnar-dotfiles home-files-service-type
                      `((".gitconfig" ,(local-file "../../git/gitconfig"))
                        (".gitignore" ,(local-file "../../git/gitignore"))
                        (".gitattributes" ,(local-file
                                            "../../git/gitattributes"))))

      ;; XDG configuration files
      (simple-service 'ragnar-xdg-config
                      home-xdg-configuration-files-service-type
                      `( ;Files from dotfiles repository
                         ("alacritty/alacritty.toml" ,(local-file
                                                       "../../alacritty/alacritty.toml"))
                        ("mpv/input.conf" ,(local-file "../../mpv/input.conf"))
                        ("mpv/mpv.conf" ,(local-file "../../mpv/mpv.conf"))
                        ("mpv/shaders" ,(local-file "../../mpv/shaders"
                                                    #:recursive? #t))))

      ;; Zsh configuration
      (service home-zsh-service-type
               (home-zsh-configuration (xdg-flavor? #t)
                                       (zshenv (list (local-file
                                                      "../../zsh/zshenv"
                                                      "zshenv"
                                                      #:recursive? #t)))
                                       (zshrc (list
                                               ;; Load main zshrc
                                               (local-file "../../zsh/zshrc"
                                                           "zshrc"
                                                           #:recursive? #t)

                                               ;; Zsh plugins
                                               (mixed-text-file
                                                "zsh-syntax-highlighting"
                                                "source "
                                                zsh-syntax-highlighting
                                                "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh")

                                               (mixed-text-file
                                                "zsh-history-substring-search"
                                                "source "
                                                zsh-history-substring-search
                                                "/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh")

                                               (mixed-text-file
                                                "zsh-completions" "fpath+=\""
                                                zsh-completions
                                                "/share/zsh/site-functions\"")

                                               (mixed-text-file
                                                "zsh-autosuggestions"
                                                "source " zsh-autosuggestions
                                                "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh")

                                               (mixed-text-file "zsh-autopair"
                                                "source " zsh-autopair
                                                "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh")))))

      ;; GPG Agent
      (service home-gpg-agent-service-type
               (home-gpg-agent-configuration (pinentry-program (file-append
                                                                pinentry-gnome3
                                                                "/bin/pinentry-gnome3"))
                                             (ssh-support? #t)
                                             (default-cache-ttl 28800) ;8 hours
                                             (max-cache-ttl 86400) ;24 hours
                                             (default-cache-ttl-ssh 28800)
                                             (max-cache-ttl-ssh 86400)
                                             (extra-content
                                              "enable-ssh-support
allow-loopback-pinentry
allow-preset-passphrase")))

      ;; SSH configuration
      (service home-openssh-service-type
               (home-openssh-configuration (hosts (list (openssh-host (name
                                                                       "github.com")
                                                                      (user
                                                                       "git")
                                                                      (identity-file
                                                                       "~/.ssh/id_ed25519")
                                                                      (port 22)
                                                                      (extra-content
                                                                       "
  ControlMaster auto
  ControlPath ~/.ssh/control-%C
  ControlPersist 10m"))
                                                        (openssh-host (name
                                                                       "gitlab.com")
                                                                      (user
                                                                       "git")
                                                                      (identity-file
                                                                       "~/.ssh/id_ed25519"))))
                                           (authorized-keys (list (local-file
                                                                   "../../keys/ssh/ragnar.pub")))
                                           (add-keys-to-agent "confirm")))

      ;; Desktop Home Services
      (service home-dbus-service-type)

      ;; Sound Home Services
      (service home-pipewire-service-type)

      ;; Networking Home Services
      (service home-syncthing-service-type)

      ;; Miscellaneous Home Services
      (service home-beets-service-type
               (home-beets-configuration (directory "/home/b/music")))))))

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

  (packages (append %compression-packages
                    %rust-packages
                    ;; Essential bundles
                    %core-packages
                    %monitoring-packages
                    %versioning-packages
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
                    %base-packages))
  (services
   (append (list (service gnome-desktop-service-type)
                 (service gdm-service-type
                          (gdm-configuration (gnome-shell-assets
                                              %my-gnome-shell-assets)
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
                 ;; Printing Services
                 (service cups-service-type
                          (cups-configuration (web-interface? #t)))

                 ;; Networking Services
                 (service openssh-service-type)
                 (service nftables-service-type)
                 (service network-manager-service-type
                          (network-manager-configuration (vpn-plugins (list
                                                                       network-manager-openvpn
                                                                       network-manager-openconnect))))

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
             (delete gdm-service-type)
             (delete network-manager-service-type))))
  (name-service-switch %mdns-host-lookup-nss))
