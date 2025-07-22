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
             (gnu packages node)
             (gnu packages python)
             (gnu packages docker)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages databases)
             (gnu packages package-management)
             (gnu services)
             (gnu services admin)
             (gnu services base)
             (gnu services avahi)
             (gnu services cuirass)
             (gnu services cups)
             (gnu services shepherd)
             (gnu services databases)
             (gnu services dbus)
             (gnu services desktop)
             (gnu services docker)
             (gnu services file-sharing)
             (gnu services guix)
             (gnu services linux)
             (gnu services monitoring)
             (gnu services networking)
             (gnu services sddm)
             (gnu services spice)
             (gnu services ssh)
             (gnu services sysctl)
             (gnu services backup)
             (gnu services virtualization)
             (gnu services cuirass)
             (gnu services web)
             (gnu services xorg)
             (gnu system)
             (gnu system install)
             (gnu system linux-initrd)
             (gnu system nss)
             (gnu system shadow)
             (guix packages)
             (guix deprecation)
             (guix gexp)
             (myguix home services emacs)
             (myguix home services nougat)
             (myguix packages base)
             (myguix packages fonts)
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
             (ice-9 match)
             (ice-9 threads))

(define-public %default-dotguile
  (plain-file "guile" "(use-modules (ice-9 readline)
                           (ice-9 colorized))
(activate-readline)
(activate-colorized)"))

;;; Home configuration
(define %my-home-config
  (home-environment
    (packages (append

                      (list ;Original fonts
                            font-google-roboto
                            font-fira-go
                            font-fira-sans
                            font-fira-code
                            font-fira-mono
                            font-tex-gyre
                            ;; Programming fonts
                            font-jetbrains-mono
                            font-hack
                            font-ibm-plex
                            font-adobe-source-code-pro
                            font-victor-mono
                            ;; Document & UI fonts
                            font-adobe-source-sans
                            font-adobe-source-serif
                            font-dejavu
                            font-liberation
                            ;; International support
                            font-google-noto
                            font-google-noto-emoji
                            font-adobe-source-han-sans
                            font-adobe-source-han-mono
                            ;; Special purpose
                            font-stix-two
                            ;; Tufte-approved fonts
                            font-et-book
                            font-sil-gentium
                            font-sil-charis
                            font-libertinus
                            font-charter
                            font-lato)))

    (services
     (list
      ;; Start with the base services from myguix
      (service home-files-service-type
               `((".guile" ,%default-dotguile)))

      ;; Environment variables
      (simple-service 'custom-environment-variables
                      home-environment-variables-service-type
                      `(("EDITOR" . "emacsclient -nw")
                        ("VISUAL" . "emacsclient -c")
                        ("BROWSER" . "firefox")
                        ("PAGER" . "less")
                        ("LESS" . "-FRX")
                        ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                        ;; Development
                        ("MAKEFLAGS" unquote
                         (string-append "-j"
                                        (number->string (max 1
                                                             (- (current-processor-count)
                                                                2)))))
                        ("CMAKE_GENERATOR" . "Ninja")
                        ;; XDG paths
                        ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share")
                        ;; Python
                        ("PYTHONDONTWRITEBYTECODE" . "1")
                        ("PYTHONUNBUFFERED" . "1")
                        ;; Rust
                        ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
                        ("RUSTUP_HOME" . "$XDG_DATA_HOME/rustup")
                        ;; Go
                        ("GOPATH" . "$XDG_DATA_HOME/go")
                        ;; Node.js
                        ("NPM_CONFIG_USERCONFIG" . "$XDG_CONFIG_HOME/npm/npmrc")
                        ("NPM_CONFIG_CACHE" . "$XDG_CACHE_HOME/npm")
                        ;; CUDA (for mileva)
                        ("CUDA_CACHE_PATH" . "$XDG_CACHE_HOME/nv")
                        ;; Wayland
                        ("MOZ_ENABLE_WAYLAND" . "1")
                        ("QT_QPA_PLATFORM" . "wayland;xcb")
                        ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

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

      ;; Default applications (XDG MIME associations)
      (simple-service 'default-applications
                      home-xdg-configuration-files-service-type
                      `(("mimeapps.list" ,(plain-file "mimeapps.list"
                                           "[Default Applications]
text/html=firefox.desktop
x-scheme-handler/http=firefox.desktop
x-scheme-handler/https=firefox.desktop
x-scheme-handler/about=firefox.desktop
x-scheme-handler/unknown=firefox.desktop
application/pdf=org.gnome.Evince.desktop
image/png=org.gnome.eog.desktop
image/jpeg=org.gnome.eog.desktop
image/gif=org.gnome.eog.desktop
image/webp=org.gnome.eog.desktop
video/mp4=mpv.desktop
video/x-matroska=mpv.desktop
video/webm=mpv.desktop
audio/mpeg=audacious.desktop
audio/flac=audacious.desktop
audio/mp3=audacious.desktop
text/plain=org.gnome.TextEditor.desktop
text/x-c=org.gnome.TextEditor.desktop
text/x-python=org.gnome.TextEditor.desktop
application/x-shellscript=org.gnome.TextEditor.desktop
inode/directory=org.gnome.Nautilus.desktop
"))))
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

      ;; Configuration files
      ;; Using simple-service to extend the existing home-files-service-type
      (simple-service 'mileva-dotfiles home-files-service-type
                      `((".gitconfig" ,(local-file "../../git/gitconfig"
                                                   "gitconfig"
                                                   #:recursive? #f))
                        (".gitignore" ,(local-file "../../git/gitignore"
                                                   "gitignore"
                                                   #:recursive? #f))
                        (".gitattributes" ,(local-file
                                            "../../git/gitattributes"
                                            "gitattributes"
                                            #:recursive? #f))))

      ;; XDG configuration files
      (simple-service 'mileva-xdg-config
                      home-xdg-configuration-files-service-type
                      `(("alacritty/alacritty.toml" ,(local-file
                                                      "../../alacritty/alacritty.toml"
                                                      "alacritty.toml"
                                                      #:recursive? #f))
                        ("mpv/input.conf" ,(local-file "../../mpv/input.conf"
                                                       "mpv-input.conf"
                                                       #:recursive? #f))
                        ("mpv/mpv.conf" ,(local-file "../../mpv/mpv.conf"
                                                     "mpv.conf"
                                                     #:recursive? #f))
                        ("mpv/shaders" ,(local-file "../../mpv/shaders"
                                                    "mpv-shaders"
                                                    #:recursive? #t))))

      ;; Zsh configuration
      (service home-zsh-service-type
               (home-zsh-configuration (xdg-flavor? #t)
                                       (zshenv (list (local-file
                                                      "../../zsh/zshenv"
                                                      "zshenv"
                                                      #:recursive? #f)))
                                       (zshrc (list
                                               ;; Load main zshrc
                                               (local-file "../../zsh/zshrc"
                                                           "zshrc"
                                                           #:recursive? #f)

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
                                                "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh")

                                               (mixed-text-file "fzf-tab"
                                                "source " fzf-tab
                                                "/share/zsh/plugins/fzf-tab/fzf-tab.plugin.zsh")))))

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
               (home-openssh-configuration (hosts (list
                                                   ;; GitHub
                                                   (openssh-host (name
                                                                  "github.com")
                                                                 (user "git")
                                                                 (identity-file
                                                                  "~/.ssh/id_ed25519")
                                                                 (port 22)
                                                                 (extra-content
                                                                  "
  ControlMaster auto
  ControlPath ~/.ssh/control-%C
  ControlPersist 10m"))

                                                   ;; GitLab
                                                   (openssh-host (name
                                                                  "gitlab.com")
                                                                 (user "git")
                                                                 (identity-file
                                                                  "~/.ssh/id_ed25519")
                                                                 (port 22))

                                                   ;; Codeberg
                                                   (openssh-host (name
                                                                  "codeberg.org")
                                                                 (user "git")
                                                                 (identity-file
                                                                  "~/.ssh/id_ed25519")
                                                                 (port 22))

                                                   ;; Spärck (laptop)
                                                   (openssh-host (name
                                                                  "spärck")
                                                                 (host-name
                                                                  "spärck.local")
                                                                 (user "b")
                                                                 (identity-file
                                                                  "~/.ssh/id_ed25519")
                                                                 (forward-agent?
                                                                  #t)
                                                                 (compression?
                                                                  #t)
                                                                 (extra-content
                                                                  "
  ControlMaster auto
  ControlPath ~/.ssh/control-%C
  ControlPersist 30m
  ServerAliveInterval 60
  ServerAliveCountMax 3"))

                                                   ;; Global SSH defaults
                                                   (openssh-host (name "*")
                                                                 (extra-content
                                                                  "
  HashKnownHosts yes
  IdentitiesOnly yes
  StrictHostKeyChecking ask
  VerifyHostKeyDNS yes
  VisualHostKey yes"))))
                                           (add-keys-to-agent "confirm")))

      ;; Desktop Home Services
      (service home-dbus-service-type)

      ;; Sound Home Services
      (service home-pipewire-service-type)

      ;; Networking Home Services
      (service home-syncthing-service-type)

      ;; Miscellaneous Home Services
      (service home-beets-service-type
               (home-beets-configuration (directory "/home/b/music")))
      (service home-nougat-service-type)))))

(operating-system
  (host-name "mileva")
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
                                (type "btrfs"))
                              (file-system
                                (device (file-system-label "my-data"))
                                (mount-point "/data")
                                (type "btrfs"))
                              (file-system
                                (device (uuid "812C-DF8C"
                                              'fat32))
                                (mount-point "/boot/efi")
                                (type "vfat"))) %base-file-systems))

  (users (cons (user-account
                 (name "b")
                 (comment "Ayan")
                 (group "users")
                 (home-directory "/home/b")
                 (shell (file-append (specification->package "zsh") "/bin/zsh"))
                 (supplementary-groups '("wheel" "netdev"
                                         "kvm"
                                         "tty"
                                         "libvirt"
                                         "input"
                                         "docker"
                                         "realtime"
                                         "lp"
                                         "audio"
                                         "video"))) %base-user-accounts))

  (groups (cons* (user-group
                   (system? #t)
                   (name "realtime")) %base-groups))

  (packages (append
             ;; Core System
             %core-minimal
             %core-extended
             %shell-modern
             %terminal-essentials
             %text-editors
             %security-essentials
             %ai-assistants
             ;; System Management
             %system-monitoring
             %hardware-monitoring
             %nvidia-monitoring ;For RTX 3060
             %system-debugging
             %performance-profiling
             %benchmark-tools
             ;; Development - Core
             %build-essentials
             %compiler-toolchains
             %version-control
             %compression-tools
             ;; Development - Languages (workstation needs all)
             %c-cpp-development
             %rust-development
             %python-development
             %python-profiling
             %haskell-development
             %clojure-development
             %guile-development
             %perl-development
             ;; Language Servers
             %language-servers-core
             %language-servers-extended
             ;; Development - Support
             %tree-sitter-core
             %tree-sitter-extended
             %documentation-tools
             %cuda-packages ;For CUDA development with RTX 3060
             ;; Networking
             %network-core
             %network-diagnostics
             %network-performance
             ;; File Management
             %filesystem-core
             %filesystem-advanced
             %cloud-sync
             %backup-tools
             %file-sharing
             %download-tools
             ;; Desktop
             %desktop-browsers
             %desktop-core
             %audio-system
             %bluetooth-system
             %my-gnome-shell-assets
             ;; Media (workstation needs full media capabilities)
             %media-players
             %media-editors
             %media-converters
             ;; Documents
             %latex-core
             %latex-extended
             %document-conversion-packages
             %spell-checkers
             ;; Math/Science
             %math-core
             %math-applications
             %scientific-computing
             ;; Fonts (comprehensive for workstation)
             %fonts-essential
             %fonts-programming
             %fonts-document
             %fonts-international
             %base-packages))
  (services
   (append (list (service gnome-desktop-service-type)
                 (service gdm-service-type
                          (gdm-configuration (wayland? #t)
                                             (debug? #f)
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
                 (service redis-service-type
                          (redis-configuration (bind "127.0.0.1")
                                               (port 6379)
                                               (working-directory
                                                "/var/lib/redis")))

                 (service postgresql-service-type
                          (postgresql-configuration (postgresql (specification->package
                                                                 "postgresql"))
                                                    (extension-packages (list (specification->package
                                                                               "postgis")))
                                                    (config-file (postgresql-config-file
                                                                  (log-destination
                                                                   "stderr")
                                                                  (hba-file (plain-file
                                                                             "pg_hba.conf"
                                                                             "# TYPE  DATABASE        USER            ADDRESS                 METHOD
local   all             postgres                                peer
local   all             all                                     peer
host    all             all             127.0.0.1/32            md5
host    all             all             ::1/128                 md5"))
                                                                  (extra-config '
                                                                   (("shared_buffers"
                                                                     "4GB")
                                                                    ;; 64GB RAM, use ~6%
                                                                    ("effective_cache_size"
                                                                     "16GB") ;~25% of RAM
                                                                    ("maintenance_work_mem"
                                                                     "1GB")
                                                                    ("work_mem"
                                                                     "64MB")
                                                                    ("max_connections"
                                                                     "200")
                                                                    ("random_page_cost"
                                                                     "1.1") ;SSD optimization
                                                                    ("effective_io_concurrency"
                                                                     "200") ;SSD optimization
                                                                    ("wal_buffers"
                                                                     "16MB")
                                                                    ("min_wal_size"
                                                                     "1GB")
                                                                    ("max_wal_size"
                                                                     "4GB")
                                                                    ("checkpoint_completion_target"
                                                                     "0.9")
                                                                    ("logging_collector"
                                                                     #t)
                                                                    ("log_directory"
                                                                     "/var/log/postgresql")
                                                                    ("log_filename"
                                                                     "postgresql-%Y-%m-%d_%H%M%S.log")
                                                                    ("log_rotation_age"
                                                                     "1d")
                                                                    ("log_rotation_size"
                                                                     "100MB")))))))

                 (service mysql-service-type
                          (mysql-configuration (mysql mariadb)
                                               (bind-address "127.0.0.1")
                                               (port 3306)
                                               (extra-content
                                                "
# Performance tuning for 64GB RAM workstation
innodb_buffer_pool_size = 8G
innodb_log_file_size = 2G
innodb_flush_method = O_DIRECT
innodb_flush_log_at_trx_commit = 2
innodb_io_capacity = 2000
innodb_io_capacity_max = 4000

# Connection settings
max_connections = 500
thread_cache_size = 50
table_open_cache = 4000

# Query cache (disabled by default in MariaDB 10.3+)
query_cache_type = 0
query_cache_size = 0

# Logging
log_error = /var/log/mysql/error.log
slow_query_log = 1
slow_query_log_file = /var/log/mysql/slow.log
long_query_time = 2

# Character set
character-set-server = utf8mb4
collation-server = utf8mb4_unicode_ci")))

                 ;; PostgreSQL roles and databases
                 (service postgresql-role-service-type
                          (postgresql-role-configuration (roles (list (postgresql-role
                                                                       (name
                                                                        "b")
                                                                       (create-database?
                                                                        #t)
                                                                       (permissions '
                                                                        (createdb
                                                                         login)))))))

                 ;; Desktop Services
                 (simple-service 'blueman dbus-root-service-type
                                 (list blueman))

                 ;; Virtualization Services
                 (service libvirt-service-type
                          (libvirt-configuration (tls-port "16555")))

                 ;; Linux Services
                 (service earlyoom-service-type
                          (earlyoom-configuration (minimum-available-memory 5) ;Kill when <5% RAM available
                                                  (minimum-free-swap 5) ;Kill when <5% swap free
                                                  (memory-report-interval 60) ;Report every minute
                                                  (avoid-regexp
                                                   "^(sshd|guix-daemon|postgres|mysql|redis-server)$")
                                                  (show-debug-messages? #f))) ;Less verbose
                 
                 (service zram-device-service-type
                          (zram-device-configuration (size "8G") ;8GB compressed swap (64GB RAM)
                                                     (compression-algorithm 'zstd) ;Better compression ratio
                                                     (memory-limit "16G") ;Max 16GB uncompressed
                                                     (priority 100))) ;High priority
                 
                 (service fstrim-service-type
                          (fstrim-configuration (schedule "0 0 * * 0") ;Weekly on Sunday at midnight
                                                (verbose? #f)))

                 ;; Guix Services
                 (service guix-home-service-type
                          `(("b" ,%my-home-config)))

                 ;; Monitoring Services
                 (service tailon-service-type
                          (tailon-configuration (config-file (tailon-configuration-file
                                                              (files (list
                                                                      "/var/log"
                                                                      (list
                                                                       "PostgreSQL Logs"
                                                                       "/var/log/postgresql")
                                                                      (list
                                                                       "MySQL Logs"
                                                                       "/var/log/mysql")
                                                                      (list
                                                                       "Docker Logs"
                                                                       "/var/log/docker")
                                                                      (list
                                                                       "System Logs"
                                                                       "/var/log/messages"
                                                                       "/var/log/secure"
                                                                       "/var/log/shepherd.log")))
                                                              (bind
                                                               "localhost:8081") ;Different port to avoid conflicts
                                                              (allowed-commands '
                                                               ("tail" "grep"
                                                                "awk"))
                                                              (tail-lines 500)
                                                              (wrap-lines #t)))))

                 (service prometheus-node-exporter-service-type
                          (prometheus-node-exporter-configuration (web-listen-address
                                                                   ":9100")))

                 (service vnstat-service-type
                          (vnstat-configuration (database-directory
                                                 "/var/lib/vnstat")
                                                (save-interval 5)
                                                (bandwidth-detection? #t)
                                                (use-logging 2)))

                 ;; Miscellaneous Services
                 (service sysctl-service-type
                          (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1")
                                                                    ("vm.max_map_count" . "262144"))
                                                           %default-sysctl-settings))))

                 ;; Extra special files for development
                 (extra-special-file "/usr/bin/python3"
                                     (file-append python "/bin/python3"))
                 (extra-special-file "/usr/bin/node"
                                     (file-append node "/bin/node"))

                 ;; Custom hosts for local development
                 (simple-service 'development-hosts hosts-service-type
                                 (list (host "127.0.0.1" "mileva.local"
                                             '("api.local" "dev.local"))
                                       (host "::1" "mileva.local"
                                             '("api.local" "dev.local"))))

                 ;; Scheduled jobs using Shepherd timers
                 (simple-service 'system-timers shepherd-root-service-type
                                 (list
                                  ;; Garbage collection daily at 4AM
                                  (shepherd-timer '(garbage-collection)
                                                  "0 4 * * *"
                                                  #~(list #$(file-append bash
                                                             "/bin/bash") "-c"
                                                          (string-append
                                                           "mkdir -p /var/log/guix-gc && "
                                                           "LOG_FILE=/var/log/guix-gc/gc-$(date +%Y%m%d-%H%M%S).log && "
                                                           "exec > \"$LOG_FILE\" 2>&1 && "
                                                           "echo \"Starting garbage collection at $(date)\" && "
                                                           "df -h / && "
                                                           #$(file-append guix
                                                              "/bin/guix")
                                                           " gc -F 50G && "
                                                           "echo \"Garbage collection completed at $(date)\" && "
                                                           "df -h / && "
                                                           "find /var/log/guix-gc -name 'gc-*.log' -mtime +30 -delete"))
                                                  #:requirement '(guix-daemon))

                                  ;; Update package database weekly on Sunday at 3AM
                                  (shepherd-timer '(update-package-database)
                                                  "0 3 * * 0"
                                                  #~(list #$(file-append bash
                                                             "/bin/bash") "-c"
                                                          (string-append
                                                           "mkdir -p /var/log/guix-updates && "
                                                           "LOG_FILE=/var/log/guix-updates/update-$(date +%Y%m%d-%H%M%S).log && "
                                                           "exec > \"$LOG_FILE\" 2>&1 && "
                                                           "echo \"Starting package database update at $(date)\" && "
                                                           #$(file-append guix
                                                              "/bin/guix")
                                                           " pull && "
                                                           "echo \"Package database updated at $(date)\" && "
                                                           #$(file-append guix
                                                              "/bin/guix")
                                                           " package --list-generations && "
                                                           "find /var/log/guix-updates -name 'update-*.log' -mtime +30 -delete"))
                                                  #:requirement '(guix-daemon
                                                                  networking))

                                  ;; Docker cleanup weekly on Saturday at 2AM
                                  (shepherd-timer '(docker-cleanup)
                                                  "0 2 * * 6"
                                                  #~(list #$(file-append bash
                                                             "/bin/bash") "-c"
                                                          (string-append
                                                           "mkdir -p /var/log/docker-cleanup && "
                                                           "LOG_FILE=/var/log/docker-cleanup/cleanup-$(date +%Y%m%d-%H%M%S).log && "
                                                           "exec > \"$LOG_FILE\" 2>&1 && "
                                                           "echo \"Starting Docker cleanup at $(date)\" && "
                                                           "echo \"Disk usage before cleanup:\" && "
                                                           "df -h /var/lib/docker && "
                                                           #$(file-append
                                                              docker-cli
                                                              "/bin/docker")
                                                           " system df && "
                                                           #$(file-append
                                                              docker-cli
                                                              "/bin/docker")
                                                           " system prune -af --volumes && "
                                                           "echo \"Docker cleanup completed at $(date)\" && "
                                                           "echo \"Disk usage after cleanup:\" && "
                                                           "df -h /var/lib/docker && "
                                                           #$(file-append
                                                              docker-cli
                                                              "/bin/docker")
                                                           " system df && "
                                                           "find /var/log/docker-cleanup -name 'cleanup-*.log' -mtime +30 -delete"))
                                                  #:requirement '(dockerd))

                                  ;; Clean CUDA cache monthly on the 1st at 3AM
                                  (shepherd-timer '(cuda-cache-cleanup)
                                                  "0 3 1 * *"
                                                  #~(list #$(file-append bash
                                                             "/bin/bash") "-c"
                                                          (string-append
                                                           "mkdir -p /var/log/cuda-cleanup && "
                                                           "LOG_FILE=/var/log/cuda-cleanup/cleanup-$(date +%Y%m%d-%H%M%S).log && "
                                                           "exec > \"$LOG_FILE\" 2>&1 && "
                                                           "echo \"Starting CUDA cache cleanup at $(date)\" && "
                                                           "if [ -d /home/b/.nv/ComputeCache ]; then "
                                                           "  SIZE=$(du -sh /home/b/.nv/ComputeCache 2>/dev/null | cut -f1 || echo '0B') && "
                                                           "  echo \"CUDA cache size before cleanup: $SIZE\" && "
                                                           "  "
                                                           #$(file-append
                                                              coreutils
                                                              "/bin/rm")
                                                           " -rf /home/b/.nv/ComputeCache && "
                                                           "  echo \"CUDA cache cleaned successfully\" "
                                                           "else "
                                                           "  echo \"No CUDA cache directory found\" "
                                                           "fi && "
                                                           "echo \"CUDA cache cleanup completed at $(date)\" && "
                                                           "find /var/log/cuda-cleanup -name 'cleanup-*.log' -mtime +90 -delete"))
                                                  #:requirement '(user-processes))

                                  ;; Daily backup of critical files to Google Drive at 1AM
                                  (shepherd-timer '(rclone-backup-critical)
                                                  "0 1 * * *"
                                                  #~(list #$(file-append bash
                                                             "/bin/bash") "-c"
                                                          (string-append
                                                           ;; Create log directory and set up logging
                                                           "mkdir -p /var/log/rclone-backup && "
                                                           "LOG_FILE=/var/log/rclone-backup/critical-$(date +%Y%m%d-%H%M%S).log && "
                                                           "exec > \"$LOG_FILE\" 2>&1 && "
                                                           "echo \"Starting critical backup at $(date)\" && "
                                                           ;; Run as user 'b' with proper environment
                                                           "su - b -c '"
                                                           ;; Documents backup
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/documents remote:backup/documents"
                                                           " --exclude \"*.tmp\" --exclude \"*~\" --exclude \".#*\""
                                                           " --log-level INFO && "
                                                           ;; Projects backup
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/projects remote:backup/projects"
                                                           " --exclude \"**/.git/**\" --exclude \"**/node_modules/**\""
                                                           " --exclude \"**/__pycache__/**\" --exclude \"**/target/**\""
                                                           " --exclude \"**/.venv/**\" --exclude \"**/venv/**\""
                                                           " --exclude \"**/.cargo/**\" --exclude \"**/dist/**\""
                                                           " --exclude \"**/build/**\" --exclude \"*.pyc\""
                                                           " --exclude \"*.pyo\" --exclude \"*.so\""
                                                           " --exclude \"*.o\" --exclude \"*.a\""
                                                           " --exclude \"*.tar.gz\" --exclude \"*.zip\""
                                                           " --log-level INFO && "
                                                           ;; Password store backup
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/.password-store remote:backup/password-store"
                                                           " --log-level INFO && "
                                                           ;; SSH config backup
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/.ssh remote:backup/ssh"
                                                           " --log-level INFO && "
                                                           ;; GnuPG backup
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/.gnupg remote:backup/gnupg"
                                                           " --log-level INFO"
                                                           "' && "
                                                           "echo \"Backup completed successfully at $(date)\" && "
                                                           ;; Keep only last 7 days of logs
                                                           "find /var/log/rclone-backup -name 'critical-*.log' -mtime +7 -delete"))
                                                  #:requirement '(networking
                                                                  user-processes))

                                  ;; Weekly backup of larger/less critical files on Sunday at 2AM
                                  (shepherd-timer '(rclone-backup-weekly)
                                                  "0 2 * * 0"
                                                  #~(list #$(file-append bash
                                                             "/bin/bash") "-c"
                                                          (string-append
                                                           ;; Create log directory and set up logging
                                                           "mkdir -p /var/log/rclone-backup && "
                                                           "LOG_FILE=/var/log/rclone-backup/weekly-$(date +%Y%m%d-%H%M%S).log && "
                                                           "exec > \"$LOG_FILE\" 2>&1 && "
                                                           "echo \"Starting weekly backup at $(date)\" && "
                                                           ;; Run as user 'b' with proper environment
                                                           "su - b -c '"
                                                           ;; Pictures backup
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/pictures remote:backup/pictures"
                                                           " --exclude \"*.tmp\""
                                                           " --log-level INFO && "
                                                           ;; Config backup (selective)
                                                           #$(file-append
                                                              rclone
                                                              "/bin/rclone")
                                                           " sync /home/b/.config remote:backup/config"
                                                           " --filter \"+ alacritty/**\""
                                                           " --filter \"+ mpv/**\""
                                                           " --filter \"+ emacs/**\""
                                                           " --filter \"+ git/**\""
                                                           " --filter \"+ direnv/**\""
                                                           " --filter \"+ *.conf\""
                                                           " --filter \"+ *.toml\""
                                                           " --filter \"- *\""
                                                           " --log-level INFO"
                                                           "' && "
                                                           "echo \"Weekly backup completed successfully at $(date)\" && "
                                                           ;; Keep only last 4 weeks of logs
                                                           "find /var/log/rclone-backup -name 'weekly-*.log' -mtime +28 -delete"))
                                                  #:requirement '(networking
                                                                  user-processes))))

                 ;; Environment variables for Wayland preference
                 (simple-service 'wayland-environment
                                 session-environment-service-type
                                 '(("MOZ_ENABLE_WAYLAND" . "1") ;Firefox Wayland
                                   ("QT_QPA_PLATFORM" . "wayland;xcb") ;Qt apps prefer Wayland
                                   ("CLUTTER_BACKEND" . "wayland") ;Clutter/GTK
                                   ("SDL_VIDEODRIVER" . "wayland") ;SDL applications
                                   ("_JAVA_AWT_WM_NONREPARENTING" . "1") ;Java apps
                                   ("GDK_BACKEND" . "wayland,x11")))

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
