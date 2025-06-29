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
             (myguix home services emacs-daemon)
             (gnu packages display-managers)
             (gnu packages admin)
             (gnu packages fonts)
             (gnu packages fontutils)
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
             (gnu packages databases)
             (gnu packages package-management)
             (gnu packages emacs)
             (gnu packages version-control)
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
             (myguix home)
             (myguix utils)
             (myguix home services emacs)
             (myguix home services nougat)
             (myguix packages base)
             (myguix packages fonts)
             (myguix packages linux)
             (myguix packages nvidia)
             (myguix packages productivity)
             (myguix packages python-pqrs)
             (myguix packages video)
             (myguix services backup)
             (myguix services desktop)
             (myguix services mcron)
             (myguix services nvidia)
             (myguix services oci-containers)
             (myguix system install)
             (myguix system linux-initrd)
             (srfi srfi-1)
             (ice-9 match)
             (ice-9 threads))

;;; Home configuration
(define %my-home-config
  (home-environment
    (packages (list
               ;; Apple fonts
               font-apple-sf-pro
               font-apple-sf-mono
               font-apple-sf-compact
               font-apple-new-york
               font-apple-sf-symbols
               font-apple-color-emoji

               ;; Roboto family
               font-google-roboto

               ;; Fira family
               font-fira-code
               font-fira-sans
               font-fira-go))

    (services
     (append
      ;; Use base services from myguix
      %my-base-home-services

      (list
       ;; Only add what's UNIQUE to this system
       (simple-service 'mileva-specific-environment
                       home-environment-variables-service-type
                       `(("EDITOR" . "nvim") ("VISUAL" . "nvim")
                         ("BROWSER" . "firefox")
                         ("MAKEFLAGS" unquote
                          (string-append "-j"
                                         (number->string (max 1
                                                              (- (current-processor-count)
                                                                 2)))))
                         ("CMAKE_GENERATOR" . "Ninja")))

       ;; Default applications (XDG MIME associations)
       (simple-service 'default-applications
                       home-xdg-configuration-files-service-type
                       `(("mimeapps.list" ,%default-mimeapps)))

       ;; Emacs services
       (service my-home-emacs-service-type)

       (service home-emacs-daemon-service-type
                (emacs-daemon-configuration (package
                                              emacs-pgtk)
                                            (server-name "mileva")))

       ;; Local configuration files
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

       ;; Local XDG configuration files
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
                                        (environment-variables
                                         %my-shell-environment-variables)
                                        (zshenv (list (local-file
                                                       "../../zsh/zshenv"
                                                       "zshenv"
                                                       #:recursive? #f)))
                                        (zshrc (list (local-file
                                                      "../../zsh/zshrc"
                                                      "zshrc"
                                                      #:recursive? #f)

                                                     ;; Add shared aliases
                                                     (plain-file "zsh-aliases"
                                                      (string-join (map (lambda 
                                                                                (alias)
                                                                          (format
                                                                           #f
                                                                           "alias ~a=\"~a\""
                                                                           (car
                                                                            alias)
                                                                           (cdr
                                                                            alias)))
                                                                    %my-shell-aliases)
                                                                   "\n"))

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
                                                      "zsh-completions"
                                                      "fpath+=\""
                                                      zsh-completions
                                                      "/share/zsh/site-functions\"")

                                                     (mixed-text-file
                                                      "zsh-autosuggestions"
                                                      "source "
                                                      zsh-autosuggestions
                                                      "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh")

                                                     (mixed-text-file
                                                      "zsh-autopair" "source "
                                                      zsh-autopair
                                                      "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh")

                                                     (mixed-text-file
                                                      "fzf-tab" "source "
                                                      fzf-tab
                                                      "/share/zsh/plugins/fzf-tab/fzf-tab.plugin.zsh")))))

       ;; GPG Agent (this might override base config, which is OK if you want custom settings)
       (service home-gpg-agent-service-type
                (home-gpg-agent-configuration (pinentry-program (file-append
                                                                 pinentry-gnome3
                                                                 "/bin/pinentry-gnome3"))
                                              (ssh-support? #t)
                                              (default-cache-ttl 28800)
                                              (max-cache-ttl 86400)
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
                                                                       (port
                                                                             22)
                                                                       (extra-content
                                                                        "ControlMaster auto
ControlPath ~/.ssh/control-%C
ControlPersist 10m"))

                                                         (openssh-host (name
                                                                        "gitlab.com")
                                                                       (user
                                                                        "git")
                                                                       (identity-file
                                                                        "~/.ssh/id_ed25519")
                                                                       (port
                                                                             22))

                                                         (openssh-host (name
                                                                        "spärck")
                                                                       (host-name
                                                                        "spärck.local")
                                                                       (user
                                                                        "b")
                                                                       (identity-file
                                                                        "~/.ssh/id_ed25519")
                                                                       (forward-agent?
                                                                        #t)
                                                                       (compression?
                                                                        #t)
                                                                       (extra-content
                                                                        "ControlMaster auto
ControlPath ~/.ssh/control-%C
ControlPersist 30m
ServerAliveInterval 60
ServerAliveCountMax 3"))

                                                         (openssh-host (name
                                                                        "*")
                                                                       (extra-content
                                                                        "HashKnownHosts yes
IdentitiesOnly yes
StrictHostKeyChecking ask
VerifyHostKeyDNS yes
VisualHostKey yes"))))
                                            (add-keys-to-agent "confirm")))

       ;; Desktop services
       (service home-dbus-service-type)
       (service home-pipewire-service-type)
       (service home-syncthing-service-type)

       ;; Media services
       (service home-beets-service-type
                (home-beets-configuration (directory "/home/b/music")))

       (service home-nougat-service-type))))))

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

  (groups (cons* (user-group
                   (system? #t)
                   (name "realtime"))
                 (user-group
                   (system? #t)
                   (name "adbusers")) %base-groups))

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
                                  ;; Filesystem maintenance
                                  (shepherd-timer '(fstrim-all)
                                                  "0 0 * * 0" ;Weekly on Sunday at midnight
                                                  #~(list #$(file-append
                                                             util-linux
                                                             "/sbin/fstrim")
                                                          "-a" "-v")
                                                  #:requirement '(file-systems))

                                  (shepherd-timer '(clean-tmp-directory)
                                                  "0 2 */14 * *" ;Every 14 days at 2 AM
                                                  #~(begin
                                                      (setenv "HOME" "/home/b")
                                                      (system* #$(file-append
                                                                  findutils
                                                                  "/bin/find")
                                                               "/home/b/tmp"
                                                               "-mindepth" "1"
                                                               "-delete"))
                                                  #:requirement '(file-systems))

                                  (shepherd-timer '(clean-old-downloads)
                                                  "0 3 */14 * *" ;Every 14 days at 3 AM
                                                  #~(begin
                                                      (setenv "HOME" "/home/b")
                                                      (system* #$(file-append
                                                                  findutils
                                                                  "/bin/find")
                                                       "/home/b/downloads"
                                                       "-mindepth"
                                                       "1"
                                                       "-atime"
                                                       "+14"
                                                       "-delete"))
                                                  #:requirement '(file-systems))

                                  ;; Package management
                                  (shepherd-timer '(garbage-collection)
                                                  "0 4 * * *" ;Daily at 4 AM
                                                  #~(list #$(file-append guix
                                                             "/bin/guix") "gc"
                                                          "-F" "50G")
                                                  #:requirement '(guix-daemon))

                                  (shepherd-timer '(update-package-database)
                                                  "0 3 * * 0" ;Weekly on Sunday at 3 AM
                                                  #~(list #$(file-append guix
                                                             "/bin/guix")
                                                     "package"
                                                     "--list-generations")
                                                  #:requirement '(guix-daemon))

                                  ;; Development tools cleanup
                                  (shepherd-timer '(docker-cleanup)
                                                  "0 2 * * 6" ;Weekly on Saturday at 2 AM
                                                  #~(list #$(file-append
                                                             docker-cli
                                                             "/bin/docker")
                                                          "system" "prune"
                                                          "-af")
                                                  #:requirement '(dockerd))

                                  (shepherd-timer '(cuda-cache-cleanup)
                                                  "0 3 1 * *" ;Monthly on the 1st at 3 AM
                                                  #~(list #$(file-append
                                                             coreutils
                                                             "/bin/rm") "-rf"
                                                     "/home/b/.nv/ComputeCache")
                                                  #:requirement '(file-systems))

                                  (shepherd-timer '(clean-package-caches)
                                                  "0 3 15 * *" ;Monthly on the 15th at 3 AM
                                                  #~(begin
                                                      (system* #$(file-append
                                                                  coreutils
                                                                  "/bin/rm")
                                                       "-rf"
                                                       "/home/b/.cache/pip")
                                                      (system* #$(file-append
                                                                  findutils
                                                                  "/bin/find")
                                                       "/home/b/.local/share/cargo/registry/cache"
                                                       "-type"
                                                       "f"
                                                       "-mtime"
                                                       "+90"
                                                       "-delete")
                                                      (system* #$(file-append
                                                                  coreutils
                                                                  "/bin/rm")
                                                       "-rf"
                                                       "/home/b/.cache/npm/_cacache"))
                                                  #:requirement '(file-systems))

                                  (shepherd-timer '(git-maintenance)
                                                  "0 4 * * 0" ;Weekly on Sunday at 4 AM
                                                  #~(begin
                                                      (setenv "HOME" "/home/b")
                                                      (system* #$(file-append
                                                                  findutils
                                                                  "/bin/find")
                                                       "/home/b/projects"
                                                       "-name"
                                                       ".git"
                                                       "-type"
                                                       "d"
                                                       "-exec"
                                                       #$(file-append git
                                                          "/bin/git")
                                                       "-C"
                                                       "{}"
                                                       ".."
                                                       "maintenance"
                                                       "run"
                                                       "--auto"
                                                       ";"))
                                                  #:requirement '(file-systems))

                                  ;; System maintenance
                                  (shepherd-timer '(updatedb)
                                                  "0 3 * * *" ;Daily at 3 AM
                                                  #~(list #$(file-append
                                                             findutils
                                                             "/bin/updatedb")
                                                     "--localpaths=/home /data"
                                                     "--prunepaths=/tmp /var/tmp /gnu")
                                                  #:requirement '(file-systems))

                                  (shepherd-timer '(clean-logs)
                                                  "0 2 * * 0" ;Weekly on Sunday at 2 AM
                                                  #~(begin
                                                      (system* #$(file-append
                                                                  findutils
                                                                  "/bin/find")
                                                               "/var/log"
                                                               "-name"
                                                               "*.log.*"
                                                               "-mtime"
                                                               "+30"
                                                               "-delete")
                                                      (system* #$(file-append
                                                                  findutils
                                                                  "/bin/find")
                                                               "/var/log"
                                                               "-name"
                                                               "*.gz"
                                                               "-mtime"
                                                               "+30"
                                                               "-delete"))
                                                  #:requirement '(file-systems))

                                  (shepherd-timer '(font-cache-update)
                                                  "0 3 * * 0" ;Weekly on Sunday at 3 AM
                                                  #~(list #$(file-append
                                                             fontconfig
                                                             "/bin/fc-cache")
                                                          "-fv")
                                                  #:requirement '(file-systems))

                                  ;; Database maintenance
                                  (shepherd-timer '(postgres-vacuum)
                                                  "0 5 * * 0" ;Weekly on Sunday at 5 AM
                                                  #~(list #$(file-append
                                                             postgresql
                                                             "/bin/psql") "-U"
                                                          "postgres" "-c"
                                                          "VACUUM ANALYZE;")
                                                  #:requirement '(postgres))

                                  ;; Security and monitoring
                                  (shepherd-timer '(disk-health-check)
                                                  "0 6 * * 0" ;Weekly on Sunday at 6 AM
                                                  #~(begin
                                                      (system* #$(file-append
                                                                  smartmontools
                                                                  "/sbin/smartctl")
                                                               "-a"
                                                               "/dev/nvme0n1"
                                                               "-l" "error")
                                                      (system* #$(file-append
                                                                  smartmontools
                                                                  "/sbin/smartctl")
                                                               "-a"
                                                               "/dev/nvme1n1"
                                                               "-l" "error"))
                                                  #:requirement '(file-systems))))

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
                                oci-qdrant-service-type oci-minio-service-type))

                 ;; Backup Service
                 (service borg-backup-service-type
                          (borg-backup-configuration (jobs (list (borg-backup-job
                                                                  (name
                                                                   "system-backup")
                                                                  (repository
                                                                   "/data/backups/borg")
                                                                  (passphrase-file
                                                                   "/root/.borg-password")
                                                                  (schedule
                                                                   "0 2 * * *")
                                                                  (paths '("/etc"
                                                                           "/root/.ssh"
                                                                           "/root/.gnupg"
                                                                           "/home/b/documents"
                                                                           "/home/b/projects"
                                                                           "/var/lib/postgresql"
                                                                           "/var/lib/mysql"))
                                                                  (exclude-patterns '
                                                                   ("*/.cache"
                                                                    "*/node_modules"
                                                                    "*/.git"
                                                                    "*/target"
                                                                    "*/__pycache__"))
                                                                  (compression
                                                                   "zstd,3")
                                                                  ;; Retention policy
                                                                  (prune-keep-daily
                                                                   7)
                                                                  (prune-keep-weekly
                                                                   4)
                                                                  (prune-keep-monthly
                                                                   12)
                                                                  (prune-keep-yearly
                                                                   2)
                                                                  ;; Optional flags
                                                                  (verbose? #f)
                                                                  (stats? #t)
                                                                  (progress?
                                                                   #t)))))))
           %my-desktop-services))
  (name-service-switch %mdns-host-lookup-nss))
