;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu home)
             (gnu packages base)
             (gnu packages docker)
             (gnu packages gnupg)
             (gnu packages shellutils)
             (gnu home services mcron)
             (gnu home services ssh)
             (gnu home services gnupg)
             (gnu home services desktop)
             (gnu home services xdg)
             (gnu services docker)
             (gnu home services)
             (gnu home services media)
             (gnu home services music)
             (gnu home services dict)
             (gnu home services sound)
             (gnu home services syncthing)
             (gnu home services shells)
             (gnu home services pm)
             (myguix home services emacs-daemon)
             (gnu system)
             (gnu services)
             (gnu services admin)
             (gnu system shadow)
             (gnu system nss)
             (gnu system install)
             (gnu services base)
             (gnu services avahi)
             (gnu services cups)
             (gnu services desktop)
             (gnu services guix)
             (gnu services ssh)
             (gnu services xorg)
             (gnu services networking)
             (gnu services sysctl)
             (gnu services shepherd)
             (gnu services monitoring)
             (gnu services pm)
             (gnu services backup)
             (guix gexp)
             (myguix packages base)
             (myguix packages fonts)
             (myguix services mcron)
             (myguix home)
             (myguix home services emacs)
             (myguix services desktop)
             (myguix system install)
             (myguix packages linux)
             (myguix system linux-initrd)
             (myguix services oci-containers)
             (srfi srfi-1))

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

(define %my-home-config
  (home-environment
    
    (packages (append
               ;; Additional packages for home
               (list
                ;; Apple fonts for UI consistency (minimal set for laptop)
                font-apple-sf-pro ;SF Pro Display/Text for UI
                font-apple-sf-mono ;SF Mono for terminals and code
                font-apple-new-york ;New York serif for documents
                font-apple-color-emoji ;Apple Color Emoji
                )))

    (services
     (append (list
              ;; Home Emacs Service
              (service my-home-emacs-service-type)
              
              (service home-emacs-daemon-service-type
                       (emacs-daemon-configuration
                        (package emacs-pgtk)
                        (server-name "spärck")))
              
              ;; Power Management Home Services
              (service home-batsignal-service-type)
              ;; Scheduled User’s Job Execution
              (service home-mcron-service-type
                       (home-mcron-configuration (jobs (list
                                                        %garbage-collector-job))))
              ;; Environment variables
              (simple-service 'laptop-environment-variables
                              home-environment-variables-service-type
                              `(("EDITOR" . "emacsclient -nw")
                                ("VISUAL" . "emacsclient -c")
                                ("BROWSER" . "firefox")
                                ("PAGER" . "less")
                                ("LESS" . "-FRX")
                                ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                                ;; Development (more conservative for laptop)
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
                                ;; Wayland
                                ("MOZ_ENABLE_WAYLAND" . "1")
                                ("QT_QPA_PLATFORM" . "wayland;xcb")
                                ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

              ;; Home Files Service
              (service home-files-service-type
                       `((".guile" ,%default-dotguile)))

              (simple-service 'my-home-files-service home-files-service-type
                              `((".gitconfig" ,(local-file
                                                "../../git/gitconfig"
                                                "gitconfig"
                                                #:recursive? #f))
                                (".gitignore" ,(local-file
                                                "../../git/gitignore"
                                                "gitignore"
                                                #:recursive? #f))
                                (".gitattributes" ,(local-file
                                                    "../../git/gitattributes"
                                                    "gitattributes"
                                                    #:recursive? #f))))

              ;; XDG configuration
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
                                                                (music
                                                                 "$HOME/music")
                                                                (pictures
                                                                 "$HOME/pictures")
                                                                (publicshare
                                                                 "$HOME/public")
                                                                (templates
                                                                 "$HOME/templates")
                                                                (videos
                                                                 "$HOME/videos")))

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
audio/mpeg=mpv.desktop
audio/flac=mpv.desktop
text/plain=emacsclient.desktop
text/x-c=emacsclient.desktop
text/x-python=emacsclient.desktop
application/x-shellscript=emacsclient.desktop
inode/directory=org.gnome.Nautilus.desktop
"))))

              ;; Config Files Service
              (simple-service 'my-config-files-service
                              home-xdg-configuration-files-service-type
                              `(("alacritty/alacritty.toml" ,(local-file
                                                              "../../alacritty/alacritty.toml"
                                                              "alacritty.toml"
                                                              #:recursive? #f))))

              ;; GPG Agent with SSH support
              (service home-gpg-agent-service-type
                       (home-gpg-agent-configuration (pinentry-program (file-append
                                                                        (specification->package
                                                                         "pinentry-gnome3")
                                                                        "/bin/pinentry-gnome3"))
                                                     (ssh-support? #t)
                                                     (default-cache-ttl 28800) ;8 hours
                                                     (max-cache-ttl 86400) ;24 hours
                                                     (default-cache-ttl-ssh
                                                                            28800) ;8 hours for SSH
                                                     (max-cache-ttl-ssh 86400) ;24 hours for SSH
                                                     (extra-content
                                                      "enable-ssh-support
allow-loopback-pinentry
allow-preset-passphrase")))

              ;; Secure Shell
              (service home-openssh-service-type
                       (home-openssh-configuration (hosts (list
                                                           ;; GitHub
                                                           (openssh-host (name
                                                                          "github.com")
                                                                         (user
                                                                          "git")
                                                                         (identity-file
                                                                          "~/.ssh/id_ed25519")
                                                                         (port
                                                                          22)
                                                                         (extra-content
                                                                          "
  ControlMaster auto
  ControlPath ~/.ssh/control-%C
  ControlPersist 10m"))

                                                           ;; GitLab
                                                           (openssh-host (name
                                                                          "gitlab.com")
                                                                         (user
                                                                          "git")
                                                                         (identity-file
                                                                          "~/.ssh/id_ed25519")
                                                                         (port
                                                                          22))

                                                           ;; Mileva (workstation)
                                                           (openssh-host (name
                                                                          "mileva")
                                                                         (host-name
                                                                          "mileva.local")
                                                                         (user
                                                                          "b")
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
                                                           (openssh-host (name
                                                                          "*")
                                                                         (extra-content
                                                                          "
  HashKnownHosts yes
  IdentitiesOnly yes
  StrictHostKeyChecking ask
  VerifyHostKeyDNS yes
  VisualHostKey yes"))))
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

              ;; Input configuration
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

              ;; Zsh configuration
              (service home-zsh-service-type
                       (home-zsh-configuration (xdg-flavor? #t)
                                               (zshenv (list (local-file
                                                              "../../zsh/zshenv"
                                                              "zshenv"
                                                              #:recursive? #f)))
                                               (zprofile (list (local-file
                                                                "../../zsh/zprofile"
                                                                "zprofile"
                                                                #:recursive?
                                                                #f)))
                                               (zshrc (list
                                                       ;; Load main zshrc
                                                       (local-file
                                                        "../../zsh/zshrc"
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
                                                        "zsh-autopair"
                                                        "source " zsh-autopair
                                                        "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh")

                                                       (mixed-text-file
                                                        "fzf-tab" "source "
                                                        fzf-tab
                                                        "/share/fzf-tab/fzf-tab.plugin.zsh")))))

              ;; Miscellaneous Home Services
              (service home-beets-service-type
                       (home-beets-configuration (directory "/home/b/music"))))
             %my-home-services))))

(operating-system
  (host-name "spärck")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (firmware (list linux-firmware sof-firmware))
  (initrd microcode-initrd)

  (keyboard-layout (keyboard-layout "us"
                                    "altgr-intl"
                                    #:model "thinkpad"
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
                 (supplementary-groups '("adbusers" "wheel"
                                         "netdev"
                                         "realtime"
                                         "audio"
                                         "video"
                                         "seat"))) %base-user-accounts))

  (groups (cons* (user-group
                   (system? #t)
                   (name "realtime"))
                 (user-group
                   (system? #t)
                   (name "seat")) %base-groups))

  (packages (append
             ;; Core System (minimal for laptop)
             %core-minimal
             %core-extended
             %shell-modern
             %terminal-essentials
             %text-editors
             %security-essentials
             %ai-assistants
             ;; System Management (laptop essentials)
             %system-monitoring
             %hardware-monitoring
             ;; Development - Core (portable dev setup)
             %build-essentials
             %version-control
             %compression-tools
             ;; Development - Languages (focused subset for laptop)
             %python-development
             %guile-development
             %perl-development
             ;; Language Servers (just core)
             %language-servers-core
             ;; Development - Support
             %tree-sitter-core
             %documentation-tools
             ;; Networking
             %network-core
             %network-diagnostics
             ;; File Management (essentials for laptop)
             %filesystem-core
             %cloud-sync
             %backup-tools
             %download-tools
             ;; Desktop
             %desktop-browsers
             %desktop-core
             %audio-system
             %bluetooth-system
             ;; Media (minimal for laptop)
             %media-players
             ;; Documents (for on-the-go work)
             %latex-core
             %document-conversion-packages
             %spell-checkers
             ;; Math/Science (basic)
             %math-core
             ;; Fonts (essential subset)
             %fonts-essential
             %fonts-programming
             %fonts-international
             %base-packages))

  (services
   (append (list
            ;; Desktop Environment
            (service gnome-desktop-service-type)
            (set-xorg-configuration
             (xorg-configuration (keyboard-layout keyboard-layout)))

            ;; Printing Services
            (service cups-service-type
                     (cups-configuration (web-interface? #t)))

            ;; Guix Services
            (service guix-home-service-type
                     `(("b" ,%my-home-config)))

            ;; Networking Services
            (service nftables-service-type)
            (service openssh-service-type)

            ;; Database Services (minimal for laptop)
            (service redis-service-type
                     (redis-configuration (bind "127.0.0.1")
                                          (port 6379)))

            (service postgresql-service-type
                     (postgresql-configuration (postgresql (specification->package
                                                            "postgresql"))
                                               (config-file (postgresql-config-file
                                                             (hba-file (plain-file
                                                                        "pg_hba.conf"
                                                                        "# TYPE  DATABASE        USER            ADDRESS                 METHOD
local   all             postgres                                peer
local   all             all                                     peer
host    all             all             127.0.0.1/32            md5
host    all             all             ::1/128                 md5"))
                                                             (extra-config '(("shared_buffers"
                                                                              "512MB")
                                                                             ;; 32GB RAM, conservative
                                                                             ("effective_cache_size"
                                                                              "4GB")
                                                                             ("work_mem"
                                                                              "16MB")
                                                                             ("max_connections"
                                                                              "100")
                                                                             ("random_page_cost"
                                                                              "1.1") ;SSD optimization
                                                                             ("logging_collector"
                                                                              #t)
                                                                             ("log_directory"
                                                                              "/var/log/postgresql")))))))

            (service postgresql-role-service-type
                     (postgresql-role-configuration (roles (list (postgresql-role
                                                                  (name "b")
                                                                  (create-database?
                                                                   #t)
                                                                  (permissions '
                                                                   (createdb
                                                                    login)))))))

            ;; Container Services
            (service containerd-service-type)
            (service docker-service-type)
            (service oci-container-service-type
                     (list oci-grobid-service-type))

            ;; Monitoring Services (minimal for laptop)
            (service prometheus-node-exporter-service-type
                     (prometheus-node-exporter-configuration (web-listen-address
                                                              ":9100")))

            (service vnstat-service-type
                     (vnstat-configuration (database-directory
                                            "/var/lib/vnstat")
                                           (save-interval 10) ;Less frequent saves for laptop
                                           (bandwidth-detection? #t)
                                           (offline-save-interval 60) ;Save less often when offline
                                           (use-logging 2)))

            ;; Miscellaneous Services
            (service sysctl-service-type
                     (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1"))
                                                      %default-sysctl-settings))))

            ;; Custom hosts for local development
            (simple-service 'development-hosts hosts-service-type
                            (list (host "127.0.0.1" "spärck.local"
                                        '("dev.local"))
                                  (host "::1" "spärck.local"
                                        '("dev.local"))))

            ;; Scheduled jobs using Shepherd timers
            (simple-service 'laptop-timers shepherd-root-service-type
                            (list
                             ;; Garbage collection every 3 days at 3AM (less frequent for laptop)
                             (shepherd-timer '(garbage-collection)
                                             "0 3 */3 * *"
                                             #~(list #$(file-append guix
                                                        "/bin/guix") "gc" "-F"
                                                     "20G")
                                             #:requirement '(guix-daemon))

                             ;; Docker cleanup weekly on Sunday at 2AM
                             (shepherd-timer '(docker-cleanup) "0 2 * * 0"
                                             #~(list #$(file-append docker-cli
                                                        "/bin/docker")
                                                     "system" "prune" "-af")
                                             #:requirement '(dockerd))

                             ;; Battery report weekly on Monday at 10AM
                             (shepherd-timer '(battery-report) "0 10 * * 1"
                                             #~(list #$(file-append coreutils
                                                                    "/bin/sh")
                                                "-c"
                                                "upower -i /org/freedesktop/UPower/devices/battery_BAT0 >> /var/log/battery-health.log"))))

            ;; Extended log rotation for laptop logs
            (simple-service 'laptop-log-rotation log-rotation-service-type
                            (log-rotation-configuration (inherit (log-rotation-configuration))
                                                        (external-log-files '("/var/log/battery-health.log"
                                                                              "/var/log/postgresql/*.log"
                                                                              "/var/log/docker/*.log"))))

            ;; Environment variables for Wayland preference
            (simple-service 'wayland-environment
                            session-environment-service-type
                            '(("MOZ_ENABLE_WAYLAND" . "1") ;Firefox Wayland
                              ("QT_QPA_PLATFORM" . "wayland;xcb") ;Qt apps prefer Wayland
                              ("CLUTTER_BACKEND" . "wayland") ;Clutter/GTK
                              ("SDL_VIDEODRIVER" . "wayland") ;SDL applications
                              ("_JAVA_AWT_WM_NONREPARENTING" . "1") ;Java apps
                              ("GDK_BACKEND" . "wayland,x11")))

            ;; Enhanced power management for laptop
            (service elogind-service-type
                     (elogind-configuration (handle-lid-switch 'suspend)
                                            (handle-lid-switch-external-power 'suspend)
                                            (handle-lid-switch-docked 'ignore)
                                            (idle-action 'suspend)
                                            (idle-action-seconds (* 20 60)) ;20 minutes
                                            (holdoff-timeout-seconds 30))) ;Wait 30s after boot before suspending
            
            ;; TLP for advanced power management
            (service tlp-service-type
                     (tlp-configuration
                      ;; CPU settings
                      (cpu-scaling-governor-on-ac (list "balance_performance"))
                      (cpu-scaling-governor-on-bat (list "balance_power"))
                      (cpu-energy-perf-policy-on-ac "balance_performance")
                      (cpu-energy-perf-policy-on-bat "balance_power")
                      (cpu-boost-on-ac? #t)
                      (cpu-boost-on-bat? #f)
                      (sched-powersave-on-bat? #t)

                      ;; Disk settings
                      (disk-apm-level-on-ac (list "254" "254"))
                      (disk-apm-level-on-bat (list "128" "128"))
                      (disk-idle-secs-on-bat 2)

                      ;; PCIe power management
                      (pcie-aspm-on-ac "performance")
                      (pcie-aspm-on-bat "powersupersave")
                      (runtime-pm-on-ac "on")
                      (runtime-pm-on-bat "auto")

                      ;; WiFi power saving
                      (wifi-pwr-on-bat? #t)

                      ;; Audio power saving
                      (sound-power-save-on-ac 10)
                      (sound-power-save-on-bat 1)

                      ;; USB autosuspend
                      (usb-autosuspend? #t)
                      (usb-blacklist-wwan? #t)

                      ;; Battery charge thresholds (if supported)
                      (start-charge-thresh-bat0 45)
                      (stop-charge-thresh-bat0 80)))

            ;; Thermal management daemon
            (service thermald-service-type
                     (thermald-configuration (adaptive? #t) ;Use Intel DPTF if available
                                             (ignore-cpuid-check? #f)))

            ;; Linux Services
            (service earlyoom-service-type
                     (earlyoom-configuration (minimum-available-memory 10) ;More conservative for laptop
                                             (minimum-free-swap 10)
                                             (memory-report-interval 120) ;Less frequent reports
                                             (avoid-regexp
                                              "^(sshd|guix-daemon|postgres|redis-server)$")
                                             (prefer-regexp
                                              "^(chromium|firefox|node|java)$") ;Target memory hogs
                                             (show-debug-messages? #f)))

            (service zram-device-service-type
                     (zram-device-configuration (size "4G") ;4GB compressed swap (32GB RAM)
                                                (compression-algorithm 'zstd)
                                                (memory-limit "8G") ;Max 8GB uncompressed
                                                (priority 100))) ;High priority
            
            (service fstrim-service-type
                     (fstrim-configuration (schedule "0 0 * * 0") ;Weekly on Sunday at midnight
                                           (verbose? #f)))

            ;; Backup Service (minimal for laptop)
            (service restic-backup-service-type
                     (restic-backup-configuration (jobs (list (restic-backup-job
                                                               (name
                                                                "laptop-backup")
                                                               (repository
                                                                "/home/b/backups/restic")
                                                               (password-file
                                                                "/home/b/.restic-password")
                                                               ;; Weekly on Sunday at 11 PM
                                                               (schedule
                                                                "0 23 * * 0")
                                                               (files '("/home/b/.ssh"
                                                                        "/home/b/documents"
                                                                        "/home/b/projects"))
                                                               (extra-flags '("--exclude-caches"
                                                                              "--exclude=node_modules"
                                                                              "--exclude=.git"
                                                                              "--exclude=target"))))))))
           (modify-services %my-desktop-services
             (console-font-service-type config =>
                                        (map (lambda (tty)
                                               (cons tty
                                                     (file-append (specification->package
                                                                   "font-terminus")
                                                      "/share/consolefonts/ter-132n")))
                                             '("tty1" "tty2" "tty3"))))))
  (name-service-switch %mdns-host-lookup-nss))
