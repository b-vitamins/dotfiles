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
             (gnu home services pm)
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
             (gnu services pm)
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
             (myguix home services emacs-daemon)
             (myguix home services nougat)
             (myguix packages base)
             (myguix packages fonts)
             (myguix packages linux)
             (myguix packages productivity)
             (myguix packages python-pqrs)
             (myguix packages video)
             (myguix services desktop)
             (myguix services mcron)
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

                      (list font-apple-sf-pro
                            font-apple-sf-mono
                            font-apple-sf-compact
                            font-apple-new-york
                            font-apple-sf-symbols
                            font-google-roboto
                            font-fira-go
                            font-fira-sans
                            font-fira-code
                            font-fira-mono)))

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
audio/mpeg=mpv.desktop
audio/flac=audacious.desktop
audio/mp3=audacious.desktop
text/plain=org.gnome.TextEditor.desktop
text/x-c=org.gnome.TextEditor.desktop
text/x-python=org.gnome.TextEditor.desktop
application/x-shellscript=org.gnome.TextEditor.desktop
inode/directory=org.gnome.Nautilus.desktop
"))))
      (service my-home-emacs-service-type)
      (service home-emacs-daemon-service-type
               (emacs-daemon-configuration (server-name "spärck")))

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
      (simple-service 'sparck-dotfiles home-files-service-type
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
      (simple-service 'sparck-xdg-config
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

                                                   ;; Mileva (workstation)
                                                   (openssh-host (name
                                                                  "mileva")
                                                                 (host-name
                                                                  "mileva.local")
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

      ;; Power Management Home Services
      (service home-batsignal-service-type)))))

(operating-system
  (host-name "spärck")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (kernel-arguments %default-kernel-arguments)
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
                 (supplementary-groups '("wheel" "netdev"
                                         "adbusers"
                                         "realtime"
                                         "audio"
                                         "video"
                                         "seat"))) %base-user-accounts))

  (groups (cons* (user-group
                   (system? #t)
                   (name "realtime"))
                 (user-group
                   (system? #t)
                   (name "seat"))
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
             ;; Development - Core
             %build-essentials
             %version-control
             %compression-tools
             ;; Development - Languages (workstation needs all)
             %c-cpp-development
             %rust-development
             %python-development
             %guile-development
             %perl-development
             ;; Language Servers
             %language-servers-core
             ;; Development - Support
             %tree-sitter-core
             %documentation-tools
             ;; Networking
             %network-core
             %network-diagnostics
             ;; File Management
             %filesystem-core
             %cloud-sync
             %backup-tools
             %download-tools
             ;; Desktop
             %desktop-browsers
             %desktop-core
             %audio-system
             %bluetooth-system
             %my-gnome-shell-assets
             ;; Media (workstation needs full media capabilities)
             %media-players
             ;; Documents
             %latex-core
             %document-conversion-packages
             %spell-checkers
             ;; Math/Science
             %math-core
             ;; Fonts (comprehensive for workstation)
             %fonts-essential
             %fonts-programming
             %fonts-international
             %base-packages))
  (services
   (append (list (service gnome-desktop-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout)))
                 ;; Printing Services
                 (service cups-service-type
                          (cups-configuration (web-interface? #t)))

                 ;; Networking Services
                 (service openssh-service-type)
                 (service nftables-service-type)

                 ;; Desktop Services
                 (simple-service 'blueman dbus-root-service-type
                                 (list blueman))

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

                 ;; Power Management for laptop
                 (service tlp-service-type
                          (tlp-configuration
                           ;; CPU settings
                           (cpu-scaling-governor-on-ac (list
                                                        "balance_performance"))
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

                 ;; Guix Services
                 (service guix-home-service-type
                          `(("b" ,%my-home-config)))

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

                 ;; Custom hosts for local development
                 (simple-service 'development-hosts hosts-service-type
                                 (list (host "127.0.0.1" "spärck.local"
                                             '("dev.local"))
                                       (host "::1" "spärck.local"
                                             '("dev.local"))))

                 ;; Scheduled jobs using Shepherd timers
                 (simple-service 'system-timers shepherd-root-service-type
                                 (list
                                  ;; Garbage collection every 3 days at 3AM (less frequent for laptop)
                                  (shepherd-timer '(garbage-collection)
                                                  "0 3 */3 * *"
                                                  #~(list #$(file-append guix
                                                             "/bin/guix") "gc"
                                                          "-F" "20G")
                                                  #:requirement '(guix-daemon))

                                  ))

                 ;; Environment variables for Wayland preference
                 (simple-service 'wayland-environment
                                 session-environment-service-type
                                 '(("MOZ_ENABLE_WAYLAND" . "1") ;Firefox Wayland
                                   ("QT_QPA_PLATFORM" . "wayland;xcb") ;Qt apps prefer Wayland
                                   ("CLUTTER_BACKEND" . "wayland") ;Clutter/GTK
                                   ("SDL_VIDEODRIVER" . "wayland") ;SDL applications
                                   ("_JAVA_AWT_WM_NONREPARENTING" . "1") ;Java apps
                                   ("GDK_BACKEND" . "wayland,x11"))))
           (modify-services %my-desktop-services
             (elogind-service-type config =>
                                   (elogind-configuration (inherit config)
                                                          (handle-lid-switch 'suspend)
                                                          (handle-lid-switch-external-power 'suspend)
                                                          (handle-lid-switch-docked 'ignore)
                                                          (idle-action 'suspend)
                                                          (idle-action-seconds
                                                           (* 20 60)) ;20 minutes
                                                          (holdoff-timeout-seconds
                                                           30))) ;Wait 30s after boot before suspending
             (console-font-service-type config =>
                                        (map (lambda (tty)
                                               (cons tty
                                                     (file-append (specification->package
                                                                   "font-terminus")
                                                      "/share/consolefonts/ter-132n")))
                                             '("tty1" "tty2" "tty3"))))))
  (name-service-switch %mdns-host-lookup-nss))
