;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu home)
             (gnu home services desktop)
             (gnu home services dict)
             (gnu home services mcron)
             (gnu home services media)
             (gnu home services music)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu home services syncthing)
             (gnu packages sync)
             (gnu packages video)
             (gnu services)
             (gnu services avahi)
             (gnu services cups)
             (gnu services databases)
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
             (myguix packages video)
             (myguix services desktop)
             (myguix services mcron)
             (myguix services nvidia)
             (myguix services oci-containers)
             (myguix system install)
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

(define %borg-backup-job
  ;; Run 'borg' to create backups of local paths in a local repository.
  #~(job '(next-hour '(0)) ;Run daily at 00:00
         (lambda ()
           (let* ((repo-path
                   "ssh://u429656@u429656.your-storagebox.de:23/./backups")
                  (backup-name (string-append "ragnar-"
                                              (strftime "%Y-%m-%d-%H-%M-%S"
                                                        (localtime (current-time)))))
                  (log-file "/home/b/.local/share/borg-backup.log")
                  (lock-file "/tmp/borg-backup.lock"))
             ;; Lock file to prevent overlapping jobs
             (if (file-exists? lock-file)
                 ;; Skip backup if another instance is running
                 (format #t
                  "Skipping Borg backup: previous instance is still running~%")
                 ;; Proceed with backup if no lock file exists
                 (begin
                   (call-with-output-file lock-file
                     (lambda (port)
                       (format port "Borg backup started at ~a~%"
                               (current-time))))

                   ;; Set BORG_PASSCOMMAND to securely retrieve the first line of the pass entry
                   (setenv "BORG_PASSCOMMAND" "pass infra/borg/passphrase")

                   ;; Create Borg backup with lzma compression (level 9)
                   (system* "borg"
                            "create"
                            (string-append repo-path "::" backup-name)
                            "/home/b"
                            "--exclude-caches"
                            "--exclude"
                            "/home/b/.gnupg/*"
                            "--exclude"
                            "/home/b/.ssh/*"
                            "--exclude"
                            "/home/b/.guix-home/*"
                            "--exclude"
                            "/home/b/.guix-profile/*"
                            "--exclude"
                            "/home/b/.npm/*"
                            "--exclude"
                            "/home/b/.cache/*"
                            "--exclude"
                            "/home/b/.local/state/*"
                            "--exclude"
                            "/home/b/downloads/*"
                            "--compression"
                            "zstd,22")

                   ;; Prune old backups (keep 7 daily, 4 weekly, and 12 monthly backups)
                   (system* "borg"
                            "prune"
                            repo-path
                            "--keep-daily=7"
                            "--keep-weekly=4"
                            "--keep-monthly=12")

                   ;; Remove lock file after backup completes
                   (delete-file lock-file)
                   (format #t "Borg backup completed successfully~%")))))
         "borg-backup"))

;; Sync job: runs every hour
(define %mega-sync-job
  ;; Run 'rclone' to sync local paths to their respective remote destinations.
  #~(job '(next-hour (range 0 24 1)) ;Run every hour
         (lambda ()
           (let* ((source-destination-pairs '(("/home/b/documents"
                                               "mega:ragnar/documents")
                                              ("/home/b/music"
                                               "mega:ragnar/music")
                                              ("/home/b/pictures"
                                               "mega:ragnar/pictures")
                                              ("/home/b/projects"
                                               "mega:ragnar/projects")
                                              ("/home/b/templates"
                                               "mega:ragnar/templates")
                                              ("/home/b/videos"
                                               "mega:ragnar/videos")))
                  (log-file "/home/b/.local/share/rclone-backup.log") ;Log file for backup
                  (lock-file "/tmp/rclone-backup.lock"))
             ;; Lock file to prevent overlapping jobs
             
             (if (file-exists? lock-file)
                 ;; Skip backup if another instance is running
                 (format #t
                  "Skipping backup: previous instance is still running~%")

                 ;; Proceed with backup if no lock file exists
                 (begin
                   (call-with-output-file lock-file
                     (lambda (port)
                       (format port "Backup started at ~a~%"
                               (current-time))))

                   ;; Perform the backup for each source-destination pair
                   (for-each (lambda (pair)
                               (let ((path (car pair))
                                     (remote (cadr pair)))
                                 (system* "rclone"
                                          "sync"
                                          path
                                          remote
                                          "--log-file"
                                          log-file))) source-destination-pairs)

                   ;; Remove lock file after backup completes
                   (delete-file lock-file)
                   (format #t "Backup completed successfully~%")))))
         "mega-sync"))

;; Dedupe job: runs 3 times a day
(define %mega-dedupe-job
  ;; Run 'rclone dedupe' on the 'mega:ragnar' path to remove duplicates.
  ;; This job runs at 8:00 AM, 2:00 PM, and 8:00 PM.
  #~(job '(next-hour '(8 14 20)) ;Run at 8:00 AM, 2:00 PM, and 8:00 PM
         (lambda ()
           (let ((remote-path "mega:ragnar")
                 ;; Remote path to deduplicate
                 (log-file "/home/b/.local/share/rclone-dedupe.log"))
             ;; Log file for deduplication process
             
             ;; Perform deduplication
             (system* "rclone" "dedupe" remote-path "--log-file" log-file)

             (format #t "Deduplication completed successfully~%")))
         "mega-dedupe"))

(define %my-home-config
  (home-environment
   (packages (map replace-mesa
                  (append %terminal-tools-packages
                          %desktop-utilities-packages
                          %diagnostic-and-maintenance-tools
                          %remote-storage-mount-packages
                          %compression-tools-packages
                          %media-consumption-packages
                          %audio-conversion-tools-packages
                          %video-conversion-tools-packages
                          %document-conversion-tools-packages
                          %video-production-packages
                          %document-authoring-packages
                          %document-manipulation-packages
                          %file-transfer-tools-packages
                          %network-analysis-tools-packages
                          %p2p-file-sharing-packages
                          %network-utilities-packages
                          %build-system-packages
                          %debugging-tools-packages
                          %memory-and-optimization-tools-packages
                          %runtime-packages
                          %tree-sitter-packages
                          %guile-development-packages
                          %rust-development-packages
                          %python-development-packages
                          %perl-development-packages
                          %opencog-packages)))

    (services
     (append (list
              ;; Home Emacs Service
              (service my-home-emacs-service-type)
              ;; Scheduled User’s Job Execution
              (service home-mcron-service-type
                       (home-mcron-configuration (jobs (list
                                                        %garbage-collector-job
                                                        %borg-backup-job
                                                        %mega-sync-job
                                                        %mega-dedupe-job))))

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
                                                                           "../../keys/ssh/freydis.pub")
                                                                          (local-file
                                                                           "../../keys/ssh/leif.pub")
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
                       (home-beets-configuration (directory "/home/b/music")
                                                 (extra-options '("
import:
  move: yes
  copy: no
  write: yes
  log: ~/.config/beets/beets.log

per_disc_numbering: yes

# Plugins for automatic metadata management, and file integrity checking
plugins: fetchart embedart duplicates badfiles

fetchart:
  auto: yes
  minwidth: 500
  sources: coverart itunes amazon fanarttv google lastfm
  cautious: yes
  cover_names: cover front folder album
")))))
             %my-home-services))))

(operating-system
  (host-name "ragnar")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (kernel-arguments (list "modprobe.blacklist=nouveau" "nvidia_drm.modeset=1" "NVreg_EnableGpuFirmware=1"))
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

  (packages (map replace-mesa
                 (append %system-core-packages
                         %nvidia-gpu-packages
                         %secret-mgmt-packages
                         %system-monitoring-packages
                         %basic-filesystem-tools
                         %ssd-tools
                         %general-purpose-fonts
                         %document-fonts
                         %google-fonts
                         %iosevka-fonts
                         %unicode-fonts
                         %version-control-packages
                         %base-packages)))

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

            ;; Database Services
            (service mysql-service-type)
            (service redis-service-type)

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
                           oci-weaviate-service-type oci-neo4j-service-type)))
           %my-desktop-services))
  (name-service-switch %mdns-host-lookup-nss))
