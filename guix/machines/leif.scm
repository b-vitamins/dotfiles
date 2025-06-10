;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu home)
             (gnu home services mcron)
             (gnu home services ssh)
             (gnu home services desktop)
             (gnu services docker)
             (gnu home services)
             (gnu home services media)
             (gnu home services music)
             (gnu home services dict)
             (gnu home services sound)
             (gnu home services syncthing)
             (gnu home services shells)
             (gnu home services pm)
             (gnu system)
             (gnu services)
             (gnu system shadow)
             (gnu system nss)
             (gnu system install)
             (gnu services avahi)
             (gnu services cups)
             (gnu services desktop)
             (gnu services guix)
             (gnu services ssh)
             (gnu services xorg)
             (gnu services networking)
             (gnu services sysctl)
             (guix gexp)
             (myguix packages base)
             (myguix services mcron)
             (myguix home)
             (myguix home services emacs)
             (myguix services desktop)
             (myguix system install)
             (myguix packages linux)
             (myguix system linux-initrd)
             (myguix services oci-containers)
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
               ;; Search and Index bundles
               %search-packages))

    (services
     (append (list
              ;; Home Emacs Service
              (service my-home-emacs-service-type)
              ;; Power Management Home Services
              (service home-batsignal-service-type)
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
                                                              "../../alacritty/alacritty.toml"))))

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
  (host-name "leif")
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
                 (supplementary-groups '("adbusers" "wheel" "netdev"
                                         "realtime" "audio" "video")))
               %base-user-accounts))

  (groups (cons (user-group
                  (system? #t)
                  (name "realtime")) %base-groups))

  (packages (append
             ;; Essential bundles
             %core-packages
             %versioning-packages
             %compression-packages
             %network-packages
             ;; Desktop bundles
             %desktop-packages
             %audio-packages
             %bluetooth-packages
             ;; File system bundles
             %basic-filesystem-packages
             %file-transfer-packages
             ;; Development packages
             %guile-packages
             %python-packages
             %perl-packages
             ;; Font bundles
             %general-fonts
             %document-fonts
             %google-fonts
             %iosevka-fonts
             %monospace-fonts
             %cjk-fonts
             %unicode-fonts
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

            ;; Container Services
            (service containerd-service-type)
            (service docker-service-type)
            (service oci-container-service-type
                     (list oci-grobid-service-type))

            ;; Miscellaneous Services
            (service sysctl-service-type
                     (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1"))
                                                      %default-sysctl-settings)))))
           (modify-services %my-desktop-services
             (console-font-service-type config =>
                                        (map (lambda (tty)
                                               (cons tty
                                                     (file-append (specification->package
                                                                   "font-terminus")
                                                      "/share/consolefonts/ter-132n")))
                                             '("tty1" "tty2" "tty3"))))))
  (name-service-switch %mdns-host-lookup-nss))
