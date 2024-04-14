;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu system nss)
             (guix utils))
(use-modules (nongnu packages linux))
(use-service-modules desktop xorg ssh)
(use-package-modules certs gnome shells base)

(operating-system
  (host-name "server-0")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")
  (kernel linux)
  (firmware (list linux-firmware))
  (kernel-arguments '("quiet" "splash" "modprobe.blacklist=radeon,amdgpu"))
  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)
                (theme (grub-theme (inherit (grub-theme))
                                   (gfxmode (list "1920x1080" "auto"))))))

  (file-systems (append (list (file-system
                                (device (file-system-label "my-root"))
                                (mount-point "/")
                                (type "btrfs"))
                              (file-system
                                (device (uuid "A2F2-9648"
                                              'fat))
                                (mount-point "/boot/efi")
                                (type "vfat"))) %base-file-systems))

  (users (cons (user-account
                 (name "b")
                 (comment "B Vitamins")
                 (group "users")
                 (home-directory "/home/b")
                 (shell (file-append zsh "/bin/zsh"))
                 (supplementary-groups '("wheel" "netdev" "kvm" "lp" "audio"
                                         "video"))) %base-user-accounts))

  (packages (append (list nss-certs coreutils gvfs) %base-packages))

  (services
   (if (target-x86-64?)
       (append (list (service gnome-desktop-service-type)
                     (service openssh-service-type)
                     (set-xorg-configuration
                      (xorg-configuration (resolutions '((1920 1080)))
                                          (keyboard-layout keyboard-layout))))
               %desktop-services)))

  (name-service-switch %mdns-host-lookup-nss))
