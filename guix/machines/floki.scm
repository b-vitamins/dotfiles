;; -*- mode: scheme; -*-
(use-modules (gnu))
(use-service-modules avahi cuirass networking ssh xorg)

(operating-system
  (host-name "floki")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "b")
                  (comment "Ayan")
                  (group "users")
                  (home-directory "/home/b")
                  (shell (file-append (specification->package "zsh")
                                      "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "curl")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "rsync")
                          (specification->package "screen")) %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service avahi-service-type)
                 (service cuirass-remote-worker-service-type
                          (cuirass-remote-worker-configuration (private-key
                                                                "/etc/guix/signing-key.sec")
                                                               (public-key
                                                                "/etc/guix/signing-key.pub")
                                                               (server
                                                                "10.0.0.3:5555")
                                                               (substitute-urls '
                                                                ("https://ci.guix.gnu.org"
                                                                 "https://substitutes.myguix.bvits.in"))
                                                               (systems '("x86_64-linux"))
                                                               (workers 4)))
                 ;; OpenSSH for remote access
                 (service openssh-service-type
                          (openssh-configuration (authorized-keys `(("b" ,(local-file
                                                                           "../../keys/ssh/helga.pub"))))
                                                 (password-authentication? #f)))
                 (service wpa-supplicant-service-type)
                 (service network-manager-service-type)
                 (service ntp-service-type)
                 (service gpm-service-type))

           ;; This is the default list of services we
           ;; are appending to.
           (modify-services %base-services
             (guix-service-type config =>
                                (guix-configuration (inherit config)
                                                    (generate-substitute-key?
                                                                              #f)
                                                    (authorized-keys (append
                                                                      %default-authorized-guix-keys
                                                                      (list (local-file
                                                                             "../../keys/guix/helga.pub")))))))))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (initrd-modules (append '("virtio_scsi") %base-initrd-modules))
  (swap-devices (list (swap-space
                        (target (uuid "22f7360c-5978-4c3e-954b-8fab6213dcde")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "081A-297F"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid "ae3725bb-022c-4599-93b7-e229cb747a78"
                                       'ext4))
                         (type "ext4")) %base-file-systems)))
