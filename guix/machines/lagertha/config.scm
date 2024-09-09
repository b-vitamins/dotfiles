;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu system)
             (gnu system nss)
             (gnu system install)
             (gnu services avahi)
             (gnu services ssh)
             (gnu services cups)
             (gnu services vpn)
             (gnu services networking)
             (gnu services syncthing)
             (gnu services linux)
             (gnu services sysctl)
             (gnu services pm)
             (myguix services base)
             (myguix system install)
             (myguix packages linux)
             (myguix system linux-initrd)
             (srfi srfi-1))

(operating-system
  (host-name "lagertha")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (kernel linux)
  (firmware (list linux-firmware))
  (initrd microcode-initrd)

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))

  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "D488-74A2"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "999203ab-375d-4a0e-90f6-bb2e6b2d71f4"
                                  'ext4))
                         (type "ext4")) %base-file-systems))

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "b")
                  (comment "Ayan")
                  (group "users")
                  (home-directory "/home/b")
									(shell (file-append (specification->package "zsh")
                                      "/bin/zsh"))
                  (supplementary-groups '("adbusers" "wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; System-wide packages
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "curl")
                          (specification->package "coreutils")
                          (specification->package "zstd")
                          (specification->package "gnupg")
                          (specification->package "pinentry")
                          (specification->package "password-store")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "screen")
                          (specification->package "font-dejavu")
                          (specification->package "font-iosevka-comfy")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-serif-cjk")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "fontconfig"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list
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
                                                                        "network-manager-openvpn")))))
            (service wpa-supplicant-service-type)
            (simple-service 'network-manager-applet profile-service-type
                            (list (specification->package
                                   "network-manager-applet")))
            (service modem-manager-service-type)
            (service usb-modeswitch-service-type)
						;; OpenSSH for remote access
						(service openssh-service-type
										 (openssh-configuration (authorized-keys `(("b" ,(local-file
																																			"keys/ragnar.pub"))
																															 ("b" ,(local-file
																																			"keys/leif.pub"))
																															 ("b" ,(local-file
																																			"keys/bjorn.pub"))
																															 ("b" ,(local-file
																																			"keys/freydis.pub"))))
																						(password-authentication? #f)))

            ;; Networking Services
            (service avahi-service-type)
            (service nftables-service-type)
            (service ntp-service-type)

            ;; VPN Services
            (service bitmask-service-type)

            ;; Linux Services
            (service earlyoom-service-type)
            (service zram-device-service-type)

            ;; Miscellaneous Services
            (service sysctl-service-type
                     (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1")
                                                               ("vm.max_map_count" . "262144"))
                                                      %default-sysctl-settings)))))
           ;; This is the default list of services we
           ;; are appending to.
					 %my-base-services))
  (name-service-switch %mdns-host-lookup-nss)
  (swap-devices (list (swap-space
                        (target (uuid
                                 "f7e87737-5365-468a-8dfa-fbe6ce790566"))))))
