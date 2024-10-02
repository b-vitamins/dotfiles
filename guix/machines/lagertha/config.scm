(use-modules (gnu)
             (gnu services certbot)
             (gnu services networking)
             (gnu services ssh)
             (gnu services web))

;; Define the Nginx deploy hook to reload Nginx on certificate renewal
(define %nginx-deploy-hook
  (program-file "nginx-deploy-hook"
                #~(let ((pid (call-with-input-file "/var/run/nginx/pid"
                               read)))
                    (kill pid SIGHUP))))

(define %bvits-server-block
  (nginx-server-configuration (server-name '("bvits.in" "www.bvits.in"))
                              (root "/srv/http/bvits.in")
                              (listen '("80"))
                              (index '("index.html"))
                              (locations (list (nginx-location-configuration (uri
                                                                              "/")
                                                                             (body
                                                                              (list
                                                                               "try_files $uri $uri/ /index.html")))))))

(operating-system
  (locale "en_US.utf8")
  (timezone "Asia/Kolkata")
  (host-name "lagertha")

  ;; Updated keyboard layout with custom options
  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  ;; User accounts with a custom shell for the 'b' user
  (users (cons* (user-account
                  (name "b")
                  (comment "Ayan")
                  (group "users")
                  (home-directory "/home/b")
                  (shell (file-append (specification->package "zsh")
                                      "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; System-wide packages
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "curl")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "node")
                          (specification->package "screen")
                          (specification->package "zstd")
                          (specification->package "coreutils")
                          (specification->package "font-dejavu")
                          (specification->package "font-iosevka-comfy")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-serif-cjk")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "fontconfig"))
                    %base-packages))

  (services
   (append (list
            ;; Certbot for handling SSL certificates
            (service certbot-service-type
                     (certbot-configuration (email "bvits@riseup.net")
                                            (certificates (list (certificate-configuration
                                                                 (domains '("bvits.in"))
                                                                 (deploy-hook
                                                                  %nginx-deploy-hook))))))

            ;; NGINX service setup
            (service nginx-service-type
                     (nginx-configuration (server-blocks (list
                                                          %bvits-server-block))))

            ;; OpenSSH for remote access
            (service openssh-service-type
                     (openssh-configuration (authorized-keys `(("b" ,(local-file
                                                                      "../../../keys/ssh/ragnar.pub"))
                                                               ("b" ,(local-file
                                                                      "../../../keys/ssh/leif.pub"))
                                                               ("b" ,(local-file
                                                                      "../../../keys/ssh/bjorn.pub"))
                                                               ("b" ,(local-file
                                                                      "../../../keys/ssh/freydis.pub"))))
                                            (password-authentication? #f)
                                            (port-number 2123)))
            (service network-manager-service-type)
            (service wpa-supplicant-service-type)
            (service ntp-service-type)) %base-services))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (initrd-modules (append '("virtio_scsi") %base-initrd-modules))
  (swap-devices (list (swap-space
                        (target (uuid "9700bf14-2fc0-4fdb-bc16-88b8f66538d9")))))

  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "B1A0-F495"
                                       'fat16))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid "ec620eab-171d-4607-8b6c-428d1e100537"
                                       'ext4))
                         (type "ext4")) %base-file-systems)))
