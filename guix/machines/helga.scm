; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services dict)
             (gnu home services mcron)
             (gnu home services media)
             (gnu home services music)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu home services syncthing)
             (gnu packages databases)
             (gnu packages virtualization)
             (myguix home)
             (myguix home services emacs)
             (myguix packages base)
             (myguix packages linux)
             (myguix system linux-initrd)
             (myguix services oci-containers)
             (guix modules)
             (ice-9 match)
             (srfi srfi-1))

(use-service-modules admin
                     avahi
                     certbot
                     dbus
                     docker
                     guix
                     linux
                     spice
                     virtualization
                     cuirass
                     databases
                     mcron
                     networking
                     shepherd
                     ssh
                     web
                     sysctl)

;; Define Cuirass specifications for building the 'myguix' channel
(define %cuirass-specs
  #~(list (specification (name "myguix")
                         (build '(channels myguix))
                         (channels (cons (channel
                                           (name 'myguix)
                                           (url
                                            "https://github.com/b-vitamins/myguix.git")
                                           (branch "master")
                                           (introduction
                                            (make-channel-introduction
                                             "85d58b09dc71e9dc9834b666b658f79d2e212d65"
                                             (openpgp-fingerprint
                                              "883B CA6B D275 A5F2 673C  C5DD 2AD3 2FC0 2A50 01F7"))))
                                         %default-channels))
                         (priority 0)
                         (systems '("x86_64-linux")))))

;; Define the Nginx deploy hook to reload Nginx on certificate renewal
(define %nginx-deploy-hook
  (program-file "nginx-deploy-hook"
                #~(let ((pid (call-with-input-file "/var/run/nginx/pid"
                               read)))
                    (kill pid SIGHUP))))

;; Run the garbe collector every day at 4:00 AM
(define garbage-collector-job
  #~(job "0 4 * * *" "guix gc"))

;; CIFS mount disappears often
(define mount-all-job
  #~(job "0 * * * *" "mount --all"
         #:user "root"))

(define-public default-module-filter
  (match-lambda
    (('guix 'config)
     #f)
    (('guix _ ...)
     #t)
    (('gnu _ ...)
     #t)
    (('myguix _ ...)
     #t)
    (_ #f)))

(define-syntax-rule (with-service-gexp-modules body ...)
  (with-imported-modules (source-module-closure (append '((gnu build shepherd))
                                                        ;; This %default-modules is from (gnu services shepherd).
                                                        %default-modules)
                                                #:select?
                                                default-module-filter) body
                         ...))

;; Define Nginx server blocks for the CI and substitute services
(define %ci-server-block
  (nginx-server-configuration (server-name '("ci.myguix.bvits.in"))
                              (listen '("443 ssl"))
                              (ssl-certificate
                               "/etc/letsencrypt/live/ci.myguix.bvits.in/fullchain.pem")
                              (ssl-certificate-key
                               "/etc/letsencrypt/live/ci.myguix.bvits.in/privkey.pem")
                              (locations (list (nginx-location-configuration (uri
                                                                              "/")
                                                                             (body
                                                                              (list
                                                                               "proxy_pass http://guix-cuirass;")))
                                               (nginx-location-configuration (uri
                                                                              "~ ^/admin")
                                                                             (body
                                                                              (list
                                                                               "if ($ssl_client_verify != SUCCESS) { return 403; } proxy_pass http://guix-cuirass;")))))
                              (raw-content (list
                                            "ssl_client_certificate /etc/ssl-ca/certs/ca.crt;"
                                            "ssl_verify_client optional;"))))

(define %substitutes-server-block
  (nginx-server-configuration (server-name '("substitutes.myguix.bvits.in"))
                              (listen '("443 ssl"))
                              (ssl-certificate
                               "/etc/letsencrypt/live/ci.myguix.bvits.in/fullchain.pem")
                              (ssl-certificate-key
                               "/etc/letsencrypt/live/ci.myguix.bvits.in/privkey.pem")
                              (raw-content '("rewrite ^//(.*)$ /$1 redirect;"))
                              (index (list "substitutes.index.html"))
                              (locations (list (nginx-location-configuration (uri
                                                                              "/signing-key.pub")
                                                                             (body '
                                                                              ("proxy_pass http://guix-publish;")))
                                               (nginx-location-configuration (uri
                                                                              "/file/")
                                                                             (body '
                                                                              ("proxy_pass http://guix-publish;")))
                                               (nginx-location-configuration (uri
                                                                              "/log/")
                                                                             (body '
                                                                              ("proxy_pass http://guix-publish;")))
                                               (nginx-location-configuration (uri
                                                                              "/nix-cache-info")
                                                                             (body
                                                                              (list
                                                                               "proxy_pass http://guix-publish;"
                                                                               "proxy_hide_header Set-Cookie;")))
                                               (nginx-location-configuration (uri
                                                                              "/nar/")
                                                                             (body
                                                                              (list
                                                                               "proxy_pass http://guix-publish;"
                                                                               "client_body_buffer_size 256k;"
                                                                               ;; Nars are already compressed. -> no perf change
                                                                               "gzip off;"
                                                                               "proxy_pass_header Cache-Control;")))
                                               (nginx-location-configuration (uri
                                                                              "~ \\.narinfo$")
                                                                             (body
                                                                              (list
                                                                               "proxy_pass http://guix-publish;"
                                                                               "client_body_buffer_size 128k;"
                                                                               "proxy_connect_timeout 2s;"
                                                                               "proxy_read_timeout 2s;"
                                                                               "proxy_send_timeout 2s;"
                                                                               "proxy_pass_header Cache-Control;"
                                                                               "proxy_ignore_client_abort on;")))))))

(define %nginx-redirect-server-block
  (nginx-server-configuration (listen '("80"))
                              (raw-content (list
                                            "return 308 https://$host$request_uri;"))))

(define %my-home-config
  (home-environment
    (packages (append %guile-packages))

    (services
     (append (list (service my-home-emacs-service-type)
                   (service home-mcron-service-type
                            (home-mcron-configuration (jobs (list
                                                             garbage-collector-job)))))
             %my-home-services))))

;; Main operating system configuration
(operating-system
  (host-name "helga")
  (locale "en_US.utf8")
  (timezone "Asia/Kolkata")
  (kernel linux)
  (firmware (list linux-firmware))
  (initrd microcode-initrd)

  ;; Updated keyboard layout with custom options
  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps")))
  ;; User accounts with a custom shell for the 'b' user
  (users (cons* (user-account
                  (name "b")
                  (comment "Ayan")
                  (group "users")
                  (home-directory "/home/b")
                  (shell (file-append (specification->package "zsh")
                                      "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev"
                                          "audio"
                                          "video"
                                          "kvm"
                                          "docker"
                                          "libvirt"))) %base-user-accounts))

  ;; System-wide packages
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "curl")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "screen")
                          (specification->package "zstd")
                          (specification->package "font-dejavu")
                          (specification->package "fontconfig"))
                    %base-packages))

  ;; System services
  (services
   (list (service avahi-service-type)
         ;; Certbot for handling SSL certificates
         (service certbot-service-type
                  (certbot-configuration (email "bvits@riseup.net")
                                         (certificates (list (certificate-configuration
                                                              (domains '("ci.myguix.bvits.in"
                                                                         "substitutes.myguix.bvits.in"))
                                                              (deploy-hook
                                                               %nginx-deploy-hook))))))
         ;; PostgreSQL database service
         (service postgresql-service-type
                  (postgresql-configuration (postgresql (specification->package
                                                         "postgresql"))))
         (service postgresql-role-service-type)

         (service iptables-service-type
                  (iptables-configuration (ipv4-rules (plain-file
                                                       "iptables.rules"
                                                       "*filter
-A INPUT -p tcp --dport 5522 ! -s 127.0.0.1 -j REJECT
-A INPUT -p tcp --dport 5555:5558 ! -s 127.0.0.1 -j REJECT
-A INPUT -p tcp --dport 8080:8081 ! -s 127.0.0.1 -j REJECT
COMMIT
"))))

         ;; Cuirass for CI builds
         (service cuirass-service-type
                  (cuirass-configuration (remote-server (cuirass-remote-server-configuration
                                                         (private-key
                                                          "/etc/guix/signing-key.sec")
                                                         (public-key
                                                          "/etc/guix/signing-key.pub")
                                                         (publish? #f)
                                                         (trigger-url
                                                          "http://localhost:8080")))
                                         (specifications %cuirass-specs)))
         ;; Guix publish service
         (service guix-publish-service-type
                  (guix-publish-configuration
                   ;; Requires manual: sudo mkdir /var/cache/publish
                   ;; sudo chown -R guix-publish:guix-publish /var/cache/publish
                   (cache "/var/cache/publish")
                   (compression '(("zstd" 19)))
                   (port 8080)))

         ;; Nginx web server for CI and substitute services
         (service nginx-service-type
                  (nginx-configuration (upstream-blocks (list (nginx-upstream-configuration
                                                               (name
                                                                "guix-cuirass")
                                                               (servers (list
                                                                         "localhost:8081")))
                                                              (nginx-upstream-configuration
                                                               (name
                                                                "guix-publish")
                                                               (servers (list
                                                                         "localhost:8080")))))
                                       (server-blocks (list
                                                       %nginx-redirect-server-block
                                                       %ci-server-block
                                                       %substitutes-server-block))))
         (service wpa-supplicant-service-type)
         (service network-manager-service-type)
         ;; OpenSSH for remote access
         (service openssh-service-type
                  (openssh-configuration (authorized-keys `(("b" ,(local-file
                                                                   "../../keys/ssh/ragnar.pub"))
                                                            ("b" ,(local-file
                                                                   "../../keys/ssh/leif.pub"))))
                                         (password-authentication? #f)
                                         (port-number 2123)))
         ;; NTP for time synchronization
         (service ntp-service-type)
         (service login-service-type)
         (service virtual-terminal-service-type)
         (service console-font-service-type
                  (map (lambda (tty)
                         (cons tty %default-console-font))
                       '("tty1" "tty2" "tty3")))
         ;; Kmscon for a better console experience with hardware acceleration on tty1
         (service kmscon-service-type
                  (kmscon-configuration (virtual-terminal "tty1")
                                        (hardware-acceleration? #t)
                                        (font-engine "pango")
                                        (font-size 12)
                                        (keyboard-layout keyboard-layout)))

         (service syslog-service-type)
         (service agetty-service-type
                  (agetty-configuration (extra-options '("-L")) ;no carrier detect
                                        (term "vt100")
                                        (tty #f) ;automatic
                                        (shepherd-requirement '(syslogd))))

         (service mingetty-service-type
                  (mingetty-configuration (tty "tty2")))
         (service mingetty-service-type
                  (mingetty-configuration (tty "tty3")))

         (service static-networking-service-type
                  (list %loopback-static-networking))
         (service urandom-seed-service-type)
         (service libvirt-service-type
                  (libvirt-configuration (tls-port "16555")))
         (simple-service 'floki shepherd-root-service-type
                         (list (shepherd-service (requirement '(file-systems
                                                                networking))
                                                 (provision '(floki))
                                                 (documentation
                                                  "Runs the qemu VM specified in floki.scm")
                                                 (start (with-service-gexp-modules #~(begin
                                                                                       (lambda _
                                                                                         (let 
                                                                                              (
                                                                                               (cmd
                                                                                                (list #$
                                                                                                 (file-append
                                                                                                  qemu
                                                                                                  "/bin/qemu-system-x86_64")
                                                                                                 "-enable-kvm"
                                                                                                 "-nographic"
                                                                                                 "-m"
                                                                                                 "50G"
                                                                                                 "-smp"
                                                                                                 "8"
                                                                                                 "-device"
                                                                                                 "e1000,netdev=net0"
                                                                                                 "-netdev"
                                                                                                 "user,id=net0,hostfwd=tcp::5522-:22,hostfwd=tcp::5558-:5558"
                                                                                                 "-drive"
                                                                                                 "file=/data/floki.qcow2,if=virtio,cache=writeback,werror=report"
                                                                                                 "-serial"
                                                                                                 "mon:stdio")))
                                                                                           
                                                                                           
                                                                                           (fork+exec-command
                                                                                            cmd
                                                                                            #:log-file
                                                                                            "/var/log/floki.log")))))))))
         (service guix-service-type
                  (guix-configuration (generate-substitute-key? #f)
                                      (authorized-keys (append
                                                        %default-authorized-guix-keys
                                                        (list (local-file
                                                               "../../keys/guix/floki.pub"))))))
         (service nscd-service-type)
         (service rottlog-service-type)
         ;; Periodically delete old build logs.
         (service log-cleanup-service-type
                  (log-cleanup-configuration (directory "/var/log/guix/drvs")))
         ;; The LVM2 rules are needed as soon as LVM2 or the device-mapper is
         ;; used, so enable them by default.  The FUSE and ALSA rules are
         ;; less critical, but handy.
         (service udev-service-type
                  (udev-configuration (rules (list (specification->package
                                                    "lvm2")
                                                   (specification->package
                                                    "fuse")
                                                   (specification->package
                                                    "alsa-utils")
                                                   (specification->package
                                                    "crda")))))
         (service special-files-service-type
                  `(("/bin/sh" ,(file-append (specification->package "bash")
                                             "/bin/sh"))
                    ("/bin/bash" ,(file-append (specification->package "bash")
                                               "/bin/bash"))
                    ("/bin/zsh" ,(file-append (specification->package "zsh")
                                              "/bin/zsh"))
                    ("/bin/perl" ,(file-append (specification->package "perl")
                                               "/bin/perl"))
                    ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))
         (service guix-home-service-type
                  `(("b" ,%my-home-config)))
         (service sysctl-service-type
                  (sysctl-configuration (settings (append '(("net.ipv4.ip_forward" . "1")
                                                            ("vm.max_map_count" . "262144"))
                                                   %default-sysctl-settings))))
         (service spice-vdagent-service-type)
         (service containerd-service-type)
         (service docker-service-type)
         (service oci-container-service-type
                  (list oci-grobid-service-type))))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid "b6a84834-c082-4f7a-87e8-9c02475b36ee")))))

  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "9E00-9B3E"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid "8e5a6a8c-6c21-4352-9bda-274656be30f1"
                                       'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/data")
                         (device (uuid "be4313ab-fb8f-44a5-8255-5bad09832265"
                                       'ext4))
                         (type "ext4")) %base-file-systems)))
