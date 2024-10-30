;; -*- mode: scheme; -*-
(use-modules (gnu)
             (gnu packages virtualization)
             (gnu services admin)
             (gnu services avahi)
             (gnu services certbot)
             (gnu services cuirass)
             (gnu services mcron)
             (gnu services networking)
             (gnu services shepherd)
             (gnu services ssh)
             (gnu services web)
             (gnu services sysctl)
             (guix channels)
             (guix modules)
             (ice-9 match))

(define %my-channels
  (cons (channel
          (name 'myguix)
          (url "https://github.com/b-vitamins/myguix.git")
          (branch "master")
          (introduction
           (make-channel-introduction
            "85d58b09dc71e9dc9834b666b658f79d2e212d65"
            (openpgp-fingerprint
             "883B CA6B D275 A5F2 673C  C5DD 2AD3 2FC0 2A50 01F7"))))
        %default-channels))

;; Define Cuirass specifications for building the 'myguix' channel
(define %cuirass-specs
  #~(list (specification (name "myguix")
                         (build '(channels myguix))
                         (channels %my-channels)
                         (priority 0)
                         (systems '("x86_64-linux")))
          (specification (name "images")
                         (build "images")
                         (channels %my-channels)
                         (priority 0)
                         (systems '("x86_64-linux")))))

;; Define the Nginx deploy hook to reload Nginx on certificate renewal
(define %nginx-deploy-hook
  (program-file "nginx-deploy-hook"
                #~(let ((pid (call-with-input-file "/var/run/nginx/pid"
                               read)))
                    (kill pid SIGHUP))))

;; Define a NGINX server block for your domain
(define %bvits-server-block
  (nginx-server-configuration (server-name '("bvits.in" "www.bvits.in"))
                              (root "/srv/http/bvits.in")
                              (listen '("80"))
                              (index '("index.html"))))

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

(define garbage-collector-job
  ;; Collect garbage 5 minutes after midnight every day.
  ;; The job's action is a shell command.
  #~(job "5 0 * * *" ;Vixie cron syntax
         "guix gc -F 10G"))

(define-public default-module-filter
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (('myguix _ ...) #t)
    (_ #f)))

(define-syntax-rule (with-service-gexp-modules body ...)
  (with-imported-modules (source-module-closure
                          (append '((gnu build shepherd))
                                  ;; This %default-modules is from (gnu services shepherd).
                                  %default-modules)
                          #:select? default-module-filter)
    body ...))

;; Main operating system configuration
(operating-system
  (host-name "helga")
  (locale "en_US.utf8")
  (timezone "Asia/Kolkata")
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
                  (supplementary-groups '("wheel" "netdev" "kvm")))
                %base-user-accounts))

  ;; System-wide packages
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "curl")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "ncdu")
                          (specification->package "qemu")
                          (specification->package "rsync")
                          (specification->package "screen")
                          (specification->package "smartmontools")
                          (specification->package "zstd")
                          (specification->package "coreutils")
                          (specification->package "font-dejavu")
                          (specification->package "font-iosevka-comfy")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-serif-cjk")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "bind")
                          (specification->package "cifs-utils")
                          (specification->package "fontconfig"))
                    %base-packages))

  ;; System services
  (services
   (list ;; Firewall Setup
         (service iptables-service-type
                  (iptables-configuration
                   (ipv4-rules (plain-file "iptables.rules" "*filter
-A INPUT -p tcp --dport 5522 ! -s 127.0.0.1 -j REJECT
-A INPUT -p tcp --dport 5555:5558 ! -s 127.0.0.1 -j REJECT
-A INPUT -p tcp --dport 8080:8081 ! -s 127.0.0.1 -j REJECT
COMMIT
"))))

         ;; Certbot for handling SSL certificates
         (service certbot-service-type
                  (certbot-configuration (email "bvits@riseup.net")
                                         (certificates (list (certificate-configuration
                                                              (domains '("ci.myguix.bvits.in"
                                                                         "substitutes.myguix.bvits.in"))
                                                              (deploy-hook
                                                               %nginx-deploy-hook))
                                                             (certificate-configuration
                                                              (domains '("bvits.in"
                                                                         "www.bvits.in"))
                                                              (deploy-hook
                                                               %nginx-deploy-hook))))))
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
                  (guix-publish-configuration (compression '(("zstd" 19)))
                                              (cache "/var/cache/publish")
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
                                                       %substitutes-server-block
                                                       %bvits-server-block))))
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
         (service guix-service-type
                  (guix-configuration (generate-substitute-key? #f)
                                      (authorized-keys (append
                                                        %default-authorized-guix-keys
                                                        (list (local-file
                                                               "../../keys/guix/myguix-cuirass-worker-signing-key.pub"))))))
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
         (service avahi-service-type)
         (service sysctl-service-type)
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
         (simple-service 'my-cron-jobs mcron-service-type
                         (list garbage-collector-job))
         (simple-service 'floki
                         shepherd-root-service-type
                         (list
                          (shepherd-service
                           (requirement '(file-systems networking))
                           (provision '(floki))
                           (documentation "Runs the qemu VM specified in floki.scm")
                           (start
                            (with-service-gexp-modules
                             #~(begin
                                 (lambda _
                                   (let ((cmd (list #$(file-append qemu "/bin/qemu-system-x86_64")
                                                    "-enable-kvm"
                                                    "-nographic"
                                                    "-m"      "80G"
                                                    "-smp"    "8"
                                                    "-device" "e1000,netdev=net0"
                                                    "-netdev" "user,id=net0,hostfwd=tcp::5522-:22,hostfwd=tcp::5558-:5558"
                                                    "-drive"  "file=/data/floki.qcow2,if=virtio,cache=writeback,werror=report"
                                                    "-serial" "mon:stdio")))
                                     (fork+exec-command
                                      cmd
                                      #:log-file "/var/log/floki.log")))))))))))
  ;; Bootloader configuration
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets (list "/dev/sda"))
               (keyboard-layout keyboard-layout)))

  ;; Initrd modules for virtio SCSI support
  (initrd-modules (append '("virtio_scsi") %base-initrd-modules))

  ;; Swap space configuration
  (swap-devices (list (swap-space
                       (target (uuid
                                "8a764dc5-460d-4bbc-833b-17deaeb41691")))))

  ;; File system configuration
  (file-systems (cons* (file-system
                        (mount-point "/")
                        (device (uuid
                                 "c39c9d7f-6528-4dd9-bce1-26c6017c8803"
                                 'ext4))
                        (type "ext4"))
                       (file-system
                        (mount-point "/data")
                        (device (uuid
                                 "1a7d0ab9-cda8-4206-a623-5de8fda3aa8f"
                                 'ext4))
                        (type "ext4"))
                       (file-system
                        (device "//u429656.your-storagebox.de/guix-publish/samba/zstd")
                        (options "uid=guix-publish,gid=guix-publish,credentials=/root/samba.credentials")
                        (mount-point "/var/cache/publish/zstd")
                        (mount? #f)
                        (type "cifs"))
                       %base-file-systems)))
