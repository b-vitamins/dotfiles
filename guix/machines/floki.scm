;; -*- mode: scheme; -*-
(use-modules (gnu)
             (guix)
             (srfi srfi-1))
(use-service-modules avahi cuirass mcron networking ssh)
(use-package-modules bootloaders
                     certs
                     fonts
                     nvi
                     package-management
                     wget
                     xorg)

(define garbage-collector-job
  #~(job "0 3 * * *" "guix gc -F 50G"))

(operating-system
  (host-name "floki")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  ;; Label for the GRUB boot menu.
  (label (string-append "GNU Guix "
                        (package-version guix)))

  ;; The list of user accounts ('root' is implicit).
  (users (cons (user-account
                 (name "guest")
                 (comment "GNU Guix Live")
                 (password "")
                 (group "users")
                 (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

  ;; Our /etc/sudoers file.  Since 'guest' initially has an empty password,
  ;; allow for password-less sudo.
  (sudoers-file (plain-file "sudoers" "root ALL=(ALL) ALL
%wheel ALL=NOPASSWD: ALL
"))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "nvi")
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
                                                                "10.0.2.2:5555")
                                                               (substitute-urls '
                                                                ("https://ci.guix.gnu.org"
                                                                 "https://substitutes.myguix.bvits.in"))
                                                               (systems '("x86_64-linux"))
                                                               (workers 2)))
                 ;; OpenSSH for remote access
                 (service openssh-service-type
                          (openssh-configuration (authorized-keys `(("b" ,(local-file
                                                                           "../../keys/ssh/helga.pub"))))
                                                 (password-authentication? #f)))
                 (simple-service 'cron-jobs mcron-service-type
                                 (list garbage-collector-job))
                 (service wpa-supplicant-service-type)
                 (service network-manager-service-type)
                 (service ntp-service-type)
                 (service gpm-service-type))

           ;; This is the default list of services we
           ;; are appending to.
           (modify-services %base-services
             (guix-service-type config =>
                                (guix-configuration (inherit config)
                                                    (authorized-keys (append
                                                                      %default-authorized-guix-keys
                                                                      (list (local-file
                                                                             "../../keys/guix/helga.pub")))))))))

  ;; Below we assume /dev/vda is the VM's hard disk.
  ;; Adjust as needed.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/vda"))
                (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda2")
                        (type "ext4")) %base-file-systems)))
