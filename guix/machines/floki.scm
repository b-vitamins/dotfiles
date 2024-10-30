;; -*- mode: scheme; -*-
;; guix system image --image-type=qcow2 /home/b/projects/dotfiles/guix/machines/floki.scm
(use-modules (gnu)
             (guix)
             (srfi srfi-1))
(use-service-modules avahi cuirass networking ssh mcron)

;; Run the garbe collector every day at 3:00 AM
(define %garbage-collector-job
  #~(job "0 3 * * *" "guix gc -F 50G"))

(operating-system
  (host-name "floki")
  (timezone "Asia/Kolkata")
  (locale "en_US.utf8")
  (label (string-append "GNU Guix "
                        (package-version (specification->package "guix"))))

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "b")
                  (comment "Ayan")
                  (group "users")
                  (password "")
                  (shell (file-append (specification->package "zsh")
                                      "/bin/zsh"))
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
                          (specification->package "curl")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "nvi")
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
                                                                "10.0.0.2:5555")
                                                               (substitute-urls '
                                                                ("https://ci.guix.gnu.org"
                                                                 "https://substitutes.myguix.bvits.in"))
                                                               (systems '("x86_64-linux"))
                                                               (workers 2)))
                 ;; OpenSSH for remote access
                 (service openssh-service-type
                          (openssh-configuration (allow-empty-passwords? #t)
                                                 (permit-root-login #t)))

                 (simple-service 'my-cron-jobs mcron-service-type
                                 (list %garbage-collector-job))

                 (service wpa-supplicant-service-type)
                 (service network-manager-service-type)
                 (service ntp-service-type)
                 (service gpm-service-type))

           (modify-services %base-services
             (guix-service-type config =>
                                (guix-configuration (inherit config)
                                                    (generate-substitute-key?
                                                                              #f)
                                                    (authorized-keys (append
                                                                      %default-authorized-guix-keys
                                                                      (list (plain-file
                                                                             "substitutes.myguix.bvits.in"
                                                                             "(public-key 
 (ecc 
  (curve Ed25519)
  (q #07F312DEF6FA7A83CD5825457EDA1388C5B6636C143096D1365DE07FCF4E3CC9#)
  )
 )"))))
                                                    (extra-options '("--max-jobs=2"
                                                                     "--cores=8")))))))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/vda"))
                (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda2")
                        (type "ext4")) %base-file-systems)))
