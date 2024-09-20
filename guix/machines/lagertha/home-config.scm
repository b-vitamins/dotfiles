;; -*- mode: scheme; -*-
(use-modules (gnu home)
             (gnu services)
             (gnu home services mcron)
             (gnu home services ssh)
             (gnu home services desktop)
             (gnu home services media)
             (gnu home services music)
             (gnu home services dict)
             (gnu home services sound)
             (gnu home services syncthing)
             (gnu home services shells)
             (gnu home services pm)
             (gnu system shadow)
             (guix gexp)
             (myguix packages base)
             (myguix services mcron)
             (myguix home)
             (myguix home services emacs))

(home-environment
  (packages (append %system-core-packages
                    %compression-tools-packages
                    %terminal-tools-packages
                    %network-tools-packages
                    %python-development-packages
                    %guile-development-packages
                    %perl-development-packages
                    %security-tools-packages))

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
                                                                         "keys/ssh/ragnar.pub")
                                                                        (local-file
                                                                         "keys/ssh/leif.pub")
                                                                        (local-file
                                                                         "keys/ssh/bjorn.pub")
                                                                        (local-file
                                                                         "keys/ssh/freydis.pub")))
                                                 (add-keys-to-agent "confirm")))

            ;; Desktop Home Services
            (service home-dbus-service-type)

            ;; Networking Home Services
            (service home-syncthing-service-type)) %my-home-services)))
