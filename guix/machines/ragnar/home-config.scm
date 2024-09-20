;; -*- mode: scheme; -*-
(use-modules (gnu home)
             (gnu services)
             (gnu packages sync)
             (gnu home services mcron)
             (gnu home services ssh)
             (gnu home services desktop)
             (gnu home services media)
             (gnu home services music)
             (gnu home services dict)
             (gnu home services sound)
             (gnu home services syncthing)
             (gnu home services shells)
             (gnu system shadow)
             (guix gexp)
             (myguix packages base)
             (myguix services mcron)
             (myguix services home))

;; Fast backup job: runs every hour
(define %rclone-fast-backup-job
  ;; Run 'rclone' to back up smaller, frequently-changing paths to their respective remote destinations.
  ;; This job runs every hour and skips execution if a previous instance is still running.
  #~(job '(next-hour (range 0 24 1)) ;Run every hour
         (lambda ()
           (let* ((source-destination-pairs '(("/home/b/.authinfo.gpg"
                                               "mega:backup/ragnar/.authinfo.gpg")
                                              ("/home/b/.config/rclone"
                                               "mega:backup/ragnar/.config/rclone")
                                              ("/home/b/.gitconfig"
                                               "mega:backup/ragnar/.gitconfig")
                                              ("/home/b/.gnupg"
                                               "mega:backup/ragnar/.gnupg")
                                              ("/home/b/.guile"
                                               "mega:backup/ragnar/.guile")
                                              ("/home/b/.local/latex-previews"
                                               "mega:backup/ragnar/.local/latex-previews")
                                              ("/home/b/.mozilla"
                                               "mega:backup/ragnar/.mozilla")
                                              ("/home/b/.password-store"
                                               "mega:backup/ragnar/.password-store")
                                              ("/home/b/projects"
                                               "mega:backup/ragnar/projects")
                                              ("/home/b/slipbox"
                                               "mega:backup/ragnar/slipbox")
                                              ("/home/b/.ssh"
                                               "mega:backup/ragnar/.ssh")))
                  (log-file "/home/b/.local/share/rclone-fast-backup.log") ;Log file for fast backup
                  (lock-file "/tmp/rclone-fast-backup.lock"))
             ;; Lock file to prevent overlapping jobs
             
             (if (file-exists? lock-file)
                 ;; Skip backup if another instance is running
                 (format #t
                  "Skipping backup: previous instance is still running~%")

                 ;; Proceed with backup if no lock file exists
                 (begin
                   (call-with-output-file lock-file
                     (lambda (port)
                       (format port "Backup started at ~a~%"
                               (current-time))))

                   ;; Perform the backup for each source-destination pair
                   (for-each (lambda (pair)
                               (let ((path (car pair))
                                     (remote (cadr pair)))
                                 (system* "rclone"
                                          "copy"
                                          path
                                          remote
                                          "--log-file"
                                          log-file))) source-destination-pairs)

                   ;; Remove lock file after backup completes
                   (delete-file lock-file)
                   (format #t "Backup completed successfully~%")))))
         "rclone-fast-backup"))

;; Slow backup job: runs every 4 hours
(define %rclone-slow-backup-job
  ;; Run 'rclone' to back up large or less-frequently-changing paths to their respective remote destinations.
  ;; This job runs every 4 hours and skips execution if a previous instance is still running.
  #~(job '(next-hour (range 0 24 4)) ;Run every 4 hours
         (lambda ()
           (let* ((source-destination-pairs '(("/home/b/library/books"
                                               "mega:backup/ragnar/library/books")
                                              ("/home/b/library/audiobooks"
                                               "mega:backup/ragnar/library/audiobooks")
                                              ("/home/b/library/pictures"
                                               "mega:backup/ragnar/library/pictures")
                                              ("/home/b/library/courses"
                                               "mega:backup/ragnar/library/courses")
                                              ("/home/b/library/documents"
                                               "mega:backup/ragnar/library/documents")
                                              ("/home/b/library/archives"
                                               "mega:backup/ragnar/library/archives")
                                              ("/home/b/library/notes"
                                               "mega:backup/ragnar/library/notes")
                                              ("/home/b/library/papers"
                                               "mega:backup/ragnar/library/papers")
                                              ("/home/b/library/slides"
                                               "mega:backup/ragnar/library/slides")))
                  (log-file "/home/b/.local/share/rclone-slow-backup.log") ;Log file for slow backup
                  (lock-file "/tmp/rclone-slow-backup.lock"))
             ;; Lock file to prevent overlapping jobs
             
             (if (file-exists? lock-file)
                 ;; Skip backup if another instance is running
                 (format #t
                  "Skipping backup: previous instance is still running~%")

                 ;; Proceed with backup if no lock file exists
                 (begin
                   (call-with-output-file lock-file
                     (lambda (port)
                       (format port "Backup started at ~a~%"
                               (current-time))))

                   ;; Perform the backup for each source-destination pair
                   (for-each (lambda (pair)
                               (let ((path (car pair))
                                     (remote (cadr pair)))
                                 (system* "rclone"
                                          "copy"
                                          path
                                          remote
                                          "--log-file"
                                          log-file))) source-destination-pairs)

                   ;; Remove lock file after backup completes
                   (delete-file lock-file)
                   (format #t "Backup completed successfully~%")))))
         "rclone-slow-backup"))

;; Dedupe job: runs 3 times a day
(define %rclone-dedupe-job
  ;; Run 'rclone dedupe' on the 'mega:backup/ragnar' path to remove duplicates.
  ;; This job runs at 8:00 AM, 2:00 PM, and 8:00 PM.
  #~(job '(next-hour '(8 14 20)) ;Run at 8:00 AM, 2:00 PM, and 8:00 PM
         (lambda ()
           (let ((remote-path "mega:backup/ragnar")
                 ;; Remote path to deduplicate
                 (log-file "/home/b/.local/share/rclone-dedupe.log"))
             ;; Log file for deduplication process
             
             ;; Perform deduplication
             (system* "rclone" "dedupe" remote-path "--log-file" log-file)

             (format #t "Deduplication completed successfully~%")))
         "rclone-dedupe"))

(home-environment
  (packages (append %system-core-packages
                    %compression-tools-packages
                    %filesystem-management-packages
                    %terminal-tools-packages
                    %network-tools-packages
                    %development-tools-packages
                    %rust-development-packages
                    %python-development-packages
                    %guile-development-packages
                    %perl-development-packages
                    %emacs-core-packages
                    %language-support-packages
                    %system-monitoring-packages
                    %security-tools-packages
                    %media-tools-packages
                    %desktop-environment-packages
                    %document-formatting-packages))

  (services
   (append (list
            ;; Scheduled User’s Job Execution
            (service home-mcron-service-type
                     (home-mcron-configuration (jobs (list
                                                      %garbage-collector-job
                                                      %rclone-fast-backup-job
                                                      %rclone-slow-backup-job
                                                      %rclone-dedupe-job))))

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
                                                                         "keys/ssh/leif.pub")
                                                                        (local-file
                                                                         "keys/ssh/lagertha.pub")
                                                                        (local-file
                                                                         "keys/ssh/bjorn.pub")
                                                                        (local-file
                                                                         "keys/ssh/freydis.pub")))
                                                 (add-keys-to-agent "confirm")))

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
                     (home-beets-configuration (directory
                                                "/home/b/library/music"))))
           %my-home-services)))
