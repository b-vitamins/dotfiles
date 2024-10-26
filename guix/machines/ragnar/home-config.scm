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
             (myguix home)
             (myguix home services emacs))

(define %borg-backup-job
  ;; Run 'borg' to create backups of local paths in a local repository.
  #~(job '(next-hour '(0)) ; Run daily at 00:00
         (lambda ()
           (let* ((repo-path "ssh://u429656@u429656.your-storagebox.de:23/./backups")
                  (backup-name (string-append "ragnar-" (strftime "%Y-%m-%d-%H-%M-%S" (localtime (current-time)))))
                  (log-file "/home/b/.local/share/borg-backup.log")
                  (lock-file "/tmp/borg-backup.lock"))
             ;; Lock file to prevent overlapping jobs
             (if (file-exists? lock-file)
                 ;; Skip backup if another instance is running
                 (format #t "Skipping Borg backup: previous instance is still running~%")
                 ;; Proceed with backup if no lock file exists
                 (begin
                   (call-with-output-file lock-file
                     (lambda (port)
                       (format port "Borg backup started at ~a~%" (current-time))))

                   ;; Set BORG_PASSCOMMAND to securely retrieve the first line of the pass entry
                   (setenv "BORG_PASSCOMMAND" "pass infra/borg/passphrase")
                   
                   ;; Create Borg backup with lzma compression (level 9)
                   (system* "borg"
                            "create"
                            (string-append repo-path "::" backup-name)
                            "/home/b"
                            "--exclude-caches"
                            "--exclude" "/home/b/.gnupg/*"
                            "--exclude" "/home/b/.ssh/*"
                            "--exclude" "/home/b/.guix-home/*"
                            "--exclude" "/home/b/.guix-profile/*"
                            "--exclude" "/home/b/.npm/*"
                            "--exclude" "/home/b/.cache/*"
                            "--exclude" "/home/b/.local/state/*"
                            "--exclude" "/home/b/downloads/*"
                            "--compression" "zstd,22")

                   ;; Prune old backups (keep 7 daily, 4 weekly, and 12 monthly backups)
                   (system* "borg"
                            "prune"
                            repo-path
                            "--keep-daily=7"
                            "--keep-weekly=4"
                            "--keep-monthly=12")

                   ;; Remove lock file after backup completes
                   (delete-file lock-file)
                   (format #t "Borg backup completed successfully~%")))))
         "borg-backup"))

;; Sync job: runs every hour
(define %mega-sync-job
  ;; Run 'rclone' to sync local paths to their respective remote destinations.
  #~(job '(next-hour (range 0 24 1)) ;Run every hour
         (lambda ()
           (let* ((source-destination-pairs '(("/home/b/documents"
                                               "mega:ragnar/documents")
                                              ("/home/b/music"
                                               "mega:ragnar/music")
                                              ("/home/b/pictures"
                                               "mega:ragnar/pictures")
                                              ("/home/b/projects"
                                               "mega:ragnar/projects")
                                              ("/home/b/templates"
                                               "mega:ragnar/templates")
                                              ("/home/b/videos"
                                               "mega:ragnar/videos")))
                  (log-file "/home/b/.local/share/rclone-backup.log") ;Log file for backup
                  (lock-file "/tmp/rclone-backup.lock"))
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
                                          "sync"
                                          path
                                          remote
                                          "--log-file"
                                          log-file))) source-destination-pairs)

                   ;; Remove lock file after backup completes
                   (delete-file lock-file)
                   (format #t "Backup completed successfully~%")))))
         "mega-sync"))

;; Dedupe job: runs 3 times a day
(define %mega-dedupe-job
  ;; Run 'rclone dedupe' on the 'mega:ragnar' path to remove duplicates.
  ;; This job runs at 8:00 AM, 2:00 PM, and 8:00 PM.
  #~(job '(next-hour '(8 14 20)) ;Run at 8:00 AM, 2:00 PM, and 8:00 PM
         (lambda ()
           (let ((remote-path "mega:ragnar")
                 ;; Remote path to deduplicate
                 (log-file "/home/b/.local/share/rclone-dedupe.log"))
             ;; Log file for deduplication process
             
             ;; Perform deduplication
             (system* "rclone" "dedupe" remote-path "--log-file" log-file)

             (format #t "Deduplication completed successfully~%")))
         "mega-dedupe"))

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
                    %language-support-packages
                    %system-monitoring-packages
                    %security-tools-packages
                    %media-tools-packages
                    %desktop-environment-packages
                    %document-formatting-packages))

  (services
   (append (list
            ;; Home Emacs Service
            (service my-home-emacs-service-type)
            ;; Scheduled User’s Job Execution
            (service home-mcron-service-type
                     (home-mcron-configuration (jobs (list
                                                      %garbage-collector-job
                                                      %borg-backup-job
                                                      %mega-sync-job
                                                      %mega-dedupe-job))))

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
                                                                         "../../../keys/ssh/freydis.pub")
                                                                        (local-file
                                                                         "../../../keys/ssh/leif.pub")
                                                                        (local-file
                                                                         "../../../keys/ssh/bjorn.pub")))
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
                     (home-beets-configuration (directory "/home/b/music"))))
           %my-home-services)))
