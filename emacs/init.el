;;; init.el --- Configuration bootstrap -*- lexical-binding: t -*-

;;; Commentary:
;; Bootstrap for Emacs configuration on Guix
;; Minimal, fast, and modular

;;; Code:

;;;; Bootstrap Timing
(defconst bv-start-time (current-time)
  "Time when Emacs init started.")

(defun bv-report-init-time ()
  "Report Emacs initialization time."
  (message "Emacs ready in %.3fs with %d garbage collections"
           (float-time (time-subtract (current-time) bv-start-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'bv-report-init-time 'append)

;;;; Directory Setup
(defconst bv-emacs-dir (file-name-directory load-file-name)
  "Root directory of configuration.")

(defconst bv-lisp-dir (expand-file-name "lisp" bv-emacs-dir)
  "Directory containing configuration modules.")

(defconst bv-var-dir
  (expand-file-name "var" (or (getenv "XDG_CACHE_HOME") "~/.cache/emacs"))
  "Directory for persistent data.")

(defconst bv-etc-dir
  (expand-file-name "etc" (or (getenv "XDG_CONFIG_HOME") "~/.config/emacs"))
  "Directory for configuration files.")

;;;; Guix System Paths
(defconst bv-guix-system-profile "/run/current-system/profile"
  "Guix system profile directory.")

(defconst bv-guix-home-profile (expand-file-name "~/.guix-home/profile")
  "Guix home profile directory.")

(defconst bv-guix-user-profile (expand-file-name "~/.guix-profile")
  "Guix user profile directory.")

;; Library paths
(defconst bv-system-lib-dir (expand-file-name "lib" bv-guix-system-profile)
  "System library directory.")

(defconst bv-home-lib-dir (expand-file-name "lib" bv-guix-home-profile)
  "Home library directory.")

;; Binary paths
(defconst bv-system-bin-dir (expand-file-name "bin" bv-guix-system-profile)
  "System binary directory.")

(defconst bv-home-bin-dir (expand-file-name "bin" bv-guix-home-profile)
  "Home binary directory.")

;; Share paths
(defconst bv-system-share-dir (expand-file-name "share" bv-guix-system-profile)
  "System share directory.")

(defconst bv-home-share-dir (expand-file-name "share" bv-guix-home-profile)
  "Home share directory.")

;; Ensure directories exist
(dolist (dir (list bv-var-dir bv-etc-dir bv-lisp-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Add lisp directory to load path
(add-to-list 'load-path bv-lisp-dir)

;;;; Guix Package Management
;; Disable package.el entirely - packages come from Guix
(setq package-enable-at-startup nil)

;; Configure use-package (provided by Guix)
(require 'use-package)
(setq use-package-always-ensure nil
      use-package-enable-imenu-support t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.01
      use-package-always-defer t
      use-package-expand-minimally t)

;;;; Path Configuration
(setq user-emacs-directory bv-emacs-dir
      custom-file (expand-file-name "custom.el" bv-var-dir)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" bv-var-dir))

;; Native compilation cache
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache" bv-var-dir)))

;;;; Core Settings
;; Re-enable native compilation after startup
(with-eval-after-load 'comp
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent))
  ;; Use the non-obsolete variable for Emacs 29+
  (if (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation t)
    (when (boundp 'native-comp-deferred-compilation)
      (setq native-comp-deferred-compilation t))))

;; Character encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Basic settings
(setq-default
 ;; Encoding
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 locale-coding-system 'utf-8
 ;; Misc
 ring-bell-function 'ignore
 visible-bell nil
 load-prefer-newer t
 enable-recursive-minibuffers t
 ;; Performance
 read-process-output-max (* 1024 1024)
 process-adaptive-read-buffering nil
 fast-but-imprecise-scrolling t
 redisplay-skip-fontification-on-input t)

;;;; Module Loading System
(require 'cl-lib)

(defvar bv-module-load-times '()
  "Alist of (module . load-time) for debugging.")

(defmacro bv-require (module &optional noerror)
  "Load MODULE and track timing.  If NOERROR is non-nil, don't signal errors."
  `(let ((start-time (current-time)))
     (condition-case err
         (progn
           (require ',module)
           (push (cons ',module (float-time (time-subtract (current-time) start-time)))
                 bv-module-load-times))
       (error
        (message "Error loading %s: %s" ',module err)
        (unless ,noerror
          (signal (car err) (cdr err)))))))

(defun bv-report-module-times ()
  "Report module loading times."
  (interactive)
  (message "Module load times:")
  (dolist (entry (sort bv-module-load-times
                       (lambda (a b) (> (cdr a) (cdr b)))))
    (message "  %-20s %.3fs" (car entry) (cdr entry))))

;;;; Load Core Modules
;; These are always loaded
(bv-require bv-core)
(bv-require bv-defaults)
(bv-require bv-ui)

;;;; Deferred Module Loading

;; Core productivity (load after 0.1s idle)
(run-with-idle-timer 0.1 nil
   (lambda ()
     (bv-require bv-completion)
     (bv-require bv-navigation)
     (bv-require bv-development)
     (bv-require bv-git)))

;; Research tools (load after 1.0s idle)
(run-with-idle-timer 1.0 nil
   (lambda ()
     (bv-require bv-org)
     (bv-require bv-research)
;;     (bv-require bv-reading)
;;     (bv-require bv-writing)
     ))

;; Language support (load after 0.5s idle)
;; (run-with-idle-timer 0.5 nil
;;   (lambda ()
;;     (bv-require bv-lang-lisp)
;;     (bv-require bv-lang-python noerror)
;;     (bv-require bv-lang-rust noerror)))

;;;; Interactive Module Loaders
(defun bv-load-all-modules ()
  "Load all available modules interactively."
  (interactive)
  (message "Loading all modules...")
  (dolist (file (directory-files bv-lisp-dir t "^bv-.*\\.el$"))
    (let ((module (intern (file-name-base file))))
      (unless (memq module '(bv-core bv-defaults bv-ui))
        (bv-require module t))))
  (message "All modules loaded"))

;;;; Post-init Setup
;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Start server if not running
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
