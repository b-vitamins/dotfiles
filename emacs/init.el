;;; init.el --- Configuration bootstrap -*- lexical-binding: t -*-

;;; Commentary:
;; Bootstrap for Emacs configuration

;;; Code:

;;;; Bootstrap timing
(defconst bv-start-time (current-time)
  "Time when Emacs init started.")

(defun bv-report-init-time ()
  "Report Emacs initialization time."
  (message "Emacs loaded in %.3fs with %d garbage collections"
           (float-time (time-subtract (current-time) bv-start-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'bv-report-init-time)

;;;; Directory setup
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

;; Ensure directories exist
(dolist (dir (list bv-var-dir bv-etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Add lisp directory to load path
(add-to-list 'load-path bv-lisp-dir)

;;;; Package management setup
;; Skip package.el when using Guix
(if (or (getenv "GUIX_ENVIRONMENT") 
        (file-exists-p (expand-file-name "~/.guix-home")))
    ;; Using Guix
    (progn
      (setq package--initialized t)
      (message "Using Guix for package management"))
  ;; Not using Guix - set up package.el
  (require 'package)

  ;; Package archives
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

  ;; Prioritize archives
  (setq package-archive-priorities
        '(("gnu" . 30)
          ("melpa" . 20)
          ("nongnu" . 10)))

  ;; Initialize package.el
  (setq package-user-dir (expand-file-name "elpa" bv-var-dir))
  (package-initialize)

  ;; Bootstrap use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Configure use-package
(eval-and-compile
  (setq use-package-enable-imenu-support t
        use-package-compute-statistics t
        use-package-minimum-reported-time 0.01
        use-package-always-defer t
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))

;; Never ensure packages when using Guix
(require 'use-package-ensure)
(setq use-package-always-ensure nil)

;;;; Path configuration
;; XDG paths
(setq user-emacs-directory bv-emacs-dir
      custom-file (expand-file-name "custom.el" bv-var-dir)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" bv-var-dir))

;; Native compilation cache
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache" bv-var-dir)))

;;;; Core configuration
;; Re-enable native compilation after startup
(with-eval-after-load 'comp
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t))

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

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;;; Module loading system
(require 'cl-lib)

(defvar bv-enabled-features '()
  "List of enabled features.")

(defvar bv-module-load-times '()
  "Alist of (module . load-time) for debugging.")

(defmacro bv-require (module &optional noerror)
  "Load MODULE and track timing."
  `(let ((start-time (current-time)))
     (condition-case err
         (progn
           (require ',module)
           (cl-pushnew ',module bv-enabled-features)
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

;;;; Phased Module Loading

;; Phase 1: Foundation (Critical)
(bv-require bv-core)
(bv-require bv-defaults)
(bv-require bv-ui)

;; Phase 2: Core Productivity (Critical)
(with-eval-after-load 'bv-core
  (run-with-idle-timer 0.1 nil
    (lambda ()
      (bv-require bv-completion)
      (bv-require bv-navigation)
      (bv-require bv-development)
      (bv-require bv-git))))

;; Phase 3: Language Support (High)
(with-eval-after-load 'bv-development
  (run-with-idle-timer 0.5 nil
    (lambda ()
      (bv-require bv-lang-python noerror)
      (bv-require bv-lang-rust noerror)
      (bv-require bv-lang-lisp)
      (bv-require bv-lang-systems noerror)
      (bv-require bv-lang-haskell noerror))))

;; Phase 4: Research Infrastructure (High)
(with-eval-after-load 'bv-core
  (run-with-idle-timer 1.0 nil
    (lambda ()
      (bv-require bv-org)
      (bv-require bv-research)
      (bv-require bv-reading)
      (bv-require bv-writing))))

;; Phase 5: Extended Productivity (Medium)
(with-eval-after-load 'bv-core
  (run-with-idle-timer 1.5 nil
    (lambda ()
      (bv-require bv-shell)
      (bv-require bv-productivity)
      (bv-require bv-communication)
      (bv-require bv-multimedia))))

;; Interactive loaders for disabled phases
(defun bv-load-research-features ()
  "Load research-related features."
  (interactive)
  (bv-require bv-org)
  (bv-require bv-research)
  (bv-require bv-reading)
  (bv-require bv-writing)
  (message "Research features loaded"))

(defun bv-load-productivity-features ()
  "Load extended productivity features."
  (interactive)
  (bv-require bv-shell)
  (bv-require bv-productivity)
  (bv-require bv-communication)
  (bv-require bv-multimedia)
  (message "Extended productivity features loaded"))

(defun bv-load-all-features ()
  "Load all remaining features."
  (interactive)
  (bv-load-research-features)
  (bv-load-productivity-features)
  (message "All features loaded"))

;;;; Post-init setup
;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; Start server if not running
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
