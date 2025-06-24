;;; bv-transient.el --- Transient menu interface -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.5.0"))
;; Keywords: convenience, lisp, extensions
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/bv-transient.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides a transient-based menu interface for configuration commands.
;;
;; This file requires:
;; - bv-core.el (core utilities)
;; - transient.el (ships with Emacs 28+, available as package for earlier
;;   versions)
;;
;; Usage:
;;   M-x bv-menu         - Main menu with common commands
;;   M-x bv-menu-advanced - Extended menu with developer tools
;;
;; Recommended keybinding:
;;   (global-set-key (kbd "C-c b") #'bv-menu)
;;
;; Or add to your leader map:
;;   (define-key bv-app-map "m" #'bv-menu)
;;
;; The menu provides interactive access to configuration management,
;; feature registration, path operations, and developer tools.

;;; Code:

(require 'bv-core)
(require 'transient)
(require 'subr-x)  ;; For string-blank-p

;; Declare functions that might not be available
(declare-function list-timers "timer-list" ())
(declare-function with-help-window "help" (buffer-or-name &rest body))
(declare-function transient-quit-one "transient" ())
(declare-function help-mode "help-mode" ())
(declare-function bv-defaults-open-cache-dir "bv-defaults" ())
(declare-function bv-defaults-reset-cache-dirs "bv-defaults" ())

;; Compatibility layer for Emacs 30+ changes
(eval-and-compile
  ;; Check if we're on Emacs 30+ with the new sort API
  (defconst bv-transient--emacs30-p (and (boundp 'emacs-version)
                                          (string-match "^30\\." emacs-version))
    "Non-nil if running on Emacs 30 or later.")

  ;; Local compatibility wrapper for sort function
  (if bv-transient--emacs30-p
      ;; Emacs 30+ with keyword-only API
      (defun bv-transient--sort-list (list predicate)
        "Sort LIST using PREDICATE in a non-destructive way."
        (sort (copy-sequence list) :lessp predicate))
    ;; Emacs < 30 with traditional API
    (defun bv-transient--sort-list (list predicate)
      "Sort LIST using PREDICATE in a non-destructive way."
      (sort (copy-sequence list) predicate))))

;;; Menu Command Definitions

;; Unfortunately, transient-define-prefix requires literal values at
;; compile time, so we can't use a fully DRY approach. However, we can
;; still minimize duplication by using helper macros for repeated patterns.

;;;###autoload (autoload 'bv-menu "bv-transient" nil t)
;;;###autoload
(put 'bv-menu 'function-documentation "Main configuration menu.")
(transient-define-prefix bv-menu ()
  "Main configuration menu."
  :value '()
  ["Configuration"
   ["Reports"
    ("rc" "Configuration Report" bv-report-config
     :description "Show all features and values")
    ("rp" "Path Report" bv-path-report
     :description "Show all configured paths")
    ("rd" "Check Dependencies" bv-check-feature-dependencies
     :description "Verify feature dependencies")
    ("q" "Quit" transient-quit-one)]

   ["Configuration"
    ("vs" "Set Value" bv-menu-set-value
     :description "Set a configuration value")
    ("vg" "Get Value" bv-menu-get-value
     :description "Display a configuration value")
    ("vr" "Require Value" bv-menu-require-value
     :description "Get required value or error")]

   ["Features"
    ("fe" "Enable Feature" bv-menu-register-feature
     :description "Register a new feature")
    ("fc" "Check Feature" bv-menu-check-feature
     :description "Check if feature is enabled")]

   ["Paths"
    ("pe" "Expand etc/ Path" bv-menu-expand-etc
     :description "Expand path relative to etc/")
    ("pv" "Expand var/ Path" bv-menu-expand-var
     :description "Expand path relative to var/")
    ("pd" "Ensure Directory" bv-menu-ensure-directory
     :description "Create directory if needed")]

   ["Maintenance"
    ("tc" "Cancel All Timers" bv-cancel-all-timers
     :description "Cancel pending setup timers")
    ("th" "Timer Help" bv-menu-timer-help
     :description "Show timer management help")]])

;; Interactive command wrappers for better UX

;;;###autoload
(defun bv-menu-set-value (key value)
  "Set configuration KEY to VALUE interactively."
  (interactive
   (let* ((key-str (read-string "Configuration key: "))
          (key (intern key-str))
          (current (bv-get-value key bv--not-found))
          (prompt (if (eq current bv--not-found)
                      (format "Value for '%s': " key)
                    (format "Value for '%s' (current: %S): " key current)))
          (input (read-string prompt)))
     (list key
           (condition-case err
               (or (car (read-from-string input)) input)
             (error
              (message "Invalid Lisp syntax, using as string: %s"
                       (error-message-string err))
              input)))))
  (bv-set-value key value)
  (message "Set '%s' to %S" key value))

;;;###autoload
(defun bv-menu-get-value (key)
  "Display configuration value for KEY."
  (interactive
   (list (intern (completing-read "Configuration key: "
                                  (let ((keys '()))
                                    (maphash (lambda (k _) (push k keys))
                                             bv-config-values)
                                    (mapcar #'symbol-name keys))))))
  (let ((value (bv-get-value key bv--not-found)))
    (if (eq value bv--not-found)
        (message "Key '%s' not found" key)
      (message "'%s' = %S" key value))))

;;;###autoload
(defun bv-menu-require-value (key)
  "Get required configuration value for KEY."
  (interactive
   (list (intern (completing-read "Required key: "
                                  (let ((keys '()))
                                    (maphash (lambda (k _) (push k keys))
                                             bv-config-values)
                                    (mapcar #'symbol-name keys))))))
  (condition-case err
      (let ((value (bv-require-value key)))
        (message "'%s' = %S (required)" key value))
    (error (message "Error: %s" (error-message-string err)))))

;;;###autoload
(defun bv-menu-register-feature (feature &optional deps)
  "Register FEATURE with optional dependencies DEPS."
  (interactive
   (condition-case err
       (let* ((feature-str (read-string "Feature name: "))
              (feature (if (string-blank-p feature-str)
                           (error "Feature name cannot be empty or blank")
                         (intern feature-str)))
              (deps-str (read-string
                         "Dependencies (space-separated, optional): "))
              (deps (when (not (string-empty-p deps-str))
                      (mapcar #'intern (split-string deps-str)))))
         (list feature deps))
     (error
      ;; If there's an error in the interactive form, signal it to be
      ;; caught by the caller
      (signal (car err) (cdr err)))))
  (condition-case err
      (progn
        (bv-register-feature feature deps)
        (message "Registered feature '%s'%s" feature
                 (if deps (format " with dependencies: %s" deps) "")))
    (error (message "Error: %s" (error-message-string err)))))

;;;###autoload
(defun bv-menu-check-feature (feature)
  "Check if FEATURE is enabled."
  (interactive
   (list (intern (completing-read "Feature: "
                                  (mapcar #'symbol-name bv-enabled-features)))))
  (if (bv-feature-enabled-p feature)
      (let ((deps (gethash feature bv-feature-dependencies)))
        (if deps
            (message "Feature '%s' is enabled (depends on: %s)"
                     feature (mapconcat #'symbol-name deps ", "))
          (message "Feature '%s' is enabled" feature)))
    (message "Feature '%s' is NOT enabled" feature)))

;;;###autoload
(defun bv-menu-expand-etc (file)
  "Expand FILE relative to etc directory.
Prompts for a file path relative to the configured etc directory.
The full path is copied to the clipboard."
  (interactive "sFile path: ")
  (if (bv-value-present-p 'etc-dir)
      (let ((expanded (bv-expand-etc-file file)))
        (when expanded
          (kill-new expanded)
          (message "Expanded to: %s (copied to clipboard)" expanded)))
    (message "etc-dir not configured")))

;;;###autoload
(defun bv-menu-expand-var (file)
  "Expand FILE relative to var directory.
Prompts for a file path relative to the configured var directory.
The full path is copied to the clipboard."
  (interactive "sFile path: ")
  (if (bv-value-present-p 'var-dir)
      (let ((expanded (bv-expand-var-file file)))
        (when expanded
          (kill-new expanded)
          (message "Expanded to: %s (copied to clipboard)" expanded)))
    (message "var-dir not configured")))

;;;###autoload
(defun bv-menu-ensure-directory (dir)
  "Ensure directory DIR exists.
Creates the directory and any parent directories if they don't exist.
Reports success or failure after the operation."
  (interactive "DDirectory: ")
  (bv-ensure-directory dir)
  (if (file-directory-p dir)
      (message "Directory exists: %s" dir)
    (message "Failed to create directory: %s" dir)))

;;;###autoload
(defun bv-menu-timer-help ()
  "Show timer management help."
  (interactive)
  (with-help-window "*bv-timer-help*"
    (with-current-buffer standard-output
      (insert "Timer Management\n")
      (insert "================\n\n")
      (insert "Timers are used for delayed initialization.\n\n")
      (insert "Commands:\n")
      (insert "  M-x list-timers          - Show all active timers\n")
      (insert "  M-x bv-cancel-all-timers - Cancel setup timers\n")
      (insert "  M-x cancel-timer         - Cancel a specific timer\n\n")
      (insert "Timer Functions:\n")
      (insert "  bv-idle-eval      - Returns cancelable timer object\n")
      (insert "  bv-with-delayed-setup - One-time setup with timer\n\n")
      (insert "Example:\n")
      (insert "  (setq my-timer (bv-idle-eval 5 (message \"Hello\")))\n")
      (insert "  (cancel-timer my-timer)\n\n")
      (insert "Press 'q' to close this help window.\n")
      (help-mode))))

;; Customization group for transient

(defgroup bv-transient nil
  "Transient menu interface."
  :group 'bv)

;; Enhanced transient with sections

;;;###autoload (autoload 'bv-menu-advanced "bv-transient" nil t)
;;;###autoload
(put 'bv-menu-advanced 'function-documentation
     "Advanced configuration menu with developer tools.")
;;;###autoload
(transient-define-prefix bv-menu-advanced ()
  "Advanced configuration menu with developer tools."
  :value '()
  ["Configuration - Advanced"
   ["Quick Actions"
    :class transient-row
    ("SPC" "Config Report" bv-report-config)
    ("?" "Path Report" bv-path-report)
    ("!" "Check Deps" bv-check-feature-dependencies)
    ("q" "Quit" transient-quit-one)]

   ["Configuration Values"
    :class transient-column
    ("vs" "Set Value..." bv-menu-set-value)
    ("vg" "Get Value..." bv-menu-get-value)
    ("vr" "Require Value..." bv-menu-require-value)
    ("vp" "Value Present?" bv-menu-check-value-presence)]

   ["Feature Management"
    :class transient-column
    ("fr" "Register Feature..." bv-menu-register-feature)
    ("fc" "Check Feature..." bv-menu-check-feature)
    ("fl" "List Features" bv-menu-list-features)]

   ["Path Operations"
    :class transient-column
    ("pe" "Expand etc/..." bv-menu-expand-etc)
    ("pv" "Expand var/..." bv-menu-expand-var)
    ("ps" "Expand system/..." bv-menu-expand-system)
    ("ph" "Expand home/..." bv-menu-expand-home)
    ("pd" "Ensure Dir..." bv-menu-ensure-directory)
    ("pc" "Open Cache Dir" bv-defaults-open-cache-dir)
    ("pr" "Reset Cache Dirs" bv-defaults-reset-cache-dirs)]

   ["Development"
    :class transient-column
    ("dr" "Reload Config" bv-menu-reload-config)
    ("dt" "Run Tests" bv-menu-run-tests)
    ("dc" "Byte Compile" bv-menu-byte-compile)]

   ["Timer Management"
    :class transient-column
    ("tc" "Cancel All Timers" bv-cancel-all-timers)
    ("tl" "List Timers" list-timers)
    ("th" "Timer Help" bv-menu-timer-help)]])

;; Additional helper commands

;;;###autoload
(defun bv-menu-check-value-presence (key)
  "Check if configuration KEY is present."
  (interactive
   (list (intern (read-string "Configuration key: "))))
  (if (bv-value-present-p key)
      (message "Key '%s' is present with value: %S" key (bv-get-value key))
    (message "Key '%s' is NOT present" key)))

;;;###autoload
(defun bv-menu-list-features ()
  "List all registered features.
Shows all enabled features in alphabetical order."
  (interactive)
  (if bv-enabled-features
      (message "Enabled features: %s"
               (mapconcat #'symbol-name
                          (bv-transient--sort-list
                           bv-enabled-features
                           (lambda (a b)
                             (string< (symbol-name a)
                                      (symbol-name b))))
                          ", "))
    (message "No features registered")))

;;;###autoload
(defun bv-menu-expand-system (type)
  "Expand FILE relative to system directories.
TYPE should be one of: lib, bin, or share.
Prompts for a relative path within the selected directory.
The full path is copied to the clipboard."
  (interactive
   (list (completing-read "Expand in: "
                          '("lib" "bin" "share"))))
  (let* ((dir-key (intern (concat "system-" type "-dir")))
         (expanded (bv-get-value dir-key)))
    (if expanded
        (let ((target (read-string (format "Path inside system %s/: " type))))
          (let ((full-path (file-name-concat expanded target)))
            (when full-path
              (kill-new full-path)
              (message "Expanded to: %s (copied to clipboard)" full-path))))
      (message "System %s directory not configured" type))))

;;;###autoload
(defun bv-menu-expand-home (type)
  "Expand FILE relative to home directories.
TYPE should be one of: lib, bin, or share.
Prompts for a relative path within the selected directory.
The full path is copied to the clipboard."
  (interactive
   (list (completing-read "Expand in: "
                          '("lib" "bin" "share"))))
  (let* ((dir-key (intern (concat "home-" type "-dir")))
         (expanded (bv-get-value dir-key)))
    (if expanded
        (let ((target (read-string (format "Path inside home %s/: " type))))
          (let ((full-path (file-name-concat expanded target)))
            (when full-path
              (kill-new full-path)
              (message "Expanded to: %s (copied to clipboard)" full-path))))
      (message "Home %s directory not configured" type))))

;;;###autoload
(defun bv-menu-reload-config ()
  "Reload configuration (developer tool).
Cancels all active timers and reloads bv-core.el.
Use with caution as this may disrupt running configurations."
  (interactive)
  (when (y-or-n-p "This may cause issues with running timers.  Reload core?")
    (bv-cancel-all-timers)
    (load-library "bv-core")
    (message "Reloaded bv-core.el")))

;;;###autoload
(defun bv-menu-run-tests ()
  "Run test suite.
Executes all ERT tests matching the pattern ^bv-test-.
Requires the ERT testing framework."
  (interactive)
  (if (require 'ert nil t)
      (ert "^bv-test-")
    (message "ERT not available")))

;;;###autoload
(defun bv-menu-byte-compile ()
  "Byte compile configuration files.
Compiles all elisp files matching ^bv-.*\\.el$ in the configured Lisp
directory.  Shows the compile log buffer if any warnings occur."
  (interactive)
  (let ((lisp-dir (bv-get-value 'lisp-dir)))
    (if lisp-dir
        (let ((files (directory-files lisp-dir t "^bv-.*\\.el$"))
              (compiled 0)
              (failed 0))
          (if files
              (progn
                (dolist (file files)
                  (when (and (not (string-match-p "\\.elc$" file))
                             (not (string-match-p "\\#" file)))
                    (condition-case err
                        (progn
                          (byte-compile-file file)
                          (setq compiled (1+ compiled)))
                      (file-error
                       (setq failed (1+ failed))
                       (message "Failed to compile %s: %s"
                                file (error-message-string err))))))
                (message "Compiled %d files%s" compiled
                         (if (> failed 0) (format " (%d failed)" failed) ""))
                ;; Show compile log if there are warnings or failures
                (when (or (> failed 0) (get-buffer "*Compile-Log*"))
                  (display-buffer "*Compile-Log*")))
            (message "No matching files found in %s" lisp-dir)))
      (message "lisp-dir not configured - cannot compile files"))))

;; Register with bv-core
(bv-register-feature 'bv-transient)

(provide 'bv-transient)

;; Example configuration:
;; (with-eval-after-load 'bv-transient
;;   (global-set-key (kbd "C-c b") #'bv-menu)
;;   (define-key bv-app-map "m" #'bv-menu)
;;   (define-key bv-app-map "M" #'bv-menu-advanced))

;;; bv-transient.el ends here
