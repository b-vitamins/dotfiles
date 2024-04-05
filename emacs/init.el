;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/init.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;              - Neal Stephenson, In the Beginning was the Command Line
;;
;; “The reasonable man adapts himself to the world: the unreasonable
;; one persists in trying to adapt the world to himself.
;; Therefore all progress depends on the unreasonable man.”
;;              - George Bernard Shaw, Man and Superman

;;; Code:

;; Dynamically set the user-emacs-directory to the directory of this init file,
;; ensuring it is a valid directory.
(let ((config-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (when (and config-dir (file-directory-p config-dir))
    (setq user-emacs-directory config-dir)))

;; Add the "lisp" directory within the user-emacs-directory to the load path.
;; This allows Emacs to find and load Lisp files located in this directory.
;;
(when user-emacs-directory
  (let* ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))))

;; Ensure the server package is loaded
;; Check if the Emacs server is already running and start it if not
;;
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'bv-essentials)
(bv-bootstrap-straight)

(let ((install-success t))
  (condition-case err
      (straight-use-package 'setup)
    (error
     (setq install-success nil)
     (message "Failed to install `setup.el`: %s" err)))
  (if install-success
      (progn
        (require 'setup)
        (require 'bv-setup)
        (message "`setup.el` loaded successfully."))
    (message "Proceeding without `setup.el`.")))

(setup whoami
  (:set-default user-full-name "Ayan Das"
                user-mail-address "ayand@iisc.ac.in"))

(setup bv-essentials
  (:global "C-c h" 'hidden-mode-line-mode))

(setup default-preferences
  ;; Basic preferences to improve user experience and workflow.
  (:set-default
   ;; Makes switch-to-buffer commands respect display actions for a more intuitive window management.
   switch-to-buffer-obey-display-actions t
   ;; Sets Org mode as the default major mode for new buffers, encouraging structured and organized note-taking.
   initial-major-mode 'org-mode
   ;; Disables the bell sound, replacing it with a silent ignore function to avoid auditory distraction.
   ring-bell-function 'ignore
   ;; Enables short answers (y/n) for prompts, streamlining user interactions.
   use-short-answers t
   ;; Adjusts the context lines for next-screen and previous-screen commands, improving readability during navigation.
   next-screen-context-lines 4
   ;; Sets a thin bar as the cursor type, providing a precise visual cue for the cursor's location.
   cursor-type 'bar
   ;; Applies the bar cursor type to non-selected windows as well, maintaining visual consistency across windows.
   cursor-in-non-selected-window 'bar
   ;; Allows C-k to kill the entire line, including the newline character, for more efficient editing.
   kill-whole-line t
   ;; Permits killing buffers that are read-only, facilitating quick closure of documentation or help buffers.
   kill-read-only-ok t
   ;; Reduces the delay before showing keystrokes in the echo area, enhancing feedback for partially typed commands.
   echo-keystrokes 0.1
   ;; Disables the creation of auto-save list files, simplifying the file system and reducing clutter.
   auto-save-list-file-prefix nil
   ;; Enables focus-follows-mouse, allowing window focus to change based on mouse position, aligning with certain user preferences or workflows.
   focus-follows-mouse t
   ;; Sets preferred positions for the recenter command, offering customization for viewing buffer content.
   recenter-positions '(top bottom middle)
   ;; Opens files in view mode when they are read-only, encouraging non-destructive file exploration.
   view-read-only t
   ;; Automatically follows symbolic links to version-controlled files, simplifying navigation in projects.
   vc-follow-symlinks t
   ;; Highlights matching portions during searches, improving visibility of search results.
   search-highlight t
   ;; Saves the clipboard contents before overwriting, preventing accidental loss of clipboard data.
   save-interprogram-paste-before-kill t
   ;; Enables Dired's "Do What I Mean" behavior for more intuitive file operations between splits.
   dired-dwim-target t
   ;; Configures tab behavior for consistent indentation, enhancing code readability and standardization.
   tab-always-indent 'complete
   ;; Allows evaluation of local variables if set, useful for project-specific settings.
   enable-local-eval t
   ;; Disables creation of backup files to minimize clutter.
   make-backup-files nil
   ;; Avoids creation of .#lockfile files, reducing file system clutter.
   create-lockfiles nil
   ;; Disables requirement for a final newline in files, accommodating varied file formats.
   require-final-newline nil
   ;; Prevents font cache compaction during garbage collection, potentially improving performance.
   inhibit-compacting-font-caches t
   ;; Adjusts the mode's requirement for a final newline, offering flexibility across different file types.
   mode-require-final-newline nil
   ;; Sets the minimum level of warnings to display, reducing noise from less critical warnings.
   warning-minimum-level :emergency
   ;; Specifies sources for authentication credentials, enhancing security by using encrypted storage.
   auth-sources '("~/.authinfo.gpg")
   ;; Controls native compilation warnings and errors display, minimizing distractions during compilation.
   native-comp-async-report-warnings-errors 'silent
   ;; Disables warnings for opening large files, streamlining access to large data or code bases.
   large-file-warning-threshold nil
   ;; Enables pruning of the native compilation cache to manage disk space usage efficiently.
   native-compile-prune-cache t
   ;; Configures behavior for async shell commands, preventing accidental process termination.
   async-shell-command-buffer 'confirm-kill-process)
  
  (:set ;; Disables the startup screen for a cleaner launch experience.
   inhibit-startup-screen t
   ;; Customizes the startup echo area message for a personalized touch or branding.
   inhibit-startup-echo-area-message "Welcome to Emacs!"
   ;; Enables fitting windows to the buffer horizontally, allowing for more flexible window sizing.
   fit-window-to-buffer-horizontally t
   ;; Allows windows to be resized to the exact pixel, offering finer control over window dimensions.
   window-resize-pixelwise t
   ;; Sets the syntax highlighting support mode to JIT Lock mode, enabling just-in-time syntax highlighting for improved performance.
   font-lock-support-mode 'jit-lock-mode
   ;; Enables maximum decoration for syntax highlighting, ensuring rich visual feedback in code.
   font-lock-maximum-decoration t))

(setup display-line-numbers
  (:hook-into prog-mode
              lisp-mode
              scheme-mode
              haskell-mode
              rust-mode
              rust-ts-mode)
  (global-visual-line-mode t))

(setup recentf
  (:require recentf)
  (:option* max-saved-items 500
            max-menu-items 25)
  (recentf-mode 1))

(setup display-time-format
  (:option display-time-format "%d %b %H:%M:%S"
           display-time-24hr-format t
           display-time-interval 1
           display-time-day-and-date t)
  (display-time))

(defvar bv-not-guix-p (if (bv-guix-p) nil t))

(setup (:straight-if doom-themes bv-not-guix-p)
  (load-theme 'doom-one t))

(setup (:straight-if doom-modeline bv-not-guix-p)
  (:require doom-modeline)
  (:hook-into after-init-hook
              after-change-major-mode-hook)
  (:option*
   height 10
   bar-width 2
   icon (display-graphic-p)
   major-mode-icon (display-graphic-p)
   major-mode-color-icon (display-graphic-p)
   buffer-state-icon (display-graphic-p)
   buffer-modification-icon (display-graphic-p)
   buffer-name t
   minor-modes t
   time t
   mu4e t
   buffer-encoding nil
   buffer-file-name-style 'truncate-except-project
   checker-simple-format nil
   number-limit 99
   vcs-max-length 12
   env-enable-python t
   env-enable-perl t
   env-enable-rust t
   env-python-executable "python"
   env-perl-executable "perl"
   env-rust-executable "rustc")
  (doom-modeline-mode 1))

(setup (:straight-if rainbow-delimiters bv-not-guix-p)
  (:hook-into prog-mode))

(setup (:straight-if rainbow-mode bv-not-guix-p)
  (:hook-into web-mode
              typescript-mode
              js2-mode
              org-mode))

(setup (:straight-if no-littering bv-not-guix-p)
  (:require no-littering)
  (:option*
   etc-directory
   (expand-file-name "etc/" user-emacs-directory)
   var-directory
   (expand-file-name "var/" user-emacs-directory))
  (:option auto-save-file-name-transforms
           `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
           backup-directory-alist
           `(("." . ,(no-littering-expand-var-file-name "backup/")))
           url-history-file
           (no-littering-expand-var-file-name "url/history")
           custom-file
           (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))
  (load custom-file t))

(provide 'init)
;;; init.el ends here
