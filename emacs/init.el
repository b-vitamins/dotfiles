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
     (message "Failed to install setup.el: %s" err)))
  (if install-success
      (progn
        (require 'setup)
        (require 'bv-setup)
        (message "setup.el loaded successfully"))
    (message "Proceeding without setup.el")))

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
   async-shell-command-buffer 'confirm-kill-process
   ;; Sets the display width of a tab character to 2 spaces, improving readability.
   tab-width 2
   )
  
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
   font-lock-maximum-decoration t)
  ;; Sets UTF-8 as the default coding system for file I/O, supporting a wide range of characters globally.
  (set-default-coding-systems 'utf-8)
  ;; Configures the Emacs environment to use UTF-8, enhancing support for international text standards.
  (set-language-environment "UTF-8")
  ;; Disables the use of tabs for indentation, using spaces instead for consistent formatting across different editors.
  (indent-tabs-mode 1)
  (message "default-preferences setup successfully"))

(if (and (member "Iosevka Comfy" (font-family-list))
         (member "DejaVu Sans" (font-family-list)))
    (progn
      (set-face-attribute 'default nil
                          :font "Iosevka Comfy"
                          :weight 'regular
                          :height 110)
      (set-face-attribute 'fixed-pitch nil
                          :font "Iosevka Comfy"
                          :weight 'light
                          :height 110)
      (set-face-attribute 'variable-pitch nil
                          :font "DejaVu Sans"
                          :height 120))
  (message "Required fonts not available, falling back to defaults"))

(setup whoami
  (:set-default user-full-name "Ayan Das"
                user-mail-address "bvits@riseup.net")
  (message "whoami setup successfully"))

(setup bv-essentials
  (:global "C-c h" 'hidden-mode-line-mode)
  (message "bv-essentials setup successfully"))

(setup display-line-numbers
  (:hook-into prog-mode
              lisp-mode
              scheme-mode
              haskell-mode
              rust-mode
              rust-ts-mode)
  (message "display-line-numbers loaded successfully")
  (global-visual-line-mode t)
  (message "global-visual-line-mode enabled"))

(setup recentf
  (:require recentf)
  (:option* max-saved-items 500
            max-menu-items 25)
  (recentf-mode 1)
  (message "recentf setup successfully"))

(setup display-time-format
  (:option display-time-format "%d %b %H:%M:%S"
           display-time-24hr-format t
           display-time-interval 1
           display-time-day-and-date t)
  (display-time)
  (message "display-time-format setup successfully"))

(defvar bv-not-guix-p (if (bv-guix-p) nil t))

(setup (:straight-if doom-themes bv-not-guix-p)
  (load-theme 'doom-one t)
  (message "doom-themes setup successfully"))

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
   minor-modes nil
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
  (doom-modeline-mode 1)
  (message "doom-modeline setup successfully"))

(setup (:straight-if rainbow-delimiters bv-not-guix-p)
  (:hook-into prog-mode)
  (message "rainbow-delimiters setup successfully"))

(setup (:straight-if rainbow-mode bv-not-guix-p)
  (:hook-into web-mode
              typescript-mode
              js2-mode
              org-mode)
  (message "rainbow-mode setup successfully"))

(setup (:straight-if adaptive-wrap bv-not-guix-p)
  (:require adaptive-wrap)
  (adaptive-wrap-prefix-mode)
  (message "adaptive-wrap setup successfully"))

(setup (:straight-if smartparens bv-not-guix-p)
  (:hook-into prog-mode)
  (message "smartparens setup successfully"))


(setup (:straight-if nerd-icons bv-not-guix-p)
  (:require nerd-icons)
  (message "nerd-icons setup successfully"))

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
  (load custom-file t)
  (message "no-littering setup successfully"))

(setup (:and (not bv-not-guix-p) guix)
  (:load-after geiser-mode)
  (:require guix)
  (:option* guile-program "guile")
  (message "guix setup successfully"))

(setup (:straight-if (mjolnir-mode :type git :host github :repo "b-vitamins/mjolnir-mode") bv-not-guix-p)
  (mjolnir-mode)
  (:global "M-n" mjolnir-cycle-window-forward
           "M-p" mjolnir-cycle-window-backward
           "C-c u" mjolnir-toggle-fixed-window)
  (message "mjolnir-mode setup successfully"))

(setup (:local-or-package cycle-buffer)
  (:require cycle-buffer)
  (:global "M-N" cycle-buffer
           "M-P" cycle-buffer-backward)
  (message "cycle-buffer setup successfully"))

(setup windmove
  (:require windmove)
  (:option* wrap-around t)
  (:global "S-<down>" windmove-down
           "S-<up>" windmove-up
           "S-<right>" windmove-right
           "S-<left>" windmove-left)
  (message "windmove setup successfully"))

(setup (:straight-if (windsize :type git :flavor melpa :host github :repo "grammati/windsize") bv-not-guix-p)
  (:require windsize)
  (:option* cols 2
            rows 2)
  (:global "S-M-<left>" windsize-left
           "S-M-<right>" windsize-right
           "S-M-<up>" windsize-up
           "S-M-<down>" windsize-down)
  (message "windsize setup successfully"))

(setup (:straight-if (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") bv-not-guix-p)
  (:option aw-scope 'frame
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-minibuffer-flag t)
  (:global "M-o" ace-window)
  (ace-window-display-mode 1)
  (message "ace-window setup successfully"))

(setup (:straight-if olivetti bv-not-guix-p)
  (:option* body-width 130)
  (:require olivetti)
  (:global "C-c C-h" olivetti-mode)
  (message "olivetti setup successfully"))

(setup (:straight-if which-key bv-not-guix-p)
  (:require which-key)
  (:option* idle-delay 1.5
            side-window-location 'right
            popup-type 'side-window
            side-window-max-width 0.40
            max-description-length 75
            max-display-columns 1
            sort-order 'which-key-local-then-key-order
            use-C-h-commands t
            show-remaining-keys t)
  (which-key-mode)
  (message "which-key setup successfully"))

(setup org
  (:require org ox-latex)

  ;; Mode Enhancements
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (org-display-inline-images)

  ;; Hooks
  (:hook (lambda () (add-hook 'before-save-hook 'org-update-all-dblocks t t)))
  (add-hook 'org-babel-post-tangle-hook #'bv/zap-newline-at-eob)
  (add-hook 'org-export-before-processing-functions 'bv-auto-insert-bibliography)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook 'bv-org-buffer-default-face)

  ;; Startup and Display Options
  (:option* require-final-newline nil
            startup-folded 'overview
            startup-with-latex-preview nil
            startup-with-inline-images t
            startup-align-all-tables t
            startup-indented t)

  ;; Visuals and UI Enhancements
  (:option* hide-leading-stars t
            hide-block-startup nil
            hide-emphasis-markers nil
            pretty-entities nil
            fontify-quote-and-verse-blocks t)

  ;; Source Code Blocks and Babel
  (:option* src-fontify-natively t
            src-tab-acts-natively t
            src-preserve-indentation nil
            edit-src-content-indentation 2
            confirm-babel-evaluate nil
            src-window-setup 'split-window-below)

  ;; Babel Configurations for Source Code Execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (shell . t) (haskell . t) (latex . t) (lisp . t)
     (scheme . t) (julia . t) (gnuplot . t) (lua . t) (ruby . t)
     (python . t) (emacs-lisp . t) (dot . t) (maxima . t) (org . t)))

  ;; Agenda and Task Management
  (:option* agenda-files '("~/slipbox/main.org")
            agenda-skip-deadline-prewarning-if-scheduled nil
            agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline
            agenda-columns-add-appointments-to-effort-sum t
            agenda-include-diary t
            agenda-todo-list-sublevels nil
            agenda-start-with-clockreport-mode t
            deadline-warning-days 1
            clock-total-time-cell-format "*%s*")

  ;; Logging and Archiving
  (:option* log-done 'time
            export-kill-after-export t)

  ;; LaTeX Configuration
  (:option* latex-default-class "article"
            latex-compiler "lualatex"
            latex-pdf-process '("lualatex -shell-escape -interaction nonstopmode %f"
                                "biber %b"
                                "lualatex -shell-escape -interaction nonstopmode %f"
                                "lualatex -shell-escape -interaction nonstopmode %f")
            latex-create-formula-image-program 'imagemagick
            format-latex-options (plist-put org-format-latex-options :scale 1.4)
            preview-latex-image-directory "~/slipbox/.latex-previews/"
            preview-latex-default-process 'imagemagick
            highlight-latex-and-related (quote (native latex script entities)))

  ;; Habit Tracking
  (:option* modules '(org-habit)
            habit-preceding-days 30
            habit-graph-column 40)

  ;; LaTeX and Image Export Settings
  (:push-to org-preview-latex-process-alist
            (:elements
             (imagemagick
              :programs ("lualatex" "convert")
              :description "pdf > png"
              :message "you need to install lualatex and imagemagick."
              :use-xcolor t
              :image-input-type "pdf"
              :image-output-type "png"
              :image-size-adjust (1.0 . 1.0)
              :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
              :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  ;; Keybindings
  (:unbind "C-c C-o")
  (:global "C-c l" org-store-link
           "C-c a" org-agenda
           "C-c c" org-capture)
  (message "org-mode setup successfully"))

(setup org-faces
  (:load-after org)
  (:also-load org-indent)
  (:when-loaded
    (bv-setup-org-fonts)))

(setup (:straight-if vertico bv-not-guix-p)
  (:require vertico)
  (:option* count 25
            cycle t
            resize t
            grid-lookahead 200)
  (vertico-mode)
  (message "vertico setup successfully"))

(setup (:straight-if orderless bv-not-guix-p)
  (:require orderless)
  (:option
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion))
                                   (command (styles))
                                   (variable (styles))
                                   (symbol (styles)))
   orderless-component-separator 'orderless-escapable-split-on-space
   orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp))
  (message "orderless setup successfully"))

(setup (:straight-if marginalia bv-not-guix-p)
  (:option* annotators '(marginalia-annotators-heavy
                         marginalia-annotators-light
                         nil)
            max-relative-age 0
            align 'left)
  (marginalia-mode)
  (message "marginalia setup successfully"))

(provide 'init)
;;; init.el ends here
