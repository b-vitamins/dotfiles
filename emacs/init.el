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

;; Helper function to log formatted messages during initialization.
(defun log-init-message (format-string &rest args)
  "Log initialization messages in a consistent format.
FORMAT-STRING is the message to display, with optional ARGS for formatting."
  (apply 'message (concat "[Init] " format-string) args))

;; Dynamically set the `user-emacs-directory` to the directory of this init file,
;; whether loaded from a file or interactively. We prioritize `load-file-name` (used
;; when loading init.el) and fallback to `buffer-file-name` in other cases.

(let ((config-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  ;; Ensure the extracted directory is valid before updating `user-emacs-directory`.
  (when (and config-dir (file-directory-p config-dir))
    (setq user-emacs-directory config-dir)
    ;; Log the directory in the *Messages* buffer for debugging purposes.
    (log-init-message "user-emacs-directory set to: %s" user-emacs-directory)))

;; Add the "lisp/" directory within `user-emacs-directory` to `load-path`.
;; This allows Emacs to find and load any custom Lisp files stored in this directory.
;; If the directory exists, all .el files within it will be logged in the *Messages* buffer.

(when user-emacs-directory
  (let ((elisp-dir (expand-file-name "lisp/" user-emacs-directory)))  ;; Define path to the lisp/ directory

    ;; Check if the lisp/ directory exists before adding it to `load-path`.
    (when (file-directory-p elisp-dir)
      (add-to-list 'load-path elisp-dir)
      (log-init-message "lisp/ directory added to load-path: %s" elisp-dir)

      ;; List and log all .el files in the lisp/ directory to the *Messages* buffer.
      (let ((elisp-files (directory-files elisp-dir t "\\.el$")))
        (if elisp-files
            (progn
              (log-init-message "Found the following .el files in lisp/:")
              (dolist (file elisp-files)
                (log-init-message "   - %s" (file-name-nondirectory file))))
          (log-init-message "No .el files found in lisp/ directory"))))))

;; Ensure the Emacs server is loaded and running.
;; If the server is not already running, start it and log the action.
(require 'server)

(if (server-running-p)
    (log-init-message "Emacs server is already running.")
  (server-start)
  (log-init-message "Emacs server started."))

;; Automatically switch to *scratch* buffer if emacsclient is invoked without a file.
;; This ensures that `emacsclient` opens the *scratch* buffer by default when no filename is provided.
(add-hook 'server-create-window-hook
          (lambda ()
            ;; Only switch to *scratch* if no other buffer is specified.
            (when (and (not (buffer-file-name))
                       (string= (buffer-name) "SERVER"))
              (switch-to-buffer "*scratch*")
              (log-init-message "Opened *scratch* buffer in new frame."))))

;; NOTE:
;; To ensure this configuration works as intended, especially for faster frame
;; opening with `emacsclient`, use the following command:
;;
;;    emacsclient -c -a emacs
;;
;; - `-c` opens a new frame.
;; - `-a emacs` starts a new Emacs instance if no server is running.
;;
;; Add this command to any shortcut you use to launch Emacs (e.g., GNOME, i3, or a custom keybinding).
;; This will allow you to open Emacs frames quickly with the *scratch* buffer, without reloading `init.el` every time.

;; Load essential configuration and define Guix environment check.
(require 'bv-essentials)

;; Define `bv-not-guix-p` as a variable that checks if the current system is not Guix.
(defvar bv-not-guix-p (not (bv-guix-p))
  "Non-nil if the system is not running Guix.  Used to conditionally manage packages.")

;; Bootstrap `straight.el` for non-Guix systems.
(when bv-not-guix-p
  (log-init-message "Bootstrapping straight.el for package management.")
  (bv-bootstrap-straight))

;; Attempt to load `setup.el`. Use Guix if available, otherwise fallback to `straight.el`.
(condition-case err
    (if bv-not-guix-p
        ;; On non-Guix systems, install and load `setup.el` using `straight.el`.
        (progn
          (when (fboundp 'straight-use-package)
            (straight-use-package 'setup)
            (log-init-message "Installed setup.el using straight.el."))
          (require 'setup)
          (log-init-message "Successfully loaded setup.el using straight.el."))
      ;; On Guix systems, load `setup.el` directly from the Guix package manager.
      (require 'setup)
      (log-init-message "Loaded setup.el from Guix package manager."))
  
  ;; Handle any errors during installation or loading.
  (error
   (log-init-message "Error loading setup.el: %s" err)
   (log-init-message "Proceeding without setup.el.")))

;; Load `bv-setup` if `setup.el` was successfully loaded.
(condition-case err
    (progn
      (require 'bv-setup)
      (log-init-message "Successfully loaded bv-setup."))
  (error
   (log-init-message "Error loading bv-setup: %s" err)
   (log-init-message "Proceeding without bv-setup.")))

(defvar cached-font-family-list (font-family-list)
  "Cache the list of available font families at startup.")

(require 'bv-defaults)

;; Check the availability of the required fonts.
(let ((iosevka-comfy-available (member "Iosevka Comfy" cached-font-family-list))
      (dejavu-sans-available (member "DejaVu Sans" cached-font-family-list)))
  (cond
   ((and iosevka-comfy-available dejavu-sans-available)
    (set-face-attribute 'default nil
                        :font "Iosevka Comfy"
                        :weight 'regular
                        :height 110)          ;; Set the default face to "Iosevka Comfy".
    (set-face-attribute 'fixed-pitch nil
                        :font "Iosevka Comfy"
                        :weight 'light
                        :height 110)          ;; Set the fixed-pitch face to "Iosevka Comfy".
    (set-face-attribute 'variable-pitch nil
                        :font "DejaVu Sans"
                        :height 120)          ;; Set the variable-pitch face to "DejaVu Sans".
    (log-init-message "Fonts successfully set: Iosevka Comfy (default, fixed-pitch) and DejaVu Sans (variable-pitch)."))
   ;; If only "Iosevka Comfy" is missing.
   ((not iosevka-comfy-available)
    (log-init-message "Font 'Iosevka Comfy' not available. Falling back to default font for fixed-pitch and default face."))
   ;; If only "DejaVu Sans" is missing.
   ((not dejavu-sans-available)
    (log-init-message "Font 'DejaVu Sans' not available. Falling back to default font for variable-pitch face.")))
  ;; If neither font is available.
  (unless (or iosevka-comfy-available dejavu-sans-available)
    (log-init-message "Neither 'Iosevka Comfy' nor 'DejaVu Sans' are available. Falling back to all default fonts.")))

(setup whoami
  (:set-default user-full-name "Ayan Das"
                user-mail-address "bvits@riseup.net"
                copyright-names-regexp
                (format "%s <%s>" user-full-name user-mail-address))
  (:require guix-copyright)
  (log-init-message (format "User: %s, Email: %s" user-full-name user-mail-address)))

;; Setup keybindings for bv-essentials
(setup bv-essentials
  (log-init-message "Setting up bv-essentials keybindings:")

  ;; ---- Delimiter Insertion Keybindings ----
  (:global
   "M-(" 'bv-insert-open-paren                         ;; Insert open parenthesis.
   "M-)" 'bv-insert-close-paren                        ;; Insert close parenthesis.
   "M-{" 'bv-insert-open-brace                         ;; Insert open brace.
   "M-}" 'bv-insert-close-brace                        ;; Insert close brace.
   "M-[" 'bv-insert-open-bracket                       ;; Insert open bracket.
   "M-]" 'bv-insert-close-bracket                      ;; Insert close bracket.
   "M-'" 'bv-insert-single-quote                       ;; Insert single quote.
   "M-\"" 'bv-insert-double-quote                      ;; Insert double quote.
   "M-`" 'bv-insert-backtick)                          ;; Insert backtick.

  (log-init-message "    Delimiter insertion keybindings set.")

  ;; ---- Code Evaluation and Search Keybindings ----
  (:global
   "C-c C-e C-b" 'eval-buffer                          ;; Evaluate the entire buffer.
   "C-c C-e C-r" 'eval-region                          ;; Evaluate the selected region.
   "C-c C-g" 'grep)                                    ;; Search using grep.

  (log-init-message "    Code evaluation and search keybindings set.")

  ;; ---- Text Manipulation Keybindings ----
  (:global
   "C-c r" 'replace-string                             ;; Replace string throughout the buffer.
   "C-c q" 'query-replace)                             ;; Query and replace throughout the buffer.

  (log-init-message "    Text manipulation keybindings set.")

  ;; ---- Visual Toggles and Utility Keybindings ----
  (:global
   "C-c C-t" 'toggle-truncate-lines                    ;; Toggle line truncation.
   "C-c C-w" 'whitespace-mode                          ;; Toggle whitespace mode.
   "C-c C-l" 'toggle-line-numbers)                     ;; Toggle line numbers.

  (log-init-message "    Visual toggles and utility keybindings set.")

  ;; ---- General Utility Keybindings ----
  (:global
   "C-c C-d" 'bv-move-to-trash                         ;; Move file to trash.
   "C-c C-h" 'hidden-mode-line-mode)                   ;; Toggle hidden mode line.

  (log-init-message "    General utility keybindings set.")
  (log-init-message "Successfully completed setting up bv-essentials keybindings."))

;; Setup keybindings for bv-file-navigation.
(setup bv-file-navigation
  (log-init-message "Setting up bv-file-navigation keybindings:")
  (:require bv-file-navigation)

  ;; ---- File and Buffer Management Keybindings ----
  (:global
   "C-c C-f l j" 'bv-open-file-left-jump    ;; Open file in the left window and jump.
   "C-c C-f l s" 'bv-open-file-left-stay    ;; Open file in the left window and stay.
   "C-c C-f r j" 'bv-open-file-right-jump   ;; Open file in the right window and jump.
   "C-c C-f r s" 'bv-open-file-right-stay)  ;; Open file in the right window and stay.

  (log-init-message "    File and buffer management keybindings set.")

  ;; ---- Frequently Visited Files Keybinding ----
  (:global
   "<f1>" 'org-agenda              ;; Open Org Agenda.
   "<f2>" 'bv-open-my-main-org              ;; Open main Org file.
   "<f9>" 'bv-open-my-init-el               ;; Open Emacs init.el file.
   "<f8>" 'bv-open-my-config-scm            ;; Open Guix config.scm file.
   "<f7>" 'bv-open-my-home-config-scm       ;; Open Guix home-config.scm file.
   "<f6>" 'bv-open-my-bib                   ;; Open bibliography file.
   "<f5>" 'bv-open-my-snippets-org)         ;; Open snippets Org file.

  (log-init-message "    Frequently visited files keybindings set (starting from <f9>).")
  (log-init-message "Successfully completed setting up bv-file-navigation keybindings."))

;; Setup line numbers for programming-related modes.
(setup display-line-numbers
  (:hook-into prog-mode
              lisp-mode
              scheme-mode
              haskell-mode)
  (log-init-message "Successfully set up display-line-numbers for programming modes."))

;; Enable visual line mode globally.
(setup global-visual-line-mode
  (global-visual-line-mode 1)
  (log-init-message "Successfully enabled global-visual-line-mode."))

;; Setup display time format in the mode-line.
(setup display-time-format
  (:option display-time-format "%d %b %a %H:%M"
           display-time-24hr-format t
           display-time-interval 1
           display-time-day-and-date t)
  (display-time)
  (log-init-message "Successfully set up display-time-format."))

;; Setup recentf - Keep track of recently opened files.
(setup recentf
  (:require recentf)
  (:option* max-saved-items 500      ;; Increase max number of saved items.
            max-menu-items 250)      ;; Increase max number of items in the menu.
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (log-init-message "Successfully set up recentf for tracking recent files."))

;; Setup savehist for saving minibuffer history.
(setup (:straight-if savehist bv-not-guix-p)
  (:set auto-save-default nil              ;; Disable auto-save.
        history-length 1000)               ;; Set the history length to 1000 items.
  (:option* autosave-interval 60                 ;; Autosave every 60 seconds.
            additional-variables '(search-ring   ;; Additional variables to save.
                                             regexp-search-ring))
  (savehist-mode 1)
  (log-init-message "Successfully set up savehist for saving minibuffer history."))

;; Setup doom-themes if on non-Guix systems.
(setup (:straight-if doom-themes bv-not-guix-p)
  ;; Disable doom-themes. Modus themes are in use.
  (:quit)
  (load-theme 'doom-one-light t)
  ;; Store the default mode line colors for custom usage.
  (bv-store-default-mode-line-colors)
  (log-init-message "Successfully set up doom-themes with Doom One Light."))

;; Setup modus-themes for automatic switching.
(setup modus-themes
  (:require bv-essentials)
  (:with-mode emacs-startup
    (:hook bv-auto-switch-modus-themes))
  (log-init-message "Successfully set up modus-themes with auto-switching."))

;; Setup doom-modeline if on non-Guix systems.
(setup (:straight-if doom-modeline bv-not-guix-p)
  (:require doom-modeline)
  (:hook-into after-init-hook
              after-change-major-mode-hook)
  (:option*
   height 10                                   ;; Set the height of the modeline.
   bar-width 2                                 ;; Set the width of the mode-line bar.
   icon (display-graphic-p)                    ;; Show icons if in a graphical environment.
   major-mode-icon (display-graphic-p)         ;; Show major mode icon if in a graphical environment.
   major-mode-color-icon (display-graphic-p)   ;; Color the major mode icon.
   buffer-state-icon (display-graphic-p)       ;; Display buffer state icon if graphical.
   buffer-modification-icon (display-graphic-p) ;; Display buffer modification icon.
   buffer-name t                               ;; Show buffer name in the modeline.
   minor-modes nil                             ;; Hide minor modes in the modeline.
   time t                                      ;; Show time in the modeline.
   mu4e t                                      ;; Enable mu4e (email client) support in the modeline.
   buffer-encoding nil                         ;; Don't show buffer encoding in the modeline.
   buffer-file-name-style 'truncate-except-project ;; Truncate file paths, except for project paths.
   checker-simple-format nil                   ;; Don't simplify the checker format.
   number-limit 99                             ;; Limit numbers (e.g., line numbers) to two digits.
   vcs-max-length 12                           ;; Truncate version control branch names.
   env-enable-python t                         ;; Enable Python environment in the modeline.
   env-enable-perl t                           ;; Enable Perl environment in the modeline.
   env-enable-rust t                           ;; Enable Rust environment in the modeline.
   env-python-executable "python"              ;; Set Python executable.
   env-perl-executable "perl"                  ;; Set Perl executable.
   env-rust-executable "rustc")                ;; Set Rust executable.

  (doom-modeline-mode 1)
  (log-init-message "Successfully set up doom-modeline."))

;; Setup rainbow-delimiters for colorful nested parentheses in programming modes.
(setup (:straight-if rainbow-delimiters bv-not-guix-p)
  (:hook-into prog-mode)
  (log-init-message "Successfully set up rainbow-delimiters for prog-mode."))

;; Setup rainbow-mode for colorizing hex and RGB colors in code.
(setup (:straight-if rainbow-mode bv-not-guix-p)
  (:hook-into web-mode
              typescript-mode
              js2-mode
              org-mode)
  (log-init-message "Successfully set up rainbow-mode for color modes."))

;; Setup adaptive-wrap for wrapping lines with indentation in visual-line-mode.
(setup (:straight-if adaptive-wrap bv-not-guix-p)
  (:require adaptive-wrap)
  (:with-mode visual-line-mode
    (:hook adaptive-wrap-prefix-mode))
  (global-visual-line-mode t)
  (log-init-message "Successfully set up adaptive-wrap for visual-line-mode."))

;; Setup smartparens for better handling of paired delimiters in code and org-mode.
(setup (:straight-if smartparens bv-not-guix-p)
  (:hook-into prog-mode
              org-mode)
  (log-init-message "Successfully set up smartparens for prog-mode and org-mode."))

;; Setup all-the-icons for rich icons support in Emacs.
(setup (:straight-if all-the-icons bv-not-guix-p)
  (:require all-the-icons)
  (log-init-message "Successfully set up all-the-icons."))

;; Setup kind-icon for icon completion support in Corfu.
(setup (:straight-if kind-icon bv-not-guix-p)
  (:load-after corfu nerd-icons)
  (:require kind-icon)
  (:option* default-face 'corfu-default      ;; Set the default face for kind-icon.
            use-icons t                      ;; Enable the use of icons.
            blend-background nil)            ;; Disable background blending for icons.
  (log-init-message "Successfully set up kind-icon for Corfu."))

;; Setup no-littering for organizing configuration files.
(setup (:straight-if no-littering bv-not-guix-p)
  (:require no-littering)
  (:option* etc-directory (expand-file-name "etc/" user-emacs-directory)
            var-directory (expand-file-name "var/" user-emacs-directory))
  (:option auto-save-file-name-transforms
           `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))   ;; Set auto-save directory.
           backup-directory-alist
           `(("." . ,(no-littering-expand-var-file-name "backup/")))        ;; Set backup directory.
           url-history-file
           (no-littering-expand-var-file-name "url/history")               ;; Set URL history file.
           custom-file
           (no-littering-expand-etc-file-name "custom.el"))                ;; Set custom file.
  ;; Redirect the native compilation cache.
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))
  ;; Load the custom file if it exists.
  (load custom-file t)
  (log-init-message "Successfully set up no-littering for organizing configuration files."))

;; Setup `jit-spell` with `hunspell` for on-the-fly spell checking.
(setup (:straight-if jit-spell bv-not-guix-p)
  ;; Define possible dictionary paths.
  (let* ((guix-home-path "~/.guix-home/profile/share/hunspell/en_GB-ize.aff")
         (guix-profile-path "~/.guix-profile/share/hunspell/en_GB-ize.aff")
         (personal-dict "~/documents/.dictionary")  ;; Personal dictionary path
         ;; Check if the dictionary exists in either location.
         (dict-path (cond
                     ((file-exists-p guix-home-path) guix-home-path)
                     ((file-exists-p guix-profile-path) guix-profile-path)
                     (nil))))
    ;; If no dictionary is found, quit the setup block.
    (unless dict-path
      (log-init-message "Hunspell dictionary not found. Disabling `jit-spell`.")
      (:quit))
    ;; Create personal dictionary if it doesn't exist and set permissions.
    (unless (file-exists-p personal-dict)
      (with-temp-buffer (write-file personal-dict))
      (set-file-modes personal-dict #o600)  ;; Set permissions to 600 (rw-------)
      (log-init-message "Created personal dictionary at %s" personal-dict))
    ;; Set `hunspell` as the spell checker program.
    (:set ispell-program-name "hunspell"
          ;; Use the found dictionary path.
          ispell-hunspell-dict-paths-alist `(("en_GB-ize" ,dict-path))
          ;; Directly point to the personal dictionary file.
          ispell-personal-dictionary personal-dict
          ;; Disable the alternate dictionary to avoid lookup errors.
          ispell-alternate-dictionary nil)
    ;; Load `jit-spell` for text and programming modes.
    (:require jit-spell)
    (:with-mode text-mode
      (:hook jit-spell-mode))
    (:with-mode prog-mode
      (:hook jit-spell-mode))
    ;; Bind keys for spell correction and adding words to the personal dictionary.
    (:bind "C-c s" jit-spell-correct-word
           "C-c d" bv-add-word-at-point-to-personal-dictionary)
    (log-init-message (format "Successfully set up `jit-spell` with `hunspell` using dictionary from %s" dict-path))))

;; Setup geiser for Scheme development, with Guile as the default implementation.
(setup (:straight-if geiser bv-not-guix-p)
  (:option geiser-default-implementation 'guile
           geiser-active-implementations '(guile))
  (:push-to geiser-implementations-alist
            (:elements (((regexp "\\.scm$") guile))))
  (:push-to geiser-implementations-alist
            (:elements (((regexp "\\.gscm$") guile))))
  (:require geiser)
  (log-init-message "Successfully set up Geiser with Guile as the default implementation."))

;; Setup exec-path-from-shell to synchronize environment variables from the shell.
(setup (:straight-if exec-path-from-shell bv-not-guix-p)
  (:require exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (log-init-message "Successfully set up exec-path-from-shell to sync environment variables."))

(setup (:straight-if geiser-guile bv-not-guix-p)
	(:load-after exec-path-from-shell)
	(:push-to exec-path-from-shell-variables
						(:elements "GUILE_LOAD_PATH"))
	(exec-path-from-shell-initialize)
	(:option geiser-guile-load-init-file t
					 geiser-guile-load-path (split-string (getenv "GUILE_LOAD_PATH") path-separator)
					 geiser-repl-add-project-paths t)
  (:require geiser-guile)
  (log-init-message "Successfully setup geiser-guile"))

;; Setup guix on Guix systems.
(setup (:and (not bv-not-guix-p) guix)
  (:load-after geiser-mode)
  (:require guix)
  (:option* program "guile")
  (:with-hook scheme-mode-hook
		(:hook guix-devel-mode))
  (log-init-message "Successfully set up Guix on a Guix system."))

;; Setup vterm for terminal emulation within Emacs.
(setup (:straight-if vterm bv-not-guix-p)
  (:require vterm)
  (log-init-message "Successfully set up vterm for terminal emulation."))

;; Setup `mjolnir-mode` for window cycling and management.
(setup (:straight-if (mjolnir-mode :type git :host github :repo "b-vitamins/mjolnir-mode") bv-not-guix-p)
  (mjolnir-mode)
  (:global "M-n" mjolnir-cycle-window-forward           ;; Cycle windows forward.
           "M-p" mjolnir-cycle-window-backward          ;; Cycle windows backward.
           "C-c u" mjolnir-toggle-fixed-window)         ;; Toggle fixed window status.
  (log-init-message "Successfully set up `mjolnir-mode` for window cycling."))

;; Setup `cycle-buffer` for efficient buffer cycling.
(setup (:local-or-package cycle-buffer)
  (:require cycle-buffer)
  (:global "M-N" cycle-buffer                           ;; Cycle buffers forward.
           "M-P" cycle-buffer-backward)                 ;; Cycle buffers backward.
  (log-init-message "Successfully set up `cycle-buffer` for buffer cycling."))

;; Setup `windmove` for easy window navigation.
(setup windmove
  (:require windmove)
  (:option* wrap-around t)                              ;; Allow wrap-around window movement.
  (:global "S-<down>" windmove-down                     ;; Move to the window below.
           "S-<up>" windmove-up                         ;; Move to the window above.
           "S-<right>" windmove-right                   ;; Move to the window on the right.
           "S-<left>" windmove-left)                    ;; Move to the window on the left.
  (log-init-message "Successfully set up `windmove` for directional window navigation."))

;; Setup `windsize` for window resizing.
(setup (:straight-if (windsize :type git :flavor melpa :host github :repo "grammati/windsize") bv-not-guix-p)
  (:require windsize)
  (:option* cols 2                                    ;; Adjust window by 2 columns.
            rows 2)                                   ;; Adjust window by 2 rows.
  (:global "M-<left>" windsize-left                   ;; Shrink window to the left.
           "M-<right>" windsize-right                 ;; Expand window to the right.
           "M-<up>" windsize-up                       ;; Shrink window upward.
           "M-<down>" windsize-down)                  ;; Expand window downward.
  (log-init-message "Successfully set up `windsize` for window resizing."))

;; Setup `ace-window` for fast window switching.
(setup (:straight-if (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") bv-not-guix-p)
  (:option aw-scope 'frame                              ;; Limit window switching to current frame.
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)        ;; Use home row keys for window selection.
           aw-minibuffer-flag t)                        ;; Include minibuffer in window switching.
  (:global "M-o" ace-window)                            ;; Activate `ace-window` with `M-o`.
  (ace-window-display-mode 1)
  (log-init-message "Successfully set up `ace-window` for fast window switching."))

(setup
		;; Makes switch-to-buffer commands respect display actions for a more intuitive window management.
		(:set switch-to-buffer-obey-display-actions t)
		(:push-to display-buffer-alist
							(:elements
							 ((or (major-mode . Info-mode)
										(major-mode . help-mode))
								(display-buffer-in-side-window)
								(reusable-frames . visible)
								(side . right)
								(slot . 0)
								(window-width . 0.4)
								(window-parameters . ((display-buffer-reuse-window . t))))
							 ((or (major-mode . org-agenda-mode)
										(major-mode . org-capture-mode)
										(major-mode . org-roam-mode))
								(display-buffer-in-side-window)
								(side . right)
								(slot . 1)
								(window-width . 0.4)
								(window-parameters . ((no-delete-other-windows . t))))
							 ("\\*vterm\\*" display-buffer-reuse-mode-window
								;; change to `t' to not reuse same window
								(inhibit-same-window . nil)
								(mode vterm-mode vterm-copy-mode))
							 (,(rx (| "*xref*"
												"*grep*"
												"*Occur*"))
								display-buffer-reuse-window
								(inhibit-same-window . nil))
							 ((derived-mode . magit-mode)
								(display-buffer-reuse-mode-window
								 display-buffer-in-direction)
								(mode magit-mode)
								(window . root)
								(window-width . 0.33)
								(direction . left))
							 (compiltation-mode
								(display-buffer-no-window)
								(allow-no-window . t))
							 ("\\*e?shell\\*" display-buffer-in-direction
								(direction . bottom)
								(window . root)
								(window-height . 0.3))
							 (,(rx (| "*compilation*" "*grep*"))
								(display-buffer-in-side-window)
								(side . right)
								(slot . 2)
								(window-parameters . ((no-delete-other-windows . t)))
								(window-width . 80))
							 ("^test[-_]"
								display-buffer-in-direction
								(direction . right))
							 )))

;; Setup `olivetti` for distraction-free writing.
(setup (:straight-if olivetti bv-not-guix-p)             ;; Install `olivetti` package conditionally if `bv-not-guix-p` is true.
  (:option* body-width 100)                              ;; Set the body width for focused writing.
  (:require olivetti)                                    ;; Require `olivetti` to use its features.
  (:global "C-c o" olivetti-mode)                        ;; Bind `C-c o` to toggle `olivetti-mode`.
  (log-init-message "Successfully set up `olivetti-mode' for a nice writing environment."))

;; Setup `which-key` for displaying available keybindings.
(setup (:straight-if which-key bv-not-guix-p)            ;; Install `which-key` package conditionally if `bv-not-guix-p` is true.
  (:require which-key)                                   ;; Require `which-key` to use its features.
  (:option* idle-delay 1.5                               ;; Set delay (in seconds) before `which-key` pops up.
            side-window-location 'right                  ;; Display `which-key` on the right side of the frame.
            popup-type 'side-window                      ;; Use side-window for displaying `which-key`.
            side-window-max-width 0.40                   ;; Set the maximum width for the side-window (40% of the frame).
            max-description-length 75                    ;; Limit the description length of displayed keybindings.
            max-display-columns 1                        ;; Limit the number of columns displayed.
            sort-order 'which-key-local-then-key-order   ;; Sort by local bindings first, then key order.
            use-C-h-commands t                           ;; Enable `C-h` help commands in `which-key`.
            show-remaining-keys t)                       ;; Show remaining keys that do not fit in the popup.
  (:with-hook after-init-hook                            ;; Enable `which-key-mode` after Emacs initialization.
    (:hook which-key-mode))
  (log-init-message "Successfully set up `which-key' for displaying available keybindings."))

(setup org
  (:require org ox-latex)
  (:require bv-org)

  ;; Mode Enhancements
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)

  ;; Hooks
	(:with-mode org-babel-post-tangle
		(:hook bv-zap-newline-at-eob))

  ;; Startup and Display Options
  (:option* require-final-newline nil
            startup-folded 'overview
            startup-with-latex-preview nil
            startup-with-inline-images t
            startup-align-all-tables t
            startup-indented t
						image-actual-width nil)

  ;; Visuals and UI Enhancements
  (:option* hide-leading-stars t
            hide-block-startup nil
            hide-emphasis-markers nil
            pretty-entities nil
            pretty-entities-include-sub-superscripts nil
            fontify-quote-and-verse-blocks t
            fontify-done-headline nil
            org-fontify-todo-headline nil
						return-follows-link t)

  ;; Source Code Blocks and Babel
  (:option* src-fontify-natively t
            src-tab-acts-natively t
            src-preserve-indentation nil
            edit-src-content-indentation 2
            confirm-babel-evaluate nil
            src-window-setup 'split-window-below
						babel-python-command "python3")

  ;; Babel Configurations for Source Code Execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (shell . t) (haskell . t) (latex . t) (lisp . t)
     (scheme . t) (julia . t) (gnuplot . t) (lua . t) (ruby . t)
     (python . t) (emacs-lisp . t) (dot . t) (maxima . t) (org . t)))

  ;; TODO Items
  (:option* todo-keywords
            '((sequence "TODO(t)" "|" "DONE(d)")
              (sequence "TOMEET(m)" "|" "CONCLUDED(c)" "CANCELED(x)")
              (sequence "TOREAD(r)" "|" "COMPLETED(p)")
              (sequence "TOWATCH(w)" "|" "FINISHED(f)")
              (sequence "TOSOLVE(s)" "|" "SOLVED(v)")
              (sequence "|" "ABANDONED(a)")))
  (:with-hook org-shiftright-hook
    (:hook bv-org-todo-cycle-right))
  (:with-hook org-shiftleft-hook
    (:hook bv-org-todo-cycle-left))
  (:option* todo-keyword-faces
            '(("TODO" . 'org-todo)
              ("DONE" . 'org-done)
              ("TOMEET" . 'org-agenda-date)
              ("CONCLUDED" . 'org-agenda-done)
              ("CANCELED" . 'org-agenda-dimmed-todo-face)
              ("TOREAD" . 'modus-themes-completion-match-0)
              ("COMPLETED" . 'org-done)
              ("TOWATCH" . 'modus-themes-completion-match-1)
              ("FINISHED" . 'org-done)
              ("TOSOLVE" . 'modus-themes-completion-match-2)
              ("SOLVED" . 'org-done)
              ("ABANDONED" . 'org-agenda-dimmed-todo-face)))

  ;; Agenda and Task Management
  (:require bv-file-navigation)
  (:option* agenda-files '("~/documents/main" "~/documents/slipbox/notes")
            agenda-skip-deadline-prewarning-if-scheduled nil
            agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline
            agenda-columns-add-appointments-to-effort-sum t
            agenda-include-diary t
            agenda-todo-list-sublevels nil
            agenda-start-with-clockreport-mode t
            agenda-clock-report-header "Clock Table"
            agenda-clockreport-parameter-plist '(:scope bv-clockable-org-files
                                                        :maxlevel 3
                                                        :block thisweek
                                                        :filetitle nil
                                                        :hidefiles t
                                                        :emphasize t
                                                        :stepskip0 t
                                                        :fileskip0 t
                                                        :level nil
                                                        :indent t
                                                        :narrow 80!
                                                        :tcolumns 3
                                                        :link t)
            deadline-warning-days 1
            clock-total-time-cell-format "*%s*"
            duration-format '((special . h:mm)))

  ;; Column View and Effort Estimates
  (:option* columns-default-format
            "%40ITEM(Task) %Effort(Estimate){:} %CLOCKSUM(Clocked)")
  (:option* global-properties
            '(("Effort_ALL" . "0 0:30 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 5:00 5:30 6:00 6:30 7:00 7:30 8:00 8:30 9:00 9:30 10:00 10:30 11:00 11:30 12:00 12:30 13:00 13:30 14:00 14:30 15:00 15:30 16:00 16:30 17:00 17:30 18:00 18:30 19:00 19:30 20:00 20:30 21:00 21:30 22:00 22:30 23:00 23:30 24:00")))

  ;; Logging and Archiving
  (:option* log-done 'time
            export-kill-after-export t)

  ;; LaTeX and Image Export Settings
  (:push-to org-preview-latex-process-alist
            (:elements
             (imagemagick
							:programs ("lualatex" "convert")
							:description "pdf > png"
							:message "you need to install lualatex and imagemagick."
							:image-input-type "pdf"
							:image-output-type "png"
							:image-size-adjust (1.0 . 1.0)
							:latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
							:image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  ;; LaTeX and Image Export Settings
  (:push-to org-preview-latex-process-alist
            (:elements
						 (dvipng :programs ("latex" "dvipng")
										 :description "dvi > png"
										 :message "you need to install the programs: latex and dvipng."
										 :image-input-type "dvi"
										 :image-output-type "png"
										 :image-size-adjust (1.0 . 1.0)
	 									 :latex-compiler ("lualatex --output-format=dvi -interaction nonstopmode -output-directory %o %f")
	 									 :image-converter ("dvipng -D %D -T tight -bg Transparent -o %O %f")
	 									 :transparent-image-converter ("dvipng -D %D -T tight -bg Transparent -o %O %f"))))

  ;; LaTeX Configuration
  (:option* latex-default-class "article"
            latex-compiler "lualatex"
            latex-pdf-process '("lualatex -shell-escape -interaction nonstopmode %f"
                                "biber %b"
                                "lualatex -shell-escape -interaction nonstopmode %f"
                                "lualatex -shell-escape -interaction nonstopmode %f")
            preview-latex-image-directory "~/pictures/.images/latex-previews/"
            latex-create-formula-image-program 'dvipng
            preview-latex-default-process 'dvipng
            highlight-latex-and-related (quote (native latex script entities)))

	;; Source: https://tecosaur.github.io/emacs-config/config.html#prettier-highlighting
	;;
	;; Using native highlighting for `highlight-latex-and-related' adds the org-block face, which
	;; may not look ideal, especially in previews. Instead of advising `org-src-font-lock-fontify-block', we can override the background color by adding another face with `:inherit default'.
	;;
	;; Inspecting `org-do-latex-and-related' reveals "latex" as the language argument passed.
	;; Therefore, we override the background color.
	(require 'org-src)
	(:push-to org-src-block-faces
						(:elements ("latex" (:inherit default :extend t))))

  ;; `bv-latex' holds a customized workflow that helps with PAIN (PDF is all I need),
  ;; among other things.
  ;;
  ;; I rarely deal with any latex machinery directly (Org mode FTW).
  ;; During these "Quick, need PDF to share with comrades..." rare events,
  ;; `bv-org-latex-compile' acts as an analgesic. As opposed to `org-latex-compile':
  ;;
  ;; 1. It compiles documents in a temporary directory (/tmp), retaining
  ;;    only the final PDF. Think "no-littering" for tex.
  ;;
  ;; 2. Given that I often create graphics via ad-hoc, disposable scripts located
  ;;    in non-standard directories, `bv-fix-graphics-paths' automatically
  ;;    canonicalizes all relative graphic paths. This ensures that compilations
  ;;    performed offsite are hassle-free, without the need to shuffle images
  ;;    around the filesystem.
  ;;
	(setup bv-latex
		(:require bv-latex)
		(:option bv-latex-output-dir "~/documents/out")
		(:alias org-latex-compile bv-org-latex-compile)
		(:global "C-c f" bv-fix-math-delimiters)
    (bv-set-org-format-latex-preview-scale))

  ;; Habit Tracking
  (:option* modules '(org-habit)
            habit-following-days 7
            habit-preceding-days 30
            habit-graph-column 40)

  ;; Keybindings
  (:unbind
	 "C-c C-o"
	 "M-{" ;; clashes with bv-insert-open-brace
	 )
  (:global "C-c l" org-store-link
           "C-c a" org-agenda
           "C-c c" org-capture
           "C-c i" org-id-get-create)
  (message "Successfully setup org-mode"))

(setup org-faces
  (:require bv-org)
	(:with-mode org-mode
		(:hook bv-setup-org-fonts))
	(message "Successfully setup org-faces"))

(setup (:straight-if org-fragtog bv-not-guix-p)
  (:load-after org)
	(:hook-into org-mode-hook)
  (message "Successfully setup org-fragtog"))

(setup (:straight-if vertico bv-not-guix-p)
  (:require vertico)
  (:option* count 25
            cycle t
            resize t
            grid-lookahead 200)

  (vertico-mode)
  (message "Successfully setup vertico"))

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
  (message "Successfully setup orderless"))

(setup (:straight-if marginalia bv-not-guix-p)
  (:option* annotators '(marginalia-annotators-heavy
                         marginalia-annotators-light
                         nil)
            max-relative-age 0
            align 'left)
  (marginalia-mode)
  (message "Successfully setup marginalia"))

(setup (:straight-if consult bv-not-guix-p)
  (:require consult)
  (:load-after doom-modeline)
  (:global [remap switch-to-buffer] #'consult-buffer
           [remap goto-line] #'consult-goto-line
           [remap imenu] #'consult-imenu
           [remap project-switch-to-buffer] #'consult-project-buffer
           [remap recentf-open-files] #'consult-recent-file
           "M-s g" (if (executable-find "rg")
                       #'consult-ripgrep
                     #'consult-grep)
           "M-s d" consult-find
           "C-s" consult-line
           "C-x C-r" consult-recent-file
           "M-s m" consult-mark
           "M-s o" consult-outline
           "M-s f" consult-flymake)

  (:option xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref
	         completion-in-region-function #'consult-completion-in-region)

  (:with-map minibuffer-local-map
    (:bind "C-r" consult-history))

  (:with-map global-map
    (:unbind "C-x C-<right>")
    (:unbind "C-x C-<left>"))
  (message "Successfully setup consult"))

(setup (:straight-if corfu bv-not-guix-p)
  (:load-after savehist-mode)
  (:require corfu)
  (:option tab-always-indent t
           completion-category-overrides '((file (styles . (partial-completion))))
           completion-cycle-threshold nil)
  (:option* auto t
            auto-prefix 2
            auto-delay 0.2
            max-width 150
						min-width 20
            count 16
            scroll-margin 3
            cycle nil
            quit-at-boundary nil
            separator ?\s
            quit-no-match 'separator
            preview-current 'insert
            preselect-first nil
            echo-documentation nil)
	(:with-mode eshell-mode
		(lambda ()
			(setq-local corfu-auto nil)
			(corfu-mode)))
  (:with-map corfu-map
    (:bind "C-n" corfu-next
           "C-p" corfu-previous
           "<return>" corfu-insert))
	(:with-mode global-corfu-mode
		(:hook corfu-history-mode corfu-indexed-mode corfu-popupinfo-mode))
	(global-corfu-mode)
  (message "Successfully setup corfu"))

(setup (:straight-if embark bv-not-guix-p)
  (:require embark)
  (:option prefix-help-command #'embark-prefix-help-command)
  (:push-to display-buffer-alist
            (:elements
             ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
              nil
              (window-parameters (mode-line-format . none)))))
  (:global "C-<return>" embark-act
           "C-c ;" embark-dwim
           "C-h B" embark-bindings)
  (message "Successfully setup embark"))

(setup (:straight-if embark-consult bv-not-guix-p)
  (:load-after consult)
  (:require embark-consult)
  (:with-feature embark-collect-mode
    (:hook consult-preview-at-point-mode))
  (message "Successfully setup embark"))

(setup oc
  (:require oc-biblatex)
  (:require oc-csl)
  (setq org-cite-global-bibliography bv-bib-path
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((latex biblatex)
                                     (t csl)))
  (:global "C-c ]" org-cite-insert))
  
(setup (:straight-if citar bv-not-guix-p)
  (:load-after org)
  (:load-after embark)

  (:require all-the-icons)
  (:require citar)

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "file-o"
              :face 'all-the-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon
              "link"
              :face 'all-the-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material
              "speaker_notes"
              :face 'all-the-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "circle-o"
              :face 'all-the-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  (:option citar-indicators
           (list citar-indicator-files-icons
                 citar-indicator-links-icons
                 citar-indicator-notes-icons))

  (:option* bibliography org-cite-global-bibliography
            library-paths bv-library-path
            notes-paths bv-notes-path
            file-extensions '("pdf" "org" "md")
            templates
            '((main . "${author editor:60}    ${date year issued:4}   ${title:150}")
              (suffix . "${=type=:12} ${tags keywords:*}")
              (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
              (note . "Notes on ${author editor}, ${title}"))
            at-point-function 'embark-act)

  (:with-mode org-mode
    (:hook citar-capf-setup))

  (:with-mode LaTeX-mode
    (:hook citar-capf-setup))

  (:global
   "C-C C-o C-i" citar-insert-citation
   "C-C C-o C-e" citar-insert-edit
   "C-C C-o C-f" citar-open
   "C-C C-o C-o" citar-open-files
   "C-C C-o C-n" citar-open-notes
   "C-C C-o C-b" citar-open-entry
   "C-C C-o C-d" citar-org-delete-citation
   "C-C C-o C-x" citar-export-local-bib-file)

  (:require citar-org)
  (citar-embark-mode)
  (message "Successfully setup citar"))

(setup (:straight-if org-roam bv-not-guix-p)
  (:load-after org)
  (:load-after consult)
  (:load-after marginalia)
  (:option
   v2-ack t
   org-roam-directory "~/documents/slipbox/notes"
   org-roam-database-connector 'sqlite-builtin
   org-roam-db-extra-links-elements '(keyword node-property)
   org-roam-mode-sections '((org-roam-backlinks-section :unique t)
														org-roam-reflinks-section)
   org-roam-link-title-format "R:%s"
   org-roam-tag-sources '(all-directories)
   org-roam-tag-sort t
   org-roam-tag-context-lines 5)
  (:require bv-org-roam)
	(:option
	 org-roam-capture-templates bv-org-roam-capture-templates
	 org-roam-node-display-template bv-org-roam-node-display-template
   org-roam-node-annotation-function bv-org-roam-node-annotation-function)

  (:with-hook after-init-hook
		(:hook org-roam-db-autosync-mode))
  (:global
   "C-c n f" org-roam-node-find
   "C-c n g" org-roam-graph
   "C-c n i" org-roam-node-insert
	 "C-c n d" org-roam-dailies-capture-today
   "C-c n c" org-roam-capture)
  (message "Successfully setup org-roam"))

(setup (:straight-if consult-org-roam bv-not-guix-p)
  (:load-after org-roam consult)
  (:require)
  (consult-org-roam-mode 1)
  (:option*
	 grep-func #'consult-ripgrep
	 buffer-narrow-key ?r
   buffer-after-buffers t)
  (:global
   "C-c n s" consult-org-roam-search
   "C-c n b" consult-org-roam-backlinks
   "C-c n B" consult-org-roam-backlinks-recursive
   "C-c n l" consult-org-roam-forward-links
   "C-c n r" consult-org-roam-search))

(setup (:straight-if websocket bv-not-guix-p)
  (:option* debug t
            websocket-callback-debug-on-error t))

(setup (:straight-if simple-httpd bv-not-guix-p))

(setup (:straight-if org-roam-ui bv-not-guix-p)
  (:load-after org-roam)
  (:option*
   sync-theme t
   follow t
   update-on-save t
   port 35901
   ws-socket 9998
   sync-theme nil
   open-on-start nil)
  (:require websocket)
  (:require f)
  (:require simple-httpd)
  (:require org-roam-ui)
  (message "Successfully setup org-roam-ui"))

(setup (:straight-if pdf-tools bv-not-guix-p)
  (:with-feature pdf-view-mode
    (:hook pdf-view-themed-minor-mode))
  (:option pdf-view-use-imagemagick t)
  (:require pdf-tools)
  (pdf-loader-install)
  (message "Successfully setup pdf-tools"))

;; Borrowed with modifications from David Wilson
;; https://github.com/daviwil
;;
(setup (:straight-if dired bv-not-guix-p)
  (:hook dired-hide-details-mode)
  (:option*
   listing-switches "-ltu --time=access --format=long --no-group --group-directories-first --almost-all --ignore=.DS_Store"
   summary-regexp (replace-regexp-in-string ":" "" "^.*: ")
   omit-files "^\\.[^.].*"
   omit-verbose nil
   hide-details-hide-symlink-targets nil
   find-file-other-window t
   delete-by-moving-to-trash t)
  (:require dired)
  (message "Successfully setup dired"))

(setup (:straight-if dired-hacks bv-not-guix-p)
  (:load-after dired))

;; Borrowed with modifications from David Wilson
;; https://github.com/daviwil
;;
(setup dired-rainbow
  (:load-after dired-hacks)
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
  (message "Successfully setup dired-rainbow"))

(setup (:straight-if oauth2 bv-not-guix-p)
  (message "Successfully setup oauth2"))

(setup (:straight-if pinentry bv-not-guix-p)
  (:require pinentry)
  (message "Successfully setup pinentry"))

(setup epa-file
  (:require epa-file)
  (message "Successfully setup epa-file"))

(setup (:straight-if auth-source bv-not-guix-p)
  (:require auth-source)
  (:push-to auth-sources
            (:elements "~/.password-store/.authinfo.gpg"))
  (message "Successfully setup auth-source"))

(setup (:straight-if password-store bv-not-guix-p)
  (:load-after auth-source-pass)
  (:option password-store-password-length 20)
  (:global
   "C-M-<return> p c" password-store-copy
   "C-M-<return> p i" password-store-insert
   "C-M-<return> p g" password-store-generate)
  (message "Successfully setup password-store"))

(setup (:straight-if yasnippet bv-not-guix-p)
  (:load-after org-mode)
  (:with-feature yas-minor-mode
    (:hook-into prog-mode-hook org-mode-hook))
  (:option yas-snippet-dirs
           (list (expand-file-name "snippets" user-emacs-directory)))
  (:require yasnippet)
	(:with-hook after-init-hook
		(:hook yas-reload-all))
  (message "Successfully setup yasnippets"))

(setup (:straight-if lsp-mode bv-not-guix-p)
  (:hook-into python-mode
              lisp-mode
              haskell-mode
              perl-mode
              lua-mode
              julia-mode
              c++-mode
              c-mode
              web-mode)
  (:global
   "C-M-<return> l d" xref-find-definitions
   "C-M-<return> l r" xref-find-references
   "C-M-<return> l n" lsp-ui-find-next-reference
   "C-M-<return> l p" lsp-ui-find-prev-reference
   "C-M-<return> l s" counsel-imenu
   "C-M-<return> l e" lsp-ui-flycheck-list
   "C-M-<return> l S" lsp-ui-sideline-mode
   "C-M-<return> l X" lsp-execute-code-action)
	(:option
   lsp-rust-analyzer-cargo-watch-command "clippy"
	 lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
   lsp-rust-analyzer-display-chaining-hints t
   lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
   lsp-rust-analyzer-display-closure-return-type-hints t
   lsp-rust-analyzer-display-parameter-hints nil
   lsp-rust-analyzer-display-reborrow-hints nil)
  (:require lsp-mode)
  (message "Successfully setup lsp-mode"))

(setup (:straight-if lsp-ui bv-not-guix-p)
  (:option* doc-position 'right
            flycheck-enable t
            sideline-enable t
            doc-enable t
            imenu-enable t
            peek-enable t
            peek-always-show t
            peek-show-directory t)
  (:require lsp-ui)
	(:with-hook lsp-mode-hook
		(:hook lsp-ui-mode))
  (message "Successfully setup lsp-ui"))

(setup (:straight-if flycheck bv-not-guix-p)
  (:load-after lsp-mode lsp-ui)
  (:hook-into lsp-mode)
  (:require flycheck)
  (:with-map flycheck-mode-map
    (:bind "C-;" bv-copy-flycheck-overlay-at-point-to-kill-ring))
  (global-flycheck-mode)
  (message "Successfully setup flycheck"))

(setup (:straight-if flycheck-inline bv-not-guix-p)
  (:load-after flycheck)
  (:hook-into flycheck-mode)
  (global-flycheck-inline-mode)
  (message "Successfully setup flycheck-inline"))

(setup (:straight-if flycheck-guile bv-not-guix-p)
  (:require flycheck-guile)
  (message "Successfully setup flycheck-guile"))

(setup (:straight-if flycheck-package bv-not-guix-p)
  (:load-after flycheck flycheck-inline)
  (:require flycheck-package)
  (:with-feature flycheck-mode-hook
    (:hook flycheck-package-setup))
  (message "Successfully setup flycheck-package"))

(setup (:straight-if flycheck-rust bv-not-guix-p)
  (:load-after flycheck flycheck-inline)
  (:require flycheck-rust)
  (:with-feature flycheck-mode-hook
    (:hook flycheck-rust-setup))
  (message "Successfully setup flycheck-rust"))

(setup (:straight-if flycheck-haskell bv-not-guix-p)
  (:load-after flycheck flycheck-inline)
  (:require flycheck-haskell)
  (:with-feature flycheck-mode-hook
    (:hook flycheck-haskell-setup))
  (message "Successfully setup flycheck-haskell"))

(setup python-mode
  (:set-default python-indent 4
            py-indent-offset 4)
  (:option py-python-command "python3"
           python-shell-interpreter "python3")
  (message "Successfully setup python-mode"))

(setup (:straight-if lsp-jedi bv-not-guix-p)
  (:load-after lsp-mode lsp-ui)
  (:require lsp-jedi)
  (message "Successfully setup lsp-jedi"))

(setup (:straight-if rust-mode bv-not-guix-p)
  (:require rust-mode)
  (message "Successfully setup rust-mode"))

(setup (:straight-if rustic bv-not-guix-p)
  (:require rustic)
  (message "Successfully setup rustic"))

(setup (:straight-if haskell-mode bv-not-guix-p)
  (:require haskell-mode)
  (message "Successfully setup haskell-mode"))

(setup (:straight-if gnuplot-mode bv-not-guix-p)
  (:require gnuplot)
  (message "Successfully setup gnuplot-mode"))

(setup (:straight-if lua-mode bv-not-guix-p)
  (:require lua-mode)
  (message "Successfully setup lua-mode"))

(setup (:straight-if json-mode bv-not-guix-p)
  (:require json-mode)
  (message "Successfully setup json-mode"))

(setup (:straight-if dockerfile-mode bv-not-guix-p)
  (:require dockerfile-mode)
  (message "Successfully setup dockerfile-mode"))

(setup (:straight-if yaml-mode bv-not-guix-p)
  (:require yaml-mode)
  (message "Successfully setup yaml-mode"))

(setup (:straight-if toml-mode bv-not-guix-p)
  (:require toml-mode)
  (message "Successfully setup toml-mode"))

(setup (:straight-if julia-mode bv-not-guix-p)
  (:require julia-mode)
  (message "Successfully setup julia-mode"))

(setup (:straight-if cmake-mode bv-not-guix-p)
  (:require cmake-mode)
  (message "Successfully setup cmake-mode"))

(setup (:straight-if god-mode bv-not-guix-p)
  (:option* exempt-predicates nil
            enable-function-key-translation nil)
  (:require god-mode)
  (:require god-mode-isearch)
  (:with-map isearch-mode-map
    (:bind "C-q" god-mode-isearch-activate))
  (:with-map god-mode-isearch-map
    (:bind "C-q" god-mode-isearch-disable))
  (:with-map god-local-mode-map
    (:bind "." repeat
           "[" backward-paragraph
           "]" forward-paragraph
           [remap self-insert-command] bv-god-mode-self-insert))
	(:with-mode post-command
		(:hook bv-god-mode-update-cursor-type
					 bv-god-mode-update-mode-line))
  (:global "C-q" god-mode
           "C-`" god-local-mode
           "C-x C-1" delete-other-windows
           "C-x C-2" split-window-below
           "C-x C-3" split-window-right
           "C-x C-0" delete-window)
  (message "Successfully setup god-mode"))

(setup (:straight-if nerd-icons bv-not-guix-p)
  (:require nerd-icons)
  (message "Successfully setup nerd-icons"))

(setup (:straight-if nerd-icons-dired bv-not-guix-p)
  (:load-after dired nerd-icons)
  (:require nerd-icons-dired)
  (:with-mode dired
    (:hook nerd-icons-dired-mode))
  (message "Successfully setup nerd-icons-dired"))

(setup (:straight-if nerd-icons-corfu bv-not-guix-p)
  (:load-after corfu nerd-icons)
  (:require corfu nerd-icons-corfu)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (message "Successfully setup nerd-icons-corfu"))

(setup (:straight-if nerd-icons-ibuffer bv-not-guix-p)
  (:load-after nerd-icons)
  (:option* ibuffer-icon t
            ibuffer-color-icon t
            ibuffer-icon-size 1.0
            ibuffer-human-readable-size t)
  (:require nerd-icons-ibuffer)
  (:with-mode ibuffer
    (:hook nerd-icons-ibuffer-mode))
  (message "Successfully setup nerd-icons-ibuffer"))

(setup (:straight-if nerd-icons-completion bv-not-guix-p)
  (:require nerd-icons-completion)
  (nerd-icons-completion-mode)
  (:with-mode marginalia
    (:hook nerd-icons-completion-marginalia-setup))
  (message "Successfully setup nerd-icons-completion"))

(setup treesit
	(:option*
	 extra-load-path (list (concat (getenv "GUIX_PROFILE") "/lib/tree-sitter"))))

(setup gptel
	(:require gptel)
	(:option gptel-model "gpt-4o-turbo")
	(:global "C-c g" gptel-send))

(setup arxiv-mode
  (:require arxiv-mode)
  (:option arxiv-default-category "cond-mat.dis-nn"
           arxiv-entries-per-fetch "25"
           arxiv-default-download-folder bv-library-path
           arxiv-default-bibliography bv-bib-path
           arxiv-pdf-open-function (lambda (fpath) (call-process "evince" nil 0 nil fpath))))

(provide 'init)
;;; init.el ends here
