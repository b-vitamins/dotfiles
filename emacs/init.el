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
  (let* ((elisp-dir (expand-file-name "lisp/" user-emacs-directory))
				 (guile-dir (expand-file-name "../guix/current/share/guile/site/3.0/" user-emacs-directory)))
    (when (file-directory-p elisp-dir)
      (add-to-list 'load-path elisp-dir))
    (when (file-directory-p guile-dir)
      (add-to-list 'load-path guile-dir))))

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
        (message "Successfully loaded setup.el"))
    (message "Proceeding without setup.el")))

(defvar cached-font-family-list (font-family-list)
  "Cache the list of available font families at startup.")

(setup default-preferences
  ;; Basic preferences to improve user experience and workflow.
  (:set-default
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
	 ;; Sets margins around text for all buffers, enhancing readability.
	 ;; Number of columns on the left margin.
   left-margin-width 2
	 ;; Number of columns on the right margin.
   right-margin-width 2
   )
  
  (:set ;; Disables the startup screen for a cleaner launch experience.
   inhibit-startup-screen t
   ;; Customizes the startup echo area message.
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
  (message "Successfully setup default preferences"))

(if (and (member "Iosevka Comfy" cached-font-family-list)
         (member "DejaVu Sans" cached-font-family-list))
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
  (message "Successfully setup whoami"))

(setup bv-essentials
  ;; Delimiter insertion keybindings
  (:global
   "M-(" 'bv-insert-open-paren
   "M-)" 'bv-insert-close-paren
   "M-{" 'bv-insert-open-brace
   "M-}" 'bv-insert-close-brace
   "M-[" 'bv-insert-open-bracket
   "M-]" 'bv-insert-close-bracket
   "M-'" 'bv-insert-single-quote
   "M-\"" 'bv-insert-double-quote
   "M-`" 'bv-insert-backtick
	 )
  ;; Code evaluation and search keybindings
  (:global
   "C-c C-e C-b" 'eval-buffer
   "C-c C-e C-r" 'eval-region
   "C-c C-g" 'grep
	 )
  ;; Text manipulation keybindings
  (:global
   "C-c r" 'replace-string
   "C-c q" 'query-replace
	 )
  ;; Visual toggles and utility keybindings
  (:global
   "C-c C-t" 'toggle-truncate-lines
   "C-c C-w" 'whitespace-mode
   "C-c C-l" 'toggle-line-numbers
	 )
  ;; General utility keybindings
  (:global
   "C-c C-d" 'bv-move-to-trash
   "C-c C-h" 'hidden-mode-line-mode)
  ;; Success message after setup
  (message "Successfully setup bv-essentials"))

(setup bv-file-navigation
  (:require bv-file-navigation)
  ;; File and buffer management keybindings
  (:global
	 "C-c C-f l j" 'bv-open-file-left-jump    ; Open left and jump
   "C-c C-f l s" 'bv-open-file-left-stay    ; Open left and stay
   "C-c C-f r j" 'bv-open-file-right-jump   ; Open right and jump
   "C-c C-f r s" 'bv-open-file-right-stay   ; Open right and stay
	 )
	;; Frequently visited files
  (:global
   "<f1>" 'bv-open-my-main-org
   "<f2>" 'bv-open-my-snippets-org
   "<f3>" 'bv-open-my-working-bib
   "<f4>" 'bv-open-my-cold-init-el
   "<f5>" 'bv-open-my-cold-config-scm
   "<f6>" 'bv-open-my-cold-zshrc
   "<f7>" 'bv-open-my-hot-zshrc
   "<f8>" 'bv-open-my-hot-config-scm
   "<f9>" 'bv-open-my-hot-init-el))

(setup display-line-numbers
  (:hook-into prog-mode
              lisp-mode
              scheme-mode
              haskell-mode
              rust-mode)
  (message "Successfully setup display-line-numbers")
  (message "Successfully enabled global-visual-line-mode"))

(setup display-time-format
  (:option display-time-format "%d %b %H:%M:%S"
           display-time-24hr-format t
           display-time-interval 1
           display-time-day-and-date t)
  (display-time)
  (message "Successfully setup display-time-format"))

(setup recentf
  (:require recentf)
  (:option* max-saved-items 500
            max-menu-items 250)
  (recentf-mode 1)
	(run-at-time nil (* 5 60) 'recentf-save-list)
  (message "Successfully setup recentf"))

(defvar bv-not-guix-p (if (bv-guix-p) nil t))

(setup (:straight-if savehist bv-not-guix-p)
  (:set auto-save-default nil
				history-length 1000)
	(:option* autosave-interval 60
						additional-variables '(search-ring regexp-search-ring))
  (savehist-mode 1)
  (message "Successfully setup savehist"))

(setup (:straight-if doom-themes bv-not-guix-p)
	(:quit)
  (load-theme 'doom-one-light t)
	(bv-store-default-mode-line-colors)
  (message "Successfully setup doom-themes"))

(setup modus-themes
	(:require bv-essentials)
	(:with-mode emacs-startup
		(:hook bv-auto-switch-modus-themes))
  (message "Successfully setup modus-themes"))

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
  (message "Successfully setup doom-modeline"))

(setup (:straight-if rainbow-delimiters bv-not-guix-p)
  (:hook-into prog-mode)
  (message "Successfully setup rainbow-delimiters"))

(setup (:straight-if rainbow-mode bv-not-guix-p)
  (:hook-into web-mode
              typescript-mode
              js2-mode
              org-mode)
  (message "Successfully setup rainbow-mode"))

(setup (:straight-if adaptive-wrap bv-not-guix-p)
  (:require adaptive-wrap)
	(:with-mode visual-line-mode
		(:hook adaptive-wrap-prefix-mode))
  (global-visual-line-mode t)
  (message "Successfully setup adaptive-wrap"))

(setup (:straight-if smartparens bv-not-guix-p)
  (:hook-into prog-mode
							org-mode)
  (message "Successfully setup smartparens"))

(setup (:straight-if all-the-icons bv-not-guix-p)
  (:require all-the-icons)
  (message "Successfully setup all-the-icons"))

(setup (:straight-if kind-icon bv-not-guix-p)
  (:load-after corfu nerd-icons)
  (:require kind-icon)
  (:option*
   default-face 'corfu-default
   use-icons t
   blend-background nil)
  (message "Successfully setup kind-icon"))

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
  (message "Successfully setup no-littering"))

(setup (:and (not bv-not-guix-p) guix)
  (:load-after geiser-mode)
  (:require guix)
  (:option* guile-program "guile")
  (message "Successfully setup guix"))

(setup (:straight-if vterm bv-not-guix-p)
  (:require vterm)
  (message "Successfully setup vterm"))

(setup (:straight-if exec-path-from-shell bv-not-guix-p)
  (:require exec-path-from-shell))

(setup (:straight-if jit-spell bv-not-guix-p)
	(:load-after exec-path-from-shell)
	(:push-to exec-path-from-shell-variables
						(:elements "MY_DICTIONARY" "DICTPATH"))
	(exec-path-from-shell-initialize)
	(:set ispell-program-name "hunspell"
				ispell-hunspell-dict-paths-alist (getenv "DICTPATH")
				ispell-personal-dictionary (getenv "MY_DICTIONARY")
				ispell-local-dictionary "en_GB-ize")
	(:require jit-spell)
	(:with-mode text-mode
		(:hook jit-spell-mode))
	(:with-mode prog-mode
		(:hook jit-spell-mode))
	(:bind "C-c s" jit-spell-correct-word
				 "C-c d" bv-add-word-at-point-to-personal-dictionary))

(setup (:straight-if (mjolnir-mode :type git :host github :repo "b-vitamins/mjolnir-mode") bv-not-guix-p)
  (mjolnir-mode)
  (:global "M-n" mjolnir-cycle-window-forward
           "M-p" mjolnir-cycle-window-backward
           "C-c u" mjolnir-toggle-fixed-window)
  (message "Successfully setup mjolnir-mode"))

(setup (:local-or-package cycle-buffer)
  (:require cycle-buffer)
  (:global "M-N" cycle-buffer
           "M-P" cycle-buffer-backward)
  (message "Successfully setup cycle-buffer"))

(setup windmove
  (:require windmove)
  (:option* wrap-around t)
  (:global "S-<down>" windmove-down
           "S-<up>" windmove-up
           "S-<right>" windmove-right
           "S-<left>" windmove-left)
  (message "Successfully setup windmove"))

(setup (:straight-if (windsize :type git :flavor melpa :host github :repo "grammati/windsize") bv-not-guix-p)
  (:require windsize)
  (:option* cols 2
            rows 2)
  (:global "S-M-<left>" windsize-left
           "S-M-<right>" windsize-right
           "S-M-<up>" windsize-up
           "S-M-<down>" windsize-down)
  (message "Successfully setup windsize"))

(setup (:straight-if (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") bv-not-guix-p)
  (:option aw-scope 'frame
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-minibuffer-flag t)
  (:global "M-o" ace-window)
  (ace-window-display-mode 1)
  (message "Successfully setup ace-window"))

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

(setup (:straight-if olivetti bv-not-guix-p)
  (:option* body-width 120)
  (:require olivetti)
  (:global "C-c o" olivetti-mode)
  (message "Successfully setup olivetti-mode"))

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
	(:with-hook after-init-hook
		(:hook which-key-mode))
  (message "Successfully setup which-key"))

(setup org
  (:require org ox-latex)

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

  ;; Agenda and Task Management
  (:option* agenda-files '("~/main.org" "~/slipbox/notes")
            agenda-skip-deadline-prewarning-if-scheduled nil
            agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline
            agenda-columns-add-appointments-to-effort-sum t
            agenda-include-diary t
            agenda-todo-list-sublevels nil
            agenda-start-with-clockreport-mode t
            agenda-clock-report-header "Clock Table"
            agenda-clockreport-parameter-plist '(:maxlevel 0 :filetitle t :compact t :stepskip0 t :fileskip0 t)
            deadline-warning-days 1
            clock-total-time-cell-format "*%s*")

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
							:image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
						 ))

  ;; LaTeX Configuration
  (:option* latex-default-class "article"
            latex-compiler "lualatex"
            latex-pdf-process '("lualatex -shell-escape -interaction nonstopmode %f"
                                "biber %b"
                                "lualatex -shell-escape -interaction nonstopmode %f"
                                "lualatex -shell-escape -interaction nonstopmode %f")
            latex-create-formula-image-program 'imagemagick
            format-latex-options (plist-put org-format-latex-options :scale 4.0)
            preview-latex-image-directory "~/.local/latex-previews/"
            preview-latex-default-process 'imagemagick
            highlight-latex-and-related (quote (native latex script entities)))

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
		(:option bv-latex-output-dir "~/slipbox/out")
		(:alias org-latex-compile bv-org-latex-compile)
		(:global "C-c f" bv-fix-math-delimiters))

  ;; `org-preview' is a minor mode which provides asynchronous,
  ;; blazing fast, latex previews.
  ;; It is written by Karthik Chikmagalur <karthikchikmagalur@gmail.com>.
	;; Demo here: https://www.youtube.com/watch?v=n-AfvuV-bYo
	;; It will be built into Org, probably in the 9.7 release.
  ;;
	(:require org-preview)

  ;; Habit Tracking
  (:option* modules '(org-habit)
            habit-preceding-days 30
            habit-graph-column 40)

  ;; Keybindings
  (:unbind
	 "C-c C-o"
	 "M-{" ;; clashes with bv-insert-open-brace
	 )
  (:global "C-c l" org-store-link
           "C-c a" org-agenda
           "C-c c" org-capture)
  (message "Successfully setup org-mode"))

(setup org-faces
  (:load-after org)
  (:also-load org-indent)
  (:when-loaded
    (bv-setup-org-fonts))
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
            auto-delay 0.10
            max-width 150
						min-width 20
            count 15
            scroll-margin 10
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
		(:hook corfu-echo-mode corfu-history-mode corfu-indexed-mode corfu-popupinfo-mode))
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
  (setq org-cite-global-bibliography bv-working-bib-path
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
  (:option*
   v2-ack t
   directory "~/slipbox"
   database-connector 'sqlite-builtin
   db-extra-links-elements '(keyword node-property)
   mode-sections '((org-roam-backlinks-section :unique t)
                   org-roam-reflinks-section)
   link-title-format "R:%s"
   tag-sources '(all-directories)
   tag-sort t
   tag-context-lines 5)
  (:require bv-org-roam)
  (:option org-roam-capture-templates bv-org-roam-capture-templates)
  (:option* node-display-template bv-org-roam-node-display-template
            org-roam-node-annotation-function bv-org-roam-node-annotation-function)
  (:with-hook after-init-hook
		(:hook org-roam-db-autosync-mode))
  (:global
   "C-c n g" org-roam-graph
   "C-c n i" org-roam-node-insert
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
   "C-c n f" consult-org-roam-file-find
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

(setup (:straight-if geiser bv-not-guix-p)
  (:option geiser-default-implementation 'guile
					 geiser-active-implementations '(guile))
  (:push-to geiser-implementations-alist
						(:elements
						 (((regexp "\\.scm$") guile))))
  (:require geiser)
  (message "Successfully setup geiser"))

(setup (:straight-if geiser-guile bv-not-guix-p)
	(:load-after exec-path-from-shell)
	(:push-to exec-path-from-shell-variables
						(:elements "GUILE_LOAD_PATH"))
	(exec-path-from-shell-initialize)
	(:option geiser-guile-load-init-file t
					 geiser-guile-load-path (split-string (getenv "GUILE_LOAD_PATH") path-separator)
					 geiser-repl-add-project-paths t)
  (:require geiser-guile)
  (message "Successfully setup geiser-guile"))

(setup (:straight-if lsp-mode bv-not-guix-p)
  (:hook-into rust-mode
              python-mode
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
	(:option gptel-model "gpt-4-turbo")
	(:global "C-c g" gptel-send))

(provide 'init)
;;; init.el ends here
