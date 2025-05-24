;;; bv-defaults.el --- Default preferences configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from init.el to keep main configuration lean.

;;; Code:

;; Set up default preferences to improve the Emacs user experience.
(setup default-preferences

  ;; Log the start of the setup process.
  (log-init-message "Setting up default preferences:")

  ;; ---- Buffer and Editing Behavior ----
  (:set-default
   initial-major-mode 'org-mode                          ;; Org mode as the default major mode for new buffers.
   kill-whole-line t                                     ;; Allow C-k to kill the whole line, including newline.
   kill-read-only-ok t                                   ;; Permit killing read-only buffers.
   use-short-answers t                                   ;; Enable short (y/n) answers for prompts.
   save-interprogram-paste-before-kill t                 ;; Save clipboard content before killing text.
   dired-dwim-target t                                   ;; Enable Dired DWIM behavior for easier file operations.
   set-default-coding-systems 'utf-8                     ;; Set default coding system for file I/O to UTF-8.
   set-language-environment "UTF-8"                      ;; Configure the environment to use UTF-8.
   ring-bell-function 'ignore                            ;; Disable the bell sound.
   echo-keystrokes 0.1)                                  ;; Reduce delay before showing keystrokes.

  (log-init-message "    Buffer and editing preferences set.")

  ;; ---- Cursor, Navigation, and Display ----
  (:set-default
   cursor-type 'bar                                      ;; Set cursor type to a thin bar for better visibility.
   cursor-in-non-selected-window 'bar                    ;; Apply bar cursor to non-selected windows as well.
   next-screen-context-lines 4                           ;; Show 4 lines of context when navigating between screens.
   recenter-positions '(top bottom middle)               ;; Customize recenter positions: top, bottom, middle.
   view-read-only t)                                     ;; Open read-only files in view mode by default.

  (log-init-message "    Cursor, navigation, and display preferences set.")

  ;; ---- File and Auto-Saving Behavior ----
  (:set-default
   auto-save-list-file-prefix nil                        ;; Disable the creation of auto-save list files.
   vc-follow-symlinks t                                  ;; Automatically follow symlinks for version-controlled files.
   make-backup-files nil                                 ;; Disable creation of backup files.
   create-lockfiles nil                                  ;; Prevent creation of lockfiles (e.g., .#file).
   require-final-newline nil)                            ;; Disable final newline requirement in files.

  (log-init-message "    File and auto-saving behavior preferences set.")

  ;; ---- Performance and Warnings ----
  (:set-default
   inhibit-compacting-font-caches t                      ;; Prevent font cache compaction during garbage collection.
   mode-require-final-newline nil                        ;; Disable mode-specific final newline requirement.
   warning-minimum-level :emergency                      ;; Set the minimum level of warnings to display to reduce noise.
   native-comp-async-report-warnings-errors 'silent      ;; Silence native compilation warnings and errors.
   large-file-warning-threshold nil                      ;; Disable large file warnings.
   native-compile-prune-cache t)                         ;; Enable native compilation cache pruning to save space.

  (log-init-message "    Performance and warning preferences set.")

  ;; ---- Tab and Indentation Behavior ----
  (:set-default
   tab-always-indent 'complete                           ;; Configure tab behavior to first indent, then complete.
   tab-width 2                                           ;; Set tab width to 2 spaces.
   indent-tabs-mode nil)                                 ;; Use spaces instead of tabs for indentation.

  (log-init-message "    Tab and indentation preferences set.")

  ;; ---- Mouse and Focus Behavior ----
  (:set-default
   focus-follows-mouse t                                 ;; Enable focus-follows-mouse behavior.
   left-margin-width 2                                   ;; Set left margin width for better text readability.
   right-margin-width 2)                                 ;; Set right margin width for better text readability.

  (log-init-message "    Mouse and focus behavior preferences set.")

  ;; ---- Shell Command and Async Behavior ----
  (:set-default
   async-shell-command-buffer 'confirm-kill-process)     ;; Confirm before killing async shell command processes.

  (log-init-message "    Shell command and async behavior preferences set.")

  ;; ---- Direct Settings (Desugars to `setq`) ----
  (:set
   inhibit-startup-screen t                              ;; Disable startup screen for a cleaner launch experience.
   inhibit-startup-echo-area-message "Welcome to Emacs!" ;; Custom startup message in the echo area.
   fit-window-to-buffer-horizontally t                   ;; Fit windows horizontally to the buffer content.
   window-resize-pixelwise t                             ;; Allow pixel-level window resizing.
   font-lock-support-mode 'jit-lock-mode                 ;; Set JIT Lock mode for just-in-time syntax highlighting.
   font-lock-maximum-decoration t)                       ;; Enable maximum decoration for syntax highlighting.

  (log-init-message "    Direct preferences set with `setq`.")

  ;; ---- File Associations ----
  (add-to-list 'auto-mode-alist '("\\.gscm\\'" . scheme-mode)) ;; Open `.gscm` files in `scheme-mode`.
  (add-to-list 'auto-mode-alist '("zshenv\\'" . sh-mode))      ;; Open `zshenv` files in `sh-mode`.
  (add-to-list 'auto-mode-alist '("zprofile\\'" . sh-mode))    ;; Open `zprofile` files in `sh-mode`.
  (add-to-list 'auto-mode-alist '("zlogin\\'" . sh-mode))      ;; Open `zlogin` files in `sh-mode`.
  (add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode))       ;; Open `zshrc` files in `sh-mode`.
  (add-to-list 'auto-mode-alist '("zlogout\\'" . sh-mode))     ;; Open `zlogout` files in `sh-mode`.
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))     ;; Open `.txt` files in `org-mode`.
  (log-init-message "    File associations set.")

  ;; ---- Additional Settings ----
  (:set-default
   search-highlight t                                    ;; Highlight matching search patterns.
   enable-local-eval t                                   ;; Allow evaluation of local variables.
   auth-sources '("~/.authinfo.gpg"))                    ;; Set encrypted `.authinfo.gpg` for authentication credentials.

  (log-init-message "    Additional preferences set.")
  (log-init-message "Successfully completed setting up default preferences."))

(provide 'bv-defaults)

;;; bv-defaults.el ends here

