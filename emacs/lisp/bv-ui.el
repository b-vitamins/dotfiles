;;; bv-ui.el --- User interface configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from init.el to modularize UI-related settings.

;;; Code:

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

;; Setup nerd-icons and related extensions for rich icon support.
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

(provide 'bv-ui)

;;; bv-ui.el ends here
