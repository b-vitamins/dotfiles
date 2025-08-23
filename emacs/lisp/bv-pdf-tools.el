;;; bv-pdf-tools.el --- PDF viewing configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced PDF viewing with theme integration.

;;; Code:

(autoload 'pdf-view-mode "pdf-view")
(autoload 'pdf-view-themed-minor-mode "pdf-view")
(autoload 'pdf-view-midnight-minor-mode "pdf-view")
(autoload 'pdf-view-redisplay "pdf-view")
(autoload 'pdf-tools-enable-minor-modes "pdf-tools")
(autoload 'pdf-tools-install "pdf-tools")

;; External variable declarations
(defvar pdf-view-continuous)
(defvar pdf-view-display-size)
(defvar pdf-view-midnight-colors)
(defvar pdf-view-use-scaling)
(defvar pdf-view-resolution)
(defvar pdf-view-resize-factor)
(defvar pdf-view-midnight-invert)
(defvar pdf-view-midnight-hue)
(defvar pdf-view-use-imagemagick)
(defvar pdf-view-image-relief)
(defvar pdf-view-use-unicode-ligther)
(defvar pdf-view-current-page)
(defvar pdf-view-mode-map)
(defvar TeX-view-program-selection)
(defvar bv-pdf-night-mode)

;; External function declarations
(declare-function bv-themes-variant "bv-themes")
(declare-function bv-circadian--should-use-dark-theme-p "bv-circadian")
(declare-function pdf-view-current-page "pdf-view")
(declare-function pdf-view-goto-page "pdf-view" (page))
(declare-function pdf-view-rotate "pdf-view" (&optional degree))

;; Auto mode
(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))


;; Custom group definition
(defgroup bv-pdf nil
  "PDF viewing configuration with theme integration."
  :group 'applications
  :prefix "bv-pdf-")

;; Theme-aware configuration
(defcustom bv-pdf-auto-sync-theme t
  "Whether PDF night mode should automatically sync with current theme."
  :type 'boolean
  :group 'bv-pdf)

(defcustom bv-pdf-sync-with-circadian t
  "Whether PDF night mode should sync with circadian rhythm if available."
  :type 'boolean
  :group 'bv-pdf)

;; Pre-render hook to prepare night mode settings
(defun bv-pdf-pre-render-setup ()
  "Prepare night mode settings before PDF rendering to prevent flash."
  (when (and (or bv-pdf-auto-sync-theme bv-pdf-sync-with-circadian)
             (bv-pdf-should-use-night-mode-p))
    ;; Just set the flag and prepare colors, don't activate modes yet
    (setq bv-pdf-night-mode t)
    (when (boundp 'pdf-view-midnight-colors)
      (setq-local pdf-view-midnight-colors '("#e8e8e8" . "#181818")))
    (when (boundp 'pdf-view-use-scaling)
      (setq-local pdf-view-use-scaling t))
    (when (boundp 'pdf-view-resolution)
      (setq-local pdf-view-resolution 144))))

;; PDF viewing setup
(defun bv-pdf-tools-setup ()
  "Setup PDF tools with theme awareness."
  ;; Prepare settings first (without activating modes)
  (bv-pdf-pre-render-setup)
  ;; Enable PDF tools
  (pdf-tools-enable-minor-modes)
  ;; Don't use themed mode by default - it inverts colors including figures
  (pdf-view-themed-minor-mode -1)
  ;; Sync theme after PDF is loaded
  (add-hook 'pdf-view-after-change-page-hook #'bv-pdf-sync-theme-once nil t))

(defun bv-pdf-sync-theme-once ()
  "Sync theme once after first page is displayed."
  (bv-pdf-sync-theme)
  ;; Remove the hook after first run
  (remove-hook 'pdf-view-after-change-page-hook #'bv-pdf-sync-theme-once t))

(defun bv-pdf-tools-install-and-setup ()
  "Install PDF tools and setup the hook."
  (pdf-tools-install :no-query)
  (remove-hook 'pdf-view-mode-hook #'bv-pdf-tools-install-and-setup)
  (add-hook 'pdf-view-mode-hook #'bv-pdf-tools-setup)
  ;; Run setup for current buffer
  (bv-pdf-tools-setup))

;; Install PDF tools on first PDF file access
(add-hook 'pdf-view-mode-hook #'bv-pdf-tools-install-and-setup)


;; PDF view settings
(with-eval-after-load 'pdf-view
  (when (boundp 'pdf-view-use-scaling)
    (setq pdf-view-use-scaling t))
  (when (boundp 'pdf-view-display-size)
    (setq pdf-view-display-size 'fit-page))
  (when (boundp 'pdf-view-resize-factor)
    (setq pdf-view-resize-factor 1.1))
  (when (boundp 'pdf-view-continuous)
    (setq pdf-view-continuous t)))

;; TeX integration
(with-eval-after-load 'tex
  (when (boundp 'TeX-view-program-selection)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))
  (add-hook 'TeX-mode-hook 'TeX-source-correlate-mode))

;; Night mode with enhanced rendering
(defvar-local bv-pdf-night-mode nil
  "Whether night mode is enabled for current PDF buffer.")

;; Custom view mode functions
(defun pdf-view-toggle-continuous ()
  "Toggle continuous scrolling mode."
  (interactive)
  (setq pdf-view-continuous (not pdf-view-continuous))
  (pdf-view-redisplay)
  (message "Continuous scrolling %s"
           (if pdf-view-continuous "enabled" "disabled")))

(defun pdf-view-toggle-spread ()
  "Toggle dual-page spread mode."
  (interactive)
  (let ((current-page (pdf-view-current-page)))
    (setq pdf-view-display-size
          (if (eq pdf-view-display-size 'fit-page)
              'fit-width  ; Switch to fit-width for better dual-page view
            'fit-page))
    (pdf-view-goto-page current-page))
  (message "Spread mode toggled"))

(defun bv-pdf-should-use-night-mode-p ()
  "Determine if night mode should be enabled based on theme or time."
  (cond
   ;; Check circadian if enabled
   ((and bv-pdf-sync-with-circadian
         (fboundp 'bv-circadian--should-use-dark-theme-p))
    (bv-circadian--should-use-dark-theme-p))
   ;; Check current theme
   ((and bv-pdf-auto-sync-theme
         (fboundp 'bv-themes-variant))
    (eq (bv-themes-variant) 'dark))
   ;; Fallback to manual setting
   (t bv-pdf-night-mode)))

(defun bv-pdf-sync-theme ()
  "Sync PDF night mode with current theme."
  (when (and (derived-mode-p 'pdf-view-mode)
             (or bv-pdf-auto-sync-theme bv-pdf-sync-with-circadian))
    (let ((should-be-night (bv-pdf-should-use-night-mode-p)))
      ;; Always set the mode to ensure proper initialization
      (bv-pdf-set-night-mode should-be-night)
      (message "PDF night mode synced: %s"
               (if should-be-night "dark" "light")))))

(defun bv-pdf-set-night-mode (enable)
  "Set PDF night mode to ENABLE."
  (setq bv-pdf-night-mode enable)
  (if enable
      (progn
        ;; High contrast colors for better text clarity
        (when (boundp 'pdf-view-midnight-colors)
          (setq-local pdf-view-midnight-colors '("#e8e8e8" . "#181818")))
        ;; Enable midnight mode with inversion
        (when (fboundp 'pdf-view-midnight-minor-mode)
          (pdf-view-midnight-minor-mode 1))
        ;; Increase resolution for crisper text
        (when (boundp 'pdf-view-use-scaling)
          (setq-local pdf-view-use-scaling t))
        (when (boundp 'pdf-view-resolution)
          (setq-local pdf-view-resolution 144)) ; Higher DPI
        ;; Force re-render with new settings only if PDF is loaded
        (when (and (fboundp 'pdf-view-redisplay)
                   (boundp 'pdf-view-current-page)
                   (> (pdf-view-current-page) 0))
          (pdf-view-redisplay)))
    (when (fboundp 'pdf-view-midnight-minor-mode)
      (pdf-view-midnight-minor-mode -1))
    ;; Restore default resolution
    (when (boundp 'pdf-view-resolution)
      (setq-local pdf-view-resolution 96))
    ;; Redisplay only if PDF is loaded
    (when (and (fboundp 'pdf-view-redisplay)
               (boundp 'pdf-view-current-page)
               (> (pdf-view-current-page) 0))
      (pdf-view-redisplay))))

(defun bv-pdf-toggle-night-mode ()
  "Toggle night mode with enhanced inversion for crisp text."
  (interactive)
  ;; Disable auto-sync when manually toggling
  (when (or bv-pdf-auto-sync-theme bv-pdf-sync-with-circadian)
    (setq-local bv-pdf-auto-sync-theme nil)
    (setq-local bv-pdf-sync-with-circadian nil)
    (message "PDF auto-sync disabled for manual control"))
  (bv-pdf-set-night-mode (not bv-pdf-night-mode))
  (message "PDF night mode %s"
           (if bv-pdf-night-mode "enabled" "disabled")))

;; Evince-like keybindings
(with-eval-after-load 'pdf-view
  (when (boundp 'pdf-view-mode-map)
    ;; Zooming (Evince-style)
    (define-key pdf-view-mode-map (kbd "f") 'pdf-view-fit-page-to-window)  ; F - Fit page
    (define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window) ; W - Fit width
    (define-key pdf-view-mode-map (kbd "a") 'pdf-view-fit-height-to-window) ; A - Auto zoom (height)
    (define-key pdf-view-mode-map (kbd "C-0") 'pdf-view-scale-reset)       ; Ctrl+0 - 1:1 zoom
    (define-key pdf-view-mode-map (kbd "C-=") 'pdf-view-enlarge)           ; Ctrl+= - Zoom in
    (define-key pdf-view-mode-map (kbd "C-+") 'pdf-view-enlarge)           ; Ctrl++ - Zoom in
    (define-key pdf-view-mode-map (kbd "+") 'pdf-view-enlarge)             ; + - Zoom in
    (define-key pdf-view-mode-map (kbd "=") 'pdf-view-enlarge)             ; = - Zoom in
    (define-key pdf-view-mode-map (kbd "C--") 'pdf-view-shrink)            ; Ctrl+- - Zoom out
    (define-key pdf-view-mode-map (kbd "-") 'pdf-view-shrink)              ; - - Zoom out

    ;; Rotation
    (define-key pdf-view-mode-map (kbd "C-<left>") 'pdf-view-rotate)       ; Ctrl+← - Rotate CCW
    (define-key pdf-view-mode-map (kbd "C-<right>") (lambda () (interactive) (pdf-view-rotate 270))) ; Ctrl+→ - Rotate CW

    ;; Navigation (Evince-style)
    (define-key pdf-view-mode-map (kbd "p") 'pdf-view-previous-page-command) ; P - Previous page
    (define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-page-command)     ; N - Next page
    (define-key pdf-view-mode-map (kbd "C-l") 'pdf-view-goto-page)           ; Ctrl+L - Go to page

    ;; View modes
    (define-key pdf-view-mode-map (kbd "c") 'pdf-view-toggle-continuous)     ; C - Toggle continuous
    (define-key pdf-view-mode-map (kbd "D") 'pdf-view-toggle-spread)         ; D - Toggle dual page (uppercase)

    ;; Night mode (already defined, keeping our custom implementation)
    (define-key pdf-view-mode-map (kbd "i") 'bv-pdf-toggle-night-mode)       ; I - Night mode (our version)
    (define-key pdf-view-mode-map (kbd "C-i") 'bv-pdf-toggle-night-mode)     ; Ctrl+I - Night mode

    ;; Document operations
    (define-key pdf-view-mode-map (kbd "r") 'revert-buffer)                  ; R - Reload
    (define-key pdf-view-mode-map (kbd "g") 'revert-buffer)                  ; G - Reload (alternative)
    (define-key pdf-view-mode-map (kbd "C-w") 'kill-current-buffer)          ; Ctrl+W - Close
    (define-key pdf-view-mode-map (kbd "P") 'pdf-misc-print-document)        ; P - Print (uppercase)

    ;; Sidebar
    (define-key pdf-view-mode-map (kbd "<f9>") 'pdf-outline)                 ; F9 - Toggle outline
    (define-key pdf-view-mode-map (kbd "o") 'pdf-outline)                    ; O - Outline (lowercase)

    ;; Fullscreen
    (define-key pdf-view-mode-map (kbd "<f11>") 'toggle-frame-fullscreen)    ; F11 - Fullscreen

    ;; Selection and copying
    (define-key pdf-view-mode-map (kbd "C-a") 'pdf-view-mark-whole-page)     ; Ctrl+A - Select all

    ;; Search
    (define-key pdf-view-mode-map (kbd "C-f") 'pdf-occur)                    ; Ctrl+F - Search
    (define-key pdf-view-mode-map (kbd "/") 'pdf-occur)                      ; / - Search
    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)              ; Ctrl+S - Incremental search
    (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)             ; Ctrl+R - Reverse search

    ;; Vim-like navigation (h,j,k,l)
    (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)         ; H - Scroll left
    (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page) ; J - Scroll down
    (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page) ; K - Scroll up
    (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)          ; L - Scroll right

    ;; History navigation
    (define-key pdf-view-mode-map (kbd "M-p") 'pdf-history-backward)         ; Alt+P - Previous in history
    (define-key pdf-view-mode-map (kbd "M-n") 'pdf-history-forward)          ; Alt+N - Next in history

    ;; Home/End navigation
    (define-key pdf-view-mode-map (kbd "<home>") 'image-bob)                 ; Home - Beginning of page
    (define-key pdf-view-mode-map (kbd "<end>") 'image-eob)                  ; End - End of page
    (define-key pdf-view-mode-map (kbd "C-<home>") 'pdf-view-first-page)     ; Ctrl+Home - First page
    (define-key pdf-view-mode-map (kbd "C-<end>") 'pdf-view-last-page)       ; Ctrl+End - Last page

    ;; Bookmarks (using standard Emacs bookmarks)
    (define-key pdf-view-mode-map (kbd "b") 'bookmark-set)                   ; B - Set bookmark
    (define-key pdf-view-mode-map (kbd "B") 'bookmark-jump)                  ; Shift+B - Jump to bookmark

    ;; Annotations
    (define-key pdf-view-mode-map (kbd "s") 'pdf-annot-add-text-annotation)  ; S - Add note
    (define-key pdf-view-mode-map (kbd "C-h") 'pdf-annot-add-highlight-markup-annotation)) ; Ctrl+H - Highlight

  ;; Midnight mode settings for better inversion
  (when (boundp 'pdf-view-midnight-invert)
    (setq pdf-view-midnight-invert t))
  (when (boundp 'pdf-view-midnight-hue)
    (setq pdf-view-midnight-hue nil))
  ;; Image processing settings for better quality
  (when (boundp 'pdf-view-use-imagemagick)
    (setq pdf-view-use-imagemagick t))
  (when (boundp 'pdf-view-image-relief)
    (setq pdf-view-image-relief 0))
  (when (boundp 'pdf-view-use-unicode-ligther)
    (setq pdf-view-use-unicode-ligther nil)))

;; Hook into theme system for automatic syncing
(defun bv-pdf-sync-all-buffers ()
  "Sync night mode in all PDF buffers with current theme."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'pdf-view-mode)
        (bv-pdf-sync-theme)))))

;; Add hooks for theme changes
(with-eval-after-load 'bv-themes
  (add-hook 'bv-themes-after-load-hook #'bv-pdf-sync-all-buffers))

;; Add hooks for circadian changes
(with-eval-after-load 'bv-circadian
  (add-hook 'bv-circadian-sunrise-hook #'bv-pdf-sync-all-buffers)
  (add-hook 'bv-circadian-sunset-hook #'bv-pdf-sync-all-buffers))

;; Helper function to display keybindings
(defun bv-pdf-show-keybindings ()
  "Display PDF keybindings in a help buffer."
  (interactive)
  (with-help-window "*PDF Keybindings*"
    (princ "PDF Viewer Keybindings (Evince-style)\n")
    (princ "=====================================\n\n")
    (princ "ZOOMING:\n")
    (princ "  f         - Fit page to window\n")
    (princ "  w         - Fit width to window\n")
    (princ "  a         - Auto zoom (fit height)\n")
    (princ "  C-0       - Reset zoom to 1:1\n")
    (princ "  +/=/C-+   - Zoom in\n")
    (princ "  -/C--     - Zoom out\n\n")
    (princ "NAVIGATION:\n")
    (princ "  n         - Next page\n")
    (princ "  p         - Previous page\n")
    (princ "  C-l       - Go to page number\n")
    (princ "  h/j/k/l   - Vim-style scrolling\n")
    (princ "  C-Home    - First page\n")
    (princ "  C-End     - Last page\n")
    (princ "  M-p/M-n   - History back/forward\n\n")
    (princ "VIEW MODES:\n")
    (princ "  c         - Toggle continuous scrolling\n")
    (princ "  D         - Toggle dual-page spread\n")
    (princ "  i/C-i     - Toggle night mode\n")
    (princ "  C-←/C-→   - Rotate left/right\n")
    (princ "  F11       - Toggle fullscreen\n")
    (princ "  F9/o      - Show outline/sidebar\n\n")
    (princ "SEARCH & SELECTION:\n")
    (princ "  C-f, /    - Search in document (pdf-occur)\n")
    (princ "  C-s       - Incremental search forward\n")
    (princ "  C-r       - Incremental search backward\n")
    (princ "  C-a       - Select all on page\n\n")
    (princ "DOCUMENT:\n")
    (princ "  r/g       - Reload document\n")
    (princ "  C-w       - Close document\n")
    (princ "  P         - Print document\n")
    (princ "  b         - Set bookmark\n")
    (princ "  B         - Jump to bookmark\n")
    (princ "  s         - Add text annotation\n")
    (princ "  C-h       - Highlight text\n\n")
    (princ "Press ? in PDF mode to show this help again.")))

;; Add help key binding
(with-eval-after-load 'pdf-view
  (when (boundp 'pdf-view-mode-map)
    (define-key pdf-view-mode-map (kbd "?") 'bv-pdf-show-keybindings)))

;; PDF tools will be installed on first use via the hook above

(provide 'bv-pdf-tools)
;;; bv-pdf-tools.el ends here
