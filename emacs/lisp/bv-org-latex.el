;;; bv-org-latex.el --- Org mode LaTeX preview configuration with multi-display support -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>
;; Package-Requires: ((emacs "27.1") (org "9.6"))
;; Version: 2.0

;;; Commentary:
;; Enhanced LaTeX preview configuration for Org mode with automatic scaling
;; for multiple displays (laptop, TV) using TeX Gyre Pagella Math font.

;;; Code:

(require 'org)

(defgroup bv-org-latex nil
  "LaTeX preview settings for Org mode with multi-display support."
  :group 'bv)

(defcustom bv-org-latex-scale 2.0
  "Scale factor for LaTeX previews."
  :type 'number
  :group 'bv-org-latex)

(defconst bv-org-latex-default-scale 2.0
  "Default scale factor for reset.")

(defcustom bv-org-latex-auto-scale t
  "Whether to automatically scale LaTeX previews based on display."
  :type 'boolean
  :group 'bv-org-latex)

(defcustom bv-org-latex-base-dpi 96
  "Base DPI for scaling calculations (standard monitor DPI)."
  :type 'number
  :group 'bv-org-latex)

(defcustom bv-org-latex-display-profiles
  '((laptop   . ((min-dpi . 250) (viewing-distance-factor . 1.0)))
    (tv       . ((min-dpi . 0)   (viewing-distance-factor . 1.5))))
  "Display profiles with DPI ranges and viewing distance factors."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-org-latex)

(defcustom bv-org-latex-display-overrides
  '(;; Update these with your actual display names
    ;; Run M-x bv-org-latex-list-displays to find your display names
    ("DP-1" . ((dpi . 49)  (type . tv)))        ; LG 45" 1080p TV
    ("eDP-1" . ((dpi . 162) (type . laptop))))  ; Laptop 14" 1920x1200 (scaled)
  "Manual DPI overrides for specific displays.
Check your display names with: \[bv-org-latex-list-displays]"
  :type '(alist :key-type string :value-type plist)
  :group 'bv-org-latex)

(defcustom bv-org-latex-packages
  '(;; Mathematics
    ("" "amsmath" t)
    ("" "amssymb" t)
    ("" "amsthm" t)
    ("" "mathtools" t)
    ("" "physics" t)
    ("" "braket" t)
    ("" "tensor" t)
    ;; Additional math symbols
    ("" "dsfont" t)
    ("" "bbm" t)
    ("" "mathrsfs" t)
    ("" "esint" t)
    ("" "cancel" t)
    ;; Better matrices and arrays
    ("" "nicematrix" t)
    ("" "array" t)
    ;; Units and numbers
    ("binary-units=true" "siunitx" t)
    ;; Graphics and diagrams
    ("" "tikz" t)
    ("" "pgfplots" t)
    ;; Chemistry
    ("" "chemfig" t)
    ("" "mhchem" t)
    ;; Algorithms
    ("" "algorithm2e" t)
    ;; Better typography
    ("" "microtype" t)
    ;; Colors
    ("" "xcolor" t))
  "Comprehensive list of LaTeX packages for mathematical and scientific work."
  :type '(repeat (list string string boolean))
  :group 'bv-org-latex)

;; Utility functions
(defun bv-org-latex--get-cache-dir ()
  "Get the XDG-compliant cache directory."
  (expand-file-name "emacs/ltximg/"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state")))

(defun bv-org-latex--get-current-monitor-name ()
  "Get the name of the current monitor."
  (cdr (assq 'name (frame-monitor-attributes))))

(defun bv-org-latex--calculate-dpi (pixel-width mm-width)
  "Calculate DPI from PIXEL-WIDTH and MM-WIDTH dimensions."
  (if (and pixel-width mm-width (> mm-width 0))
      (round (/ (* pixel-width 25.4) mm-width))
    nil))

(defun bv-org-latex--detect-display-type (dpi)
  "Detect display type based on DPI."
  (if (>= dpi 250)
      'laptop
    'tv))

(defun bv-org-latex--get-display-info ()
  "Get comprehensive display information with fallbacks."
  (let* ((monitor-name (bv-org-latex--get-current-monitor-name))
         (override (cdr (assoc monitor-name bv-org-latex-display-overrides)))
         (attrs (frame-monitor-attributes))
         (geometry (alist-get 'geometry attrs))
         (mm-size (alist-get 'mm-size attrs))
         (pixel-width (and geometry (nth 2 geometry)))
         (mm-width (and mm-size (car mm-size)))
         ;; Try multiple methods to get DPI
         (emacs-pixel-height (and (display-graphic-p) (display-pixel-height)))
         (emacs-mm-height (and (display-graphic-p) (display-mm-height)))
         (dpi-from-emacs
          (when (and emacs-pixel-height
                     emacs-mm-height
                     (> emacs-mm-height 0))
            (round (/ emacs-pixel-height (/ emacs-mm-height 25.4)))))
         (detected-dpi (or
                        ;; 1. Use override if available
                        (plist-get override :dpi)
                        ;; 2. Calculate from monitor attributes
                        (bv-org-latex--calculate-dpi pixel-width mm-width)
                        ;; 3. Try Emacs display info when reliable
                        dpi-from-emacs
                        ;; 4. Default fallback
                        96))
         (display-type (or (plist-get override :type)
                          (bv-org-latex--detect-display-type detected-dpi))))
    (list :name monitor-name
          :dpi detected-dpi
          :type display-type
          :pixel-width pixel-width
          :mm-width mm-width)))

(defun bv-org-latex--calculate-scale-for-display (display-info)
  "Calculate optimal scale based on DISPLAY-INFO characteristics."
  (let* ((dpi (plist-get display-info :dpi))
         (display-type (plist-get display-info :type))
         (profile (alist-get display-type bv-org-latex-display-profiles))
         (viewing-factor (or (alist-get 'viewing-distance-factor profile) 1.0))
         ;; Get font size factor
         (font-height (face-attribute 'default :height))
         (font-scale (/ font-height 120.0))  ; Normalized to 12pt
         ;; Calculate base scale differently based on display type
         (base-scale
          (cond
           ;; For TVs: inverse relationship - lower DPI needs higher scale
           ((eq display-type 'tv)
            (* (/ 96.0 (float dpi)) font-scale))
           ;; For high-DPI displays: direct relationship
           ((> dpi 150)
            (* (/ (float dpi) 96.0) font-scale))
           ;; For normal displays: moderate scaling
           (t
            (* 1.2 font-scale))))
         ;; Apply viewing distance correction
         (final-scale (* base-scale viewing-factor)))
    ;; Ensure reasonable bounds with different minimums per display type
    (let ((min-scale (if (eq display-type 'tv) 2.0 1.0)))
      (max min-scale (min 6.0 final-scale)))))

(defun bv-org-latex--get-display-dpi ()
  "Get current display DPI with enhanced detection."
  (plist-get (bv-org-latex--get-display-info) :dpi))

(defun bv-org-latex--calculate-auto-scale ()
  "Calculate optimal scale for current display."
  (let ((display-info (bv-org-latex--get-display-info)))
    (bv-org-latex--calculate-scale-for-display display-info)))

;; User-facing functions
(defun bv-org-latex-show-display-info ()
  "Show current display information for debugging."
  (interactive)
  (let ((info (bv-org-latex--get-display-info)))
    (message "Display: %s | DPI: %d | Type: %s | Scale: %.2f"
             (or (plist-get info :name) "Unknown")
             (plist-get info :dpi)
             (plist-get info :type)
             (bv-org-latex--calculate-auto-scale))))

(defun bv-org-latex-list-displays ()
  "List all available displays with their properties."
  (interactive)
  (let ((monitors (display-monitor-attributes-list)))
    (with-output-to-temp-buffer "*Display Information*"
      (princ "Available Displays:\n\n")
      (dolist (monitor monitors)
        (let* ((name (alist-get 'name monitor))
               (geometry (alist-get 'geometry monitor))
               (mm-size (alist-get 'mm-size monitor))
               (pixel-width (and geometry (nth 2 geometry)))
               (pixel-height (and geometry (nth 3 geometry)))
               (mm-width (and mm-size (car mm-size)))
               (mm-height (and mm-size (cdr mm-size)))
               (dpi (bv-org-latex--calculate-dpi pixel-width mm-width)))
          (princ (format "Display: %s\n" (or name "Unknown")))
          (princ (format "  Resolution: %dx%d pixels\n"
                         (or pixel-width 0) (or pixel-height 0)))
          (princ (format "  Physical size: %dx%d mm\n"
                         (or mm-width 0) (or mm-height 0)))
          (princ (format "  Calculated DPI: %d\n" (or dpi 0)))
          (princ (format "  Override: %s\n\n"
                         (if (assoc name bv-org-latex-display-overrides)
                             "Yes" "No"))))))))

(defun bv-org-latex-setup ()
  "Configure Org mode LaTeX preview settings with display detection."

  ;; Set cache directory
  (let ((cache-dir (bv-org-latex--get-cache-dir)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    (setq org-preview-latex-image-directory cache-dir))

  ;; Calculate and set scale with display info
  (when bv-org-latex-auto-scale
    (let ((display-info (bv-org-latex--get-display-info)))
      (setq bv-org-latex-scale (bv-org-latex--calculate-scale-for-display display-info))
      (message "LaTeX preview scale: %.2f (Display: %s, DPI: %d, Type: %s)"
               bv-org-latex-scale
               (or (plist-get display-info :name) "Unknown")
               (plist-get display-info :dpi)
               (plist-get display-info :type))))

  ;; Basic options
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :foreground 'default)

  ;; Add luamagick process if not already present
  (unless (assq 'luamagick org-preview-latex-process-alist)
    (push '(luamagick
            :programs ("lualatex" "convert")
            :description "pdf > png (LuaLaTeX + ImageMagick)"
            :message "you need to install the programs: lualatex and imagemagick (convert)."
            :image-input-type "pdf"
            :image-output-type "png"
            :image-size-adjust (1.0 . 1.0)
            :post-clean (".aux" ".out" ".synctex.gz")
            :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
            :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")
            :transparent-image-converter ("convert -density %D -trim -antialias -background none %f -quality 100 %O"))
          org-preview-latex-process-alist))

  ;; Set preview method to our custom luamagick
  (setq org-preview-latex-default-process 'luamagick)

  ;; Add packages to org-latex-packages-alist
  (dolist (pkg bv-org-latex-packages)
    (add-to-list 'org-latex-packages-alist pkg t))

  ;; Modify org-format-latex-header to add LuaLaTeX font support
  (setq org-format-latex-header
        "\\documentclass{article}
\\usepackage[usenames]{color}
[DEFAULT-PACKAGES]
[PACKAGES]
% Font setup for LuaLaTeX
\\usepackage{unicode-math}
\\setmathfont{TeX Gyre Pagella Math}
\\setmainfont{TeX Gyre Pagella}
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

  ;; Use org-fragtog for automatic preview if available
  (when (require 'org-fragtog nil t)
    (add-hook 'org-mode-hook 'org-fragtog-mode)))

;; Scale adjustment commands
(defun bv-org-latex-increase-scale ()
  "Increase LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-auto-scale nil)  ; Disable auto-scale when manually adjusting
  (setq bv-org-latex-scale (* bv-org-latex-scale 1.1))
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (when (derived-mode-p 'org-mode)
    (org-latex-preview '(64))  ; Clear
    (org-latex-preview '(16))) ; Regenerate
  (message "LaTeX preview scale: %.2f (auto-scale disabled)" bv-org-latex-scale))

(defun bv-org-latex-decrease-scale ()
  "Decrease LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-auto-scale nil)  ; Disable auto-scale when manually adjusting
  (setq bv-org-latex-scale (/ bv-org-latex-scale 1.1))
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (when (derived-mode-p 'org-mode)
    (org-latex-preview '(64))
    (org-latex-preview '(16)))
  (message "LaTeX preview scale: %.2f (auto-scale disabled)" bv-org-latex-scale))

(defun bv-org-latex-reset-scale ()
  "Reset LaTeX preview scale to auto-calculated value."
  (interactive)
  (setq bv-org-latex-auto-scale t)  ; Re-enable auto-scale
  (let ((display-info (bv-org-latex--get-display-info)))
    (setq bv-org-latex-scale (bv-org-latex--calculate-scale-for-display display-info))
    (plist-put org-format-latex-options :scale bv-org-latex-scale)
    (when (derived-mode-p 'org-mode)
      (org-latex-preview '(64))
      (org-latex-preview '(16)))
    (message "LaTeX preview scale auto-adjusted to %.2f (Display: %s, DPI: %d)"
             bv-org-latex-scale
             (or (plist-get display-info :name) "Unknown")
             (plist-get display-info :dpi))))

(defun bv-org-latex-monitor-change-hook (&optional _frame)
  "Hook to run when monitor configuration change."
  (when (and (derived-mode-p 'org-mode) bv-org-latex-auto-scale)
    (let* ((old-scale bv-org-latex-scale)
           (display-info (bv-org-latex--get-display-info))
           (new-scale (bv-org-latex--calculate-scale-for-display display-info)))
      (unless (= old-scale new-scale)
        (setq bv-org-latex-scale new-scale)
        (plist-put org-format-latex-options :scale bv-org-latex-scale)
        (message "Display changed to %s (DPI: %d) - LaTeX scale: %.2f → %.2f"
                 (or (plist-get display-info :name) "Unknown")
                 (plist-get display-info :dpi)
                 old-scale new-scale)))))

;; Key bindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x +") #'bv-org-latex-increase-scale)
  (define-key org-mode-map (kbd "C-c C-x -") #'bv-org-latex-decrease-scale)
  (define-key org-mode-map (kbd "C-c C-x 0") #'bv-org-latex-reset-scale))

;; Initialize on load
(with-eval-after-load 'org
  (bv-org-latex-setup))

;; Also run in org-mode-hook for buffers opened later
(add-hook 'org-mode-hook #'bv-org-latex-setup)

;; Hook into display change events
(add-hook 'window-configuration-change-hook #'bv-org-latex-monitor-change-hook)
(add-hook 'after-make-frame-functions #'bv-org-latex-monitor-change-hook)
(add-function :after after-focus-change-function #'bv-org-latex-monitor-change-hook)

;; Handle moving frames between monitors
(add-hook 'move-frame-functions #'bv-org-latex-monitor-change-hook)

;; Surface preview failures with cache hint
(defun bv-org-latex--report-preview-failure (orig-fun &rest args)
  "Wrap `org-create-formula-image' to report failures with cache hints."
  (condition-case err
      (apply orig-fun args)
    (error
     (message "Org LaTeX preview failed (%s). Check logs in %s"
              (error-message-string err)
              (bv-org-latex--get-cache-dir))
     (signal (car err) (cdr err)))))

(advice-add 'org-create-formula-image :around #'bv-org-latex--report-preview-failure)

(provide 'bv-org-latex)
;;; bv-org-latex.el ends here
