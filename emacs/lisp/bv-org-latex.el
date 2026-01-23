;;; bv-org-latex.el --- Org mode LaTeX preview configuration with multi-display support -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>
;; Package-Requires: ((emacs "27.1") (org "9.6"))
;; Version: 2.0

;;; Commentary:
;; Enhanced LaTeX preview configuration for Org mode with automatic scaling
;; for multiple displays (laptop, TV) using TeX Gyre Pagella Math font.

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup bv-org-latex nil
  "LaTeX preview settings for Org mode with multi-display support."
  :group 'bv)

(defcustom bv-org-latex-scale 0.95
  "Scale factor for LaTeX previews."
  :type 'number
  :group 'bv-org-latex)

(defconst bv-org-latex-default-scale 0.95
  "Default scale factor for reset.")

(defcustom bv-org-latex-auto-scale t
  "Whether to automatically scale LaTeX previews based on display."
  :type 'boolean
  :group 'bv-org-latex)

(defcustom bv-org-latex-use-monitor-dpi t
  "Whether to use per-monitor DPI for Org LaTeX preview generation.

Org's built-in `org--get-display-dpi' uses `display-mm-height', which is not
reliable on multi-monitor setups (or when the compositor applies scaling).
When non-nil, this module advises `org--get-display-dpi' to use DPI derived
from `frame-monitor-attributes' and `bv-org-latex-display-overrides'."
  :type 'boolean
  :group 'bv-org-latex)

(defcustom bv-org-latex-base-dpi 96
  "Base DPI for scaling calculations (standard monitor DPI)."
  :type 'number
  :group 'bv-org-latex)

(defcustom bv-org-latex-display-profiles
  '((laptop   . ((min-dpi . 250) (viewing-distance-factor . 1.0)))
    (tv       . ((min-dpi . 0)   (viewing-distance-factor . 1.2))))
  "Display profiles with DPI ranges and viewing distance factors."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-org-latex)

(defcustom bv-org-latex-display-overrides
  '(;; Update these with your actual display names
    ;; Run M-x bv-org-latex-list-displays to find your display names
    ("DP-1" . (:dpi 49 :type tv))        ; LG 45" 1080p TV
    ("eDP-1" . (:dpi 162 :type laptop))) ; Laptop 14" 1920x1200 (scaled)
  "Manual display overrides for specific displays.

Each entry is (MONITOR-NAME . PLIST).  Supported PLIST keys:
- `:dpi'   (number) Override effective DPI used by Org previews.
- `:type'  (symbol) Display profile key from `bv-org-latex-display-profiles'.
- `:scale' (number) Extra multiplier applied after auto scaling.

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

(defun bv-org-latex--get-current-monitor-name (&optional frame)
  "Get the name of the current monitor for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (attrs (frame-monitor-attributes frame)))
    (alist-get 'name attrs)))

(defun bv-org-latex--normalize-display-override (override)
  "Normalize OVERRIDE into a keyword plist.

Historically, `bv-org-latex-display-overrides' used an alist like:
  ((dpi . 162) (type . laptop))

The current format is a plist:
  (:dpi 162 :type laptop :scale 1.2)"
  (cond
   ((null override) nil)
   ;; Already a plist.
   ((and (consp override) (keywordp (car override))) override)
   ;; Convert legacy alist.
   ((and (listp override) (consp (car override)))
    (let ((dpi (alist-get 'dpi override))
          (type (alist-get 'type override))
          (scale (alist-get 'scale override))
          (plist nil))
      (when (numberp dpi)
        (setq plist (plist-put plist :dpi dpi)))
      (when (symbolp type)
        (setq plist (plist-put plist :type type)))
      (when (numberp scale)
        (setq plist (plist-put plist :scale scale)))
      plist))
   (t nil)))

(defun bv-org-latex--calculate-dpi (pixels mm &optional scale-factor)
  "Calculate DPI from PIXELS and MM.

PIXELS is the monitor dimension in pixels (width or height).  MM is the
corresponding physical dimension in millimeters.

When SCALE-FACTOR is a positive number, it is applied to PIXELS to account
for compositor scaling."
  (when (and (numberp pixels) (numberp mm) (> pixels 0) (> mm 0))
    (let ((factor (if (and (numberp scale-factor) (> scale-factor 0))
                      scale-factor
                    1.0)))
      (round (/ (* pixels factor 25.4) mm)))))

(defun bv-org-latex--calculate-monitor-dpi (geometry mm-size scale-factor)
  "Calculate DPI from GEOMETRY, MM-SIZE and SCALE-FACTOR.

GEOMETRY is the monitor geometry list from `frame-monitor-attributes'.
MM-SIZE is the physical size entry from `frame-monitor-attributes'.
SCALE-FACTOR is the monitor scale factor (usually 1 or 2)."
  (let* ((px-width (and (consp geometry) (nth 2 geometry)))
         (px-height (and (consp geometry) (nth 3 geometry)))
         (mm-width (cond
                    ((and (consp mm-size)
                          (numberp (car mm-size))
                          (numberp (cdr mm-size)))
                     (car mm-size))
                    ((and (listp mm-size) (>= (length mm-size) 1))
                     (nth 0 mm-size))
                    (t nil)))
         (mm-height (cond
                     ((and (consp mm-size)
                           (numberp (car mm-size))
                           (numberp (cdr mm-size)))
                      (cdr mm-size))
                     ((and (listp mm-size) (>= (length mm-size) 2))
                      (nth 1 mm-size))
                     (t nil)))
         (dpi-x (bv-org-latex--calculate-dpi px-width mm-width scale-factor))
         (dpi-y (bv-org-latex--calculate-dpi px-height mm-height scale-factor)))
    (cond
     ((and (numberp dpi-x) (numberp dpi-y))
      (round (/ (+ dpi-x dpi-y) 2.0)))
     ((numberp dpi-x) dpi-x)
     ((numberp dpi-y) dpi-y)
     (t nil))))

(defun bv-org-latex--detect-display-type (dpi)
  "Detect display type based on DPI."
  (if (>= dpi 250)
      'laptop
    'tv))

(defun bv-org-latex--get-display-info (&optional frame)
  "Get comprehensive display information with fallbacks."
  (let* ((frame (or frame (selected-frame)))
         (monitor-name (bv-org-latex--get-current-monitor-name frame))
         (override-raw (cdr (assoc monitor-name bv-org-latex-display-overrides)))
         (override (bv-org-latex--normalize-display-override override-raw))
         (attrs (frame-monitor-attributes frame))
         (geometry (alist-get 'geometry attrs))
         (mm-size (alist-get 'mm-size attrs))
         (scale-factor (or (alist-get 'scale-factor attrs) 1.0))
         (pixel-width (and geometry (nth 2 geometry)))
         (pixel-height (and geometry (nth 3 geometry)))
         (mm-width (and (consp mm-size) (car mm-size)))
         (mm-height (and (consp mm-size) (cdr mm-size)))
         ;; Try multiple methods to get DPI
         (emacs-pixel-height (and (display-graphic-p) (display-pixel-height)))
         (emacs-mm-height (and (display-graphic-p) (display-mm-height)))
         (dpi-from-emacs
          (when (and emacs-pixel-height
                     emacs-mm-height
                     (> emacs-mm-height 0))
            (round (/ emacs-pixel-height (/ emacs-mm-height 25.4)))))
         (dpi-from-monitor
          (bv-org-latex--calculate-monitor-dpi geometry mm-size scale-factor))
         (detected-dpi (or
                        ;; 1. Use override if available
                        (plist-get override :dpi)
                        ;; 2. Calculate from monitor attributes
                        dpi-from-monitor
                        ;; 3. Try Emacs display info when reliable
                        dpi-from-emacs
                        ;; 4. Default fallback
                        bv-org-latex-base-dpi))
         (display-type (or (plist-get override :type)
                          (bv-org-latex--detect-display-type detected-dpi))))
    (list :name monitor-name
          :dpi detected-dpi
          :type display-type
          :scale-factor scale-factor
          :override-scale (plist-get override :scale)
          :pixel-width pixel-width
          :pixel-height pixel-height
          :mm-width mm-width
          :mm-height mm-height)))

(defun bv-org-latex--calculate-scale-for-display (display-info)
  "Calculate optimal scale based on DISPLAY-INFO characteristics."
  (let* ((display-type (plist-get display-info :type))
         (profile (alist-get display-type bv-org-latex-display-profiles))
         (viewing-factor (or (alist-get 'viewing-distance-factor profile) 1.0))
         (override-scale (or (plist-get display-info :override-scale) 1.0))
         ;; Get font size factor
         (font-height (face-attribute 'default :height))
         (font-scale (/ font-height 120.0))  ; Normalized to 12pt
         (base-scale (* bv-org-latex-default-scale font-scale))
         ;; Apply viewing distance correction
         (final-scale (* base-scale viewing-factor override-scale)))
    ;; Ensure reasonable bounds with different minimums per display type
    (let ((min-scale (if (eq display-type 'tv) 1.1 0.9)))
      (max min-scale (min 3.0 final-scale)))))

(defun bv-org-latex--get-display-dpi ()
  "Get current display DPI with enhanced detection."
  (plist-get (bv-org-latex--get-display-info) :dpi))

(defun bv-org-latex--calculate-auto-scale ()
  "Calculate optimal scale for current display."
  (let ((display-info (bv-org-latex--get-display-info)))
    (bv-org-latex--calculate-scale-for-display display-info)))

(defun bv-org-latex--org-display-dpi (&optional frame)
  "Return the effective Org LaTeX preview DPI for FRAME."
  (plist-get (bv-org-latex--get-display-info frame) :dpi))

(defun bv-org-latex--override-org-display-dpi (orig-fun &rest args)
  "Advice for `org--get-display-dpi' using monitor-aware DPI detection."
  (if (and bv-org-latex-use-monitor-dpi (display-graphic-p))
      (let ((dpi (bv-org-latex--org-display-dpi (selected-frame))))
        (if (and (numberp dpi) (> dpi 0))
            dpi
          (apply orig-fun args)))
    (apply orig-fun args)))

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

(defun bv-org-latex--buffer-has-previews-p ()
  "Return non-nil when current Org buffer has LaTeX preview overlays."
  (and (derived-mode-p 'org-mode)
       (cl-some (lambda (ov)
                  (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
                (overlays-in (point-min) (point-max)))))

(defvar-local bv-org-latex--last-preview-dpi nil
  "DPI used for the last Org LaTeX preview render in this buffer.")

(defvar-local bv-org-latex--last-preview-scale nil
  "Scale used for the last Org LaTeX preview render in this buffer.")

(defun bv-org-latex--record-preview-settings (&rest _args)
  "Record the current preview DPI/scale in the active Org buffer."
  (when (derived-mode-p 'org-mode)
    (setq bv-org-latex--last-preview-dpi (bv-org-latex--org-display-dpi (selected-frame))
          bv-org-latex--last-preview-scale
          (or (plist-get org-format-latex-options :scale) 1.0))))

(defun bv-org-latex--preview-stale-p (&optional frame)
  "Return non-nil when previews in current Org buffer are stale for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (current-dpi (bv-org-latex--org-display-dpi frame))
         (current-scale (or (plist-get org-format-latex-options :scale) 1.0)))
    (or (not (numberp bv-org-latex--last-preview-dpi))
        (not (numberp bv-org-latex--last-preview-scale))
        (not (numberp current-dpi))
        (> (abs (- current-dpi bv-org-latex--last-preview-dpi)) 1)
        (> (abs (- current-scale bv-org-latex--last-preview-scale)) 0.01))))

(defun bv-org-latex--maybe-refresh-buffer (frame)
  "Refresh Org LaTeX previews in current buffer when needed for FRAME."
  (when (and (derived-mode-p 'org-mode) (display-graphic-p frame))
    (when bv-org-latex-auto-scale
      (let* ((display-info (bv-org-latex--get-display-info frame))
             (new-scale (bv-org-latex--calculate-scale-for-display display-info)))
        (setq bv-org-latex-scale new-scale)
        (plist-put org-format-latex-options :scale bv-org-latex-scale)))
    (when (and (bv-org-latex--buffer-has-previews-p)
               (bv-org-latex--preview-stale-p frame))
      (bv-org-latex--schedule-refresh (current-buffer) frame))))

(defun bv-org-latex--maybe-refresh-window-buffer (window _prev-buffer)
  "Refresh Org LaTeX previews for WINDOW's current buffer when needed."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (bv-org-latex--maybe-refresh-buffer (window-frame window)))))

(defun bv-org-latex-refresh-previews (&optional buffer)
  "Refresh LaTeX previews in BUFFER.

When BUFFER is nil, operate on the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      (let ((inhibit-message t))
        (org-latex-preview '(64))
        (org-latex-preview '(16))))))

;; Scale adjustment commands
(defun bv-org-latex-increase-scale ()
  "Increase LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-auto-scale nil)  ; Disable auto-scale when manually adjusting
  (setq bv-org-latex-scale (* bv-org-latex-scale 1.1))
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (bv-org-latex-refresh-previews (current-buffer))
  (message "LaTeX preview scale: %.2f (auto-scale disabled)" bv-org-latex-scale))

(defun bv-org-latex-decrease-scale ()
  "Decrease LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-auto-scale nil)  ; Disable auto-scale when manually adjusting
  (setq bv-org-latex-scale (/ bv-org-latex-scale 1.1))
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (bv-org-latex-refresh-previews (current-buffer))
  (message "LaTeX preview scale: %.2f (auto-scale disabled)" bv-org-latex-scale))

(defun bv-org-latex-reset-scale ()
  "Reset LaTeX preview scale to auto-calculated value."
  (interactive)
  (setq bv-org-latex-auto-scale t)  ; Re-enable auto-scale
  (let ((display-info (bv-org-latex--get-display-info)))
    (setq bv-org-latex-scale (bv-org-latex--calculate-scale-for-display display-info))
    (plist-put org-format-latex-options :scale bv-org-latex-scale)
    (bv-org-latex-refresh-previews (current-buffer))
    (message "LaTeX preview scale auto-adjusted to %.2f (Display: %s, DPI: %d)"
             bv-org-latex-scale
             (or (plist-get display-info :name) "Unknown")
             (plist-get display-info :dpi))))

(defvar bv-org-latex--monitor-signatures (make-hash-table :test 'eq)
  "Hash table mapping frames to their last monitor signatures.")

(defvar bv-org-latex--refresh-timer nil
  "Idle timer used to debounce LaTeX preview refreshes.")

(defun bv-org-latex--frame-monitor-signature (frame)
  "Return a stable monitor signature for FRAME."
  (let* ((attrs (frame-monitor-attributes frame))
         (name (alist-get 'name attrs))
         (geometry (alist-get 'geometry attrs))
         (mm-size (alist-get 'mm-size attrs))
         (scale-factor (alist-get 'scale-factor attrs)))
    (list name geometry mm-size scale-factor)))

(defun bv-org-latex--schedule-refresh (buffer frame)
  "Schedule a debounced LaTeX preview refresh for BUFFER on FRAME."
  (when (timerp bv-org-latex--refresh-timer)
    (cancel-timer bv-org-latex--refresh-timer))
  (setq bv-org-latex--refresh-timer
        (run-with-idle-timer
         0.25 nil
         (lambda (buf frm)
           (when (and (buffer-live-p buf) (frame-live-p frm))
             (with-selected-frame frm
               (with-current-buffer buf
                 (when (bv-org-latex--buffer-has-previews-p)
                   (bv-org-latex-refresh-previews buf))))))
         buffer frame)))

(defun bv-org-latex-monitor-change-hook (&optional frame)
  "Handle monitor configuration changes for Org LaTeX previews."
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame) (display-graphic-p frame))
      (let* ((sig (bv-org-latex--frame-monitor-signature frame))
             (prev (gethash frame bv-org-latex--monitor-signatures)))
        (unless (equal sig prev)
          (puthash frame sig bv-org-latex--monitor-signatures)
          (when (derived-mode-p 'org-mode)
            (let* ((display-info (bv-org-latex--get-display-info frame))
                   (old-scale bv-org-latex-scale)
                   (new-scale (if bv-org-latex-auto-scale
                                  (bv-org-latex--calculate-scale-for-display display-info)
                                old-scale)))
              (when bv-org-latex-auto-scale
                (setq bv-org-latex-scale new-scale)
                (plist-put org-format-latex-options :scale bv-org-latex-scale))
              (bv-org-latex--maybe-refresh-buffer frame)
              (message "Display changed to %s (DPI: %d) - LaTeX scale: %.2f → %.2f"
                       (or (plist-get display-info :name) "Unknown")
                       (plist-get display-info :dpi)
                       old-scale new-scale))))))))

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

(with-eval-after-load 'org
  (when (fboundp 'org--get-display-dpi)
    (advice-add 'org--get-display-dpi :around #'bv-org-latex--override-org-display-dpi)))

(with-eval-after-load 'org
  (advice-add 'org-latex-preview :after #'bv-org-latex--record-preview-settings))

(if (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'bv-org-latex--maybe-refresh-window-buffer)
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (bv-org-latex--maybe-refresh-buffer (selected-frame)))))

(provide 'bv-org-latex)
;;; bv-org-latex.el ends here
