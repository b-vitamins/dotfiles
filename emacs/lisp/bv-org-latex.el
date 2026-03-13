;;; bv-org-latex.el --- Org mode LaTeX preview configuration with multi-display support -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>
;; Package-Requires: ((emacs "27.1") (org "9.6"))
;; Version: 2.0

;;; Commentary:
;; Enhanced LaTeX preview configuration for Org mode with explicit
;; per-display preview scaling and a fast-preview integration path.

;;; Code:

(require 'org)
(require 'cl-lib)

;; Optional external fast preview package.  These declarations keep this file
;; byte-compilable even when the package is installed and loaded separately.
(defvar org-fast-latex-preview-cache-directory)
(defvar org-fast-latex-preview-dpi-function)
(defvar org-fast-latex-preview-mode)
(declare-function org-fast-latex-preview-mode "org-fast-latex-preview" (&optional arg))
(declare-function org-fast-latex-preview-buffer "org-fast-latex-preview" (&optional refresh))
(declare-function org-fast-latex-preview "org-fast-latex-preview" (&optional arg))
(declare-function org-fast-latex-preview-region "org-fast-latex-preview"
                  (beg end &optional refresh))
(declare-function org-fast-latex-preview-subtree "org-fast-latex-preview"
                  (&optional refresh))
(declare-function org-fast-latex-preview-at-point "org-fast-latex-preview"
                  (&optional refresh))
(declare-function org-fast-latex-preview-refresh "org-fast-latex-preview"
                  (&optional arg))
(declare-function org-fragtog-mode "org-fragtog" (&optional arg))

(defgroup bv-org-latex nil
  "LaTeX preview settings for Org mode with multi-display support."
  :group 'bv)

(defcustom bv-org-latex-default-scale 0.95
  "Fallback scale factor for LaTeX previews."
  :type 'number
  :group 'bv-org-latex)

(defvar-local bv-org-latex-scale bv-org-latex-default-scale
  "Current buffer's effective LaTeX preview scale.")

(defcustom bv-org-latex-auto-scale t
  "Whether to follow the configured per-display preview scale automatically."
  :type 'boolean
  :group 'bv-org-latex)

(defcustom bv-org-latex-render-baseline 140.0
  "Effective baseline used for preview renderers.

This is not intended to model physical monitor DPI.  It is a stable
render baseline used by the fast preview path so per-display tuning is
expressed through preview scale alone."
  :type 'number
  :group 'bv-org-latex)

(defcustom bv-org-latex-display-overrides
  '(;; Update these with your actual display names
    ;; Run M-x bv-org-latex-list-displays to find your display names
    ("LG TV" . (:scale 2.0))          ; LG 45\" 1080p TV, far viewing distance
    ("BenQ RD240Q" . (:scale 1.7))    ; BenQ 24\" 4K monitor
    ("0x419f" . (:scale 1.1)))        ; ThinkPad X1 Carbon 14\" 1920x1200
  "Explicit preview settings for specific displays.

Each entry is (MONITOR-NAME . PLIST).  Supported PLIST keys:
- `:scale' (number) Preview scale used by both Org and OFLP.

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

(defcustom bv-org-latex-use-fast-preview t
  "Whether to use `org-fast-latex-preview' when it is available."
  :type 'boolean
  :group 'bv-org-latex)

(make-variable-buffer-local 'bv-org-latex-auto-scale)
(put 'bv-org-latex-scale 'permanent-local t)
(put 'bv-org-latex-auto-scale 'permanent-local t)

;; Utility functions
(defun bv-org-latex--get-cache-dir ()
  "Get the XDG-compliant cache directory."
  (expand-file-name "emacs/ltximg/"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state")))

(defun bv-org-latex--get-fast-preview-cache-dir ()
  "Return the cache directory used by `org-fast-latex-preview'."
  (expand-file-name "emacs/org-fast-latex-preview/"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state")))

(defvar bv-org-latex--global-configured nil
  "Non-nil once Org fallback preview settings have been configured.")

(defun bv-org-latex--copy-latex-options-for-buffer ()
  "Ensure the current buffer has its own LaTeX preview option plist."
  (unless (local-variable-p 'org-format-latex-options)
    (setq-local org-format-latex-options (copy-tree org-format-latex-options))))

(defun bv-org-latex--apply-buffer-preview-scale (&optional frame)
  "Apply the configured preview scale for FRAME to the current buffer."
  (let* ((display-info (bv-org-latex--get-display-info frame))
         (scale (if bv-org-latex-auto-scale
                    (or (plist-get display-info :scale)
                        bv-org-latex-default-scale)
                  (or bv-org-latex-scale
                      bv-org-latex-default-scale))))
    (setq-local bv-org-latex-scale scale)
    (bv-org-latex--copy-latex-options-for-buffer)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale bv-org-latex-scale))
    (setq org-format-latex-options
          (plist-put org-format-latex-options :background "Transparent"))
    (setq org-format-latex-options
          (plist-put org-format-latex-options :foreground 'default))
    bv-org-latex-scale))

(defun bv-org-latex--configure-org-preview-globally ()
  "Configure the shared Org preview fallback state."
  (unless bv-org-latex--global-configured
    (let ((cache-dir (bv-org-latex--get-cache-dir)))
      (unless (file-exists-p cache-dir)
        (make-directory cache-dir t))
      (setq org-preview-latex-image-directory cache-dir))

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

    (setq org-preview-latex-default-process 'luamagick)

    (dolist (pkg bv-org-latex-packages)
      (add-to-list 'org-latex-packages-alist pkg t))

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

    (setq bv-org-latex--global-configured t)))

(defun bv-org-latex--ensure-fast-preview ()
  "Load `org-fast-latex-preview' when configured and installed."
  (and bv-org-latex-use-fast-preview
       (require 'org-fast-latex-preview nil t)))

(defun bv-org-latex--configure-fast-preview ()
  "Apply local OFLP integration settings and enable it in the current Org buffer.

OFLP should derive compiler, header, package, and appearance decisions
from the active Org preview configuration.  This bridge therefore only
provides cache placement, a stable render baseline, and mode enablement."
  (when (bv-org-latex--ensure-fast-preview)
    (when (derived-mode-p 'org-mode)
      (org-fast-latex-preview-mode 1))
    t))

(defun bv-org-latex--fast-preview-active-p ()
  "Return non-nil when OFLP is active in the current buffer."
  (and (featurep 'org-fast-latex-preview)
       (bound-and-true-p org-fast-latex-preview-mode)))

(defun bv-org-latex--get-current-monitor-name (&optional frame)
  "Get the name of the current monitor for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (attrs (frame-monitor-attributes frame)))
    (alist-get 'name attrs)))

(defun bv-org-latex--normalize-display-override (override)
  "Normalize OVERRIDE into a keyword plist.

The supported format is a plist like `(:scale 1.2)'."
  (when (and (consp override) (keywordp (car override)))
    override))

(defun bv-org-latex--get-display-info (&optional frame)
  "Return explicit preview settings for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (monitor-name (bv-org-latex--get-current-monitor-name frame))
         (override-raw (cdr (assoc monitor-name bv-org-latex-display-overrides)))
         (override (bv-org-latex--normalize-display-override override-raw))
         (attrs (frame-monitor-attributes frame))
         (geometry (alist-get 'geometry attrs))
         (pixel-width (and geometry (nth 2 geometry)))
         (pixel-height (and geometry (nth 3 geometry)))
         (configured-scale (or (plist-get override :scale)
                               bv-org-latex-default-scale)))
    (list :name monitor-name
          :scale configured-scale
          :pixel-width pixel-width
          :pixel-height pixel-height)))

(defun bv-org-latex--fast-preview-dpi ()
  "Return the stable render baseline used for OFLP."
  (float bv-org-latex-render-baseline))

;; User-facing functions
(defun bv-org-latex-show-display-info ()
  "Show the configured preview settings for the current display."
  (interactive)
  (let ((info (bv-org-latex--get-display-info)))
    (message "Display: %s | Preview scale: %.2f"
             (or (plist-get info :name) "Unknown")
             (or (plist-get info :scale) bv-org-latex-default-scale))))

(defun bv-org-latex-list-displays ()
  "List all available displays with their properties."
  (interactive)
  (let ((monitors (display-monitor-attributes-list)))
    (with-output-to-temp-buffer "*Display Information*"
      (princ "Available Displays:\n\n")
      (dolist (monitor monitors)
        (let* ((name (alist-get 'name monitor))
               (geometry (alist-get 'geometry monitor))
               (override-raw (cdr (assoc name bv-org-latex-display-overrides)))
               (override (bv-org-latex--normalize-display-override override-raw))
               (pixel-width (and geometry (nth 2 geometry)))
               (pixel-height (and geometry (nth 3 geometry)))
               (scale (or (plist-get override :scale) bv-org-latex-default-scale)))
          (princ (format "Display: %s\n" (or name "Unknown")))
          (princ (format "  Resolution: %dx%d pixels\n"
                         (or pixel-width 0) (or pixel-height 0)))
          (princ (format "  Preview scale: %.2f\n" scale))
          (princ (format "  Override: %s\n\n"
                         (if (assoc name bv-org-latex-display-overrides)
                             "Yes" "No"))))))))

(defun bv-org-latex-setup ()
  "Configure Org mode LaTeX preview settings for the current buffer."
  (bv-org-latex--configure-org-preview-globally)
  (when (derived-mode-p 'org-mode)
    (let* ((display-info (bv-org-latex--get-display-info))
           (scale (bv-org-latex--apply-buffer-preview-scale)))
      (message "LaTeX preview scale: %.2f (Display: %s)"
               scale
               (or (plist-get display-info :name) "Unknown"))))

  (if (bv-org-latex--configure-fast-preview)
      (progn
        (remove-hook 'org-mode-hook #'org-fragtog-mode)
        (when (bound-and-true-p org-fragtog-mode)
          (org-fragtog-mode -1)))
    ;; Use org-fragtog for automatic preview only when OFLP is unavailable.
    (when (require 'org-fragtog nil t)
      (add-hook 'org-mode-hook #'org-fragtog-mode))))

(defun bv-org-latex--buffer-has-previews-p ()
  "Return non-nil when current Org buffer has LaTeX preview overlays."
  (and (derived-mode-p 'org-mode)
       (cl-some (lambda (ov)
                  (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
                (overlays-in (point-min) (point-max)))))

(defvar-local bv-org-latex--last-preview-scale nil
  "Scale used for the last Org LaTeX preview render in this buffer.")

(defun bv-org-latex--record-preview-settings (&rest _args)
  "Record the current preview scale in the active Org buffer."
  (when (derived-mode-p 'org-mode)
    (setq bv-org-latex--last-preview-scale
          (or (plist-get org-format-latex-options :scale) 1.0))))

(defun bv-org-latex--preview-stale-p (&optional _frame)
  "Return non-nil when previews in current Org buffer are stale."
  (let ((current-scale (or (plist-get org-format-latex-options :scale)
                           bv-org-latex-scale
                           bv-org-latex-default-scale)))
    (or (not (numberp bv-org-latex--last-preview-scale))
        (> (abs (- current-scale bv-org-latex--last-preview-scale)) 0.01))))

(defun bv-org-latex--maybe-refresh-buffer (frame)
  "Refresh Org LaTeX previews in current buffer when needed for FRAME."
  (when (and (derived-mode-p 'org-mode) (display-graphic-p frame))
    (bv-org-latex--apply-buffer-preview-scale frame)
    (when (and (bv-org-latex--buffer-has-previews-p)
               (bv-org-latex--preview-stale-p frame))
      (bv-org-latex--schedule-refresh (current-buffer) frame))))

(defun bv-org-latex--maybe-refresh-window-buffer (window-or-frame &rest _args)
  "Refresh Org LaTeX previews for WINDOW-OR-FRAME when needed.

This is intended for `window-buffer-change-functions'.  Functions installed in
the hook's default value are called with a frame.  Buffer-local hook functions
are called with a window."
  (cond
   ((windowp window-or-frame)
    (when (window-live-p window-or-frame)
      (with-current-buffer (window-buffer window-or-frame)
        (bv-org-latex--maybe-refresh-buffer (window-frame window-or-frame)))))
   ((framep window-or-frame)
    (when (frame-live-p window-or-frame)
      (dolist (window (window-list window-or-frame 'no-minibuf))
        (when (window-live-p window)
          (with-current-buffer (window-buffer window)
            (bv-org-latex--maybe-refresh-buffer window-or-frame))))))))

(defun bv-org-latex--org-fragtog-clamp-prev-point (&rest _args)
  "Prevent `org-fragtog--post-cmd' from `goto-char' errors after buffer edits."
  (when (and (boundp 'org-fragtog--prev-point)
             (integerp org-fragtog--prev-point))
    (let ((min (point-min))
          (max (point-max)))
      (unless (<= min org-fragtog--prev-point max)
        (setq org-fragtog--prev-point (min max (max min org-fragtog--prev-point)))))))

(with-eval-after-load 'org-fragtog
  (unless (advice-member-p #'bv-org-latex--org-fragtog-clamp-prev-point
                           'org-fragtog--post-cmd)
    (advice-add 'org-fragtog--post-cmd :before
                #'bv-org-latex--org-fragtog-clamp-prev-point)))

(defun bv-org-latex-refresh-previews (&optional buffer)
  "Refresh LaTeX previews in BUFFER.

When BUFFER is nil, operate on the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'org-mode)
      (let ((inhibit-message t))
        (if (bv-org-latex--fast-preview-active-p)
            (org-fast-latex-preview-buffer t)
          (org-latex-preview '(64))
          (org-latex-preview '(16))))
      (bv-org-latex--record-preview-settings))))

;; Scale adjustment commands
(defun bv-org-latex-increase-scale ()
  "Increase LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-auto-scale nil)  ; Disable auto-scale when manually adjusting
  (setq bv-org-latex-scale (* (or bv-org-latex-scale bv-org-latex-default-scale) 1.1))
  (bv-org-latex--apply-buffer-preview-scale)
  (bv-org-latex-refresh-previews (current-buffer))
  (message "LaTeX preview scale: %.2f (auto-scale disabled)" bv-org-latex-scale))

(defun bv-org-latex-decrease-scale ()
  "Decrease LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-auto-scale nil)  ; Disable auto-scale when manually adjusting
  (setq bv-org-latex-scale (/ (or bv-org-latex-scale bv-org-latex-default-scale) 1.1))
  (bv-org-latex--apply-buffer-preview-scale)
  (bv-org-latex-refresh-previews (current-buffer))
  (message "LaTeX preview scale: %.2f (auto-scale disabled)" bv-org-latex-scale))

(defun bv-org-latex-reset-scale ()
  "Reset LaTeX preview scale to the configured value for the current display."
  (interactive)
  (setq bv-org-latex-auto-scale t)  ; Re-enable auto-scale
  (let ((display-info (bv-org-latex--get-display-info)))
    (setq bv-org-latex-scale (bv-org-latex--apply-buffer-preview-scale))
    (bv-org-latex-refresh-previews (current-buffer))
    (message "LaTeX preview scale reset to %.2f (Display: %s)"
             bv-org-latex-scale
             (or (plist-get display-info :name) "Unknown"))))

(defvar bv-org-latex--monitor-signatures (make-hash-table :test 'eq)
  "Hash table mapping frames to their last monitor signatures.")

(defvar bv-org-latex--refresh-timer nil
  "Idle timer used to debounce LaTeX preview refreshes.")

(defun bv-org-latex--frame-monitor-signature (frame)
  "Return the display identity relevant for preview settings on FRAME."
  (let* ((attrs (frame-monitor-attributes frame))
         (name (alist-get 'name attrs)))
    name))

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
          (dolist (window (window-list frame 'no-minibuf))
            (when (window-live-p window)
              (with-current-buffer (window-buffer window)
                (when (derived-mode-p 'org-mode)
                  (let* ((display-info (bv-org-latex--get-display-info frame))
                         (old-scale (or bv-org-latex-scale
                                        bv-org-latex-default-scale))
                         (new-scale (bv-org-latex--apply-buffer-preview-scale frame)))
                    (bv-org-latex--maybe-refresh-buffer frame)
                    (message "Display changed to %s - LaTeX scale: %.2f -> %.2f"
                             (or (plist-get display-info :name) "Unknown")
                             old-scale new-scale)))))))))))

;; Key bindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x +") #'bv-org-latex-increase-scale)
  (define-key org-mode-map (kbd "C-c C-x -") #'bv-org-latex-decrease-scale)
  (define-key org-mode-map (kbd "C-c C-x 0") #'bv-org-latex-reset-scale))

;; Initialize on load
(with-eval-after-load 'org
  (bv-org-latex--configure-org-preview-globally))

;; Also run in org-mode-hook for buffers opened later
(add-hook 'org-mode-hook #'bv-org-latex-setup)

;; Hook into display change events
(add-hook 'window-configuration-change-hook #'bv-org-latex-monitor-change-hook)
(add-hook 'after-make-frame-functions #'bv-org-latex-monitor-change-hook)
(remove-function after-focus-change-function
                 #'bv-org-latex-monitor-change-hook)
(add-function :after after-focus-change-function
              #'bv-org-latex-monitor-change-hook)

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

(unless (advice-member-p #'bv-org-latex--report-preview-failure
                         'org-create-formula-image)
  (advice-add 'org-create-formula-image :around
              #'bv-org-latex--report-preview-failure))

(with-eval-after-load 'org
  (unless (advice-member-p #'bv-org-latex--record-preview-settings
                           'org-latex-preview)
    (advice-add 'org-latex-preview :after
                #'bv-org-latex--record-preview-settings)))

(with-eval-after-load 'org-fast-latex-preview
  (let ((cache-dir (bv-org-latex--get-fast-preview-cache-dir)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    (setq org-fast-latex-preview-cache-directory cache-dir
          org-fast-latex-preview-dpi-function #'bv-org-latex--fast-preview-dpi))
  (dolist (fn '(org-fast-latex-preview
                org-fast-latex-preview-buffer
                org-fast-latex-preview-region
                org-fast-latex-preview-subtree
                org-fast-latex-preview-at-point
                org-fast-latex-preview-refresh))
    (unless (advice-member-p #'bv-org-latex--record-preview-settings fn)
      (advice-add fn :after #'bv-org-latex--record-preview-settings))))

(if (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'bv-org-latex--maybe-refresh-window-buffer)
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (bv-org-latex--maybe-refresh-buffer (selected-frame)))))

(provide 'bv-org-latex)
;;; bv-org-latex.el ends here
