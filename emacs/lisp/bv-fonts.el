;;; bv-fonts.el --- Font configuration and optimization -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; Comprehensive font configuration for optimal rendering and readability.
;; Provides sharp, crystal-clear text rendering with carefully chosen fonts
;; for different contexts.

;;; Code:

;; `cl-find-if' and friends.
(require 'cl-lib)

;; External function declarations
(declare-function ligature-set-ligatures "ligature" (modes ligature-list))
(declare-function global-ligature-mode "ligature" (&optional arg))

;; External variable declarations
(defvar x-gtk-use-system-tooltips)
(defvar x-gtk-antialiasing-mode)
(defvar cairo-font-options)

(defgroup bv-fonts nil
  "Font configuration and rendering optimization."
  :group 'faces)

(defcustom bv-fonts-default-family "JetBrains Mono"
  "Default monospace font family for coding."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-monospace-fallbacks
  '("JetBrains Mono"
    "Fira Code"
    "Iosevka Term"
    "Iosevka Fixed"
    "Iosevka"
    "Inconsolata"
    "DejaVu Sans Mono")
  "Fallback monospace font families used when `bv-fonts-default-family' is unavailable."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-default-size 120
  "Default font size in 1/10 pt units."
  :type 'integer
  :group 'bv-fonts)

(defcustom bv-fonts-variable-family "FiraGO"
  "Default proportional font family for UI and prose."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-variable-fallbacks
  '("FiraGO"
    "Fira Sans"
    "Noto Sans"
    "Cantarell")
  "Fallback proportional font families used when `bv-fonts-variable-family' is unavailable."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-serif-family "Noto Serif"
  "Default serif font family for documents."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-serif-fallbacks
  '("Noto Serif"
    "TeX Gyre Termes"
    "DejaVu Serif"
    "Bitstream Vera Serif")
  "Fallback serif font families used when `bv-fonts-serif-family' is unavailable."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-math-fallbacks
  '("Noto Sans Math" "DejaVu Math TeX Gyre")
  "Fallback font families for mathematical symbols."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-symbol-fallbacks
  '("Noto Sans Symbols 2" "Symbols Nerd Font Mono" "DejaVu Sans")
  "Fallback font families for miscellaneous symbols."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-cjk-fallbacks
  '("Sarasa Gothic SC" "Sarasa UI SC" "Sarasa Mono SC" "Noto Sans CJK SC" "Noto Sans")
  "Fallback font families for CJK character sets."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-enable-ligatures t
  "Enable font ligatures if supported."
  :type 'boolean
  :group 'bv-fonts)

(defcustom bv-fonts-dpi-scaling t
  "When non-nil, scale font size per monitor DPI for consistent physical size.

This is applied per frame using monitor information from
`frame-monitor-attributes'."
  :type 'boolean
  :group 'bv-fonts)

(defcustom bv-fonts-dpi-reference-ppi nil
  "Reference monitor PPI for `bv-fonts-dpi-scaling'.

When nil, the first detected GUI frame's monitor PPI becomes the reference."
  :type '(choice (const :tag "Auto" nil)
                 (number :tag "Reference PPI"))
  :group 'bv-fonts)

(defcustom bv-fonts-display-overrides nil
  "Per-display overrides for DPI scaling.

Each entry is (REGEXP . PLIST) matched against monitor `name' from
`frame-monitor-attributes'.

Supported keys in PLIST:
- `:size'  (integer) Base size in 1/10 pt (overrides `bv-fonts-default-size')
- `:scale' (number)  Extra multiplier applied after DPI scaling."
  :type '(repeat (cons (string :tag "Monitor name regexp")
                       (plist :options ((:size integer)
                                        (:scale number)))))
  :group 'bv-fonts)

(defvar bv-fonts--base-size nil
  "Session base font size in units of 1/10 pt.

This is the \"reference monitor\" size used before DPI scaling.")

(defvar bv-fonts--reference-ppi nil
  "Auto-detected reference PPI used when `bv-fonts-dpi-reference-ppi' is nil.")

(defvar bv-fonts--rendering-setup nil
  "Non-nil once GUI rendering options have been applied.")

(defvar bv-fonts--configured nil
  "Non-nil once base font faces and unicode fallbacks are configured for GUI.")

(defvar bv-fonts--monitor-signatures (make-hash-table :test 'eq)
  "Hash table mapping frames to last monitor signatures.")

(defun bv-fonts--base-size ()
  "Return the current session base font size in 1/10 pt."
  (or bv-fonts--base-size bv-fonts-default-size))

(defun bv-fonts--frame-monitor-attributes (frame)
  "Return monitor attributes for FRAME or nil."
  (when (and (fboundp 'frame-monitor-attributes) (frame-live-p frame))
    (frame-monitor-attributes frame)))

(defun bv-fonts--frame-monitor-name (frame)
  "Return monitor name for FRAME, or an empty string."
  (let ((attrs (bv-fonts--frame-monitor-attributes frame)))
    (or (and attrs (alist-get 'name attrs))
        "")))

(defun bv-fonts--frame-monitor-signature (frame)
  "Return a stable signature for FRAME's current monitor."
  (let* ((attrs (bv-fonts--frame-monitor-attributes frame))
         (name (and attrs (alist-get 'name attrs)))
         (geometry (and attrs (alist-get 'geometry attrs))))
    (list name geometry)))

(defun bv-fonts--frame-ppi (frame)
  "Estimate FRAME monitor PPI using `frame-monitor-attributes'."
  (let* ((attrs (bv-fonts--frame-monitor-attributes frame))
         (geometry (and attrs (alist-get 'geometry attrs)))
         (mm-size (and attrs (alist-get 'mm-size attrs)))
         (px-width (and (consp geometry) (nth 2 geometry)))
         (mm-width (and (consp mm-size) (car mm-size))))
    (when (and (numberp px-width) (numberp mm-width)
               (> px-width 0) (> mm-width 0))
      (/ (float px-width) (/ (float mm-width) 25.4)))))

(defun bv-fonts--reference-ppi (frame)
  "Return reference PPI for FRAME."
  (or (and (numberp bv-fonts-dpi-reference-ppi) bv-fonts-dpi-reference-ppi)
      (and (numberp bv-fonts--reference-ppi) bv-fonts--reference-ppi)
      (let ((ppi (bv-fonts--frame-ppi frame)))
        (when (numberp ppi)
          (setq bv-fonts--reference-ppi ppi))
        (or ppi 96.0))))

(defun bv-fonts--display-override (frame)
  "Return matching override entry for FRAME, or nil."
  (let ((name (bv-fonts--frame-monitor-name frame)))
    (cl-find-if (lambda (entry)
                  (and (stringp (car entry))
                       (stringp name)
                       (string-match-p (car entry) name)))
                bv-fonts-display-overrides)))

(defun bv-fonts--scaled-height (frame)
  "Compute a frame-local `default' face height for FRAME."
  (let* ((override (bv-fonts--display-override frame))
         (plist (cdr override))
         (base (or (plist-get plist :size) (bv-fonts--base-size)))
         (scale (if (and bv-fonts-dpi-scaling (display-graphic-p frame))
                    (let ((ppi (bv-fonts--frame-ppi frame))
                          (ref (bv-fonts--reference-ppi frame)))
                      (if (and (numberp ppi) (numberp ref) (> ref 0))
                          (/ ppi ref)
                        1.0))
                  1.0))
         (extra (plist-get plist :scale))
         (scale (if (numberp extra) (* scale extra) scale)))
    (max 1 (round (* base scale)))))

(defun bv-fonts-apply-to-frame (&optional frame)
  "Apply per-frame font settings to FRAME.

This primarily adjusts the `default' face height for DPI consistency."
  (let ((frame (or frame (selected-frame))))
    (when (and (framep frame) (frame-live-p frame) (display-graphic-p frame))
      (let ((height (bv-fonts--scaled-height frame)))
        (set-face-attribute 'default frame :height height)))))

(defun bv-fonts-apply-to-frames ()
  "Apply per-frame font settings to all live frames."
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (bv-fonts-apply-to-frame frame))))

(defun bv-fonts--maybe-rescale-frame (frame)
  "Rescale fonts for FRAME if its monitor has changed."
  (when (and (framep frame) (frame-live-p frame) (display-graphic-p frame))
    (let* ((sig (bv-fonts--frame-monitor-signature frame))
           (prev (gethash frame bv-fonts--monitor-signatures)))
      (unless (equal sig prev)
        (puthash frame sig bv-fonts--monitor-signatures)
        (bv-fonts-apply-to-frame frame)))))

(defun bv-fonts--configure-info-faces ()
  "Configure face attributes used by Info buffers."
  (let ((mono-family bv-fonts-default-family)
        (serif-family bv-fonts-serif-family))
    (when (facep 'Info-quoted)
      (set-face-attribute 'Info-quoted nil
                          :family mono-family
                          :height 0.95
                          :weight 'medium))
    (when (facep 'info-node)
      (set-face-attribute 'info-node nil
                          :family serif-family
                          :height 1.2
                          :weight 'semibold))
    (when (facep 'info-title-1)
      (set-face-attribute 'info-title-1 nil
                          :family serif-family
                          :height 1.8
                          :weight 'bold))
    (when (facep 'info-title-2)
      (set-face-attribute 'info-title-2 nil
                          :family serif-family
                          :height 1.5
                          :weight 'bold))
    (when (facep 'info-title-3)
      (set-face-attribute 'info-title-3 nil
                          :family serif-family
                          :height 1.3
                          :weight 'semibold))
    (when (facep 'info-title-4)
      (set-face-attribute 'info-title-4 nil
                          :family serif-family
                          :height 1.1
                          :weight 'semibold))))

(with-eval-after-load 'info
  (bv-fonts--configure-info-faces))

(defun bv-fonts--font-available-p (family)
  "Return non-nil when font FAMILY is available."
  (and (stringp family)
       (> (length family) 0)
       (find-font (font-spec :family family))))

(defun bv-fonts--resolve-family (preferred fallbacks)
  "Return a usable font family name.
PREFERRED is the user preference. FALLBACKS is a list of candidate families."
  (or (and (bv-fonts--font-available-p preferred) preferred)
      (cl-find-if #'bv-fonts--font-available-p fallbacks)
      preferred))

(defun bv-fonts--set-fontset-font (target charset family &rest args)
  "Set font FAMILY for CHARSET in fontset TARGET when available.
ARGS are forwarded to `set-fontset-font'."
  (when (bv-fonts--font-available-p family)
    (apply #'set-fontset-font target charset family args)))

(defun bv-fonts-setup-rendering ()
  "Configure font rendering for optimal clarity."
  (when (display-graphic-p)
    ;; Enable subpixel antialiasing for LCD screens
    (setq-default x-gtk-use-system-tooltips t)

    ;; Optimize font rendering
    (when (boundp 'x-gtk-antialiasing-mode)
      (setq x-gtk-antialiasing-mode 'subpixel))

    ;; Set font rendering hints
    (when (boundp 'cairo-font-options)
      (setq cairo-font-options
            '(:antialias subpixel
              :hint-style full
              :lcd-filter lcddefault)))

    ;; Disable font rescaling
    (setq face-font-rescale-alist nil)

    ;; Enable ligatures if requested and available
    (when (and bv-fonts-enable-ligatures
               (fboundp 'ligature-mode))
      ;; Enable the www ligature in every possible major mode
      (ligature-set-ligatures 't '("www"))
      ;; Enable ligatures in programming modes
      (ligature-set-ligatures
       'prog-mode
       '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
         "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
         "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
         "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
         ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
         "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
         "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
         "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
         ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
         "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
         "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
         "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"))
      (global-ligature-mode 1))))

(defun bv-fonts-configure-faces ()
  "Configure all font faces for optimal readability."
  (let* ((mono-family (bv-fonts--resolve-family
                       bv-fonts-default-family
                       bv-fonts-monospace-fallbacks))
         (variable-family (bv-fonts--resolve-family
                           bv-fonts-variable-family
                           bv-fonts-variable-fallbacks))
         (serif-family (bv-fonts--resolve-family
                         bv-fonts-serif-family
                         bv-fonts-serif-fallbacks)))
    (setq bv-fonts-default-family mono-family
          bv-fonts-variable-family variable-family
          bv-fonts-serif-family serif-family)

    ;; Default face - primary coding font
    (set-face-attribute 'default nil
                        :family mono-family
                        :height (bv-fonts--base-size)
                        :weight 'regular
                        :width 'normal)

    ;; Fixed-pitch - same as default for consistency
    (set-face-attribute 'fixed-pitch nil
                        :family mono-family
                        :height 1.0
                        :weight 'regular)

    ;; Fixed-pitch-serif - alternative monospace with serifs
    (set-face-attribute 'fixed-pitch-serif nil
                        :family (bv-fonts--resolve-family "Fira Mono" (list mono-family))
                        :height 1.0
                        :weight 'regular
                        :slant 'normal)

    ;; Variable-pitch - for UI elements and prose
    (set-face-attribute 'variable-pitch nil
                        :family variable-family
                        :height 1.05
                        :weight 'regular)

    ;; Variable-pitch-text - for larger text blocks
    (set-face-attribute 'variable-pitch-text nil
                        :family serif-family
                        :height 1.1
                        :weight 'normal)

    (bv-fonts--configure-info-faces)))

(defun bv-fonts-setup-unicode ()
  "Configure fallback fonts for Unicode coverage."
  ;; Mathematical symbols
  (bv-fonts--set-fontset-font
   t 'mathematical
   (bv-fonts--resolve-family nil bv-fonts-math-fallbacks)
   nil 'prepend)

  ;; Emoji support
  (bv-fonts--set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend)

  ;; CJK support
  (dolist (charset '(kana han cjk-misc bopomofo))
    (bv-fonts--set-fontset-font
     t charset
     (bv-fonts--resolve-family nil bv-fonts-cjk-fallbacks)
     nil 'prepend))

  ;; Symbol fallbacks
  (dolist (family bv-fonts-symbol-fallbacks)
    (bv-fonts--set-fontset-font t 'symbol family nil 'append)))

(defun bv-fonts-adjust-size (increment)
  "Adjust the default font size by INCREMENT.
INCREMENT is in units of 1/10 pt."
  (interactive "nSize increment (1/10 pt): ")
  (setq bv-fonts--base-size (+ (bv-fonts--base-size) increment))
  (when (boundp 'bv-themes-font-size)
    (setq bv-themes-font-size (bv-fonts--base-size)))
  (bv-fonts-configure-faces)
  (bv-fonts-apply-to-frames)
  (message "Base font size: %d (this frame: %d)"
           (bv-fonts--base-size)
           (face-attribute 'default :height (selected-frame))))

(defun bv-fonts-increase-size ()
  "Increase font size by 10 units (1 pt)."
  (interactive)
  (bv-fonts-adjust-size 10))

(defun bv-fonts-decrease-size ()
  "Decrease font size by 10 units (1 pt)."
  (interactive)
  (bv-fonts-adjust-size -10))

(defun bv-fonts-reset-size ()
  "Reset font size to default."
  (interactive)
  (setq bv-fonts--base-size bv-fonts-default-size)
  (set-face-attribute 'default nil :height (bv-fonts--base-size))
  (when (boundp 'bv-themes-font-size)
    (setq bv-themes-font-size (bv-fonts--base-size)))
  (bv-fonts-configure-faces)
  (bv-fonts-apply-to-frames)
  (message "Base font size reset to %d" (bv-fonts--base-size)))

;; Key bindings
(global-set-key (kbd "C-+") #'bv-fonts-increase-size)
(global-set-key (kbd "C--") #'bv-fonts-decrease-size)
(global-set-key (kbd "C-0") #'bv-fonts-reset-size)

;; Alternative bindings for better compatibility
(global-set-key (kbd "C-=") #'bv-fonts-increase-size)
(global-set-key (kbd "C-_") #'bv-fonts-decrease-size)

;; Initialize font configuration
(defun bv-fonts-init ()
  "Initialize font configuration."
  (unless bv-fonts--base-size
    (setq bv-fonts--base-size bv-fonts-default-size))
  (when (boundp 'bv-themes-font-size)
    (setq bv-themes-font-size (bv-fonts--base-size)))
  (when (display-graphic-p)
    (when (not bv-fonts--rendering-setup)
      (setq bv-fonts--rendering-setup t)
      (bv-fonts-setup-rendering))
    (unless bv-fonts--configured
      (bv-fonts-configure-faces)
      (bv-fonts-setup-unicode)
      (setq bv-fonts--configured t))
    (bv-fonts--maybe-rescale-frame (selected-frame))))

(add-hook 'window-size-change-functions #'bv-fonts--maybe-rescale-frame)

(add-hook 'after-make-frame-functions #'bv-fonts--maybe-rescale-frame)

(with-eval-after-load 'bv-themes
  (add-hook 'bv-themes-after-load-theme-hook #'bv-fonts-apply-to-frames))

;; Setup fonts after frame creation for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (bv-fonts-init))))
  (bv-fonts-init))

(provide 'bv-fonts)
;;; bv-fonts.el ends here
