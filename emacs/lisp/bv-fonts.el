;;; bv-fonts.el --- Font configuration and optimization -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; A role-based typography system for the BV Emacs configuration.
;;
;; The policy is deliberately explicit:
;; - code is a dense, high-legibility monospace grid;
;; - UI text is a readable proportional sans;
;; - long-form prose gets a real serif;
;; - math, emoji, CJK, symbols, and icons have isolated fallback roles;
;; - per-display scaling is separated from temporary runtime size changes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; External function declarations
(declare-function ligature-set-ligatures "ligature" (modes ligature-list))
(declare-function global-ligature-mode "ligature" (&optional arg))

;; External variable declarations
(defvar x-gtk-use-system-tooltips)
(defvar x-gtk-antialiasing-mode)
(defvar cairo-font-options)
(defvar bv-themes-font-family-monospaced)
(defvar bv-themes-font-family-proportional)
(defvar bv-themes-font-size)
(defvar bv-themes-after-load-theme-hook)

(defgroup bv-fonts nil
  "Font configuration and rendering optimization."
  :group 'faces)

;;; Font roles

(defcustom bv-fonts-default-family "Iosevka Term"
  "Default monospace font family for coding."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-monospace-fallbacks
  '("Iosevka Term"
    "Iosevka Fixed"
    "Iosevka"
    "IBM Plex Mono"
    "Source Code Pro"
    "JetBrains Mono"
    "Fira Code"
    "DejaVu Sans Mono")
  "Fallback monospace font families used when
`bv-fonts-default-family' is unavailable."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-mono-serif-family "Libertinus Mono"
  "Fixed-pitch serif-like family used for `fixed-pitch-serif'."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-mono-serif-fallbacks
  '("Libertinus Mono"
    "Go Mono"
    "Fira Mono"
    "IBM Plex Mono"
    "DejaVu Sans Mono")
  "Fallback families for `bv-fonts-mono-serif-family'."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-variable-family "IBM Plex Sans"
  "Default proportional font family for UI and prose."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-variable-fallbacks
  '("IBM Plex Sans"
    "Source Sans 3"
    "FiraGO"
    "Fira Sans"
    "Noto Sans"
    "Cantarell"
    "DejaVu Sans")
  "Fallback proportional font families used when
`bv-fonts-variable-family' is unavailable."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-serif-family "Source Serif 4"
  "Default serif font family for documents."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-serif-fallbacks
  '("Source Serif 4"
    "STIX Two Text"
    "Libertinus Serif"
    "Noto Serif"
    "TeX Gyre Termes"
    "DejaVu Serif")
  "Fallback serif font families used when
`bv-fonts-serif-family' is unavailable."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-math-fallbacks
  '("STIX Two Math"
    "Libertinus Math"
    "IBM Plex Math"
    "Noto Sans Math"
    "DejaVu Math TeX Gyre")
  "Fallback font families for mathematical symbols."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-symbol-fallbacks
  '("Noto Sans Symbols 2"
    "Noto Sans Symbols"
    "DejaVu Sans")
  "Fallback font families for miscellaneous symbols.

Icon fonts are intentionally not part of this generic symbol stack; Nerd Font
PUA glyphs are configured in `bv-nerd-icons' so they cannot steal ordinary
Unicode characters."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-cjk-fallbacks
  '("IBM Plex Sans SC"
    "Sarasa Gothic SC"
    "Sarasa UI SC"
    "Sarasa Mono SC"
    "Noto Sans CJK SC"
    "Noto Sans")
  "Fallback font families for CJK character sets."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-emoji-family "Noto Color Emoji"
  "Default color emoji family."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-emoji-fallbacks
  '("Noto Color Emoji"
    "Twemoji"
    "Apple Color Emoji"
    "Segoe UI Emoji")
  "Fallback font families for emoji."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-icon-family "Symbols Nerd Font Mono"
  "Icon-only Nerd Font family.

This is reported here as a typography role, but range installation is handled
by `bv-nerd-icons'."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-icon-fallbacks
  '("Symbols Nerd Font Mono")
  "Fallback font families for icon-only glyphs."
  :type '(repeat string)
  :group 'bv-fonts)

(defcustom bv-fonts-roles
  '((code
     :label "Code grid"
     :family-var bv-fonts-default-family
     :fallbacks-var bv-fonts-monospace-fallbacks
     :faces (default fixed-pitch))
    (mono-serif
     :label "Fixed serif mono"
     :family-var bv-fonts-mono-serif-family
     :fallbacks-var bv-fonts-mono-serif-fallbacks
     :faces (fixed-pitch-serif))
    (ui
     :label "Interface sans"
     :family-var bv-fonts-variable-family
     :fallbacks-var bv-fonts-variable-fallbacks
     :faces (variable-pitch))
    (prose
     :label "Long-form serif"
     :family-var bv-fonts-serif-family
     :fallbacks-var bv-fonts-serif-fallbacks
     :faces (variable-pitch-text Info titles org-document-title))
    (math
     :label "Math"
     :fallbacks-var bv-fonts-math-fallbacks
     :charsets (mathematical))
    (symbol
     :label "Symbols"
     :fallbacks-var bv-fonts-symbol-fallbacks
     :charsets (symbol))
    (cjk
     :label "CJK"
     :fallbacks-var bv-fonts-cjk-fallbacks
     :charsets (kana han cjk-misc bopomofo))
    (emoji
     :label "Emoji"
     :family-var bv-fonts-emoji-family
     :fallbacks-var bv-fonts-emoji-fallbacks
     :charsets (emoji))
    (icon
     :label "Icons"
     :family-var bv-fonts-icon-family
     :fallbacks-var bv-fonts-icon-fallbacks))
  "Typography roles used by the BV font system.

Each entry is (ROLE . PLIST).  The `:family-var' and `:fallbacks-var' keys
point at the defcustoms that provide the actual family policy.  This keeps old
customizations useful while making each typographic job explicit."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-fonts)

;;; Rendering and ligatures

(defcustom bv-fonts-default-size 120
  "Default font size in 1/10 pt units."
  :type 'integer
  :group 'bv-fonts)

(defcustom bv-fonts-min-size 80
  "Smallest interactive font size adjustment allowed, in 1/10 pt units."
  :type 'integer
  :group 'bv-fonts)

(defcustom bv-fonts-max-size 260
  "Largest interactive font size adjustment allowed, in 1/10 pt units."
  :type 'integer
  :group 'bv-fonts)

(defcustom bv-fonts-enable-ligatures t
  "Enable font ligatures if supported."
  :type 'boolean
  :group 'bv-fonts)

(defcustom bv-fonts-ligature-profile 'programming
  "Ligature profile to enable when `bv-fonts-enable-ligatures' is non-nil.

The `minimal' profile enables only low-risk typography.  The `programming'
profile covers common operator ligatures.  The `full' profile currently aliases
`programming' so the policy can grow without changing call sites."
  :type '(choice (const :tag "No ligature setup" nil)
                 (const :tag "Minimal" minimal)
                 (const :tag "Programming" programming)
                 (const :tag "Full" full))
  :group 'bv-fonts)

(defcustom bv-fonts-ligature-sets
  '((minimal
     :global ("www")
     :prog nil)
    (programming
     :global ("www")
     :prog ("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
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
    (full
     :inherits programming))
  "Named ligature profiles.

Each profile plist can provide `:global' ligatures for all modes, `:prog'
ligatures for `prog-mode', or `:inherits' to reuse another profile."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-fonts)

;;; Display sizing

(defvar bv-fonts--configured-display-overrides nil
  "Configured per-display font settings before runtime adjustments.")

(defun bv-fonts--set-display-overrides (symbol value)
  "Set SYMBOL to VALUE and refresh the configured display baselines."
  (set-default symbol value)
  (setq bv-fonts--configured-display-overrides (copy-tree value)))

(defcustom bv-fonts-display-overrides
  '(("LG TV" . (:size 160))
    ("BenQ RD240Q" . (:size 140))
    ("0x419f" . (:size 125)))
  "Explicit font settings for specific displays.

Each entry is (MONITOR-NAME . PLIST) matched exactly against monitor `name'
from `frame-monitor-attributes'.

Supported keys in PLIST:
- `:size'  (integer) Base size in 1/10 pt.
- `:scale' (number)  Optional extra multiplier applied to `:size'."
  :type '(alist :key-type string
                :value-type (plist :options ((:size integer)
                                             (:scale number))))
  :set #'bv-fonts--set-display-overrides
  :group 'bv-fonts)

(defcustom bv-fonts-display-rules
  '((:description "Dense laptop or HiDPI panel"
     :min-dpi 155
     :size 130)
    (:description "Large high-resolution desktop panel"
     :min-pixel-width 3000
     :min-dpi 105
     :size 145)
    (:description "Large low-density external display"
     :min-pixel-width 1800
     :max-dpi 95
     :size 130))
  "Heuristic display font settings used after exact overrides.

Rules are plists.  Supported match keys are `:name' as a regular expression,
`:min-pixel-width', `:max-pixel-width', `:min-pixel-height',
`:max-pixel-height', `:min-dpi', and `:max-dpi'.  Result keys are `:size' and
`:scale'.  Exact `bv-fonts-display-overrides' always win."
  :type '(repeat plist)
  :group 'bv-fonts)

;;; State

(defvar bv-fonts--base-size nil
  "Session base font size in units of 1/10 pt.")

(defvar bv-fonts--rendering-setup nil
  "Non-nil once GUI rendering options have been applied.")

(defvar bv-fonts--configured nil
  "Non-nil once base font faces and unicode fallbacks are configured for GUI.")

(defvar bv-fonts--monitor-signatures (make-hash-table :test 'eq)
  "Hash table mapping frames to last monitor signatures.")

(defvar bv-fonts--runtime-display-sizes (make-hash-table :test 'equal)
  "Frame display keys mapped to temporary runtime font sizes.")

(defvar bv-fonts--font-availability-cache (make-hash-table :test 'equal)
  "Cache of font family availability checks.")

(defvar bv-fonts--resolved-roles nil
  "Alist of resolved font roles from the last configuration pass.")

(defvar bv-fonts--missing-fonts nil
  "Alist of roles whose configured font candidates were unavailable.")

(defcustom bv-fonts-warn-on-missing-fonts t
  "Warn after GUI setup when a typography role has no available candidate."
  :type 'boolean
  :group 'bv-fonts)

;;; Role helpers

(defun bv-fonts--role-spec (role)
  "Return the plist for typography ROLE."
  (cdr (assq role bv-fonts-roles)))

(defun bv-fonts--role-label (role)
  "Return the human label for typography ROLE."
  (or (plist-get (bv-fonts--role-spec role) :label)
      (symbol-name role)))

(defun bv-fonts--role-value (role key)
  "Return KEY from ROLE, resolving KEY variables when present."
  (let* ((spec (bv-fonts--role-spec role))
         (var (plist-get spec (intern (format "%s-var" key)))))
    (cond
     ((and var (boundp var)) (symbol-value var))
     ((plist-member spec key) (plist-get spec key))
     (t nil))))

(defun bv-fonts--role-family (role)
  "Return the preferred family for ROLE."
  (bv-fonts--role-value role :family))

(defun bv-fonts--role-fallbacks (role)
  "Return the fallback families for ROLE."
  (let ((fallbacks (bv-fonts--role-value role :fallbacks)))
    (cond
     ((null fallbacks) nil)
     ((listp fallbacks) fallbacks)
     (t (list fallbacks)))))

(defun bv-fonts--candidate-families (preferred fallbacks)
  "Return normalized candidate families from PREFERRED and FALLBACKS."
  (delete-dups
   (cl-remove-if
    (lambda (family)
      (or (not (stringp family)) (string-empty-p family)))
    (append (list preferred) fallbacks))))

(defun bv-fonts--font-backend-ready-p ()
  "Return non-nil when Emacs can reliably inspect GUI fonts."
  (cl-some #'display-graphic-p (frame-list)))

(defun bv-fonts--font-available-p (family)
  "Return non-nil when font FAMILY is available."
  (when (and (bv-fonts--font-backend-ready-p)
             (stringp family)
             (not (string-empty-p family)))
    (let ((cached (gethash family bv-fonts--font-availability-cache :unknown)))
      (if (not (eq cached :unknown))
          cached
        (let ((available (and (fboundp 'find-font)
                              (find-font (font-spec :family family))
                              t)))
          (puthash family available bv-fonts--font-availability-cache)
          available)))))

(defun bv-fonts--record-missing (role candidates)
  "Record unavailable CANDIDATES for ROLE."
  (when (and role candidates)
    (setf (alist-get role bv-fonts--missing-fonts)
          candidates)))

(defun bv-fonts--resolve-family (preferred fallbacks &optional role quiet)
  "Return a usable font family name.

PREFERRED is the first candidate.  FALLBACKS is a list of candidate families.
ROLE is used for diagnostics.  QUIET suppresses missing-font bookkeeping."
  (let* ((candidates (bv-fonts--candidate-families preferred fallbacks))
         (resolved (cl-find-if #'bv-fonts--font-available-p candidates)))
    (cond
     ((not (bv-fonts--font-backend-ready-p))
      (car candidates))
     (resolved resolved)
     (candidates
      (unless quiet
        (bv-fonts--record-missing role candidates))
      (car candidates))
     (t nil))))

(defun bv-fonts--resolve-role (role &optional quiet)
  "Return the resolved font family for typography ROLE."
  (bv-fonts--resolve-family
   (bv-fonts--role-family role)
   (bv-fonts--role-fallbacks role)
   role
   quiet))

(defun bv-fonts--resolved-role (role)
  "Return cached resolved font family for ROLE, resolving quietly if needed."
  (or (alist-get role bv-fonts--resolved-roles)
      (bv-fonts--resolve-role role t)))

(defun bv-fonts--resolve-roles ()
  "Resolve all configured typography roles and update compatibility variables."
  (setq bv-fonts--missing-fonts nil
        bv-fonts--resolved-roles
        (mapcar (lambda (entry)
                  (let ((role (car entry)))
                    (cons role (bv-fonts--resolve-role role))))
                bv-fonts-roles))
  (setq bv-fonts-default-family (bv-fonts--resolved-role 'code)
        bv-fonts-mono-serif-family (bv-fonts--resolved-role 'mono-serif)
        bv-fonts-variable-family (bv-fonts--resolved-role 'ui)
        bv-fonts-serif-family (bv-fonts--resolved-role 'prose)
        bv-fonts-emoji-family (bv-fonts--resolved-role 'emoji)
        bv-fonts-icon-family (bv-fonts--resolved-role 'icon)))

;;; Display helpers

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

(defun bv-fonts--dimension-value (value index)
  "Return dimension component INDEX from VALUE."
  (cond
   ((vectorp value) (aref value index))
   ((and (consp value) (numberp (cdr value)))
    (if (= index 0) (car value) (cdr value)))
   ((listp value) (nth index value))
   (t nil)))

(defun bv-fonts--monitor-pixel-width (attrs)
  "Return monitor pixel width from ATTRS."
  (let ((geometry (alist-get 'geometry attrs)))
    (bv-fonts--dimension-value geometry 2)))

(defun bv-fonts--monitor-pixel-height (attrs)
  "Return monitor pixel height from ATTRS."
  (let ((geometry (alist-get 'geometry attrs)))
    (bv-fonts--dimension-value geometry 3)))

(defun bv-fonts--monitor-mm-width (attrs)
  "Return monitor physical width in millimeters from ATTRS."
  (let ((mm-size (alist-get 'mm-size attrs)))
    (bv-fonts--dimension-value mm-size 0)))

(defun bv-fonts--monitor-mm-height (attrs)
  "Return monitor physical height in millimeters from ATTRS."
  (let ((mm-size (alist-get 'mm-size attrs)))
    (bv-fonts--dimension-value mm-size 1)))

(defun bv-fonts--monitor-dpi (attrs)
  "Return approximate monitor DPI from ATTRS."
  (let ((pixel-width (bv-fonts--monitor-pixel-width attrs))
        (pixel-height (bv-fonts--monitor-pixel-height attrs))
        (mm-width (bv-fonts--monitor-mm-width attrs))
        (mm-height (bv-fonts--monitor-mm-height attrs)))
    (when (and (numberp pixel-width)
               (numberp pixel-height)
               (numberp mm-width)
               (numberp mm-height)
               (> mm-width 0)
               (> mm-height 0))
      (/ (+ (/ pixel-width (/ mm-width 25.4))
            (/ pixel-height (/ mm-height 25.4)))
         2.0))))

(defun bv-fonts--rule-number-match-p (rule key value predicate)
  "Return non-nil if RULE KEY matches VALUE using PREDICATE."
  (let ((threshold (plist-get rule key)))
    (or (not threshold)
        (and (numberp value)
             (funcall predicate value threshold)))))

(defun bv-fonts--display-rule-match-p (rule frame)
  "Return non-nil when display RULE applies to FRAME."
  (let* ((attrs (bv-fonts--frame-monitor-attributes frame))
         (name (bv-fonts--frame-monitor-name frame))
         (pixel-width (and attrs (bv-fonts--monitor-pixel-width attrs)))
         (pixel-height (and attrs (bv-fonts--monitor-pixel-height attrs)))
         (dpi (and attrs (bv-fonts--monitor-dpi attrs)))
         (name-regexp (plist-get rule :name)))
    (and (or (not name-regexp)
             (and (stringp name)
                  (string-match-p name-regexp name)))
         (bv-fonts--rule-number-match-p
          rule :min-pixel-width pixel-width #'>=)
         (bv-fonts--rule-number-match-p
          rule :max-pixel-width pixel-width #'<=)
         (bv-fonts--rule-number-match-p
          rule :min-pixel-height pixel-height #'>=)
         (bv-fonts--rule-number-match-p
          rule :max-pixel-height pixel-height #'<=)
         (bv-fonts--rule-number-match-p
          rule :min-dpi dpi #'>=)
         (bv-fonts--rule-number-match-p
          rule :max-dpi dpi #'<=))))

(defun bv-fonts--matching-display-rule (frame)
  "Return the first heuristic display rule matching FRAME."
  (cl-find-if (lambda (rule)
                (bv-fonts--display-rule-match-p rule frame))
              bv-fonts-display-rules))

(defun bv-fonts--configured-display-override (frame)
  "Return the configured exact override entry for FRAME, or nil."
  (let ((name (bv-fonts--frame-monitor-name frame)))
    (and (stringp name)
         (assoc name bv-fonts--configured-display-overrides))))

(defun bv-fonts--display-baseline (frame)
  "Return baseline font settings for FRAME as a plist."
  (let ((exact (bv-fonts--configured-display-override frame))
        (rule (bv-fonts--matching-display-rule frame)))
    (cond
     (exact
      (list :source 'exact
            :source-label (car exact)
            :plist (copy-sequence (cdr exact))))
     (rule
      (list :source 'heuristic
            :source-label (or (plist-get rule :description) "display rule")
            :plist (copy-sequence rule)))
     (t
      (list :source 'default
            :source-label "default"
            :plist nil)))))

(defun bv-fonts--display-key (frame)
  "Return stable runtime key for FRAME's current display."
  (let ((name (bv-fonts--frame-monitor-name frame)))
    (if (and (stringp name) (not (string-empty-p name)))
        name
      (format "%S" (bv-fonts--frame-monitor-signature frame)))))

(defun bv-fonts--runtime-display-size (frame)
  "Return temporary runtime size for FRAME's display, or nil."
  (gethash (bv-fonts--display-key frame) bv-fonts--runtime-display-sizes))

(defun bv-fonts--set-display-size (frame size)
  "Set temporary runtime font SIZE for FRAME's display."
  (puthash (bv-fonts--display-key frame)
           (min bv-fonts-max-size (max bv-fonts-min-size size))
           bv-fonts--runtime-display-sizes))

(defun bv-fonts--clear-display-size (frame)
  "Clear temporary runtime font size for FRAME's display."
  (remhash (bv-fonts--display-key frame) bv-fonts--runtime-display-sizes))

(defun bv-fonts--display-settings (&optional frame)
  "Return effective font settings for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (attrs (bv-fonts--frame-monitor-attributes frame))
         (name (bv-fonts--frame-monitor-name frame))
         (baseline (bv-fonts--display-baseline frame))
         (plist (copy-sequence (plist-get baseline :plist)))
         (runtime-size (bv-fonts--runtime-display-size frame))
         (baseline-size (or (plist-get plist :size) (bv-fonts--base-size)))
         (size (or runtime-size baseline-size)))
    (list :name name
          :key (bv-fonts--display-key frame)
          :source (plist-get baseline :source)
          :source-label (plist-get baseline :source-label)
          :baseline-size baseline-size
          :runtime-size runtime-size
          :size size
          :scale (or (plist-get plist :scale) 1.0)
          :pixel-width (and attrs (bv-fonts--monitor-pixel-width attrs))
          :pixel-height (and attrs (bv-fonts--monitor-pixel-height attrs))
          :mm-width (and attrs (bv-fonts--monitor-mm-width attrs))
          :mm-height (and attrs (bv-fonts--monitor-mm-height attrs))
          :dpi (and attrs (bv-fonts--monitor-dpi attrs)))))

(defun bv-fonts--configured-display-size (&optional frame)
  "Return the configured baseline font size for FRAME."
  (plist-get (bv-fonts--display-settings frame) :baseline-size))

(defun bv-fonts--scaled-height (frame)
  "Compute a frame-local `default' face height for FRAME."
  (let* ((settings (bv-fonts--display-settings frame))
         (base (plist-get settings :size))
         (scale (plist-get settings :scale)))
    (max 1 (round (* base scale)))))

(defun bv-fonts--apply-to-display (&optional frame)
  "Apply font settings to every live frame on FRAME's current display."
  (let* ((frame (or frame (selected-frame)))
         (name (bv-fonts--frame-monitor-name frame))
         (matched nil))
    (dolist (other (frame-list))
      (when (and (frame-live-p other)
                 (display-graphic-p other)
                 (equal (bv-fonts--frame-monitor-name other) name))
        (setq matched t)
        (bv-fonts-apply-to-frame other)))
    (unless matched
      (bv-fonts-apply-to-frame frame))))

(defun bv-fonts-apply-to-frame (&optional frame)
  "Apply per-frame font settings to FRAME.

This primarily adjusts the `default' face height using exact display settings,
heuristic display rules, and temporary runtime adjustments."
  (let ((frame (or frame (selected-frame))))
    (when (and (framep frame) (frame-live-p frame) (display-graphic-p frame))
      (set-face-attribute 'default frame
                          :family (bv-fonts--resolved-role 'code)
                          :height (bv-fonts--scaled-height frame)))))

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

;;; Face and fontset configuration

(defun bv-fonts--configure-info-faces ()
  "Configure face attributes used by Info buffers."
  (let ((mono-family (bv-fonts--resolved-role 'code))
        (serif-family (bv-fonts--resolved-role 'prose)))
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

(defun bv-fonts--set-fontset-font (target charset family &rest args)
  "Set font FAMILY for CHARSET in fontset TARGET when available.
ARGS are forwarded to `set-fontset-font'."
  (when (bv-fonts--font-available-p family)
    (apply #'set-fontset-font
           target
           charset
           (font-spec :family family)
           args)))

(defun bv-fonts--ligature-profile-plist (profile)
  "Return fully resolved ligature plist for PROFILE."
  (let* ((plist (copy-sequence (cdr (assq profile bv-fonts-ligature-sets))))
         (parent (plist-get plist :inherits)))
    (if parent
        (let ((parent-plist (bv-fonts--ligature-profile-plist parent)))
          (append plist parent-plist))
      plist)))

(defun bv-fonts--setup-ligatures ()
  "Configure ligatures from `bv-fonts-ligature-profile'."
  (when (and bv-fonts-enable-ligatures
             bv-fonts-ligature-profile
             (fboundp 'ligature-mode))
    (let* ((plist (bv-fonts--ligature-profile-plist
                   bv-fonts-ligature-profile))
           (global (plist-get plist :global))
           (prog (plist-get plist :prog)))
      (when global
        (ligature-set-ligatures 't global))
      (when prog
        (ligature-set-ligatures 'prog-mode prog))
      (global-ligature-mode 1))))

(defun bv-fonts-setup-rendering ()
  "Configure font rendering for optimal clarity."
  (when (display-graphic-p)
    ;; Enable subpixel antialiasing for LCD screens.
    (setq-default x-gtk-use-system-tooltips t)

    (when (boundp 'x-gtk-antialiasing-mode)
      (setq x-gtk-antialiasing-mode 'subpixel))

    (when (boundp 'cairo-font-options)
      (setq cairo-font-options
            '(:antialias subpixel
              :hint-style full
              :lcd-filter lcddefault)))

    ;; Avoid hidden per-family scale hacks; display scale belongs to the
    ;; display rules above.
    (setq face-font-rescale-alist nil)

    (bv-fonts--setup-ligatures)))

(defun bv-fonts-configure-faces ()
  "Configure all font faces for optimal readability."
  (bv-fonts--resolve-roles)
  (let ((mono-family (bv-fonts--resolved-role 'code))
        (mono-serif-family (bv-fonts--resolved-role 'mono-serif))
        (variable-family (bv-fonts--resolved-role 'ui))
        (serif-family (bv-fonts--resolved-role 'prose)))
    (set-face-attribute 'default nil
                        :family mono-family
                        :height (bv-fonts--base-size)
                        :weight 'regular
                        :width 'normal)

    (set-face-attribute 'fixed-pitch nil
                        :family mono-family
                        :height 1.0
                        :weight 'regular)

    (set-face-attribute 'fixed-pitch-serif nil
                        :family mono-serif-family
                        :height 1.0
                        :weight 'regular
                        :slant 'normal)

    (set-face-attribute 'variable-pitch nil
                        :family variable-family
                        :height 1.05
                        :weight 'regular)

    (set-face-attribute 'variable-pitch-text nil
                        :family serif-family
                        :height 1.1
                        :weight 'normal)

    (bv-fonts--configure-info-faces)))

(defun bv-fonts-setup-unicode ()
  "Configure fallback fonts for Unicode coverage."
  (bv-fonts--set-fontset-font
   t 'mathematical
   (bv-fonts--resolved-role 'math)
   nil 'prepend)

  (bv-fonts--set-fontset-font
   t 'emoji
   (bv-fonts--resolved-role 'emoji)
   nil 'prepend)

  (dolist (charset '(kana han cjk-misc bopomofo))
    (bv-fonts--set-fontset-font
     t charset
     (bv-fonts--resolved-role 'cjk)
     nil 'prepend))

  (dolist (family bv-fonts-symbol-fallbacks)
    (bv-fonts--set-fontset-font t 'symbol family nil 'append)))

;;; Theme integration

(defun bv-fonts-sync-theme ()
  "Sync `bv-themes' font variables from resolved BV font roles."
  (interactive)
  (bv-fonts--resolve-roles)
  (when (boundp 'bv-themes-font-family-monospaced)
    (setq bv-themes-font-family-monospaced (bv-fonts--resolved-role 'code)))
  (when (boundp 'bv-themes-font-family-proportional)
    (setq bv-themes-font-family-proportional (bv-fonts--resolved-role 'ui)))
  (when (boundp 'bv-themes-font-size)
    (setq bv-themes-font-size (bv-fonts--base-size))))

;;; Interactive commands

(defun bv-fonts-adjust-size (increment)
  "Adjust the current display font size by INCREMENT.
INCREMENT is in units of 1/10 pt."
  (interactive "nSize increment (1/10 pt): ")
  (let* ((frame (selected-frame))
         (settings (bv-fonts--display-settings frame))
         (name (or (plist-get settings :name) "Unknown"))
         (size (bv-fonts--set-display-size
                frame
                (+ (plist-get settings :size) increment))))
    (bv-fonts--apply-to-display frame)
    (message "Font size for %s: %d" name size)))

(defun bv-fonts-increase-size ()
  "Increase font size by 10 units (1 pt)."
  (interactive)
  (bv-fonts-adjust-size 10))

(defun bv-fonts-decrease-size ()
  "Decrease font size by 10 units (1 pt)."
  (interactive)
  (bv-fonts-adjust-size -10))

(defun bv-fonts-reset-size ()
  "Reset the current display font size to its configured baseline."
  (interactive)
  (let* ((frame (selected-frame))
         (name (or (bv-fonts--frame-monitor-name frame) "Unknown"))
         (size (bv-fonts--configured-display-size frame)))
    (bv-fonts--clear-display-size frame)
    (bv-fonts--apply-to-display frame)
    (message "Font size for %s reset to %d" name size)))

(defun bv-fonts--format-number (value &optional precision)
  "Format numeric VALUE with optional PRECISION."
  (cond
   ((not (numberp value)) "n/a")
   (precision (format (format "%%.%df" precision) value))
   (t (number-to-string value))))

(defun bv-fonts--insert-role-report ()
  "Insert font role report at point."
  (insert "* Roles\n")
  (dolist (entry bv-fonts-roles)
    (let* ((role (car entry))
           (preferred (bv-fonts--role-family role))
           (fallbacks (bv-fonts--role-fallbacks role))
           (resolved (bv-fonts--resolve-family preferred fallbacks role t))
           (candidates (bv-fonts--candidate-families preferred fallbacks)))
      (insert (format "- %-12s %s\n"
                      (symbol-name role)
                      (bv-fonts--role-label role)))
      (insert (format "  preferred: %s\n" (or preferred "n/a")))
      (insert (format "  resolved:  %s%s\n"
                      (or resolved "n/a")
                      (cond
                       ((not (bv-fonts--font-backend-ready-p))
                        " (availability unchecked; no GUI font backend)")
                       ((and resolved
                             (bv-fonts--font-available-p resolved))
                        "")
                       (t " (not found; Emacs will fall back)"))))
      (insert (format "  candidates: %s\n\n"
                      (if candidates
                          (string-join candidates ", ")
                        "n/a"))))))

(defun bv-fonts--insert-display-report (&optional frame)
  "Insert display report for FRAME at point."
  (let* ((frame (or frame (selected-frame)))
         (settings (bv-fonts--display-settings frame)))
    (insert "* Current Display\n")
    (insert (format "- name: %s\n"
                    (if (string-empty-p (or (plist-get settings :name) ""))
                        "n/a"
                      (plist-get settings :name))))
    (insert (format "- key: %s\n" (plist-get settings :key)))
    (insert (format "- pixels: %sx%s\n"
                    (bv-fonts--format-number
                     (plist-get settings :pixel-width))
                    (bv-fonts--format-number
                     (plist-get settings :pixel-height))))
    (insert (format "- millimeters: %sx%s\n"
                    (bv-fonts--format-number
                     (plist-get settings :mm-width))
                    (bv-fonts--format-number
                     (plist-get settings :mm-height))))
    (insert (format "- dpi: %s\n"
                    (bv-fonts--format-number
                     (plist-get settings :dpi)
                     1)))
    (insert (format "- baseline: %d from %s (%s)\n"
                    (plist-get settings :baseline-size)
                    (plist-get settings :source)
                    (plist-get settings :source-label)))
    (insert (format "- runtime override: %s\n"
                    (or (plist-get settings :runtime-size) "none")))
    (insert (format "- effective size: %d\n"
                    (plist-get settings :size)))
    (insert (format "- scale: %s\n\n"
                    (bv-fonts--format-number
                     (plist-get settings :scale)
                     2)))))

(defun bv-fonts--insert-runtime-report ()
  "Insert runtime font system report at point."
  (insert "* Runtime\n")
  (insert (format "- graphical frame: %s\n"
                  (if (display-graphic-p) "yes" "no")))
  (insert (format "- rendering setup: %s\n"
                  (if bv-fonts--rendering-setup "yes" "no")))
  (insert (format "- faces configured: %s\n"
                  (if bv-fonts--configured "yes" "no")))
  (insert (format "- ligatures enabled: %s\n"
                  (if (and bv-fonts-enable-ligatures
                           (fboundp 'ligature-mode))
                      (format "yes (%s)" bv-fonts-ligature-profile)
                    "no")))
  (insert (format "- cairo options: %S\n"
                  (and (boundp 'cairo-font-options)
                       cairo-font-options)))
  (insert (format "- face-font-rescale-alist: %S\n\n"
                  face-font-rescale-alist)))

(defun bv-fonts--insert-theme-report ()
  "Insert theme integration report at point."
  (insert "* Theme Sync\n")
  (insert (format "- bv-themes mono: %s\n"
                  (if (boundp 'bv-themes-font-family-monospaced)
                      bv-themes-font-family-monospaced
                    "unbound")))
  (insert (format "- bv-themes proportional: %s\n"
                  (if (boundp 'bv-themes-font-family-proportional)
                      bv-themes-font-family-proportional
                    "unbound")))
  (insert (format "- bv-themes size: %s\n\n"
                  (if (boundp 'bv-themes-font-size)
                      bv-themes-font-size
                    "unbound"))))

(defun bv-fonts--insert-missing-report ()
  "Insert missing font diagnostics at point."
  (insert "* Missing Candidates\n")
  (if bv-fonts--missing-fonts
      (dolist (entry bv-fonts--missing-fonts)
        (insert (format "- %s: %s\n"
                        (car entry)
                        (string-join (cdr entry) ", "))))
    (insert "- none recorded\n"))
  (insert "\n"))

;;;###autoload
(defun bv-fonts-report ()
  "Show a diagnostic atlas for the current BV typography system."
  (interactive)
  (bv-fonts--resolve-roles)
  (let ((buffer (get-buffer-create "*BV Fonts Report*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+title: BV Fonts Report\n\n")
        (bv-fonts--insert-display-report)
        (bv-fonts--insert-runtime-report)
        (bv-fonts--insert-theme-report)
        (bv-fonts--insert-role-report)
        (bv-fonts--insert-missing-report)
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

;; Key bindings
(global-set-key (kbd "C-+") #'bv-fonts-increase-size)
(global-set-key (kbd "C--") #'bv-fonts-decrease-size)
(global-set-key (kbd "C-0") #'bv-fonts-reset-size)

;; Alternative bindings for better compatibility.
(global-set-key (kbd "C-=") #'bv-fonts-increase-size)
(global-set-key (kbd "C-_") #'bv-fonts-decrease-size)

;;; Initialization

(defun bv-fonts--warn-missing-fonts ()
  "Warn about unavailable font roles when configured to do so."
  (when (and bv-fonts-warn-on-missing-fonts bv-fonts--missing-fonts)
    (message "BV fonts missing candidates: %s"
             (string-join
              (mapcar (lambda (entry) (symbol-name (car entry)))
                      bv-fonts--missing-fonts)
              ", "))))

(defun bv-fonts-init ()
  "Initialize font configuration."
  (unless bv-fonts--configured-display-overrides
    (setq bv-fonts--configured-display-overrides
          (copy-tree bv-fonts-display-overrides)))
  (unless bv-fonts--base-size
    (setq bv-fonts--base-size bv-fonts-default-size))
  (bv-fonts-sync-theme)
  (when (display-graphic-p)
    (unless bv-fonts--rendering-setup
      (setq bv-fonts--rendering-setup t)
      (bv-fonts-setup-rendering))
    (unless bv-fonts--configured
      (bv-fonts-configure-faces)
      (bv-fonts-setup-unicode)
      (setq bv-fonts--configured t)
      (bv-fonts--warn-missing-fonts))
    (bv-fonts--maybe-rescale-frame (selected-frame))))

(add-hook 'window-size-change-functions #'bv-fonts--maybe-rescale-frame)
(add-hook 'after-make-frame-functions #'bv-fonts--maybe-rescale-frame)

(with-eval-after-load 'bv-themes
  (add-hook 'bv-themes-after-load-theme-hook #'bv-fonts-sync-theme)
  (add-hook 'bv-themes-after-load-theme-hook #'bv-fonts-apply-to-frames))

;; Setup fonts after frame creation for daemon mode.
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (bv-fonts-init))))
  (bv-fonts-init))

(provide 'bv-fonts)
;;; bv-fonts.el ends here
