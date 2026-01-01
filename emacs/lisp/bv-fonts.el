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
    (global-ligature-mode 1)))

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
                        :height bv-fonts-default-size
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
  (let ((new-size (+ (face-attribute 'default :height) increment)))
    (set-face-attribute 'default nil :height new-size)
    (when (boundp 'bv-themes-font-size)
      (setq bv-themes-font-size new-size))
    (message "Font size: %d" new-size)))

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
  (set-face-attribute 'default nil :height bv-fonts-default-size)
  (when (boundp 'bv-themes-font-size)
    (setq bv-themes-font-size bv-fonts-default-size))
  (message "Font size reset to %d" bv-fonts-default-size))

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
  (bv-fonts-setup-rendering)
  (bv-fonts-configure-faces)
  (bv-fonts-setup-unicode))

;; Setup fonts after frame creation for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (bv-fonts-init))))
  (bv-fonts-init))

(provide 'bv-fonts)
;;; bv-fonts.el ends here
