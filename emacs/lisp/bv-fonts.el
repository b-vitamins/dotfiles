;;; bv-fonts.el --- Font configuration and optimization -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; Comprehensive font configuration for optimal rendering and readability.
;; Provides sharp, crystal-clear text rendering with carefully chosen fonts
;; for different contexts.

;;; Code:

(defgroup bv-fonts nil
  "Font configuration and rendering optimization."
  :group 'faces)

(defcustom bv-fonts-default-family "Fira Code"
  "Default monospace font family for coding."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-default-size 120
  "Default font size in 1/10 pt units."
  :type 'integer
  :group 'bv-fonts)

(defcustom bv-fonts-variable-family "IBM Plex Sans"
  "Default proportional font family for UI and prose."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-serif-family "ET Book"
  "Default serif font family for documents."
  :type 'string
  :group 'bv-fonts)

(defcustom bv-fonts-enable-ligatures t
  "Enable font ligatures if supported."
  :type 'boolean
  :group 'bv-fonts)

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
  ;; Default face - primary coding font
  (set-face-attribute 'default nil
                      :family bv-fonts-default-family
                      :height bv-fonts-default-size
                      :weight 'regular
                      :width 'normal)

  ;; Fixed-pitch - same as default for consistency
  (set-face-attribute 'fixed-pitch nil
                      :family bv-fonts-default-family
                      :height 1.0
                      :weight 'regular)

  ;; Fixed-pitch-serif - alternative monospace with serifs
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Fira Mono"
                      :height 1.0
                      :weight 'regular
                      :slant 'normal)

  ;; Variable-pitch - for UI elements and prose
  (set-face-attribute 'variable-pitch nil
                      :family bv-fonts-variable-family
                      :height 1.05
                      :weight 'regular)

  ;; Variable-pitch-text - for larger text blocks
  (set-face-attribute 'variable-pitch-text nil
                      :family bv-fonts-serif-family
                      :height 1.1
                      :weight 'normal)

  ;; Configure Info mode faces
  (with-eval-after-load 'info
    ;; Use ligature-enabled font for code blocks
    (set-face-attribute 'Info-quoted nil
                        :family "Victor Mono"
                        :height 0.95
                        :weight 'medium)

    ;; Use serif for body text in Info
    (set-face-attribute 'info-node nil
                        :family bv-fonts-serif-family
                        :height 1.2
                        :weight 'semibold)

    (set-face-attribute 'info-title-1 nil
                        :family "Libertinus Serif"
                        :height 1.8
                        :weight 'bold)

    (set-face-attribute 'info-title-2 nil
                        :family "Libertinus Serif"
                        :height 1.5
                        :weight 'bold)

    (set-face-attribute 'info-title-3 nil
                        :family "Libertinus Serif"
                        :height 1.3
                        :weight 'semibold)

    (set-face-attribute 'info-title-4 nil
                        :family "Libertinus Serif"
                        :height 1.1
                        :weight 'semibold)))

(defun bv-fonts-setup-unicode ()
  "Configure fallback fonts for Unicode coverage."
  ;; Mathematical symbols
  (set-fontset-font t 'mathematical "STIX Two Math" nil 'prepend)

  ;; Emoji support
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend)

  ;; CJK support
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset "Source Han Sans" nil 'prepend))

  ;; Symbol fallbacks
  (set-fontset-font t 'symbol "Symbola" nil 'append)
  (set-fontset-font t 'symbol "DejaVu Sans" nil 'append))

(defun bv-fonts-adjust-size (increment)
  "Adjust the default font size by INCREMENT.
INCREMENT is in units of 1/10 pt."
  (interactive "nSize increment (1/10 pt): ")
  (let ((new-size (+ (face-attribute 'default :height) increment)))
    (set-face-attribute 'default nil :height new-size)
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
