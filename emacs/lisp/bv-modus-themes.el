;;; bv-modus-themes.el --- Modus themes configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for Modus themes with custom face settings and theme toggling.
;; Provides deuteranopia-friendly theme variants and custom styling.

;;; Code:

(eval-when-compile
  (require 'modus-themes)
  (require 'cl-seq))

(require 'modus-operandi-deuteranopia-theme)


(eval-when-compile
  (enable-theme 'modus-operandi-deuteranopia))

(defgroup bv-modus-themes nil
  "Configuration related to `modus-themes'."
  :group 'bv)

(defcustom bv-modus-themes-mode-line-padding 1
  "The padding of the mode line."
  :type 'number
  :group 'bv-modus-themes)

(defcustom bv-modus-themes-tab-bar-padding 1
  "The padding of the tab bar."
  :type 'number
  :group 'bv-modus-themes)

(defcustom bv-modus-themes-header-line-padding 1
  "The padding of the header line."
  :type 'number
  :group 'bv-modus-themes)

(defcustom bv-modus-themes-after-enable-theme-hook nil
  "Normal hook run after enabling a theme."
  :type 'hook
  :group 'bv-modus-themes)

(defun bv-modus-themes-run-after-enable-theme-hook (&rest _args)
  "Run `bv-modus-themes-after-enable-theme-hook'."
  (run-hooks 'bv-modus-themes-after-enable-theme-hook))

(defun bv-modus-themes-set-custom-faces (&optional _theme)
  "Set faces based on the current theme."
  (interactive)
  (when (modus-themes--current-theme)
    (modus-themes-with-colors
      (custom-set-faces
       `(window-divider ((,c :foreground ,bg-main)))
       `(window-divider-first-pixel ((,c :foreground ,bg-main)))
       `(window-divider-last-pixel ((,c :foreground ,bg-main)))
       `(vertical-border ((,c :foreground ,bg-main)))
       `(tab-bar ((,c :background ,bg-dim
                      :box (:line-width ,bv-modus-themes-tab-bar-padding
                            :color ,bg-dim
                            :style nil))))
       `(mode-line ((,c :box (:line-width ,bv-modus-themes-mode-line-padding
                              :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width ,bv-modus-themes-mode-line-padding
                                       :color ,bg-mode-line-inactive))))
       `(header-line ((,c :box (:line-width ,bv-modus-themes-header-line-padding
                                :color ,bg-dim))))
       `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe :background ,bg-main)))
       `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe :background ,bg-main)))
       `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe :background ,bg-main)))
       `(aw-leading-char-face ((,c :height 1.0 :foreground ,blue-cooler)))))))

(defun bv-modus-themes--dark-theme-p (&optional theme)
  "Indicate if there is a curently-active dark THEME."
  (if theme
      (eq theme 'modus-operandi-deuteranopia)
    (when (boundp 'custom-enabled-themes)
      (eq (car custom-enabled-themes) 'modus-vivendi-deuteranopia))))

(setq bv-modus-themes-header-line-padding 6)
(setq bv-modus-themes-tab-bar-padding 6)
(setq bv-modus-themes-mode-line-padding 6)

(advice-add 'enable-theme :after 'bv-modus-themes-run-after-enable-theme-hook)

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "t") 'modus-themes-toggle)))

(with-eval-after-load 'modus-themes
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (bg-region bg-ochre)
          (fg-region unspecified)))
  (setq modus-themes-to-toggle
        '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts t))

(load-theme 'modus-operandi-deuteranopia t (not (display-graphic-p)))

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (boundp 'custom-enabled-themes)
                       (null custom-enabled-themes))
              (enable-theme 'modus-operandi-deuteranopia))))

(provide 'bv-modus-themes)
;;; bv-modus-themes.el ends here