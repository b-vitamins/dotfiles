;;; bv-faces.el --- Face settings  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Defines the fundamental faces from which all other faces derive.

;;; Code:

(require 'bv-base-colors)

(defcustom bv-font-family-monospaced "Roboto Mono"
  "Name of the font-family to use.
Defaults to Roboto Mono. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'bv
  :type 'string)

(defcustom bv-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows displaying variable pitch faces when
variable-pitch-mode or mixed-pitch-mode is active.
Defaults to nil."
  :group 'bv
  :type 'string)

(defcustom bv-font-size 12
  "Default value for the font size in pt units."
  :group 'bv
  :type 'integer)


(defface bv-face-default nil
  "Default face is used for regular information."
  :group 'bv)

(defface bv-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'bv)

(defface bv-face-critical nil
  "Critical face is for information that requires immediate action.
High contrast face with intense background color."
  :group 'bv)

(defface bv-face-popout nil
  "Popout face is used for information that needs attention.
Uses contrasting hue to attract attention."
  :group 'bv)

(defface bv-face-strong nil
  "Strong face is used for structural elements.
Same color as default with different weight."
  :group 'bv)

(defface bv-face-salient nil
  "Salient face is used for important information.
Different hue with similar intensity to default."
  :group 'bv)

(defface bv-face-faded nil
  "Faded face is for less important information.
Same hue as default with reduced intensity."
  :group 'bv)

(defface bv-face-subtle nil
  "Subtle face is used to delineate areas.
Light background color that is barely perceptible."
  :group 'bv)

(defun bv-faces ()
  "Derive face attributes for bv-faces using bv-theme values."
  (set-face-attribute 'bv-face-default nil
                      :foreground bv-color-foreground
                      :background bv-color-background
                      :family     bv-font-family-monospaced
                      :height     (* bv-font-size 10))

  (set-face-attribute 'bv-face-critical nil
                      :foreground bv-color-foreground
                      :background bv-color-critical)

  (set-face-attribute 'bv-face-popout nil
                      :foreground bv-color-popout)

  (set-face-attribute 'bv-face-variable-pitch nil
                      :foreground (face-foreground 'bv-face-default)
                      :background (face-background 'bv-face-default)
                      :family (or bv-font-family-proportional
                                  bv-font-family-monospaced)
                      :height (* bv-font-size 10))

  (if (display-graphic-p)
      (set-face-attribute 'bv-face-strong nil
                          :foreground bv-color-strong
                          :weight 'medium)
    (set-face-attribute 'bv-face-strong nil
                        :foreground bv-color-strong
                        :weight 'bold))

  (set-face-attribute 'bv-face-salient nil
                      :foreground bv-color-salient
                      :weight 'light)

  (set-face-attribute 'bv-face-faded nil
                      :foreground bv-color-faded
                      :weight 'light)

  (set-face-attribute 'bv-face-subtle nil
                      :background bv-color-subtle))

(provide 'bv-faces)
;;; bv-faces.el ends here