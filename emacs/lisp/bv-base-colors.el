;;; bv-base-colors.el --- Base color definitions  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Defines the 9 basic colors following nano-emacs architecture.
;; Colors are loaded from well-known faces or can be customized.

;;; Code:

(defgroup bv '()
  "Personal Emacs customizations.")

;; Derive default colors from classic Emacs faces
(defvar bv-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun bv-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name bv-base-colors--defaults)))

(defcustom bv-color-foreground (bv-base-colors--get 'foreground)
  "Foreground color."
  :type 'color
  :group 'bv)

(defcustom bv-color-background (bv-base-colors--get 'background)
  "Background color."
  :type 'color
  :group 'bv)

(defcustom bv-color-highlight (bv-base-colors--get 'highlight)
  "Highlight color."
  :type 'color
  :group 'bv)

(defcustom bv-color-critical (bv-base-colors--get 'critical)
  "Critical color."
  :type 'color
  :group 'bv)

(defcustom bv-color-salient (bv-base-colors--get 'salient)
  "Salient color."
  :type 'color
  :group 'bv)

(defcustom bv-color-strong (bv-base-colors--get 'strong)
  "Strong color."
  :type 'color
  :group 'bv)

(defcustom bv-color-popout (bv-base-colors--get 'popout)
  "Popout color."
  :type 'color
  :group 'bv)

(defcustom bv-color-subtle (bv-base-colors--get 'subtle)
  "Subtle color."
  :type 'color
  :group 'bv)

(defcustom bv-color-faded (bv-base-colors--get 'faded)
  "Faded color."
  :type 'color
  :group 'bv)

(provide 'bv-base-colors)
;;; bv-base-colors.el ends here