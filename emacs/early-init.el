;;; early-init.el --- Early Emacs initialization file -*- lexical-binding: t -*-

;; Copyright (c) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/early-init.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; "What does it mean to pre-board? Do you get on before you get on?"
;;                                          - George Carlin

;;; Code:

;; Set garbage collection threshold and file name handler alist during startup.
;;
;; This configuration optimizes Emacs startup performance by setting the
;; garbage collection threshold to its maximum value and disabling the file
;; name handler alist. This reduces garbage collection pauses and speeds up
;; file processing during startup.
;;
;; Borrowed from Protesilaos Stavrou <public@protesilaos.com>
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/early-init.el
;; 
(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)

(defvar emacs-gc-cons-threshold gc-cons-threshold
  "Default value of `gc-cons-threshold' before startup.")

(defvar emacs-file-name-handler-alist file-name-handler-alist
  "Default value of `file-name-handler-alist' before startup.")

;; Restore default settings after startup.
;;
;; This hook restores the garbage collection threshold and file name handler
;; alist to their default values after Emacs startup is complete.
;;
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold emacs-gc-cons-threshold
                  file-name-handler-alist emacs-file-name-handler-alist)))

;; Macro to safely enable/disable modes.
;;
;; This macro checks if a mode function is bound before attempting to
;; enable or disable it. It then executes the function with the
;; specified ENABLE value (1 for enable, -1 for disable).
;;
(defmacro with-safe-mode (mode &optional enable)
  "Safely enable or disable MODE if currently bound.
If ENABLE is non-nil, enable MODE; otherwise disable it."
  `(when (fboundp ',mode)
     (funcall ',mode ,(if enable 1 -1))))

;; Adjust UI and modes for improved Emacs experience.
;;
;; This section configures various UI elements and modes to enhance the
;; Emacs user interface and behavior. It disables the menu bar, tool bar,
;; scroll bar, blinking cursor, electric indent, and tooltip mode. It also
;; enables the display of time in the mode line and activates recentf mode
;; to manage recently visited files. Additionally, it sets various startup
;; options such as disabling the splash screen, emptying the initial scratch
;; buffer message, and suppressing echo area messages during startup.
;;
(with-safe-mode menu-bar-mode)
(with-safe-mode tool-bar-mode)
(with-safe-mode scroll-bar-mode)
(with-safe-mode blink-cursor-mode)
(with-safe-mode electric-indent-mode)
(with-safe-mode tooltip-mode)
(display-time-mode 1)
(recentf-mode 1)
(setq line-move-visual nil
      inhibit-splash-screen t
      initial-scratch-message ""
      inhibit-startup-echo-area-message t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      mode-line-format nil)

;; Check for font availability and set it as default.
;;
;; This section checks if the "Iosevka Comfy" font, created by Protesilaos Stavrou,
;; is available in the system's font list before setting it as the default font
;; for Emacs frames. If the font is available, it is added to the default frame
;; parameters with a size of 12 points. This ensures that Emacs gracefully handles
;; situations where the font is not installed, preventing errors during startup.
;; The font can be found at https://github.com/protesilaos/iosevka-comfy.
;;
(when (member "Iosevka Comfy" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-12")))

;; Configure frame appearance and initial size.
;;
;; This section sets the default parameters for Emacs frames, including
;; dimensions, transparency, scroll bars, tool bar, menu bar, background,
;; and foreground colors. Frame transparency is set to 90%. Vertical and horizontal
;; scroll bars, tool bar, and menu bar are disabled to maximize screen real
;; estate. The left and right fringes are set to 0 to remove them entirely,
;; blending them with the background color.
;;
(setq default-frame-alist
      '((width . 120)
        (height . 34)
        (alpha . (95 . 95))
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (left-fringe . 0)
        (right-fringe . 0)
        (undecorated . t)))

;; Additional optimizations for improved Emacs performance.
;;
;; This section configures additional options to optimize Emacs performance.
;; These settings include enabling pixel-wise frame resizing, inhibiting
;; automatic frame resizing based on content, enabling the use of dialog
;; boxes for questions, and disabling the graphical file dialog for reading
;; file names. These optimizations help streamline Emacs interaction and
;; reduce unnecessary processing overhead.
;;
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t
      use-file-dialog nil)

(provide 'early-init)
;;; early-init.el ends here