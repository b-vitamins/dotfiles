;;; bv-file-navigation.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/lisp/bv-file-navigation.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module provides functions to open files in new windows with specific directions
;; and focus settings. It includes customizable paths for frequently accessed files
;; and macros to easily create specific file opening functions.
;;

;;; Code:

;; Customizable file paths
(defgroup bv-file-paths nil
  "Customizable file paths for bv-open-file functions."
  :group 'files
  :prefix "bv-")

(defcustom bv-main-path "~/documents/main"
  "Path to the main Org file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-main-org-path "~/documents/main/main.org"
  "Path to the main Org file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-snippets-org-path "~/projects/latex-snippets/snippets.org"
  "Path to the LaTeX snippets Org file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-bib-path "~/documents/slipbox/bibliographies/working.bib"
  "Path to the bibliography file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-library-path "~/documents/papers"
  "Path to the library files."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-notes-path "~/documents/slipbox/notes"
  "Path to notes files."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-init-el-path "~/.config/emacs/init.el"
  "Path to the Emacs init file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-config-scm-path "~/.config/guix/config.scm"
  "Path to the Guix configuration file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-home-config-scm-path "~/.config/guix/home-config.scm"
  "Path to the Zsh configuration file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-file-open-width-ratio 0.45
  "Width ratio for opening files in a new window, relative to the frame width."
  :type 'float
  :group 'bv-file-paths)

(defun bv-open-file-in-window (filename &optional direction focus ratio)
  "Open FILENAME in a new window in the specified DIRECTION.
DIRECTION can be `left` or `right`. If FOCUS is non-nil, the new window
will receive focus. The window width is determined by RATIO, defaulting
to `bv-file-open-width-ratio`."
  (interactive
   (list (read-file-name "Open file: ")
         (intern (completing-read "Direction (left or right): " '("left" "right") nil t "right"))
         (y-or-n-p "Switch to the new window? ")
         (or (read-number "Window width ratio (default 0.45): " bv-file-open-width-ratio) bv-file-open-width-ratio)))
  (unless (file-exists-p filename)
    (error "The file does not exist"))
  (let ((buffer-new (find-file-noselect filename))
        (current-window (selected-window))
        (total-width (frame-width))
        (side (if (eq direction 'right) 'right 'left)))
    (dolist (window (window-list))
      (unless (eq window current-window)
        (delete-window window)))
    (let* ((new-width (round (* total-width ratio)))
           (split-side (- total-width new-width))
           (new-window (split-window current-window split-side side)))
      (set-window-buffer new-window buffer-new)
      (when focus
        (select-window new-window)))))

(defun bv-open-file-left-jump (filename)
  "Open FILENAME in a new window to the left and jump to it."
  (interactive "FOpen file on left and jump: ")
  (bv-open-file-in-window filename 'left t bv-file-open-width-ratio))

(defun bv-open-file-left-stay (filename)
  "Open FILENAME in a new window to the left without changing focus."
  (interactive "FOpen file on left and stay: ")
  (bv-open-file-in-window filename 'left nil bv-file-open-width-ratio))

(defun bv-open-file-right-jump (filename)
  "Open FILENAME in a new window to the right and jump to it."
  (interactive "FOpen file on right and jump: ")
  (bv-open-file-in-window filename 'right t bv-file-open-width-ratio))

(defun bv-open-file-right-stay (filename)
  "Open FILENAME in a new window to the right without changing focus."
  (interactive "FOpen file on right and stay: ")
  (bv-open-file-in-window filename 'right nil bv-file-open-width-ratio))

(defmacro bv-define-open-file-function (func-name file-path direction focus)
  "Define a function FUNC-NAME to open FILE-PATH in a window.
DIRECTION (`left` or `right`) specifies window placement. If FOCUS is
non-nil, the window will receive focus after opening."
  `(defun ,func-name ()
     (interactive)
     (bv-open-file-in-window ,file-path ,direction ,focus bv-file-open-width-ratio)
     (when (called-interactively-p 'any)
       (message "Opened %s in a %s window %s focus."
                ,file-path
                (if (eq ,direction 'left) "left" "right")
                (if ,focus "with" "without")))))

;; Define file opening functions (starting from <f9>).
(bv-define-open-file-function bv-open-my-init-el bv-init-el-path 'right t)
(bv-define-open-file-function bv-open-my-config-scm bv-config-scm-path 'right t)
(bv-define-open-file-function bv-open-my-home-config-scm bv-home-config-scm-path 'right t)
(bv-define-open-file-function bv-open-my-bib bv-bib-path 'right t)
(bv-define-open-file-function bv-open-my-snippets-org bv-snippets-org-path 'right t)
(bv-define-open-file-function bv-open-my-main-org bv-main-org-path 'right t)


(defun bv-clockable-org-files ()
  "Return a list of Org files in `bv-main-path' and `bv-notes-path' directories."
  (append
   (directory-files-recursively bv-notes-path "\\.org$")
   (directory-files-recursively bv-main-path "\\.org$")))

(provide 'bv-file-navigation)
;;; bv-file-navigation.el ends here
