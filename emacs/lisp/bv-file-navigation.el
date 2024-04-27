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
;; “People sometimes ask me if it is a sin in the Church of Emacs to use vi.
;; Using a free version of vi is not a sin; it is a penance.  So happy hacking.”
;;                                                    - Richard Stallman

;;; Code:

;; Customizable file paths
(defgroup bv-file-paths nil
  "Customizable file paths for bv-open-file functions."
  :group 'files
  :prefix "bv-")

(defcustom bv-init-el-path "~/projects/dotfiles/emacs/init.el"
  "Path to the Emacs init file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-config-scm-path "~/projects/dotfiles/guix/config.scm"
  "Path to the Guix configuration file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-main-org-path "~/main.org"
  "Path to the main Org file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-snippets-org-path "~/projects/latex-snippets/snippets.org"
  "Path to the LaTeX snippets Org file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-zshrc-path "~/projects/dotfiles/zsh/zshrc"
  "Path to the Zsh configuration file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-working-bib-path "~/slipbox/bibliographies/working.bib"
  "Path to the working bibliography file."
  :type 'string
  :group 'bv-file-paths)

(defcustom bv-file-open-width-ratio 0.4
  "Width ratio for opening files in a new window, relative to the frame width."
  :type 'float
  :group 'bv-file-paths)

(defun bv-open-file-in-window (filename &optional direction focus ratio)
  "Open FILENAME from the current window in the specified DIRECTION.
Optional arguments include DIRECTION for window placement (`left or `right),
FOCUS to determine if focus should switch to the new window, and RATIO for
width of the new window relative to the frame width (default is 2/5, or 0.4)."
  (interactive
   (list (read-file-name "Open file in window: ")
         (intern (completing-read "Direction (left or right): " '("left" "right")
                                  nil t "right"))
         (y-or-n-p "Switch to the new window? ")
         (or (read-number "Window width ratio (default 2/5): " bv-file-open-width-ratio) bv-file-open-width-ratio)))
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
  "Define a function FUNC-NAME to open FILE-PATH..
DIRECTION (`left or `right) specifies to which side the window will open.
If FOCUS is non-nil, the new window will receive focus after opening.
A width ratio specified by `bv-file-open-width-ratio' is used by default."
  `(defun ,func-name ()
     (interactive)
     (bv-open-file-in-window ,file-path ,direction ,focus bv-file-open-width-ratio)
     (when (called-interactively-p 'any)
       (message "Opened %s in a %s window %s focus."
                ,file-path
                (if (eq ,direction 'left) "left" "right")
                (if ,focus "with" "without")))))

(bv-define-open-file-function bv-open-my-init-el bv-init-el-path 'right t)
(bv-define-open-file-function bv-open-my-config-scm bv-config-scm-path 'right t)
(bv-define-open-file-function bv-open-my-main-org bv-main-org-path 'right t)
(bv-define-open-file-function bv-open-my-snippets-org bv-snippets-org-path 'right t)
(bv-define-open-file-function bv-open-my-zshrc bv-zshrc-path 'right t)
(bv-define-open-file-function bv-open-my-working-bib bv-working-bib-path 'right t)

(provide 'bv-file-navigation)
;;; bv-file-navigation.el ends here