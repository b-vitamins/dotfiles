;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/init.el
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

(require 'windmove)
(require 'lsp)

(defun bv-not-guix ()
  "Check if running on a non-GNU Guix OS."
  (let ((os-release "/etc/os-release"))
    (if (and (file-readable-p os-release)
             (string-match-p "ID=guix" (with-temp-buffer
                                         (insert-file-contents os-release)
                                         (buffer-string))))
        nil
      t)))

(defun bv-windmove-nw (&optional arg)
  "Select the window in the northwest direction from the current one.
With a prefix ARG, move ARG windows at a time."
  (interactive "P")
  (windmove-do-window-select 'left (and arg (prefix-numeric-value arg)))
  (windmove-do-window-select 'up (and arg (prefix-numeric-value arg))))

(defun bv-windmove-ne (&optional arg)
  "Select the window in the northeast direction from the current one.
With a prefix ARG, move ARG windows at a time."
  (interactive "P")
  (windmove-do-window-select 'right (and arg (prefix-numeric-value arg)))
  (windmove-do-window-select 'up (and arg (prefix-numeric-value arg))))

(defun bv-windmove-sw (&optional arg)
  "Select the window in the southwest direction from the current one.
With a prefix ARG, move ARG windows at a time."
  (interactive "P")
  (windmove-do-window-select 'left (and arg (prefix-numeric-value arg)))
  (windmove-do-window-select 'down (and arg (prefix-numeric-value arg))))

(defun bv-windmove-se (&optional arg)
  "Select the window in the southeast direction from the current one.
With a prefix ARG, move ARG windows at a time."
  (interactive "P")
  (windmove-do-window-select 'right (and arg (prefix-numeric-value arg)))
  (windmove-do-window-select 'down (and arg (prefix-numeric-value arg))))

;; Borrowed from Bastien Guerry <bzg@bzg.fr>
;; https://bzg.fr/en/emacs-strip-tease/
;;
(defvar-local hide-mode-line nil
  "Variable to store the original mode-line format.")

(defvar-local hidden-mode-line-mode nil
  "Variable to store the original mode-line format.")

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (progn
        (setq hide-mode-line mode-line-format
              mode-line-format nil)
        (force-mode-line-update)
        (redraw-display)
        (when (called-interactively-p 'interactive)
          (run-with-idle-timer
           0 nil 'message
           (concat "Hidden Mode Line Mode enabled.  "
                   "Use M-x hidden-mode-line-mode to make the mode-line appear."))))
    (setq mode-line-format hide-mode-line
          hide-mode-line nil)
    (force-mode-line-update)
    (redraw-display)))

(defun bv-lsp-copy-diagnostic-at-point ()
  "Copy the lsp-ui diagnostic message at point to the clipboard."
  (interactive)
  (let ((diagnostics (lsp-diagnostics))
        (current-point (point))
        (message-to-copy nil))
    (maphash
     (lambda (file diagnostic-data)
       (when (string= (buffer-file-name) file)
         (dolist (diag (gethash "publishDiagnostics" diagnostic-data))
           (let* ((range (gethash "range" diag))
                  (start (gethash "start" range))
                  (end (gethash "end" range))
                  (start-point (lsp-point-to-position start))
                  (end-point (lsp-point-to-position end)))
             (when (and (>= current-point start-point) (<= current-point end-point))
               (setq message-to-copy (gethash "message" diag)))))))
     diagnostics)
    (if message-to-copy
        (progn
          (kill-new message-to-copy)
          (message "Diagnostic message copied to clipboard."))
      (message "No diagnostic message at point."))))

(provide 'bv-essentials)
;;; bv-essentials.el ends here
