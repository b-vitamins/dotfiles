;;; bv-essentials.el --- Emacs configuration file -*- lexical-binding: t -*-

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

(require 'flycheck)
(require 'god-mode)

(defvar-local bv-straight-bootstrap-retries 3
  "Default number of retries for bootstrapping straight.el.")

;; Define global variables to store the default mode line settings
(defvar bv-default-mode-line-foreground nil
	"Default foreground color of the mode line.")

(defvar bv-default-mode-line-background nil
"Default background color of the mode line.")

(defvar bv-default-mode-line-inactive-foreground nil
	"Default foreground color of the inactive mode line.")

(defvar bv-default-mode-line-inactive-background nil
"Default background color of the inactive mode line.")

(defun bv-store-default-mode-line-colors ()
	"Save default mode line faces for later use."
  (setq bv-default-mode-line-foreground (face-foreground 'mode-line))
  (setq bv-default-mode-line-background (face-background 'mode-line))
  (setq bv-default-mode-line-inactive-foreground (face-foreground 'mode-line-inactive))
  (setq bv-default-mode-line-inactive-background (face-background 'mode-line-inactive)))

(defun bv-bootstrap-straight (&optional retries)
  "Bootstrap straight.el with optional RETRIES on failure."
  (interactive "P")
  (unless (featurep 'straight)
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el"
                             (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
          (bootstrap-version 7)
          (attempt 1)
          ;; Use the provided RETRIES value or the default stored in `bv-straight-bootstrap-retries`.
          (max-attempts (if retries (prefix-numeric-value retries) bv-straight-bootstrap-retries)))
      (while (and (not (file-exists-p bootstrap-file))
                  (<= attempt max-attempts))
        (message "Attempting to bootstrap straight.el (Attempt %d/%d)..." attempt max-attempts)
        (condition-case err
            (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
              (goto-char (point-max))
              (eval-print-last-sexp))
          (error
           (message "Failed to bootstrap straight.el: %s" err)
           (setq attempt (1+ attempt))
           (when (< attempt max-attempts)
             (message "Retrying in 5 seconds...")
             (sleep-for 5)))))
      (if (file-exists-p bootstrap-file)
          (progn
            (load bootstrap-file nil 'nomessage)
            (message "Successfully bootstrapped straight.el."))
        (when (= attempt (1+ max-attempts))
          (error "Failed to bootstrap straight.el after %d attempts" max-attempts))))))

(defun bv-guix-p ()
  "Return t if running on a GNU Guix OS, nil otherwise."
  (let ((os-release "/etc/os-release"))
    (if (and (file-readable-p os-release)
             (string-match-p "ID=guix" (with-temp-buffer
                                         (insert-file-contents os-release)
                                         (buffer-string))))
        t
      nil)))

;; Borrowed with modification from Bastien Guerry <bzg@bzg.fr>
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
        ;; Simplified message for enabling the mode
        (when (called-interactively-p 'interactive)
          (message "hidden-mode-line-mode enabled.")))
    (setq mode-line-format hide-mode-line
          hide-mode-line nil)
    (force-mode-line-update)
    (redraw-display)
    ;; Simplified message for disabling the mode
    (when (called-interactively-p 'interactive)
      (message "hidden-mode-line-mode disabled."))))

(defun bv-setup-org-fonts ()
  "Configure font settings for Org-mode, checking font availability."
  (let ((dejavu-available (member "DejaVu Sans" (font-family-list)))
        (iosevka-available (member "Iosevka Comfy" (font-family-list))))
    ;; Set the document title font if DejaVu Sans is available
    (when dejavu-available
      (set-face-attribute 'org-document-title nil
                          :font "DejaVu Sans" :height 1.0))
    ;; Set heading fonts if Iosevka Comfy is available
    (when iosevka-available
      (dolist (face '((org-level-1 . 1.1)
                      (org-level-2 . 1.05)
                      (org-level-3 . 1.0)
                      (org-level-4 . 1.0)
                      (org-level-5 . 1.0)
                      (org-level-6 . 1.0)
                      (org-level-7 . 1.0)
                      (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil
                            :font "Iosevka Comfy"
                            :weight 'normal
                            :height (* 1.0 (cdr face)))))
    ;; Set fixed-pitch attributes for specific elements if either font is available
    (when (or dejavu-available iosevka-available)
      (dolist (face '(org-block org-table org-formula org-code org-verbatim
                     org-special-keyword org-meta-line org-checkbox))
        (set-face-attribute face nil :inherit 'fixed-pitch)))))

(defun bv-copy-flycheck-overlay-at-point-to-kill-ring ()
    "Copy the Flycheck overlay messages at point to the kill ring."
    (interactive)
    (let ((overlays (flycheck-overlays-at (point))))
      (if overlays
          (let ((overlay-msgs (mapconcat
                               (lambda (ov)
                                 (let ((help-echo (overlay-get ov 'help-echo))
                                       (pos (point)))
                                   (if (functionp help-echo)
                                       (funcall help-echo nil ov pos)
                                     help-echo)))
                               overlays "\n")))
            (kill-new overlay-msgs)
            (message "Copied Flycheck messages to kill ring."))
        (message "No Flycheck messages at point."))))

(defun bv-blacken-buffer ()
  "Run `black' on the current file with a line length of 80 and reload the buffer.
Ensures that `black' is installed and available."
  (interactive)
  (if (buffer-file-name)
      (if (executable-find "black")
          (let ((file (buffer-file-name)))
            (shell-command (format "black --line-length 80 %s" (shell-quote-argument file)))
            (revert-buffer t t t)  ; Revert the buffer without confirmation
            (message "Buffer has been blackened to 80 columns and reloaded."))
        (message "Error: `black` is not installed or not in the executable path."))
    (message "No file is associated with this buffer.")))

(defun bv-god-mode-update-cursor-type ()
  "Update the cursor type based on the current mode.
Use a `box cursor when in `god-local-mode' or when
the buffer is read-only, and a `bar cursor otherwise."
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(defun bv-god-mode-update-mode-line ()
  "Update the mode line appearance based on `god-local-mode'.
When `god-local-mode' is active, set distinct colors.
Otherwise, revert to theme defaults stored in global variables."
  (cond
	 (god-local-mode
    (set-face-attribute 'mode-line nil
                        :foreground "#303030"
                        :background "#fffff5")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#383838"
                        :background "#fffff2"))
   (t
    (set-face-attribute 'mode-line nil
                        :foreground bv-default-mode-line-foreground
                        :background bv-default-mode-line-background)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground bv-default-mode-line-inactive-foreground
                        :background bv-default-mode-line-inactive-background))))

(defun bv-god-mode-self-insert ()
  "Handle self-insert operations differently based on context.
In `org-mode` at the beginning of a line, delegate to `org-self-insert-command`.
Use `god-mode-self-insert` in other cases."
  (interactive)
  (if (and (bolp) (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))

(defun bv-zap-newline-at-eob ()
	"Delete the last newline character at the end of the buffer, if present.
If `make-backup-files' is enabled, this function will disable it temporarily."
	(let ((make-backup-files nil))
		(goto-char (point-max))
		(when (equal (char-before) ?\n)
			(delete-char -1)
			(save-buffer))))

(defmacro bv-define-insert-delimiter (name char)
  "Define a function NAME for inserting a single character CHAR."
  `(defun ,name ()
     (interactive)
     (insert ,char)))

;; Generate insert functions for various delimiters
(bv-define-insert-delimiter bv-insert-open-paren "(")
(bv-define-insert-delimiter bv-insert-close-paren ")")
(bv-define-insert-delimiter bv-insert-open-brace "{")
(bv-define-insert-delimiter bv-insert-close-brace "}")
(bv-define-insert-delimiter bv-insert-open-bracket "[")
(bv-define-insert-delimiter bv-insert-close-bracket "]")
(bv-define-insert-delimiter bv-insert-single-quote "'")
(bv-define-insert-delimiter bv-insert-double-quote "\"")
(bv-define-insert-delimiter bv-insert-backtick "`")

(defun bv-move-to-trash ()
  "Move the current file to ~/trash and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (trash-dir (expand-file-name "~/trash")))
    (when filename
      (if (yes-or-no-p (format "Really move file '%s' to trash? " filename))
          (progn
            (unless (file-directory-p trash-dir)
              (make-directory trash-dir t))
            (let ((new-location (expand-file-name (file-name-nondirectory filename) trash-dir)))
              (if (file-exists-p new-location)
                  (message "A file with the same name already exists in the trash.")
                (progn
                  (rename-file filename new-location)
                  (message "Moved file to trash: %s" new-location)
                  (kill-buffer)))))
        (message "Operation cancelled.")))))

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
         (or (read-number "Window width ratio (default 2/5): " 0.4) 0.4)))
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
  (bv-open-file-in-window filename 'left t 0.4))

(defun bv-open-file-left-stay (filename)
  "Open FILENAME in a new window to the left without changing focus."
  (interactive "FOpen file on left and stay: ")
  (bv-open-file-in-window filename 'left nil 0.4))

(defun bv-open-file-right-jump (filename)
  "Open FILENAME in a new window to the right and jump to it."
  (interactive "FOpen file on right and jump: ")
  (bv-open-file-in-window filename 'right t 0.4))

(defun bv-open-file-right-stay (filename)
  "Open FILENAME in a new window to the right without changing focus."
  (interactive "FOpen file on right and stay: ")
  (bv-open-file-in-window filename 'right nil 0.4))

(provide 'bv-essentials)
;;; bv-essentials.el ends here
