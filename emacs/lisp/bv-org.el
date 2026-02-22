;;; bv-org.el --- Org mode configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; A lightweight Org setup:
;; - Default TODO keywords (TODO/DONE)
;; - Stable clock-in/out commands (no window stealing)
;; - Optional idle auto clock-out
;; - A single inbox file + minimal capture template

;;; Code:

(require 'seq)

;; Load LaTeX preview configuration early.
(require 'bv-org-latex nil t)

(define-prefix-command 'bv-org-map)

(defgroup bv-org nil
  "Lightweight Org configuration."
  :group 'org
  :prefix "bv-org-")

(defcustom bv-org-directory (expand-file-name "~/org")
  "Base directory for Org files."
  :type 'directory
  :group 'bv-org)

(defcustom bv-org-agenda-exclude-dirs '("archive" "roam" "slipbox" "daily")
  "Subdirectories under `bv-org-directory' excluded from agenda discovery."
  :type '(repeat string)
  :group 'bv-org)

(defcustom bv-org-clock-idle-minutes 12
  "Clock out after this many minutes of Emacs idleness.

When nil, do not auto clock out."
  :type '(choice (const :tag "Disable" nil)
                 (integer :tag "Minutes"))
  :group 'bv-org)

(defcustom bv-org-clock-idle-check-interval 60
  "Seconds between idle checks for automatic clock-out."
  :type 'integer
  :group 'bv-org)

(defconst bv-org--inbox-file "inbox.org")

(defun bv-org--path (relative)
  "Return absolute path for RELATIVE inside `bv-org-directory'."
  (expand-file-name relative bv-org-directory))

(defun bv-org--agenda-file-p (file)
  "Return non-nil when FILE should be included in `org-agenda-files'."
  (let ((rel (file-relative-name file (file-name-as-directory bv-org-directory))))
    (and (string-suffix-p ".org" file t)
         (not (string-prefix-p "." (file-name-nondirectory file)))
         (not (seq-some (lambda (dir)
                          (string-prefix-p (file-name-as-directory dir) rel))
                        bv-org-agenda-exclude-dirs)))))

(defun bv-org--discover-agenda-files ()
  "Discover agenda files under `bv-org-directory'."
  (when (file-directory-p bv-org-directory)
    (let ((files (directory-files-recursively bv-org-directory "\\.org\\'")))
      (seq-filter #'bv-org--agenda-file-p files))))

(defun bv-org--ensure-file (file initial-contents)
  "Ensure FILE exists; when missing, create it with INITIAL-CONTENTS."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (unless (file-exists-p file)
    (with-temp-buffer
      (insert initial-contents)
      (write-region (point-min) (point-max) file nil 'silent))))

(defun bv-org-ensure-structure ()
  "Ensure `bv-org-directory' and a minimal inbox file exist."
  (unless (file-directory-p bv-org-directory)
    (make-directory bv-org-directory t))
  (bv-org--ensure-file
   (bv-org--path bv-org--inbox-file)
   "#+title: Inbox\n#+startup: overview\n\n* Inbox\n"))

;;;; Clocking: one-task-at-a-time + idle auto clock-out

(defvar bv-org--clock-idle-timer nil
  "Timer used to enforce idle auto clock-out.")

(defun bv-org--maybe-auto-clock-out ()
  "Clock out when Emacs has been idle long enough."
  (when (and (numberp bv-org-clock-idle-minutes)
             (> bv-org-clock-idle-minutes 0)
             (fboundp 'org-clocking-p)
             (org-clocking-p))
    (when-let ((idle (current-idle-time)))
      (when (>= (float-time idle) (* 60 bv-org-clock-idle-minutes))
        (ignore-errors (org-clock-out))
        (message "Org: clocked out due to %dm idle" bv-org-clock-idle-minutes)))))

(defun bv-org--setup-idle-clockout ()
  "Start or stop the idle clockout timer based on configuration."
  (when (timerp bv-org--clock-idle-timer)
    (cancel-timer bv-org--clock-idle-timer)
    (setq bv-org--clock-idle-timer nil))
  (when (and (numberp bv-org-clock-idle-minutes)
             (> bv-org-clock-idle-minutes 0))
    (setq bv-org--clock-idle-timer
          (run-with-timer bv-org-clock-idle-check-interval
                          bv-org-clock-idle-check-interval
                          #'bv-org--maybe-auto-clock-out))))

(defun bv-org-clock-in (arg)
  "Clock in without stealing the current window.

In `org-agenda-mode', clock in the item at point.  Otherwise, call
`org-clock-in'.

With prefix ARG, pass it through."
  (interactive "P")
  (require 'org-clock)
  (save-window-excursion
    (cond
     ((derived-mode-p 'org-agenda-mode)
      (require 'org-agenda)
      (org-agenda-clock-in arg))
     (t
      (org-clock-in arg))))
  (force-mode-line-update t))

(defun bv-org-clock-out (arg)
  "Clock out without stealing the current window.

In `org-agenda-mode', stop the current clock via `org-agenda-clock-out'.
Otherwise call `org-clock-out'.

With prefix ARG, pass it through to `org-clock-out'."
  (interactive "P")
  (require 'org-clock)
  (save-window-excursion
    (cond
     ((derived-mode-p 'org-agenda-mode)
      (require 'org-agenda)
      (org-agenda-clock-out))
     (t
      (org-clock-out arg))))
  (force-mode-line-update t))

(with-eval-after-load 'org-id
  (when (boundp 'org-id-locations-file)
    (setq org-id-locations-file
          (concat
           (or (getenv "XDG_CACHE_HOME") "~/.cache")
           "/emacs/org-id-locations"))))

(with-eval-after-load 'org
  (bv-org-ensure-structure)

  (setq org-directory bv-org-directory)
  (setq org-default-notes-file (bv-org--path bv-org--inbox-file))
  (setq org-agenda-files (bv-org--discover-agenda-files))

  ;; Editing preferences.
  (setq org-adapt-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)

  ;; Logging.
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)

  ;; Clocking.
  (setq org-clock-into-drawer "LOGBOOK")
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)
  (setq org-clock-in-resume t)
  (setq org-clock-out-when-done t)
  (setq org-clock-history-length 20)

  (bv-org--setup-idle-clockout)

  ;; Keep clocking keys stable (don't steal window focus).
  (define-key org-mode-map (kbd "C-c C-x C-i") #'bv-org-clock-in)
  (define-key org-mode-map (kbd "C-c C-x TAB") #'bv-org-clock-in)
  (define-key org-mode-map (kbd "C-c C-x C-o") #'bv-org-clock-out)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c C-x C-i") #'bv-org-clock-in)
    (define-key org-agenda-mode-map (kbd "C-c C-x TAB") #'bv-org-clock-in)
    (define-key org-agenda-mode-map (kbd "C-c C-x C-o") #'bv-org-clock-out))

  ;; Minimal capture: a single TODO entry into the inbox.
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(bv-org--path bv-org--inbox-file) "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
           :empty-lines 1))))

(with-eval-after-load 'bv-bindings
  (defvar bv-app-map)
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "o") bv-org-map)
    (define-key bv-org-map (kbd "a") #'org-agenda)
    (define-key bv-org-map (kbd "c") #'org-capture)
    (define-key bv-org-map (kbd "i")
                (lambda ()
                  (interactive)
                  (find-file (bv-org--path bv-org--inbox-file))))
    (define-key bv-org-map (kbd "I") #'bv-org-clock-in)
    (define-key bv-org-map (kbd "O") #'bv-org-clock-out)))

(provide 'bv-org)
;;; bv-org.el ends here
