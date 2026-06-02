;;; bv-org.el --- Minimal Org setup  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Basic Org mode setup.

;;; Code:

;; Load LaTeX preview configuration early so Org buffers inherit the resolved
;; preview/font policy on first paint.
(require 'bv-org-latex nil t)

;;;; Command map and external declarations

(unless (boundp 'bv-org-map)
  (defvar bv-org-map (make-sparse-keymap)
    "BV Org, agenda, capture, and clocking commands."))

(defvar org-adapt-indentation)
(defvar org-agenda-mode-map)
(defvar org-agenda-files)
(defvar org-archive-location)
(defvar org-blank-before-new-entry)
(defvar org-capture-templates)
(defvar org-clock-history-length)
(defvar org-clock-in-resume)
(defvar org-clock-in-switch-to-state)
(defvar org-clock-into-drawer)
(defvar org-clock-out-remove-zero-time-clocks)
(defvar org-clock-out-when-done)
(defvar org-clock-persist)
(defvar org-clock-report-include-clocking-task)
(defvar org-default-notes-file)
(defvar org-directory)
(defvar org-enforce-todo-dependencies)
(defvar org-hide-emphasis-markers)
(defvar org-log-done)
(defvar org-log-into-drawer)
(defvar org-mode-map)
(defvar org-outline-path-complete-in-steps)
(defvar org-refile-allow-creating-parent-nodes)
(defvar org-refile-targets)
(defvar org-refile-use-outline-path)
(defvar org-src-content-indentation)
(defvar org-startup-indented)

(declare-function org-agenda-clock-in "org-agenda" (&optional arg))
(declare-function org-agenda-clock-out "org-agenda" ())
(declare-function org-clock-in "org-clock" (&optional select start-time))
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-persistence-insinuate "org-clock" ())

;;;; Customization

(defgroup bv-org nil
  "BV Org configuration."
  :group 'org
  :prefix "bv-org-")

(defcustom bv-org-directory (expand-file-name "~/org")
  "Base directory for Org files."
  :type 'directory
  :group 'bv-org)

;;;; Paths

(defun bv-org--main-file ()
  "Return the absolute path to the main Org file."
  (expand-file-name "main.org" bv-org-directory))

;;;; File commands

(defun bv-org-open-main ()
  "Open the Org main file."
  (interactive)
  (find-file (bv-org--main-file)))

;;;; Clocking

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

;;;; Runtime setup

(with-eval-after-load 'org-id
  (when (boundp 'org-id-locations-file)
    (setq org-id-locations-file
          (concat
           (or (getenv "XDG_CACHE_HOME") "~/.cache")
           "/emacs/org-id-locations"))))

(with-eval-after-load 'org
  (setq org-directory bv-org-directory)
  (setq org-default-notes-file (bv-org--main-file))
  (setq org-agenda-files (list (bv-org--main-file)))

  (setq org-capture-templates
        `(("t" "Task" entry
           (file ,(bv-org--main-file))
           "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n%a\n"
           :empty-lines 1)
          ("n" "Note" entry
           (file ,(bv-org--main-file))
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a\n"
           :empty-lines 1)))

  ;; Editing preferences.
  (setq org-adapt-indentation nil)
  (setq org-src-content-indentation 0)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto)))

  ;; Logging and movement inside the main file.
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-archive-location
        (concat (bv-org--main-file) "::* Archive"))

  ;; Clocking.
  (setq org-clock-into-drawer "LOGBOOK")
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)
  (setq org-clock-in-resume t)
  (setq org-clock-in-switch-to-state nil)
  (setq org-clock-out-when-done t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-clock-history-length 40)

  ;; Keep clocking keys stable (do not steal window focus).
  (define-key org-mode-map (kbd "C-c C-x C-i") #'bv-org-clock-in)
  (define-key org-mode-map (kbd "C-c C-x TAB") #'bv-org-clock-in)
  (define-key org-mode-map (kbd "C-c C-x C-o") #'bv-org-clock-out)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c C-x C-i") #'bv-org-clock-in)
    (define-key org-agenda-mode-map (kbd "C-c C-x TAB") #'bv-org-clock-in)
    (define-key org-agenda-mode-map (kbd "C-c C-x C-o") #'bv-org-clock-out)))

;;;; Keybindings

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-org-map)
    (define-key bv-org-map (kbd "a") #'org-agenda)
    (define-key bv-org-map (kbd "c") #'org-capture)
    (define-key bv-org-map (kbd "m") #'bv-org-open-main)
    (define-key bv-org-map (kbd "I") #'bv-org-clock-in)
    (define-key bv-org-map (kbd "O") #'bv-org-clock-out)))

(provide 'bv-org)
;;; bv-org.el ends here
