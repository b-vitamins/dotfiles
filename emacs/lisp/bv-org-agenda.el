;;; bv-org-agenda.el --- Org agenda configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Agenda dashboards and review views.

;;; Code:

(require 'calendar)
(require 'org-agenda)
(require 'bv-org)

(defvar bv-org-directory)

(defun bv-org-agenda--inbox-file ()
  "Return the absolute path of the inbox file."
  (expand-file-name "inbox.org" bv-org-directory))

(defun bv-org-agenda--goals-file ()
  "Return the absolute path of the goals file."
  (expand-file-name "goals.org" bv-org-directory))

(defun bv-org-agenda--project-has-next-action-p ()
  "Return non-nil when the current subtree contains a NEXT/STARTED task."
  (save-restriction
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\)\\b" nil t)))

(defun bv-org-agenda-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects.

A stuck project is a PROJ heading without any NEXT/STARTED items in its
subtree."
  (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
    (cond
     ((not (equal (org-get-todo-state) "PROJ")) subtree-end)
     ((bv-org-agenda--project-has-next-action-p) subtree-end)
     (t nil))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-block-separator nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-tags-column 0)
  (setq org-agenda-todo-keyword-format "")
  (setq org-agenda-prefix-format
        '((agenda . "  %-12:c%?-12t% s")
          (todo . "  %-12:c ")
          (tags . "  %-12:c ")
          (search . "  %-12:c ")))

  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-current-time-string
        "◀ ─────────────────────────────────────────────── now")

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)

  (setq org-agenda-custom-commands
        `(("d" "Dashboard"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day "+0d")
                     (org-agenda-overriding-header "Today")
                     (org-agenda-format-date "\n%A %-e %B %Y\n")
                     (org-deadline-warning-days 7)))
            (todo "STARTED" ((org-agenda-overriding-header "In progress")))
            (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
            (tags-todo "STYLE=\"habit\""
                       ((org-agenda-overriding-header "Habits")
                        (org-agenda-files (list (bv-org-agenda--goals-file)))))
            (tags-todo "deep/!NEXT|STARTED"
                       ((org-agenda-overriding-header "Deep work")))
            (tags-todo "@errand/!NEXT|STARTED"
                       ((org-agenda-overriding-header "Errands")))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting / blocked")))
            (alltodo ""
                     ((org-agenda-overriding-header "Inbox (process/refile)")
                      (org-agenda-files (list (bv-org-agenda--inbox-file)))
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-scheduled t))))
           ((org-agenda-window-setup 'current-window)))

          ("R" "Weekly review"
           ((agenda ""
                    ((org-agenda-start-day "-14d")
                     (org-agenda-span 14)
                     (org-agenda-overriding-header "Past 2 weeks")))
            (agenda ""
                    ((org-agenda-start-day "+0d")
                     (org-agenda-span 14)
                     (org-agenda-overriding-header "Next 2 weeks")))
            (alltodo ""
                     ((org-agenda-overriding-header "Inbox (process/refile)")
                      (org-agenda-files (list (bv-org-agenda--inbox-file)))))
            (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
            (todo "PROJ"
                  ((org-agenda-overriding-header "Stuck projects (no NEXT)")
                   (org-agenda-skip-function 'bv-org-agenda-skip-non-stuck-projects))))
           ((org-agenda-start-with-log-mode t)
            (org-agenda-log-mode-items '(closed clock))
            (org-agenda-window-setup 'current-window))))))

(defun bv-org-agenda-calendar ()
  "Show the dashboard with a calendar split."
  (interactive)
  (org-agenda nil "d")
  (delete-other-windows)
  (split-window-horizontally -24)
  (other-window 1)
  (calendar)
  (other-window 1))

(defun bv-org-agenda-quick-task ()
  "Capture a quick task (template \"q\")."
  (interactive)
  (require 'org-capture)
  (org-capture nil "q"))

(with-eval-after-load 'calendar
  (setq calendar-week-start-day 1))

(provide 'bv-org-agenda)
;;; bv-org-agenda.el ends here
