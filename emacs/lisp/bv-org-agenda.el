;;; bv-org-agenda.el --- Org agenda configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for org agenda mode and appointments.

;;; Code:

(eval-when-compile (require 'org-agenda))


(autoload 'org-agenda-to-appt "org-agenda")
(autoload 'org-wild-notifier-mode "org-wild-notifier")
(autoload 'hack-dir-local-variables-non-file-buffer "files-x")
(autoload 's-truncate "s")
(autoload 's-pad-right "s")


(defgroup bv-org-agenda nil
  "Custom enhancements to the Org Agenda."
  :group 'bv)


(defun bv-org-agenda-to-appt ()
  "Reset the `appt-mode' list and initialize it from Agenda entries."
  (interactive)
  (when (boundp 'appt-time-msg-list)
    (setq appt-time-msg-list nil))
  (org-agenda-to-appt))

(defun bv-org-agenda-appt-reset ()
  "Initialize the `appt-mode' list for today and reset the timer."
  (interactive)
  (bv-org-agenda-to-appt)
  (set (if (boundp 'bv-org-agenda-appt-timer)
           'bv-org-agenda-appt-timer
         (make-local-variable 'bv-org-agenda-appt-timer))
       (run-at-time "24:01" nil 'bv-org-agenda-appt-reset)))

(define-minor-mode bv-org-agenda-appt-mode
  "Set up `appt-mode' integration for Agenda items."
  :global t
  :group 'bv-org-agenda
  (if bv-org-agenda-appt-mode
      (progn
        (set (if (boundp 'bv-org-agenda-appt-timer)
                 'bv-org-agenda-appt-timer
               (make-local-variable 'bv-org-agenda-appt-timer))
             (bv-org-agenda-appt-reset))
        (add-hook 'org-agenda-finalize-hook 'bv-org-agenda-to-appt))
    (progn
      (remove-hook 'org-agenda-finalize-hook 'bv-org-agenda-to-appt)
      (when (and (boundp 'bv-org-agenda-appt-timer) bv-org-agenda-appt-timer)
        (cancel-timer bv-org-agenda-appt-timer)))))

(bv-org-agenda-appt-mode)
(advice-add 'org-redo :after 'bv-org-agenda-to-appt)
(when (boundp 'org-capture-after-finalize-hook)
  (add-hook 'org-capture-after-finalize-hook 'bv-org-agenda-to-appt))

(defun bv-org-agenda-category (&optional len)
  "Get category of the Org Agenda item at point.
When LEN is a number, truncate and pad the result to LEN characters."
  (let* ((filename
          (when buffer-file-name
            (file-name-sans-extension
             (file-name-nondirectory buffer-file-name))))
         (title (cadr (assoc "TITLE" (org-collect-keywords '("title")))))
         (project-title
          (if (and title
                   (string-match (rx (group (+ any)) ":" (+ any)) title))
              (match-string 1 title)
            title))
         (category (org-get-category))
         (agenda-category
          (if (and title (string= category filename))
              project-title
            category))
         (result
          (or (if (numberp len)
                  (s-truncate len (s-pad-right len " " agenda-category))
                agenda-category)
              "")))
    (if (and (not project-title) (numberp len))
        (s-truncate len (s-pad-right len " " result))
      result)))

(when (boundp 'global-map)
  (define-key global-map (kbd "C-x C-a") 'org-agenda))
(when (boundp 'org-agenda-mode-hook)
  (add-hook 'org-agenda-mode-hook 'hack-dir-local-variables-non-file-buffer))

(defun bv-start-org-wild-notifier-for-primary-daemon ()
  "Run `org-wild-notifier-mode' when Emacs is started as primary daemon."
  (require 'server)
  (when (and (daemonp) 
             (boundp 'server-name) 
             (string= (symbol-value 'server-name) "server"))
    (when (fboundp 'org-wild-notifier-mode)
      (org-wild-notifier-mode))))

(when (boundp 'after-init-hook)
  (add-hook 'after-init-hook 'bv-start-org-wild-notifier-for-primary-daemon))

(with-eval-after-load 'org-agenda
  (setq org-agenda-tags-column 'auto)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-files
        '("~/documents/org/tasks.org"
          "~/documents/org/meetings.org"
          "~/documents/slipbox/slips/"))
  (setq org-agenda-sticky t)
  (setq org-agenda-block-separator ?-)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ "
          "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-log-mode-add-notes nil)
  (setq org-agenda-custom-commands
        (list (list (kbd "C-d")
                    "Agenda for the day"
                    '((agenda
                       ""
                       ((org-agenda-span 1)
                        (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
                        (org-agenda-block-separator nil)
                        (org-scheduled-past-days 0)
                        (org-super-agenda-unmatched-name 'none)
                        (org-super-agenda-unmatched-order 5)
                        (org-super-agenda-header-separator "\n")
                        (org-super-agenda-groups
                         `((:name "Clocked today" :log t :order 100)
                           (:name none :todo ("IDEA") :order 1)
                           (:name none :todo ("PROJ") :order 2)
                           (:name none
                                  :todo ,org-done-keywords-for-agenda
                                  :order 10)))
                        (org-agenda-day-face-function
                         (lambda (date) 'org-agenda-date))
                        (org-agenda-format-date "%A %-e %B %Y")
                        (org-agenda-overriding-header
                         "\nAgenda for the day\n")))
                     (todo "NEXT"
                           ((org-agenda-block-separator nil)
                            (org-agenda-overriding-header
                             "\nCurrent Tasks\n")))))
              (list (kbd "C-o")
                    "Overview"
                    '((agenda
                       "*"
                       ((org-agenda-scheduled-leaders '("" "Sched. %2dx:"))
                        (org-super-agenda-unmatched-name 'none)
                        (org-super-agenda-unmatched-order 5)
                        (org-super-agenda-header-separator "\n")
                        (org-super-agenda-groups
                         `((:name "Clocked today" :log t :order 100)
                           (:name none :todo ("IDEA") :order 1)
                           (:name none :todo ("PROJ") :order 2)
                           (:name none
                                  :todo ,org-done-keywords-for-agenda
                                  :order 10)))
                        (org-agenda-block-separator nil)
                        (org-agenda-span 14)
                        (org-agenda-show-future-repeats nil)
                        (org-agenda-skip-deadline-prewarning-if-scheduled t)
                        (org-agenda-overriding-header "\nAgenda\n")))
                     (agenda
                      ""
                      ((org-agenda-start-on-weekday nil)
                       (org-agenda-start-day "+1d")
                       (org-agenda-span 14)
                       (org-agenda-show-all-dates nil)
                       (org-agenda-time-grid nil)
                       (org-agenda-show-future-repeats nil)
                       (org-agenda-block-separator nil)
                       (org-agenda-entry-types '(:deadline))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'done))
                       (org-agenda-overriding-header
                        "\nUpcoming deadlines (+14d)\n")))
                     (alltodo
                      ""
                      ((org-agenda-block-separator nil)
                       (org-agenda-skip-function
                        '(or (org-agenda-skip-if nil '(scheduled))))
                       (org-super-agenda-unmatched-name 'none)
                       (org-super-agenda-unmatched-order 5)
                       (org-super-agenda-header-separator "\n")
                       (org-super-agenda-groups
                        `((:name "Clocked today" :log t :order 100)
                          (:name none :todo ("IDEA") :order 1)
                          (:name none :todo ("PROJ") :order 2)
                          (:name none
                                 :todo ,org-done-keywords-for-agenda
                                 :order 10)))
                       (org-agenda-overriding-header "\nBacklog\n")))))))
  (setq org-agenda-bulk-custom-functions
        '((?P (lambda nil (org-agenda-priority 'set)))))
  (setq org-agenda-prefix-format '())
  (autoload 'org-super-agenda-mode "org-super-agenda"))

(with-eval-after-load 'org-super-agenda
  (when (fboundp 'org-super-agenda-mode)
    (org-super-agenda-mode)))

(provide 'bv-org-agenda)
;;; bv-org-agenda.el ends here