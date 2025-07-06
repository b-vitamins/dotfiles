;;; bv-org-agenda.el --- Org agenda configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Beautiful agenda views inspired by nano-emacs.

;;; Code:

(require 'org-agenda)
(require 'calendar)

;; Clean agenda display
(with-eval-after-load 'org-agenda
  ;; Remove clutter
  (when (boundp 'org-agenda-block-separator)
    (setq org-agenda-block-separator nil))
  (when (boundp 'org-agenda-compact-blocks)
    (setq org-agenda-compact-blocks t))
  (when (boundp 'org-agenda-tags-column)
    (setq org-agenda-tags-column 0))
  (when (boundp 'org-agenda-todo-keyword-format)
    (setq org-agenda-todo-keyword-format ""))
  (when (boundp 'org-agenda-prefix-format)
    (setq org-agenda-prefix-format
          '((agenda . "  %?-12t% s")
            (todo . "  ")
            (tags . "  ")
            (search . "  "))))

  ;; Time grid
  (when (boundp 'org-agenda-time-grid)
    (setq org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))
  (when (boundp 'org-agenda-current-time-string)
    (setq org-agenda-current-time-string
          "◀ ─────────────────────────────────────────────── now"))

  ;; Window behavior
  (when (boundp 'org-agenda-window-setup)
    (setq org-agenda-window-setup 'current-window))
  (when (boundp 'org-agenda-restore-windows-after-quit)
    (setq org-agenda-restore-windows-after-quit t))

  ;; Files
  (when (boundp 'org-agenda-files)
    (setq org-agenda-files '("~/documents/org/")))

  ;; Custom agenda views
  (when (boundp 'org-agenda-custom-commands)
    (setq org-agenda-custom-commands
          '(("d" "Daily view"
             ((agenda ""
                      ((org-agenda-span 'day)
                       (org-agenda-overriding-header "")
                       (org-agenda-format-date "\n%A %-e %B %Y\n")
                       (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                       (org-deadline-warning-days 0)
                       (org-agenda-skip-scheduled-if-deadline-is-shown t)))))

            ("w" "Week view"
             ((agenda ""
                      ((org-agenda-span 'week)
                       (org-agenda-overriding-header "")
                       (org-agenda-format-date "%A %-e %B")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))))

            ("n" "Now - Active tasks"
             ((todo "STARTED|NEXT"
                    ((org-agenda-overriding-header "\nActive tasks\n")))
              (agenda ""
                      ((org-agenda-span 'day)
                       (org-agenda-overriding-header "\nToday\n")))))

            ("r" "Research"
             ((todo "IDEA|DRAFT|EXPERIMENT"
                    ((org-agenda-overriding-header "\nResearch pipeline\n")
                     (org-agenda-sorting-strategy '(priority-down effort-up)))))))))

  ;; Face customization
  (defun bv-org-agenda-setup-faces ()
    "Apply minimal, elegant faces to org-agenda."
    ;; Headers
    (when (facep 'org-agenda-structure)
      (set-face-attribute 'org-agenda-structure nil
                          :inherit 'bv-face-strong
                          :weight 'light
                          :height 1.2))
    ;; Dates
    (when (facep 'org-agenda-date)
      (set-face-attribute 'org-agenda-date nil
                          :inherit 'bv-face-strong
                          :weight 'regular
                          :underline nil))
    (when (facep 'org-agenda-date-today)
      (set-face-attribute 'org-agenda-date-today nil
                          :inherit 'bv-face-salient
                          :weight 'regular
                          :underline t))
    (when (facep 'org-agenda-date-weekend)
      (set-face-attribute 'org-agenda-date-weekend nil
                          :inherit 'bv-face-faded
                          :weight 'regular))
    ;; Current time
    (when (facep 'org-agenda-current-time)
      (set-face-attribute 'org-agenda-current-time nil
                          :inherit 'bv-face-popout))
    ;; Tasks
    (when (facep 'org-scheduled)
      (set-face-attribute 'org-scheduled nil
                          :inherit 'bv-face-faded))
    (when (facep 'org-scheduled-today)
      (set-face-attribute 'org-scheduled-today nil
                          :inherit 'bv-face-default))
    (when (facep 'org-agenda-done)
      (set-face-attribute 'org-agenda-done nil
                          :inherit 'bv-face-subtle
                          :strike-through nil))
    ;; Hide keywords
    (when (facep 'org-todo)
      (set-face-attribute 'org-todo nil
                          :inherit 'bv-face-popout
                          :weight 'regular
                          :box nil))
    (when (facep 'org-done)
      (set-face-attribute 'org-done nil
                          :inherit 'bv-face-subtle
                          :weight 'regular
                          :box nil)))

  (add-hook 'org-agenda-mode-hook #'bv-org-agenda-setup-faces)
  (add-hook 'bv-after-theme-hook #'bv-org-agenda-setup-faces))

;; Nano-style calendar popup
(defun bv-org-agenda-calendar ()
  "Show a calendar-centric agenda view."
  (interactive)
  (let ((org-agenda-span 'day)
        (org-agenda-window-setup 'only-window)
        (org-agenda-format-date "\n  %A %-e %B %Y\n")
        (org-agenda-overriding-header ""))
    (org-agenda nil "d")
    (delete-other-windows)
    (split-window-horizontally -24)
    (other-window 1)
    (calendar)
    (other-window 1)))

;; Quick capture
(defun bv-org-agenda-quick-task ()
  "Quickly add a task without leaving current context."
  (interactive)
  (let ((org-capture-templates
         '(("q" "Quick task" entry
            (file "~/documents/org/tasks.org")
            "* TODO %?\n  SCHEDULED: %t\n"))))
    (org-capture nil "q")))

;; Keybindings
(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "a") 'org-agenda)
    (define-key bv-app-map (kbd "A") 'bv-org-agenda-calendar)
    (define-key bv-app-map (kbd "t") 'bv-org-agenda-quick-task)))

;; Calendar beautification
(with-eval-after-load 'calendar
  (when (boundp 'calendar-week-start-day)
    (setq calendar-week-start-day 1))

  (defun bv-calendar-setup-faces ()
    "Beautify calendar."
    (when (facep 'calendar-today)
      (set-face-attribute 'calendar-today nil
                          :inherit 'bv-face-salient
                          :weight 'bold
                          :underline t))
    (when (facep 'calendar-weekday-header)
      (set-face-attribute 'calendar-weekday-header nil
                          :inherit 'bv-face-faded))
    (when (facep 'calendar-weekend-header)
      (set-face-attribute 'calendar-weekend-header nil
                          :inherit 'bv-face-subtle))
    (when (facep 'calendar-month-header)
      (set-face-attribute 'calendar-month-header nil
                          :inherit 'bv-face-strong
                          :height 1.1)))

  (add-hook 'calendar-mode-hook #'bv-calendar-setup-faces))

(provide 'bv-org-agenda)
;;; bv-org-agenda.el ends here