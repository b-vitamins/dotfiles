;;; bv-org.el --- Org mode configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Org mode for research notes and task management.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)

(define-prefix-command 'bv-org-map)
(define-prefix-command 'bv-org-timer-map)

(eval-when-compile
  (require 'org-refile)
  (require 'org-modern))

;; Load LaTeX preview configuration early
(require 'bv-org-latex nil t)

;; External function declarations
(autoload 'consult--buffer-state "consult")
(autoload 'org-timer-value-string "org-timer")
(autoload 'org-html-stable-ids-add "ox-html-stable-ids")
(autoload 'org-buffer-list "org")
(declare-function olivetti-mode "olivetti")
(declare-function global-org-modern-mode "org-modern")
(declare-function bv-org-agenda-calendar "bv-org-agenda")
(declare-function bv-org-agenda-quick-task "bv-org-agenda")
(declare-function citar-select-refs "citar")
(declare-function citar-get-entry "citar")
(declare-function citar-get-value "citar")
(declare-function org-roam-capture- "org-roam-capture")
(declare-function org-roam-node-create "org-roam-node")
(declare-function org-roam-node-id "org-roam-node")
(declare-function bv-org-roam-slug-from-title "bv-org-roam")

;; External variable declarations
(defvar mode-specific-map)
(defvar consult-buffer-sources)
(defvar org-timer-mode-line-string)
(defvar org-id-locations-file)
(defvar org-timer-pause-time)
(defvar org-agenda-files)
(defvar org-capture-templates)
(defvar org-modern-todo)
(defvar org-modern-timestamp)
(defvar org-modern-statistics)
(defvar org-modern-tag)
(defvar org-modern-priority)
(defvar org-modern-hide-stars)
(defvar org-hide-leading-stars)
(defvar bv-app-map)

(with-eval-after-load 'ox-html
  (require 'ox-html-stable-ids)
  (org-html-stable-ids-add))

;;;; Directory & File Layout

(defgroup bv-org nil
  "Research + life management with Org."
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

(defcustom bv-org-auto-promote-next-action t
  "When non-nil, keep projects moving by promoting a next action.

When completing a task inside a project, automatically set another
TODO to NEXT if the project has no NEXT tasks remaining."
  :type 'boolean
  :group 'bv-org)

(defconst bv-org--inbox-file "inbox.org")
(defconst bv-org--projects-file "projects.org")
(defconst bv-org--journal-file "journal.org")
(defconst bv-org--meetings-file "meetings.org")
(defconst bv-org--reading-file "reading.org")
(defconst bv-org--goals-file "goals.org")
(defconst bv-org--reviews-file "reviews.org")

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
  "Ensure the Org directory and core files exist."
  (unless (file-directory-p bv-org-directory)
    (make-directory bv-org-directory t))
  (let ((archive-dir (bv-org--path "archive/")))
    (unless (file-directory-p archive-dir)
      (make-directory archive-dir t)))
  (bv-org--ensure-file
   (bv-org--path bv-org--inbox-file)
   "#+title: Inbox\n#+startup: overview\n\n* Inbox\n\n* Refile\n\n* Links\n\n* Ideas\n")
  (bv-org--ensure-file
   (bv-org--path bv-org--projects-file)
   "#+title: Projects\n#+startup: overview\n\n* Research\n\n* Thesis\n\n* Courses\n\n* Teaching\n\n* Admin\n\n* Personal\n")
  (bv-org--ensure-file
   (bv-org--path bv-org--journal-file)
   "#+title: Journal\n#+startup: overview\n\n")
  (bv-org--ensure-file
   (bv-org--path bv-org--meetings-file)
   "#+title: Meetings\n#+startup: overview\n\n")
  (bv-org--ensure-file
   (bv-org--path bv-org--reading-file)
   "#+title: Reading\n#+startup: overview\n\n* Queue\n\n* Notes\n")
  (bv-org--ensure-file
   (bv-org--path bv-org--goals-file)
   "#+title: Goals\n#+startup: overview\n\n* Motivators\n** Active\n** Tangible\n** Conceptual\n\n* Habits\n\n* Weekly Review\n")
  (bv-org--ensure-file
   (bv-org--path bv-org--reviews-file)
   "#+title: Reviews\n#+startup: overview\n\n"))

;;;; Capture Context Helpers

(defun bv-org--project-root ()
  "Return the current project root, or nil."
  (when-let ((proj (project-current nil)))
    (ignore-errors (project-root proj))))

(defun bv-org--project-name ()
  "Return a concise name for the current project, or nil."
  (when-let ((root (bv-org--project-root)))
    (file-name-nondirectory (directory-file-name root))))

(defun bv-org-capture--project-line ()
  "Return a property line describing the current project."
  (if-let ((name (bv-org--project-name)))
      (format ":PROJECT: %s\n" name)
    ":PROJECT: \n"))

(defun bv-org-capture--context-line ()
  "Return a property line describing capture context."
  (let* ((file (or (buffer-file-name) ""))
         (line (number-to-string (line-number-at-pos))))
    (format ":CONTEXT: %s:%s\n" (if (string-empty-p file) (buffer-name) file) line)))

(defun bv-org-capture--clock-line ()
  "Return a property line describing the currently clocked task."
  (if (and (boundp 'org-clock-current-task) org-clock-current-task)
      (format ":CLOCKED: %s\n" org-clock-current-task)
    ":CLOCKED: \n"))

(defun bv-org-capture--citar-cite ()
  "Prompt for a citation via citar when available, else ask for a key.

Returns a string in Org-cite syntax, e.g. \"[cite:@key]\"."
  (let ((key nil))
    (cond
     ((fboundp 'citar-select-refs)
      (let ((keys (citar-select-refs)))
        (setq key (cond
                   ((stringp keys) keys)
                   ((and (listp keys) (car keys)) (car keys))
                   (t nil)))))
     (t nil))
    (setq key (or key (string-trim (read-string "Cite key (or empty): "))))
    (if (string-empty-p key)
        ""
      (format "[cite:@%s]" key))))

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
          (message "Org: clocked out due to %dm idle"
                 bv-org-clock-idle-minutes)))))

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

;;;; Weekly review file helper

(defun bv-org-weekly-review-create ()
  "Create (or jump to) a weekly review entry in `reviews.org'."
  (interactive)
  (require 'org)
  (bv-org-ensure-structure)
  (let* ((file (bv-org--path bv-org--reviews-file))
         (week (format-time-string "%G-W%V"))
         (heading (format "* Weekly Review %s\n" week)))
    (find-file file)
    (goto-char (point-min))
    (unless (re-search-forward (regexp-quote heading) nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert heading)
      (insert "** Checklist\n")
      (insert "- [ ] Inbox zero: process/refile\n")
      (insert "- [ ] Review NEXT / WAITING\n")
      (insert "- [ ] Unblock stuck projects (ensure one NEXT)\n")
      (insert "- [ ] Review calendar (past 2w / next 2w)\n")
      (insert "- [ ] Update goals/motivators\n")
      (insert "\n** Metrics (last week)\n")
      (insert "#+BEGIN: clocktable :scope agenda :block lastweek :maxlevel 3 :fileskip0 t\n")
      (insert "#+END:\n\n")
      (insert "** Notes\n\n"))
    (goto-char (point-min))
    (re-search-forward (regexp-quote heading) nil t)
    (if (fboundp 'org-fold-show-entry)
        (org-fold-show-entry)
      (with-no-warnings (org-show-entry)))
    (save-excursion
      (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
        (when (re-search-forward "^#\\+BEGIN: clocktable\\b" subtree-end t)
          (org-update-dblock))))))

;;;; Metrics dashboard (scoreboard)

(defgroup bv-org-metrics nil
  "Lightweight metrics and gamification for Org clocking."
  :group 'bv-org
  :prefix "bv-org-metrics-")

(defcustom bv-org-metrics-streak-minutes 30
  "Minimum minutes clocked per day to count towards the streak."
  :type 'integer
  :group 'bv-org-metrics)

(defcustom bv-org-metrics-top-projects 7
  "How many projects to show in the metrics dashboard."
  :type 'integer
  :group 'bv-org-metrics)

(defun bv-org-metrics--agenda-files ()
  "Return resolved agenda files."
  (require 'org-agenda)
  (org-agenda-files t))

(defun bv-org-metrics--collect-clock-data (params)
  "Return a cons (TOTAL-MINUTES . FILE-TABLES) for PARAMS.

FILE-TABLES is an alist of (FILE . ROWS) as returned by
`org-clock-get-table-data'."
  (require 'org)
  (require 'org-clock)
  (let ((total 0)
        (tables nil))
    (dolist (file (bv-org-metrics--agenda-files))
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
            (let* ((data (org-clock-get-table-data file params))
                   (file-mins (or (nth 1 data) 0))
                   (rows (or (nth 2 data) nil)))
              (setq total (+ total file-mins))
              (push (cons file rows) tables))))))
    (cons total (nreverse tables))))

(defun bv-org-metrics--format-minutes (minutes)
  "Format MINUTES as H:MM."
  (let ((minutes (max 0 (truncate (or minutes 0)))))
    (format "%d:%02d" (/ minutes 60) (% minutes 60))))

(defun bv-org-metrics--day-range (time)
  "Return (START END) times for the day containing TIME."
  (let* ((dt (decode-time time))
         (start (encode-time 0 0 0 (nth 3 dt) (nth 4 dt) (nth 5 dt))))
    (list start (time-add start (days-to-time 1)))))

(defun bv-org-metrics--this-week-range ()
  "Return (START END) times for the current week (Mon..Mon)."
  (let* ((dt (decode-time (current-time)))
         (dow (nth 6 dt))
         (today-start (car (bv-org-metrics--day-range (current-time))))
         (days-since-monday (mod (- dow 1) 7))
         (start (time-subtract today-start (days-to-time days-since-monday))))
    (list start (time-add start (days-to-time 7)))))

(defun bv-org-metrics--time-in-range-p (time start end)
  "Return non-nil when TIME is within [START, END)."
  (and (not (time-less-p time start))
       (time-less-p time end)))

(defun bv-org-metrics--count-papers-read (start end)
  "Count papers read (DONE \"Read\" tasks under tag `paper') in [START, END)."
  (require 'org)
  (let ((count 0))
    (dolist (file (bv-org-metrics--agenda-files) count)
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (when (and (org-entry-is-done-p)
                         (member "paper" (org-get-tags))
                         (string-equal (downcase (org-get-heading t t t t)) "read"))
                (when-let ((closed (org-entry-get (point) "CLOSED")))
                  (when (bv-org-metrics--time-in-range-p
                         (org-time-string-to-time closed) start end)
                    (setq count (1+ count))))))))))))

(defun bv-org-metrics--hash-inc (table key delta)
  "Increment TABLE at KEY by DELTA."
  (puthash key (+ (gethash key table 0) delta) table))

(defun bv-org-metrics--row-project (file row)
  "Return a project name for ROW in FILE."
  (let* ((props (nth 5 row))
         (project (and (listp props) (cdr (assoc "PROJECT" props))))
         (project (and project (string-trim project))))
    (cond
     ((and project (not (string-empty-p project))) project)
     (t (format "Unclassified (%s)" (file-name-base file))))))

(defun bv-org-metrics--accumulate-project-minutes (table file rows)
  "Accumulate exclusive minutes per project into TABLE from ROWS in FILE."
  (let ((stack nil))
    (dolist (row rows)
      (let* ((level (nth 0 row))
             (time (nth 4 row))
             (project (bv-org-metrics--row-project file row)))
        (while (and stack (>= (nth 0 (car stack)) level))
          (let* ((item (car stack))
                 (itime (nth 1 item))
                 (ichild (nth 2 item))
                 (iproj (nth 3 item))
                 (exclusive (max 0 (- itime ichild))))
            (bv-org-metrics--hash-inc table iproj exclusive)
            (setq stack (cdr stack))
            (when stack
              (setcar (nthcdr 2 (car stack))
                      (+ (nth 2 (car stack)) itime)))))
        (push (list level time 0 project) stack)))
    (while stack
      (let* ((item (car stack))
             (itime (nth 1 item))
             (ichild (nth 2 item))
             (iproj (nth 3 item))
             (exclusive (max 0 (- itime ichild))))
        (bv-org-metrics--hash-inc table iproj exclusive)
        (setq stack (cdr stack))
        (when stack
          (setcar (nthcdr 2 (car stack))
                  (+ (nth 2 (car stack)) itime)))))))

(defun bv-org-metrics--top-projects-this-week ()
  "Return list of (PROJECT . MINUTES) sorted desc for this week."
  (let* ((data (bv-org-metrics--collect-clock-data
                (list :block 'thisweek :maxlevel 10
                      :properties '("PROJECT") :inherit-props t)))
         (tables (cdr data))
         (acc (make-hash-table :test 'equal)))
    (dolist (ft tables)
      (bv-org-metrics--accumulate-project-minutes acc (car ft) (cdr ft)))
    (let (items)
      (maphash (lambda (k v) (push (cons k v) items)) acc)
      (setq items (sort items (lambda (a b) (> (cdr a) (cdr b)))))
      (seq-take items (max 0 bv-org-metrics-top-projects)))))

(defun bv-org-metrics--streak ()
  "Return the current streak length for `bv-org-metrics-streak-minutes'."
  (let ((days 0)
        (cursor (current-time)))
    (catch 'done
      (while t
        (let* ((range (bv-org-metrics--day-range cursor))
               (start (car range))
               (end (cadr range))
               (tstart (decode-time start))
               (tend (decode-time end))
               (total (car (bv-org-metrics--collect-clock-data
                            (list :tstart (list (nth 4 tstart) (nth 3 tstart) (nth 5 tstart))
                                  :tend (list (nth 4 tend) (nth 3 tend) (nth 5 tend))
                                  :maxlevel 10)))))
          (if (>= total bv-org-metrics-streak-minutes)
              (progn
                (setq days (1+ days))
                (setq cursor (time-subtract cursor (days-to-time 1))))
            (throw 'done days)))))))

(defun bv-org-metrics-dashboard ()
  "Show a compact gamified metrics dashboard for Org clocking."
  (interactive)
  (require 'org)
  (bv-org-ensure-structure)
  (let* ((today-total (car (bv-org-metrics--collect-clock-data (list :block 'today :maxlevel 10))))
         (week-total (car (bv-org-metrics--collect-clock-data (list :block 'thisweek :maxlevel 10))))
         (today-deep (car (bv-org-metrics--collect-clock-data (list :block 'today :maxlevel 10 :match "+deep"))))
         (week-deep (car (bv-org-metrics--collect-clock-data (list :block 'thisweek :maxlevel 10 :match "+deep"))))
         (today-writing (car (bv-org-metrics--collect-clock-data (list :block 'today :maxlevel 10 :match "+writing"))))
         (week-writing (car (bv-org-metrics--collect-clock-data (list :block 'thisweek :maxlevel 10 :match "+writing"))))
         (today-reading (car (bv-org-metrics--collect-clock-data (list :block 'today :maxlevel 10 :match "+reading"))))
         (week-reading (car (bv-org-metrics--collect-clock-data (list :block 'thisweek :maxlevel 10 :match "+reading"))))
         (today-range (bv-org-metrics--day-range (current-time)))
         (week-range (bv-org-metrics--this-week-range))
         (today-papers (bv-org-metrics--count-papers-read (car today-range) (cadr today-range)))
         (week-papers (bv-org-metrics--count-papers-read (car week-range) (cadr week-range)))
         (streak (bv-org-metrics--streak))
         (top (bv-org-metrics--top-projects-this-week)))
    (with-current-buffer (get-buffer-create "*BV Org Metrics*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "#+title: Org Scoreboard\n")
      (insert (format "#+date: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert "* Scoreboard\n")
      (insert "| Metric | Today | This week |\n")
      (insert "|-|\n")
      (insert (format "| Total | %s | %s |\n"
                      (bv-org-metrics--format-minutes today-total)
                      (bv-org-metrics--format-minutes week-total)))
      (insert (format "| Deep work | %s | %s |\n"
                      (bv-org-metrics--format-minutes today-deep)
                      (bv-org-metrics--format-minutes week-deep)))
      (insert (format "| Writing | %s | %s |\n"
                      (bv-org-metrics--format-minutes today-writing)
                      (bv-org-metrics--format-minutes week-writing)))
      (insert (format "| Reading | %s | %s |\n"
                      (bv-org-metrics--format-minutes today-reading)
                      (bv-org-metrics--format-minutes week-reading)))
      (insert (format "| Papers read | %d | %d |\n" today-papers week-papers))
      (insert "\n")
      (insert (format "- Streak: %d day(s) ≥ %d minutes/day\n\n"
                      streak bv-org-metrics-streak-minutes))
      (insert "* Top projects (this week)\n")
      (if (null top)
          (insert "- No clocked project data yet.\n")
        (insert "| Project | Time |\n")
        (insert "|-|\n")
        (dolist (item top)
          (insert (format "| %s | %s |\n"
                          (car item)
                          (bv-org-metrics--format-minutes (cdr item))))))
      (org-mode)
      (goto-char (point-min))
      (when (fboundp 'org-table-align)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^|.*|$" nil t)
            (ignore-errors (org-table-align)))))
      (setq buffer-read-only t)
      (display-buffer (current-buffer)))))

;;;; Focus (pomodoro) + inbox processing + paper pipeline

(defcustom bv-org-focus-minutes 45
  "Default focus timer duration in minutes for `bv-org-focus'."
  :type 'integer
  :group 'bv-org)

(defcustom bv-org-reading-roam-backlink nil
  "When non-nil, `bv-org-reading-add-paper' creates a linked org-roam note.

Use a prefix argument to toggle this per invocation."
  :type 'boolean
  :group 'bv-org)

(defvar bv-org-focus--session nil
  "Internal state for an active focus session.

When non-nil, a plist with keys:
- :marker  Marker of the clocked heading
- :minutes Timer length in minutes.")

(defun bv-org-focus--clear-session ()
  "Clear focus session state."
  (setq bv-org-focus--session nil))

(defun bv-org-focus--timer-done ()
  "Auto clock-out when a focus timer finishes."
  (when (and bv-org-focus--session
             (fboundp 'org-clocking-p)
             (org-clocking-p))
    (let* ((session-marker (plist-get bv-org-focus--session :marker))
           (clock-marker (and (boundp 'org-clock-hd-marker) org-clock-hd-marker)))
      (when (and (markerp session-marker)
                 (markerp clock-marker)
                 (marker-buffer session-marker)
                 (marker-buffer clock-marker)
                 (eq (marker-buffer session-marker) (marker-buffer clock-marker))
                 (= (marker-position session-marker) (marker-position clock-marker)))
        (ignore-errors (org-clock-out))
        (message "Focus: clocked out (timer done)"))))
  (bv-org-focus--clear-session))

(defun bv-org-focus--timer-stopped ()
  "Clear focus session state when the countdown timer stops."
  (bv-org-focus--clear-session))

(defun bv-org-focus--clocked-out ()
  "Clear focus session state when clocking out."
  (bv-org-focus--clear-session))

(defun bv-org-focus--cleanup-timer ()
  "Stop any running countdown timer without prompting."
  (when (or (boundp 'org-timer-countdown-timer)
            (boundp 'org-timer-start-time))
    (ignore-errors (org-timer-stop))))

(defun bv-org-focus--read-minutes (arg)
  "Return focus duration in minutes from ARG."
  (cond
   ((numberp arg) arg)
   ((and (consp arg) (equal arg '(4)))
    (read-number "Focus minutes: " bv-org-focus-minutes))
   (t bv-org-focus-minutes)))

(defun bv-org-focus--current-task-marker ()
  "Return a marker for the current heading when it's a focus-able task."
  (when (and (derived-mode-p 'org-mode) (org-at-heading-p))
    (let ((state (org-get-todo-state)))
      (when (and state (member state '("NEXT" "STARTED" "TODO")))
        (point-marker)))))

(defun bv-org-focus--collect-candidates ()
  "Collect (DISPLAY . MARKER) candidates for focus selection."
  (require 'org)
  (let ((cands nil))
    (dolist (file (bv-org-metrics--agenda-files))
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (when-let ((state (org-get-todo-state)))
                (when (member state '("STARTED" "NEXT"))
                  (let* ((title (org-get-heading t t t t))
                         (project (string-trim (or (org-entry-get (point) "PROJECT" t) "")))
                         (context (if (string-empty-p project)
                                      (org-get-category)
                                    project))
                         (display (format "%s · %s · %s (%s)"
                                          state context title (file-name-base file))))
                    (push (cons display (point-marker)) cands)))))))))
    (nreverse cands)))

(defun bv-org-focus (arg)
  "Focus on a NEXT task: clock in and start a countdown timer.

With numeric prefix ARG, use ARG minutes.
With \\[universal-argument], prompt for minutes."
  (interactive "P")
  (require 'org)
  (let* ((minutes (bv-org-focus--read-minutes arg))
         (marker (or (bv-org-focus--current-task-marker)
                     (let* ((cands (bv-org-focus--collect-candidates))
                            (choice (completing-read "Focus task: " cands nil t)))
                       (cdr (assoc choice cands))))))
    (unless (markerp marker)
      (user-error "No focusable NEXT tasks found"))
    (when (and (fboundp 'org-clocking-p) (org-clocking-p))
      (ignore-errors (org-clock-out)))
    (bv-org-focus--cleanup-timer)
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-back-to-heading t)
    (if (fboundp 'org-fold-show-subtree)
        (org-fold-show-subtree)
      (with-no-warnings (org-show-subtree)))
    (org-clock-in)
    (let ((clock-marker (and (boundp 'org-clock-hd-marker) org-clock-hd-marker)))
      (setq bv-org-focus--session
            (list :marker (and (markerp clock-marker) (copy-marker clock-marker))
                  :minutes minutes)))
    (org-timer-set-timer minutes)
    (message "Focus: %d minutes" minutes)))

(defun bv-org-inbox-process ()
  "Process the Org inbox with a fast inbox-zero loop."
  (interactive)
  (require 'org)
  (bv-org-ensure-structure)
  (let ((file (bv-org--path bv-org--inbox-file)))
    (pop-to-buffer (find-file-noselect file))
    (widen)
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Inbox\\b" nil t)
      (user-error "Inbox heading not found in %s" file))
    (org-back-to-heading t)
    (if (fboundp 'org-fold-show-subtree)
        (org-fold-show-subtree)
      (with-no-warnings (org-show-subtree)))
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (catch 'done
        (ignore-errors (org-next-visible-heading 1))
        (while (and (org-at-heading-p) (< (point) (point-max)))
          (let* ((title (org-get-heading t t t t))
                 (next (save-excursion
                         (org-end-of-subtree t t)
                         (ignore-errors (org-next-visible-heading 1))
                         (point-marker)))
                 (cmd (read-char-choice
                       (format "Inbox: %s  [r]efile [d]one [c]ancel [a]rchive [s]kip [q]uit "
                               title)
                       '(?r ?d ?c ?a ?s ?q))))
            (pcase cmd
              (?q (throw 'done t))
              (?s nil)
              (?d (org-todo "DONE"))
              (?c (org-todo "CANCELLED"))
              (?a (save-window-excursion (org-archive-subtree)))
              (?r (save-window-excursion (org-refile))))
            (goto-char next))))
      (widen)
      (message "Inbox: done"))))

(defun bv-org-reading--sanitize-title (title)
  "Sanitize citation TITLE for use in a heading."
  (let ((s (string-trim (or title ""))))
    (setq s (replace-regexp-in-string "[\n\r\t]+" " " s))
    (setq s (replace-regexp-in-string "[{}]" "" s))
    (string-trim s)))

(defun bv-org-reading--short-title (title)
  "Return a short heading-friendly TITLE."
  (let ((s (bv-org-reading--sanitize-title title)))
    (if (> (length s) 88)
        (concat (substring s 0 85) "…")
      s)))

(defun bv-org-reading--citar-select ()
  "Return a plist (:key :title) from citar, or prompt manually."
  (let* ((key nil)
         (title nil))
    (when (fboundp 'citar-select-refs)
      (let ((keys (citar-select-refs)))
        (setq key (cond
                   ((stringp keys) keys)
                   ((and (listp keys) (car keys)) (car keys))
                   (t nil)))))
    (setq key (or key (string-trim (read-string "Cite key: "))))
    (when (string-empty-p key)
      (user-error "Cite key is required"))
    (when (and (fboundp 'citar-get-entry) (fboundp 'citar-get-value) (not (string-empty-p key)))
      (condition-case nil
          (let ((entry (citar-get-entry key)))
            (setq title (citar-get-value "title" entry)))
        (error nil)))
    (setq title (or title (read-string "Title: ")))
    (list :key key :title (bv-org-reading--sanitize-title title))))

(defun bv-org-reading--maybe-create-roam-note (title citekey)
  "Create an org-roam note for TITLE and CITEKEY, returning the node ID."
  (when (and (fboundp 'org-roam-capture-) (fboundp 'org-roam-node-create))
    (require 'org-roam nil t)
    (let* ((node (org-roam-node-create :title title))
           (templates
            `(("p" "paper" plain "%?"
               :target (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                                  "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: reference paper\n#+ROAM_REFS: cite:@${citekey}\n\n* Notes\n\n")
               :unnarrowed t)))
           (info (list :citekey citekey :slug (bv-org-roam-slug-from-title title))))
      (save-window-excursion
        (org-roam-capture- :node node
                           :info info
                           :templates templates
                           :keys "p"
                           :props '(:immediate-finish t)))
      (org-roam-node-id node))))

(defun bv-org-reading-add-paper (arg)
  "Add a paper to the reading queue via citar.

With prefix ARG, also create an org-roam note and link it."
  (interactive "P")
  (require 'org)
  (bv-org-ensure-structure)
  (let* ((project-line (bv-org-capture--project-line))
         (context-line (bv-org-capture--context-line))
         (selection (bv-org-reading--citar-select))
         (key (plist-get selection :key))
         (title (bv-org-reading--short-title (plist-get selection :title)))
         (cite (format "[cite:@%s]" key))
         (want-roam (or arg bv-org-reading-roam-backlink))
         (roam-id nil)
         (created (format-time-string "%Y-%m-%d %a %H:%M")))
    (when (and want-roam (fboundp 'bv-org-roam-slug-from-title))
      (setq roam-id (bv-org-reading--maybe-create-roam-note title key)))
    (with-current-buffer (find-file-noselect (bv-org--path bv-org--reading-file))
      (org-with-wide-buffer
        (goto-char (point-min))
        (unless (re-search-forward "^\\* Queue\\b" nil t)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "* Queue\n"))
        (org-back-to-heading t)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (insert (format "* PROJ %s  :paper:\n" title))
        (insert ":PROPERTIES:\n")
        (insert (format ":CREATED: [%s]\n" created))
        (insert (format ":CITE: %s\n" cite))
        (insert project-line)
        (insert context-line)
        (when roam-id
          (insert (format ":ROAM: [[id:%s][Roam note]]\n" roam-id)))
        (insert ":END:\n\n")
        (insert "** NEXT Read  :reading:\n\n")
        (insert "** TODO Take notes  :writing:\n\n")
        (insert "** TODO Write summary  :writing:\n\n")
        (insert "** Notes\n")
        (when roam-id
          (insert (format "- Roam: [[id:%s][Notes]]\n" roam-id)))
        (insert "- \n")
        (save-buffer)))
    (message "Reading: queued %s" title)))

;;;; Consult integration for org buffers

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources
                 `(:name "Org"
                         :narrow ?o
                         :category buffer
                         :state ,'consult--buffer-state
                         :items ,(lambda () (mapcar 'buffer-name (org-buffer-list))))
                 'append)))

(defun bv-org-consult-heading ()
  "Jump to an Org heading across agenda files using Consult."
  (interactive)
  (if (fboundp 'consult-org-heading)
      (consult-org-heading)
    (user-error "consult-org-heading is unavailable")))

(defun bv-org-consult-agenda ()
  "Search agenda items using Consult."
  (interactive)
  (if (fboundp 'consult-org-agenda)
      (consult-org-agenda)
    (user-error "consult-org-agenda is unavailable")))

(defun bv-org-search ()
  "Search all Org files (DWIM) using Consult ripgrep."
  (interactive)
  (require 'consult nil t)
  (let ((root (file-name-as-directory bv-org-directory)))
    (if (fboundp 'consult-ripgrep)
        (consult-ripgrep root)
      (user-error "consult-ripgrep is unavailable"))))

(defun bv-org-timer-reset ()
  "Set `org-timer-mode-line-string' to nil."
  (interactive)
  (when (boundp 'org-timer-mode-line-string)
    (setq org-timer-mode-line-string nil)))

(with-eval-after-load 'org-id
  (when (boundp 'org-id-locations-file)
    (setq org-id-locations-file
          (concat
           (or (getenv "XDG_CACHE_HOME") "~/.cache")
           "/emacs/org-id-locations"))))


(declare-function bv-org-rename-buffer-to-title "bv-org")

(with-eval-after-load 'org
  (bv-org-ensure-structure)
  (setopt org-M-RET-may-split-line '((default . nil)))
  (setopt org-insert-heading-respect-content t)
  (setq org-adapt-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-indented t)
  (setq org-directory bv-org-directory)
  (setq org-default-notes-file (bv-org--path bv-org--inbox-file))
  (setq org-agenda-files (bv-org--discover-agenda-files))

  (defun bv-org-timer-update-mode-line ()
    "Update timer in mode line."
    (if (and (boundp 'org-timer-pause-time) org-timer-pause-time)
        nil
      (when (boundp 'org-timer-mode-line-string)
        (setq org-timer-mode-line-string
              (substring (org-timer-value-string) 0 -1)))
      (force-mode-line-update)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'full-file-path)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets
        `((,(bv-org--path bv-org--projects-file) :maxlevel . 4)
          (,(bv-org--path bv-org--reading-file) :maxlevel . 3)
          (,(bv-org--path bv-org--goals-file) :maxlevel . 3)
          (nil :maxlevel . 4)))
  (setq org-ellipsis "⤵")
  (set-face-attribute
   'org-ellipsis
   nil
   :inherit '(font-lock-comment-face default)
   :weight 'normal)
  (setq org-hide-emphasis-markers t)
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-log-redeadline 'note)
  (setq org-log-reschedule 'note)
  (setq org-clock-into-drawer "LOGBOOK")
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)
  (setq org-clock-in-resume t)
  (setq org-clock-out-when-done t)
  (setq org-clock-history-length 20)
  (setq org-clock-report-include-clocking-task t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (require 'org-habit)
  (setq org-habit-preceding-days 21)
  (setq org-habit-following-days 7)
  (setq org-habit-graph-column 55)

  (add-hook 'org-timer-done-hook #'bv-org-focus--timer-done)
  (add-hook 'org-timer-stop-hook #'bv-org-focus--timer-stopped)
  (add-hook 'org-clock-out-hook #'bv-org-focus--clocked-out)

  (setq org-todo-keywords
        '((sequence
           "PROJ(p)"
           "TODO(t)"
           "NEXT(n)"
           "STARTED(s!)"
           "WAITING(w@)"
           "HOLD(h@)"
           "SOMEDAY(m)"
           "|"
           "DONE(d!)"
           "CANCELLED(c@)")))

  (setq org-todo-state-tags-triggers
        '(("WAITING" ("WAITING" . t))
          ("HOLD" ("HOLD" . t))
          ("SOMEDAY" ("SOMEDAY" . t))
          ("CANCELLED" ("CANCELLED" . t))
          (done ("WAITING") ("HOLD") ("SOMEDAY"))
          ("TODO" ("WAITING") ("HOLD") ("SOMEDAY"))
          ("NEXT" ("WAITING") ("HOLD") ("SOMEDAY"))
          ("STARTED" ("WAITING") ("HOLD") ("SOMEDAY"))))

  ;; When clocking in, automatically mark tasks STARTED (but don't touch projects).
  (setq org-clock-in-switch-to-state
        (lambda (state)
          (cond
           ((member state '("TODO" "NEXT")) "STARTED")
           (t state))))

  ;; Capture templates: inbox/journal/meeting/idea/reading/link/quick task.
  (setq org-capture-templates
	        `(("i" "Inbox item" entry
	           (file+headline ,(bv-org--path bv-org--inbox-file) "Inbox")
	           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n%(bv-org-capture--project-line)%(bv-org-capture--clock-line)%(bv-org-capture--context-line):END:\n%i\n%a\n"
	           :empty-lines 1 :clock-in t :clock-resume t)
          ("q" "Quick task (today)" entry
           (file+headline ,(bv-org--path bv-org--inbox-file) "Inbox")
           "* NEXT %?\nDEADLINE: %t\n:PROPERTIES:\n:CREATED: %U\n%(bv-org-capture--project-line)%(bv-org-capture--clock-line)%(bv-org-capture--context-line):END:\n%a\n"
           :empty-lines 1 :clock-in t :clock-resume t)
	          ("j" "Journal" entry
	           (file+olp+datetree ,(bv-org--path bv-org--journal-file))
	           "* %<%H:%M> %?\n:PROPERTIES:\n:CREATED: %U\n%(bv-org-capture--project-line)%(bv-org-capture--clock-line):END:\n%i\n"
	           :empty-lines 1 :clock-in t :clock-resume t)
	          ("m" "Meeting" entry
	           (file+olp+datetree ,(bv-org--path bv-org--meetings-file))
	           "* %<%H:%M> %^{Title}  :meeting:\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES: %^{Attendees}\n:LOCATION: %^{Location}\n%(bv-org-capture--project-line)%(bv-org-capture--clock-line):END:\n\n** Context\n%a\n\n** Agenda\n- %?\n\n** Notes\n\n** Decisions\n\n** Actions\n"
	           :empty-lines 1 :clock-in t :clock-resume t)
          ("l" "Link" entry
           (file+headline ,(bv-org--path bv-org--inbox-file) "Links")
           "* %? :link:\n:PROPERTIES:\n:CREATED: %U\n%(bv-org-capture--project-line)%(bv-org-capture--context-line):END:\n%a\n"
           :empty-lines 1)
          ("d" "Idea" entry
           (file+headline ,(bv-org--path bv-org--inbox-file) "Ideas")
           "* %? :idea:\n:PROPERTIES:\n:CREATED: %U\n%(bv-org-capture--project-line)%(bv-org-capture--context-line):END:\n%i\n%a\n"
           :empty-lines 1)
          ("r" "Reading (paper → task + notes)" entry
           (file+headline ,(bv-org--path bv-org--reading-file) "Queue")
           "* PROJ %^{Short title}  :paper:\n:PROPERTIES:\n:CREATED: %U\n:CITE: %(bv-org-capture--citar-cite)\n%(bv-org-capture--project-line)%(bv-org-capture--context-line):END:\n\n** NEXT Read  :reading:\n\n** TODO Take notes  :writing:\n\n** TODO Write summary  :writing:\n\n** Notes\n%a\n\n- %?\n"
           :empty-lines 1 :clock-in t :clock-resume t)))

  (setq org-tag-alist
        '((:startgroup . nil)
          ("@lab" . ?l)
          ("@campus" . ?c)
          ("@home" . ?h)
          ("@computer" . ?p)
          ("@errand" . ?e)
          (:endgroup . nil)
          ("research" . ?r)
          ("thesis" . ?T)
          ("course" . ?C)
          ("teaching" . ?t)
          ("admin" . ?a)
          ("paper" . ?P)
          ("deep" . ?d)
          ("writing" . ?w)
          ("reading" . ?R)
          ("shallow" . ?s)
          ("meeting" . ?m)
          ("refile" . ?f)))

  (setq org-archive-location
        (concat (bv-org--path "archive/")
                "%s_archive::datetree/"))

  (bv-org--setup-idle-clockout)

  (defun bv-org--project-has-next-p ()
    "Return non-nil when the current subtree contains a NEXT task."
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (re-search-forward "^\\*+ NEXT\\b" nil t)))

  (defun bv-org--project-promote-next-action ()
    "Promote a TODO task to NEXT when in a PROJ subtree without a NEXT."
    (when (and bv-org-auto-promote-next-action
               (derived-mode-p 'org-mode))
      (save-excursion
        (org-back-to-heading t)
        (when-let ((parent (save-excursion (org-up-heading-safe))))
          (save-excursion
            (goto-char parent)
            (when (equal (org-get-todo-state) "PROJ")
              (unless (bv-org--project-has-next-p)
                (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
                  (forward-line 1)
                  (when (re-search-forward "^\\*+ \\(TODO\\|STARTED\\)\\b" subtree-end t)
                    (org-todo "NEXT"))))))))))

  (add-hook 'org-after-todo-state-change-hook #'bv-org--project-promote-next-action)

  (defun bv-org-rename-buffer-to-title (&optional _end)
    "Rename buffer to #+TITLE: value, handling PROPERTIES drawers."
    (interactive)
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        ;; Search in first 30 lines for #+TITLE
        (when (re-search-forward
               "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$"
               (save-excursion (goto-char (point-min))
                              (forward-line 30)
                              (point))
               t)
          (let ((title (match-string 1)))
            (when (and title (not (string-empty-p title)))
              (rename-buffer title t))))))
    nil)

  (defun bv-org-rename-buffer-to-title-config ()
    "Run buffer renaming setup for org mode."
    ;; Run immediately when opening the file
    (bv-org-rename-buffer-to-title))

  (add-hook 'org-mode-hook 'bv-org-rename-buffer-to-title-config)
  (with-eval-after-load 'notmuch (require 'ol-notmuch))
  (add-hook 'org-mode-hook 'olivetti-mode)

  (with-eval-after-load 'org-modern
    (when (boundp 'org-modern-todo)
      (setq org-modern-todo nil))
    (when (boundp 'org-modern-timestamp)
      (setq org-modern-timestamp nil))
    (when (boundp 'org-modern-statistics)
      (setq org-modern-statistics nil))
    (when (boundp 'org-modern-tag)
      (setq org-modern-tag nil))
    (when (boundp 'org-modern-priority)
      (setq org-modern-priority nil))
    (when (boundp 'org-modern-hide-stars)
      (setq org-modern-hide-stars nil))
    (when (boundp 'org-hide-leading-stars)
      (setq org-hide-leading-stars t)))

  (advice-add
   'org-timer-update-mode-line
   :override
   'bv-org-timer-update-mode-line)
  (add-hook 'org-timer-stop-hook 'bv-org-timer-reset)

  (with-eval-after-load 'bv-bindings
    (when (boundp 'bv-app-map)
      (define-key bv-app-map (kbd "o") bv-org-map)
      (define-key bv-org-map (kbd "a") (lambda () (interactive) (org-agenda nil "d")))
      (define-key bv-org-map (kbd "r") (lambda () (interactive) (org-agenda nil "R")))
      (define-key bv-org-map (kbd "w") #'bv-org-weekly-review-create)
      (define-key bv-org-map (kbd "f") #'bv-org-consult-heading)
      (define-key bv-org-map (kbd "s") #'bv-org-search)
      (define-key bv-org-map (kbd "g") (lambda () (interactive) (find-file (bv-org--path bv-org--goals-file))))
      (define-key bv-org-map (kbd "A") #'bv-org-consult-agenda)
      (define-key bv-org-map (kbd "c") #'org-capture)
      (define-key bv-org-map (kbd "i") #'bv-org-inbox-process)
      (define-key bv-org-map (kbd "p") #'bv-org-focus)
      (define-key bv-org-map (kbd "P") #'bv-org-reading-add-paper)
      (define-key bv-org-map (kbd "v") #'bv-org-agenda-calendar)
      (define-key bv-org-map (kbd "q") #'bv-org-agenda-quick-task)
      (define-key bv-org-map (kbd "m") #'bv-org-metrics-dashboard)
      (define-key bv-org-map (kbd "t") bv-org-timer-map)))

  (when (boundp 'bv-org-timer-map)
    (define-key bv-org-timer-map (kbd "s") 'org-timer-start)
    (define-key bv-org-timer-map (kbd "q") 'org-timer-stop)
    (define-key bv-org-timer-map (kbd "p") 'org-timer-pause-or-continue)
    (define-key bv-org-timer-map (kbd "t") 'org-timer-set-timer))

  (autoload 'global-org-modern-mode "org-modern"))

(provide 'bv-org)
;;; bv-org.el ends here
