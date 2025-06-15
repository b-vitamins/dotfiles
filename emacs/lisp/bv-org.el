;;; bv-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode configuration with agenda, babel, export, and visual enhancements.

;;; Code:

(require 'bv-core)
(require 'seq)

;; External variable declarations
(defvar bv-cache-dir)
(defvar bv-app-map)
(defvar tempel-template-sources)
(defvar org-timer-pause-time)
(defvar org-timer-mode-line-string)
(defvar org-super-agenda-groups)
(defvar org-wild-notifier-alert-time)
(defvar org-wild-notifier-keyword-whitelist)
(defvar org-wild-notifier-notification-title)
(defvar org-id-locations-file)
(defvar org-shiftright-hook)
(defvar org-shiftleft-hook)
(defvar org-src-block-faces)
(defvar bv-not-guix-p)
(defvar text-scale-mode-step)
(defvar text-scale-mode-amount)
(defvar org-habit-following-days)
(defvar org-habit-preceding-days)
(defvar org-habit-graph-column)
(defvar org-babel-python-command)
(defvar org-todo-keywords)
(defvar org-format-latex-options)
(defvar org-preview-latex-process-alist)
(defvar org-preview-latex-default-process)
(defvar org-preview-latex-image-directory)

;; Function declarations
(declare-function org-modern-mode "org-modern" (&optional arg))
(declare-function org-collect-keywords "org" (keywords &optional unique directory))
(declare-function org-get-category "org" (&optional pos force-refresh))
(declare-function org-get-todo-state "org" ())
(declare-function s-truncate "s" (len s &optional ellipsis))
(declare-function s-pad-right "s" (len padding s))
(declare-function org-super-agenda-mode "org-super-agenda" (&optional arg))
(declare-function org-wild-notifier-mode "org-wild-notifier" (&optional arg))
(declare-function org-html-stable-ids-add "ox-html-stable-ids" ())
(declare-function org-timer-value-string "org-timer" ())

;;;; Custom Variables
(defgroup bv-org nil
  "Org mode configuration."
  :group 'bv)

(defcustom bv-org-directory "~/documents/main"
  "Directory for org files."
  :type 'directory
  :group 'bv-org)

(defcustom bv-org-default-notes-file "~/documents/main/main.org"
  "Default file for capturing notes."
  :type 'string
  :group 'bv-org)

(defcustom bv-org-agenda-files '("~/documents/main")
  "Files to include in agenda."
  :type '(repeat file)
  :group 'bv-org)

(defcustom bv-org-rename-buffer-to-title t
  "Rename org buffers to their #+TITLE."
  :type 'boolean
  :group 'bv-org)

(defcustom bv-org-indent-mode t
  "Enable `org-indent-mode' by default."
  :type 'boolean
  :group 'bv-org)

(defcustom bv-org-modern-mode t
  "Enable org-modern for visual enhancements."
  :type 'boolean
  :group 'bv-org)

(defcustom bv-org-auto-update-toc nil
  "Automatically update table of contents."
  :type 'boolean
  :group 'bv-org)

(defcustom bv-org-variable-pitch-mode t
  "Enable `variable-pitch-mode' in org buffers."
  :type 'boolean
  :group 'bv-org)

(defcustom bv-org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
    (sequence "TOMEET(m)" "|" "CONCLUDED(c)" "CANCELED(x)")
    (sequence "TOREAD(r)" "|" "COMPLETED(p)")
    (sequence "TOWATCH(w)" "|" "FINISHED(f)")
    (sequence "TOSOLVE(s)" "|" "SOLVED(v)")
    (sequence "|" "ABANDONED(a)"))
  "Org todo keywords."
  :type '(repeat (cons (choice :tag "Type"
                               (const :tag "Sequence" sequence)
                               (const :tag "Type" type))
                       (repeat :tag "Keywords" string)))
  :group 'bv-org)

(defcustom bv-org-clockable-files nil
  "Files to include in clock reports."
  :type '(repeat file)
  :group 'bv-org)

(defcustom bv-org-capture-templates
  '(("t" "Todo" entry (file+headline bv-org-default-notes-file "Inbox")
     "* TODO %?\n  %U\n  %a")
    ("n" "Note" entry (file+headline bv-org-default-notes-file "Notes")
     "* %? :NOTE:\n  %U\n  %a")
    ("j" "Journal" entry (file+datetree "journal.org")
     "* %?\n  %U")
    ("m" "Meeting" entry (file+headline bv-org-default-notes-file "Meetings")
     "* MEETING %? :MEETING:\n  %U")
    ("i" "Idea" entry (file+headline bv-org-default-notes-file "Ideas")
     "* IDEA %? :IDEA:\n  %U"))
  "Org capture templates."
  :type 'sexp
  :group 'bv-org)

;;;; Helper Functions (defined early to avoid warnings)
(defun bv-org-todo-find-sequence (keyword)
  "Return the sequence in org's `org-todo-keywords' that contain KEYWORD.
Uses prefix matching to allow for fast-selection keywords."
  (seq-find (lambda (seq)
              (seq-some (lambda (kw)
                          (string-prefix-p keyword kw))
                        (cdr seq)))
            org-todo-keywords))

(defun bv-org-todo-jump-from-edge-to-edge (keyword direction)
  "Cycle TODO keyword to the opposite end of its sequence based on DIRECTION.
If DIRECTION is \\='left, and KEYWORD is its sequence's first, return the last
keyword.  If DIRECTION is \\='right, and KEYWORD is its sequence's last, return
the first keyword.  If KEYWORD is \\='ABANDONED\\=', return itself, enforcing
sequence closure.  Otherwise, return nil to permit normal cycling."
  (if (string= keyword "ABANDONED")
      keyword
    (let ((sequence (bv-org-todo-find-sequence keyword)))
      (when sequence
        (let* ((keywords (cdr sequence))
               (first-keyword (car (split-string (car keywords) "(")))
               (last-keyword (car (split-string (car (last keywords)) "("))))
          (cond
           ((and (eq direction 'left) (string= keyword first-keyword))
            last-keyword)
           ((and (eq direction 'right) (string= keyword last-keyword))
            first-keyword)
           (t nil)))))))

(defun bv-org-todo-cycle-left ()
  "Restrict TODO cycling to jump from the first to last keyword when moving left."
  (let* ((current-state (substring-no-properties (org-get-todo-state)))
         (next-state (bv-org-todo-jump-from-edge-to-edge current-state 'left)))
    (when next-state
      (org-todo next-state)
      t)))

(defun bv-org-todo-cycle-right ()
  "Restrict TODO cycling to jump from the last to first keyword when moving right."
  (let* ((current-state (substring-no-properties (org-get-todo-state)))
         (next-state (bv-org-todo-jump-from-edge-to-edge current-state 'right)))
    (when next-state
      (org-todo next-state)
      t)))

(defun bv-org-mode-visual-setup ()
  "Setup visual enhancements for org mode."
  (when bv-org-variable-pitch-mode
    (variable-pitch-mode 1))
  (auto-fill-mode 0))

(defun bv-org-mark-done-and-archive ()
  "Mark the current task as done and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun bv-org-latex-preview-scale ()
  "Adjust LaTeX preview scale based on text scale."
  (setq org-format-latex-options
        (plist-put org-format-latex-options
                   :scale (* 1.5 (expt text-scale-mode-step
                                       text-scale-mode-amount)))))

;;;; Core Org Configuration
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c C-x C-s" . bv-org-mark-done-and-archive)
         ("C-c C-x C-a" . org-archive-subtree)
         ("C-c t" . org-todo))
  :custom
  ;; General
  (org-directory bv-org-directory)
  (org-default-notes-file bv-org-default-notes-file)
  (org-startup-indented bv-org-indent-mode)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-startup-align-all-tables t)
  (org-image-actual-width nil)

  ;; Appearance
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars t)
  (org-hide-block-startup nil)
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-pretty-entities nil)
  (org-pretty-entities-include-sub-superscripts nil)

  ;; Behavior
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 2)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)
  (org-use-speed-commands t)
  (org-src-window-setup 'split-window-below)
  (require-final-newline nil)

  ;; Logging
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-refile 'time)

  ;; Refile
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Tags
  (org-fast-tag-selection-single-key t)
  (org-tags-column -80)

  ;; Todo keywords
  (org-todo-keywords bv-org-todo-keywords)
  (org-todo-keyword-faces
   '(("TODO" . org-todo)
     ("NEXT" . (:foreground "#FFA500" :weight bold))
     ("PROJ" . (:foreground "#9370DB" :weight bold))
     ("WAITING" . org-warning)
     ("HOLD" . (:foreground "#888888" :weight bold))
     ("CANCELLED" . (:foreground "#888888" :strike-through t))
     ("TOMEET" . org-agenda-date)
     ("CONCLUDED" . org-agenda-done)
     ("CANCELED" . org-agenda-dimmed-todo-face)
     ("TOREAD" . (:foreground "#0184BC" :weight bold))
     ("COMPLETED" . org-done)
     ("TOWATCH" . (:foreground "#238b22" :weight bold))
     ("FINISHED" . org-done)
     ("TOSOLVE" . (:foreground "#b22222" :weight bold))
     ("SOLVED" . org-done)
     ("ABANDONED" . org-agenda-dimmed-todo-face)))

  ;; Priorities
  (org-priority-highest ?A)
  (org-priority-lowest ?C)
  (org-priority-faces
   '((?A . (:foreground "#FF0000" :weight bold))
     (?B . (:foreground "#FFA500"))
     (?C . (:foreground "#00AA00"))))

  ;; Archive
  (org-archive-location "%s_archive::* Archived Tasks")

  ;; Column View and Effort
  (org-columns-default-format "%40ITEM(Task) %Effort(Estimate){:} %CLOCKSUM(Clocked)")
  (org-global-properties
   '(("Effort_ALL" . "0 0:30 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 5:00 5:30 6:00 6:30 7:00 7:30 8:00 8:30 9:00 9:30 10:00 10:30 11:00 11:30 12:00 12:30 13:00 13:30 14:00 14:30 15:00 15:30 16:00 16:30 17:00 17:30 18:00 18:30 19:00 19:30 20:00 20:30 21:00 21:30 22:00 22:30 23:00 23:30 24:00")))

  ;; Capture
  (org-capture-templates bv-org-capture-templates)
  :config
  ;; Load modules
  (require 'org-protocol)
  (require 'org-habit)
  (require 'org-id)
  (require 'ox-latex nil t)

  ;; Habit tracking configuration
  (setq org-modules '(org-habit)
        org-habit-following-days 7
        org-habit-preceding-days 30
        org-habit-graph-column 40)

  ;; Habit faces
  (custom-set-faces
   '(org-habit-clear-face ((t (:background "#8270f9"))))
   '(org-habit-clear-future-face ((t (:background "#777399"))))
   '(org-habit-ready-face ((t (:background "#4df4ff"))))
   '(org-habit-ready-future-face ((t (:background "#40a8a0"))))
   '(org-habit-alert-face ((t (:background "#f2f08a" :foreground "#000000"))))
   '(org-habit-alert-future-face ((t (:background "#a0a060" :foreground "#000000"))))
   '(org-habit-overdue-face ((t (:background "#ff5555"))))
   '(org-habit-overdue-future-face ((t (:background "#903030")))))

  (when (boundp 'bv-cache-dir)
    (setq org-id-locations-file
          (expand-file-name "org-id-locations" bv-cache-dir)))

  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)

  (custom-set-faces
   '(org-ellipsis ((t (:inherit font-lock-comment-face :weight normal))))
   '(org-document-title ((t (:height 1.0 :weight bold)))))

  ;; Rename buffer to title
  (defun bv-org-rename-buffer-to-title (&optional end)
    "Rename buffer to value of #+TITLE:."
    (interactive)
    (when bv-org-rename-buffer-to-title
      (let ((case-fold-search t)
            (beg (or (and end (point)) (point-min))))
        (save-excursion
          (when end
            (goto-char end)
            (setq end (line-end-position)))
          (goto-char beg)
          (when (re-search-forward
                 "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$"
                 end t)
            (rename-buffer (match-string 1) t)))))
    nil)

  (defun bv-org-rename-buffer-to-title-config ()
    "Configure Org to rename buffer to value of #+TITLE:."
    (font-lock-add-keywords nil '(bv-org-rename-buffer-to-title)))

  (when bv-org-rename-buffer-to-title
    (add-hook 'org-mode-hook 'bv-org-rename-buffer-to-title-config))

  ;; Add TODO cycling hooks
  (add-hook 'org-shiftright-hook #'bv-org-todo-cycle-right)
  (add-hook 'org-shiftleft-hook #'bv-org-todo-cycle-left)

  ;; Add visual setup hook
  (add-hook 'org-mode-hook #'bv-org-mode-visual-setup))

;;;; Org Modern
(use-package org-modern
  :hook (org-mode . (lambda ()
                      (when bv-org-modern-mode
                        (org-modern-mode 1))))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "▶"))
  (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
  (org-modern-checkbox '((?X . "☑") (?- . "☐") (?\s . "☐")))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-priority nil)
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-timestamp nil)
  (org-modern-statistics nil)
  (org-modern-progress nil)
  :config
  (custom-set-faces
   '(org-modern-symbol ((t (:family "SF Pro"))))))

;;;; Org Appear
(use-package org-appear
  :ensure nil
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-delay 0.3))

;;;; Table of Contents
(use-package org-make-toc
  :when bv-org-auto-update-toc
  :hook (org-mode . org-make-toc-mode))

;;;; Org Agenda
(use-package org-agenda
  :after org
  :bind (:map org-agenda-mode-map
              ("r" . org-agenda-redo)
              ("R" . org-agenda-redo-all)
              ("c" . org-agenda-capture))
  :custom
  (org-agenda-files (or bv-org-agenda-files
                        (list org-directory)))
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-sticky t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday 1)
  (org-agenda-include-diary nil)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled nil)
  (org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-todo-list-sublevels nil)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-clock-report-header "Clock Table")
  (org-deadline-warning-days 1)
  (org-clock-total-time-cell-format "*%s*")
  (org-duration-format '((special . h:mm)))
  (org-agenda-block-separator ?─)
  (org-agenda-compact-blocks nil)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-clockreport-parameter-plist
   '(:scope agenda
     :maxlevel 3
     :block thisweek
     :filetitle nil
     :hidefiles t
     :emphasize t
     :stepskip0 t
     :fileskip0 t
     :level nil
     :indent t
     :narrow 80!
     :tcolumns 3
     :link t))
  (org-agenda-custom-commands
   '(("d" "Daily Review"
      ((agenda "" ((org-agenda-span 1)
                   (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
                   (org-scheduled-past-days 0)
                   (org-agenda-day-face-function
                    (lambda (date) 'org-agenda-date))
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-overriding-header "Today's Agenda")))
       (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
       (todo "WAITING" ((org-agenda-overriding-header "Waiting For")))))

     ("w" "Weekly Review"
      ((agenda "" ((org-agenda-span 'week)
                   (org-agenda-start-day "-1d")
                   (org-agenda-overriding-header "Week Overview")))
       (todo "PROJ" ((org-agenda-overriding-header "Active Projects")))
       (todo "TODO" ((org-agenda-overriding-header "All TODOs")))))

     ("p" "Projects"
      ((todo "PROJ" ((org-agenda-overriding-header "All Projects")))
       (tags-todo "PROJECT" ((org-agenda-overriding-header "Project Tasks")))))))

  :config
  (defun bv-org-agenda-category (&optional len)
    "Get category of the Org Agenda item at point."
    (let* ((filename (when buffer-file-name
                       (file-name-sans-extension
                        (file-name-nondirectory buffer-file-name))))
           (title (cadr (assoc "TITLE"
                               (org-collect-keywords '("title")))))
           (category (org-get-category))
           (result (or (if (and title (string= category filename))
                           title
                         category)
                       "")))
      (if (numberp len)
          (if (require 's nil t)
              (s-truncate len (s-pad-right len " " result))
            (let ((truncated (if (> (length result) len)
                                 (substring result 0 len)
                               result)))
              (concat truncated (make-string (- len (length truncated)) ?\s))))
        result)))

  (setq org-agenda-prefix-format
        '((agenda . " %i %(bv-org-agenda-category 12)%?-12t% s")
          (todo . " %i %(bv-org-agenda-category 12) ")
          (tags . " %i %(bv-org-agenda-category 12) ")
          (search . " %i %(bv-org-agenda-category 12) "))))

;;;; Org Super Agenda
(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Important" :priority "A")
          (:name "Today" :time-grid t :scheduled today :deadline today)
          (:name "Due Soon" :deadline future)
          (:name "Overdue" :deadline past)
          (:name "Next Actions" :todo "NEXT")
          (:name "Projects" :todo "PROJ")
          (:name "Waiting" :todo "WAITING")
          (:name "Maybe" :tag "maybe" :order 100))))

;;;; Org Wild Notifier
(use-package org-wild-notifier
  :after org
  :config
  (org-wild-notifier-mode)
  (setq org-wild-notifier-alert-time '(10 5)
        org-wild-notifier-keyword-whitelist '("TODO" "NEXT")
        org-wild-notifier-notification-title "Org Agenda"))

;;;; Org Babel
(use-package ob
  :after org
  :config
  (setq org-confirm-babel-evaluate nil
        org-babel-python-command "python3")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t) (python . t) (js . t)
     (css . t) (dot . t) (plantuml . t) (C . t)
     (haskell . t) (latex . t) (lisp . t) (scheme . t)
     (julia . t) (gnuplot . t) (lua . t) (ruby . t)
     (maxima . t) (org . t)))

  (require 'org-tempo)
  (dolist (template '(("sh" . "src shell")
                      ("el" . "src emacs-lisp")
                      ("py" . "src python")
                      ("js" . "src js")
                      ("json" . "src json")
                      ("css" . "src css")
                      ("dot" . "src dot")
                      ("plantuml" . "src plantuml")))
    (add-to-list 'org-structure-template-alist template)))

;;;; Org Export
(use-package ox
  :after org
  :custom
  (org-export-with-toc t)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-todo-keywords t)
  (org-export-with-timestamps t)
  (org-export-preserve-breaks nil)
  (org-export-with-sub-superscripts '{})
  (org-export-with-smart-quotes t)
  (org-export-backends '(ascii html icalendar latex md odt))
  (org-export-kill-after-export t)
  :config
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-postamble nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"))

;;;; LaTeX Export and Preview
(use-package ox-latex
  :after ox
  :custom
  (org-latex-default-class "article")
  (org-latex-compiler "lualatex")
  (org-latex-pdf-process '("lualatex -shell-escape -interaction nonstopmode %f"
                           "biber %b"
                           "lualatex -shell-escape -interaction nonstopmode %f"
                           "lualatex -shell-escape -interaction nonstopmode %f"))
  (org-highlight-latex-and-related '(native latex script entities))
  :config
  ;; LaTeX preview configuration
  (setq org-preview-latex-image-directory
        (expand-file-name "org-latex-previews/"
                          (or (getenv "XDG_CACHE_HOME")
                              "~/.cache/")))

  ;; Add our custom lualatex+dvisvgm process
  (add-to-list 'org-preview-latex-process-alist
               '(lualatex-dvisvgm
                 :programs ("lualatex" "dvisvgm")
                 :description "lualatex dvi > svg"
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("lualatex -output-format=dvi -interaction=nonstopmode -output-directory=%o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))

  ;; Use our custom process
  (setq org-preview-latex-default-process 'lualatex-dvisvgm)

  ;; LaTeX preview scale
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.3))

  ;; Prettier highlighting for LaTeX
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  ;; Hook for text scale adjustment
  (add-hook 'text-scale-mode-hook #'bv-org-latex-preview-scale))

;;;; Org Fragtog
(use-package org-fragtog
  :if (bound-and-true-p bv-not-guix-p)
  :hook (org-mode . org-fragtog-mode))

;;;; Stable HTML IDs
(use-package ox-html-stable-ids
  :if (require 'ox-html-stable-ids nil t)
  :after ox
  :config
  (org-html-stable-ids-add))

;;;; Additional Org Packages
(use-package org-contrib
  :after org)

(use-package htmlize
  :defer t)

;;;; Timer Functions
(defun bv-org-timer-reset ()
  "Set `org-timer-mode-line-string' to nil."
  (interactive)
  (setq org-timer-mode-line-string nil))

(defun bv-org-timer-update-mode-line ()
  "Update the timer in the mode line without surrounding brackets."
  (if org-timer-pause-time
      nil
    (setq org-timer-mode-line-string
          (substring (org-timer-value-string) 0 -1))
    (force-mode-line-update)))

(advice-add 'org-timer-update-mode-line
            :override 'bv-org-timer-update-mode-line)
(add-hook 'org-timer-stop-hook 'bv-org-timer-reset)

;;;; Keybindings
(defvar bv-org-timer-map (make-sparse-keymap)
  "Keymap for `org-timer' commands.")

(define-key bv-org-timer-map "s" 'org-timer-start)
(define-key bv-org-timer-map "q" 'org-timer-stop)
(define-key bv-org-timer-map "p" 'org-timer-pause-or-continue)
(define-key bv-org-timer-map "t" 'org-timer-set-timer)

(with-eval-after-load 'bv-core
  (when (boundp 'bv-app-map)
    (define-key bv-app-map "o" bv-org-timer-map)))

;;;; Templates
(bv-with-feature tempel
  (with-eval-after-load 'tempel
    (defvar bv-org-tempel-templates
      '(org-mode
        (title "#+title: " p n "#+author: " user-full-name n
               "#+date: " (format-time-string "%Y-%m-%d") n n)
        (quote "#+begin_quote" n> r> n> "#+end_quote")
        (example "#+begin_example" n> r> n> "#+end_example")
        (center "#+begin_center" n> r> n> "#+end_center")
        (comment "#+begin_comment" n> r> n> "#+end_comment")
        (verse "#+begin_verse" n> r> n> "#+end_verse")
        (src "#+begin_src " p n> r> n> "#+end_src" :post (org-edit-src-code))
        (elisp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
        (properties ":PROPERTIES:" n ":ID: " (org-id-new) n ":END:")
        (drawer ":" p ":" n r n ":END:")
        (options "#+OPTIONS: " p)
        (latex "#+begin_export latex" n> r> n> "#+end_export")
        (html "#+begin_export html" n> r> n> "#+end_export"))
      "Org mode templates.")

    (when (boundp 'tempel-template-sources)
      (add-to-list 'tempel-template-sources 'bv-org-tempel-templates))))

;;;; Olivetti Integration
(use-package olivetti
  :hook (org-mode . olivetti-mode))

;;;; Feature Definition
(defun bv-org-load ()
  "Load Org mode configuration."
  (add-to-list 'bv-enabled-features 'org))

(provide 'bv-org)
;;; bv-org.el ends here
