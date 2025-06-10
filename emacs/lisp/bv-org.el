;;; bv-org.el --- Org mode foundation -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Org mode configuration with modern aesthetics and functionality.
;; Includes agenda, babel, export, and visual enhancements.

;;; Code:

;;;; Dependencies
(require 'bv-core)

;;;; Custom Variables
(defgroup bv-org nil
  "Org mode configuration."
  :group 'bv)

(defcustom bv-org-directory "~/org"
  "Directory for org files."
  :type 'directory
  :group 'bv-org)

(defcustom bv-org-default-notes-file "todo.org"
  "Default file for capturing notes."
  :type 'string
  :group 'bv-org)

(defcustom bv-org-agenda-files nil
  "Files to include in agenda.
If nil, will be set based on org-roam-todo? feature."
  :type '(repeat file)
  :group 'bv-org)

(defcustom bv-org-rename-buffer-to-title t
  "Rename org buffers to their #+TITLE."
  :type 'boolean
  :group 'bv-org)

(defcustom bv-org-indent-mode t
  "Enable org-indent-mode by default."
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

(defcustom bv-org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
  "Org todo keywords."
  :type '(repeat (cons (choice :tag "Type"
                               (const :tag "Sequence" sequence)
                               (const :tag "Type" type))
                       (repeat :tag "Keywords" string)))
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

;;;; Core Org Configuration
(use-package org
  :ensure nil  ; Built-in
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c C-x C-s" . bv-org-mark-done-and-archive)
         ("C-c C-x C-a" . org-archive-subtree)
         ("C-c t" . org-todo))
  :custom
  ;; General settings
  (org-directory bv-org-directory)
  (org-default-notes-file (expand-file-name bv-org-default-notes-file 
                                            bv-org-directory))
  (org-startup-indented bv-org-indent-mode)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(400))
  
  ;; Appearance
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  
  ;; Behavior
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)
  (org-use-speed-commands t)
  
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
     ("CANCELLED" . (:foreground "#888888" :strike-through t))))
  
  ;; Priorities
  (org-priority-highest ?A)
  (org-priority-lowest ?C)
  (org-priority-faces
   '((?A . (:foreground "#FF0000" :weight bold))
     (?B . (:foreground "#FFA500"))
     (?C . (:foreground "#00AA00"))))
  
  ;; Archive
  (org-archive-location "%s_archive::* Archived Tasks")
  
  ;; Capture
  (org-capture-templates bv-org-capture-templates)
  
  :config
  ;; Load modules
  (require 'org-protocol)
  (require 'org-habit)
  (require 'org-id)
  
  ;; Set ID locations file
  (setq org-id-locations-file
        (expand-file-name "org-id-locations" bv-cache-dir))
  
  ;; Better defaults for lists
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+")))
  
  ;; Fontify native blocks (e.g., source code)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  
  ;; Custom faces
  (custom-set-faces
   '(org-ellipsis ((t (:inherit font-lock-comment-face :weight normal))))
   '(org-document-title ((t (:height 1.5 :weight bold)))))
  
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
  
  ;; Archive helpers
  (defun bv-org-mark-done-and-archive ()
    "Mark the state of the current subtree as DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree)))

;;;; Org Modern - Visual Enhancements
(use-package org-modern
  :ensure t
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
  ;; Use fixed-pitch for tables
  (custom-set-faces
   '(org-modern-symbol ((t (:family "Iosevka"))))))

;;;; Org Appear - Hide/Show Markup
(use-package org-appear
  :ensure t
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
  :ensure t
  :when bv-org-auto-update-toc
  :hook (org-mode . org-make-toc-mode))

;;;; Org Agenda
(use-package org-agenda
  :ensure nil  ; Part of org
  :after org
  :bind (:map org-agenda-mode-map
              ("r" . org-agenda-redo)
              ("R" . org-agenda-redo-all)
              ("c" . org-agenda-capture))
  :custom
  ;; Files
  (org-agenda-files (or bv-org-agenda-files
                       (list org-directory)))
  
  ;; Display
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-sticky t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday 1)  ; Monday
  (org-agenda-include-diary nil)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-block-separator ?─)
  (org-agenda-compact-blocks nil)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  
  ;; Logging
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(closed clock state))
  
  ;; Custom commands
  (org-agenda-custom-commands
   '(("d" "Daily Review"
      ((agenda "" ((org-agenda-span 1)
                   (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
                   (org-scheduled-past-days 0)
                   (org-agenda-day-face-function
                    (lambda (date) 'org-agenda-date))
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-overriding-header "Today's Agenda")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Actions")))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting For")))))
     
     ("w" "Weekly Review"
      ((agenda "" ((org-agenda-span 'week)
                   (org-agenda-start-day "-1d")
                   (org-agenda-overriding-header "Week Overview")))
       (todo "PROJ"
             ((org-agenda-overriding-header "Active Projects")))
       (todo "TODO"
             ((org-agenda-overriding-header "All TODOs")))))
     
     ("p" "Projects"
      ((todo "PROJ"
             ((org-agenda-overriding-header "All Projects")))
       (tags-todo "PROJECT"
                  ((org-agenda-overriding-header "Project Tasks")))))))
  
  :config
  ;; Custom category function
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
          (s-truncate len (s-pad-right len " " result))
        result)))
  
  ;; Better prefix format
  (setq org-agenda-prefix-format
        '((agenda . " %i %(bv-org-agenda-category 12)%?-12t% s")
          (todo . " %i %(bv-org-agenda-category 12) ")
          (tags . " %i %(bv-org-agenda-category 12) ")
          (search . " %i %(bv-org-agenda-category 12) "))))

;;;; Org Super Agenda
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Important"
                 :priority "A")
          (:name "Today"
                 :time-grid t
                 :scheduled today
                 :deadline today)
          (:name "Due Soon"
                 :deadline future)
          (:name "Overdue"
                 :deadline past)
          (:name "Next Actions"
                 :todo "NEXT")
          (:name "Projects"
                 :todo "PROJ")
          (:name "Waiting"
                 :todo "WAITING")
          (:name "Maybe"
                 :tag "maybe"
                 :order 100))))

;;;; Org Wild Notifier
(use-package org-wild-notifier
  :ensure t
  :after org
  :config
  (org-wild-notifier-mode)
  (setq org-wild-notifier-alert-time '(10 5))
  (setq org-wild-notifier-keyword-whitelist '("TODO" "NEXT"))
  (setq org-wild-notifier-notification-title "Org Agenda"))

;;;; Org Babel
(use-package ob
  :ensure nil  ; Part of org
  :after org
  :config
  ;; Don't ask for confirmation
  (setq org-confirm-babel-evaluate nil)
  
  ;; Languages to load
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (js . t)
     (css . t)
     (dot . t)
     (plantuml . t)))
  
  ;; Structure templates
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
  :ensure nil  ; Part of org
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
  :config
  ;; HTML export settings
  (setq org-html-doctype "html5")
  (setq org-html-html5-fancy t)
  (setq org-html-postamble nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  
  ;; Custom CSS for HTML export
  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"))

;;;; Stable HTML IDs
(use-package ox-html-stable-ids
  :ensure t
  :after ox
  :config
  (org-html-stable-ids-add))

;;;; Additional Org Packages
(use-package org-contrib
  :ensure t
  :after org)

(use-package htmlize
  :ensure t
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
  "Keymap for org-timer commands.")

(define-key bv-org-timer-map "s" 'org-timer-start)
(define-key bv-org-timer-map "q" 'org-timer-stop)
(define-key bv-org-timer-map "p" 'org-timer-pause-or-continue)
(define-key bv-org-timer-map "t" 'org-timer-set-timer)

(with-eval-after-load 'bv-core
  (define-key bv-app-map "o" bv-org-timer-map))

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
    
    (add-to-list 'tempel-template-sources 'bv-org-tempel-templates)))

;;;; Olivetti Integration
(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 85))

;;;; Feature Definition
(defun bv-org-load ()
  "Load Org mode configuration."
  (add-to-list 'bv-enabled-features 'org))

(provide 'bv-org)
;;; bv-org.el ends here