;;; bv-org.el --- Org mode configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Org mode for research notes and task management.

;;; Code:

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

(when (boundp 'mode-specific-map)
  (define-key mode-specific-map (kbd "c") 'org-capture))

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources
                 `(:name "Org"
                         :narrow ?o
                         :category buffer
                         :state ,'consult--buffer-state
                         :items ,(lambda () (mapcar 'buffer-name (org-buffer-list))))
                 'append)))

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


(with-eval-after-load 'org
  (setopt org-M-RET-may-split-line '((default . nil)))
  (setopt org-insert-heading-respect-content t)
  (setq org-adapt-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-indented t)

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
        `((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
  (setq org-ellipsis "⤵")
  (set-face-attribute
   'org-ellipsis
   nil
   :inherit '(font-lock-comment-face default)
   :weight 'normal)
  (setq org-hide-emphasis-markers t)
  (setq org-log-into-drawer t)
  (setq org-directory "~/documents/org")
  (setq org-default-notes-file (concat org-directory "/todo.org"))
  (when (boundp 'org-capture-templates)
    (setq org-capture-templates
          '(("t"
           "Task"
           entry
           (file+headline "tasks.org" "Inbox")
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
          ("r"
           "Research Note"
           entry
           (file+headline "research.org" "Notes")
           "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :SOURCE: %a\n  :END:\n")
          ("p"
           "Paper Idea"
           entry
           (file+headline "papers.org" "Ideas")
           "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
          ("m"
           "Meeting"
           entry
           (file+headline "meetings.org" "Meetings")
           "* %? :meeting:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
          ("c"
           "Code Snippet"
           entry
           (file+headline "code.org" "Snippets")
           "* %?\n  #+BEGIN_SRC \n\n  #+END_SRC\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n)"
           "STARTED(s)"
           "WAITING(w@)"
           "|"
           "DONE(d)"
           "CANCELLED(c@)")
          (sequence
           "IDEA(i)"
           "DRAFT(f)"
           "REVIEW(r)"
           "|"
           "PUBLISHED(p)"
           "REJECTED(j)")
          (sequence
           "EXPERIMENT(e)"
           "RUNNING(u)"
           "ANALYSIS(a)"
           "|"
           "COMPLETE(o)"
           "FAILED(l)"))))
  (setq org-tag-alist
        '((:startgroup . nil)
          ("@office" . ?o)
          ("@home" . ?h)
          ("@computer" . ?c)
          (:endgroup . nil)
          ("physics" . ?p)
          ("ml" . ?m)
          ("theory" . ?t)
          ("experiment" . ?e)
          ("coding" . ?g)
          ("writing" . ?w)
          ("reading" . ?r)
          ("urgent" . ?u)
          ("someday" . ?s)
          ("meeting" . ?n)
          ("thesis" . ?T)
          ("paper" . ?P)
          ("course" . ?C)))

  (defun bv-org-rename-buffer-to-title (&optional end)
    "Rename buffer to #+TITLE: value."
    (interactive)
    (let ((case-fold-search t) (beg (or (and end (point)) (point-min))))
      (save-excursion
        (when end (goto-char end) (setq end (line-end-position)))
        (goto-char beg)
        (when (re-search-forward
               "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$"
               end
               t)
          (rename-buffer (match-string 1)))))
    nil)

  (defun bv-org-rename-buffer-to-title-config ()
    "Add buffer renaming to font-lock."
    (font-lock-add-keywords nil '(bv-org-rename-buffer-to-title)))

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

  (with-eval-after-load 'bv-keymaps
    (when (boundp 'bv-app-map)
      (define-key bv-app-map (kbd "o") 'bv-org-timer-map)))

  (when (boundp 'bv-org-timer-map)
    (define-key bv-org-timer-map (kbd "s") 'org-timer-start)
    (define-key bv-org-timer-map (kbd "q") 'org-timer-stop)
    (define-key bv-org-timer-map (kbd "p") 'org-timer-pause-or-continue)
    (define-key bv-org-timer-map (kbd "t") 'org-timer-set-timer))

  (autoload 'global-org-modern-mode "org-modern"))

(provide 'bv-org)
;;; bv-org.el ends here