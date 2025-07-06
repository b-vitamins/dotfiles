;;; bv-org-ql.el --- Org query language configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Advanced org searching with org-ql.

;;; Code:

(autoload 'org-ql-search "org-ql")
(autoload 'org-ql-view "org-ql")
(autoload 'org-ql-sparse-tree "org-ql")

(with-eval-after-load 'org-ql
  ;; Predefined searches
  (when (boundp 'org-ql-views)
    (setq org-ql-views
          '(("Overview" :query (and (todo) (not (done)))
             :title "All active tasks"
             :sort (priority date)
             :super-groups ((:auto-category t)))

            ("Recent" :query (ts :from -7 :to today)
             :title "Recent items (last week)"
             :sort (date priority))

            ("Research" :query (or (tags "research") (tags "paper") (todo "IDEA" "DRAFT"))
             :title "Research items"
             :sort (priority))

            ("Stalled" :query (and (todo "STARTED" "WAITING") (ts :to -7))
             :title "Stalled tasks"
             :sort (date))))))

;; Custom search functions
(defun bv-org-ql-search-current-project ()
  "Search in current project files."
  (interactive)
  (org-ql-search (project-files (project-current))
                 (read-string "Query: ")
                 :title "Project search"))

(defun bv-org-ql-search-research ()
  "Search research-related entries."
  (interactive)
  (org-ql-search (org-agenda-files)
                 '(or (tags "research") (tags "paper") (todo "IDEA" "DRAFT" "EXPERIMENT"))
                 :title "Research search"
                 :sort '(priority date)))

(defun bv-org-ql-upcoming-deadlines ()
  "Show upcoming deadlines."
  (interactive)
  (org-ql-search (org-agenda-files)
                 '(deadline :to +14)
                 :title "Upcoming deadlines (14 days)"
                 :sort '(date priority)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "q") 'org-ql-search)
    (define-key bv-app-map (kbd "Q") 'org-ql-view)))

(provide 'bv-org-ql)
;;; bv-org-ql.el ends here