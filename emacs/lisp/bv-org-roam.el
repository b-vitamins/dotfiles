;;; bv-org-roam.el --- Enhanced Org-roam knowledge base -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (consult "0.20") (consult-org-roam "0.1"))

;;; Commentary:

;; A streamlined and robust Org-roam configuration with enhanced completion,
;; project management, and deep consult integration.
;;
;; Key features:
;; - Clean, aligned completion display with tags and modification time
;; - Project management with automatic org-agenda integration
;; - Multiple capture templates for different note types
;; - Advanced search and filtering capabilities
;; - Full consult-org-roam integration
;; - Daily notes and task tracking

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Declare dependencies
(declare-function org-roam-node-file "org-roam-node")
(declare-function org-roam-node-title "org-roam-node")
(declare-function org-roam-node-tags "org-roam-node")
(declare-function org-roam-node-id "org-roam-node")
(declare-function org-roam-node-todo "org-roam-node")
(declare-function org-roam-node-scheduled "org-roam-node")
(declare-function org-roam-node-deadline "org-roam-node")
(declare-function org-roam-node-file-mtime "org-roam-node")
(declare-function org-roam-node-list "org-roam")
(declare-function org-roam-node-visit "org-roam")
(declare-function org-roam-node-at-point "org-roam")
(declare-function org-roam-db-query "org-roam-db")
(declare-function consult--read "consult")
(declare-function consult--file-action "consult")
(declare-function marginalia--time "marginalia")


;;; Customization

(defgroup bv-org-roam nil
  "Enhanced Org-roam configuration."
  :group 'org-roam
  :prefix "bv-org-roam-")

(defcustom bv-org-roam-directory "~/documents/slipbox/slips"
  "Directory for org-roam files."
  :type 'directory
  :group 'bv-org-roam)

(defcustom bv-org-roam-show-backlinks t
  "Whether to show backlink count in completions."
  :type 'boolean
  :group 'bv-org-roam)

(defcustom bv-org-roam-show-modified-time t
  "Whether to show modified time in completions."
  :type 'boolean
  :group 'bv-org-roam)

(defcustom bv-org-roam-time-format "%Y-%m-%d"
  "Format string for displaying time."
  :type 'string
  :group 'bv-org-roam)

(defcustom bv-org-roam-tag-width 30
  "Width allocated for tags in completion display."
  :type 'integer
  :group 'bv-org-roam)


;;; Setup and initialization

(defun bv-org-roam-setup-directories ()
  "Ensure all org-roam directories exist."
  (let ((dirs (list bv-org-roam-directory
                    (expand-file-name "daily" bv-org-roam-directory)
                    (expand-file-name "literature" bv-org-roam-directory)
                    (expand-file-name "concepts" bv-org-roam-directory)
                    (expand-file-name "fleeting" bv-org-roam-directory)
                    (expand-file-name "projects" bv-org-roam-directory))))
    (dolist (dir dirs)
      (unless (file-exists-p dir)
        (make-directory dir t)))))


;;; Custom display methods
;; These MUST be defined before org-roam loads for the display template to work

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-filetags ((node org-roam-node))
    "Return formatted tags for NODE."
    (let ((tags (org-roam-node-tags node)))
      (if tags
          (format ":%s:" (string-join tags ":"))
        "")))

  (cl-defmethod org-roam-node-backlinks ((node org-roam-node))
    "Return backlink count for NODE."
    (let ((count (caar (org-roam-db-query
                        [:select (funcall count source)
                         :from links
                         :where (= dest $s1)
                         :and (= type "id")]
                        (org-roam-node-id node)))))
      (if (> count 0)
          (format "← %d" count)
        "")))

  (cl-defmethod org-roam-node-mtime ((node org-roam-node))
    "Return formatted modification time for NODE."
    (let ((mtime (org-roam-node-file-mtime node)))
      (if mtime
          (format-time-string bv-org-roam-time-format mtime)
        ""))))


;;; Configure org-roam

(use-package org-roam
  :demand t
  :init
  ;; Acknowledge v2
  (setq org-roam-v2-ack t)

  :custom
  (org-roam-directory bv-org-roam-directory)
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-db-location
   (expand-file-name "org-roam.db"
                     (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs")))

  ;; Configure display template
  (org-roam-node-display-template
   (concat "${title:*}  "
           (propertize "${filetags:25}  " 'face 'org-tag)
           " "
           (propertize "${mtime:12}" 'face 'font-lock-comment-face)))

  :config
  ;; Ensure directories exist
  (bv-org-roam-setup-directories)

  ;; Start database sync
  (org-roam-db-autosync-enable)

  ;; Configure capture templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags:\n\n")
           :unnarrowed t)

          ("l" "literature" plain
           "* Summary\n\n%?\n\n* Key Points\n\n* Questions\n\n* References"
           :target (file+head "literature/${slug}.org"
                              "#+title: ${title}\n#+filetags: :literature:\n\n")
           :unnarrowed t)

          ("c" "concept" plain
           "* Definition\n\n%?\n\n* Properties\n\n* Examples\n\n* Related"
           :target (file+head "concepts/${slug}.org"
                              "#+title: ${title}\n#+filetags: :concept:\n\n")
           :unnarrowed t)

          ("p" "project" plain
           "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n** TODO \n\n* Resources"
           :target (file+head "projects/${slug}.org"
                              "#+title: ${title}\n#+filetags: :project:\n")
           :unnarrowed t)

          ("f" "fleeting" plain "%?"
           :target (file+head "fleeting/${slug}.org"
                              "#+title: ${title}\n#+filetags: :fleeting:\n")
           :immediate-finish t)))

  ;; Configure daily notes
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n* Tasks\n\n* Notes\n\n"))))

  :bind ((:map org-mode-map
          ("C-c n i" . org-roam-node-insert)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n r" . org-roam-ref-add))))


;;; Enhanced completion interface

(defun bv-org-roam-node-read (&optional initial-input filter-fn sort-fn require-match)
  "Enhanced node selection with preview and annotations.
INITIAL-INPUT is the initial minibuffer input.
FILTER-FN is a function to filter nodes.
SORT-FN is a function to sort nodes.
REQUIRE-MATCH determines if existing node is required."
  (let* ((nodes (org-roam-node-list))
         (nodes (if filter-fn (seq-filter filter-fn nodes) nodes))
         (nodes (if sort-fn (seq-sort sort-fn nodes) nodes))
         (templates (list (cons "" org-roam-capture-templates)))
         (node (org-roam-node-read initial-input filter-fn sort-fn require-match templates)))
    node))


;;; Project management

(defun bv-org-roam-filter-by-tag (tag-name)
  "Return filter function for nodes with TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun bv-org-roam-project-files ()
  "Return list of all project files."
  (seq-map #'org-roam-node-file
           (seq-filter (bv-org-roam-filter-by-tag "project")
                       (org-roam-node-list))))

(defun bv-org-roam-update-agenda-files ()
  "Update org-agenda-files with project files."
  (interactive)
  (setq org-agenda-files
        (seq-uniq (append (bv-org-roam-project-files)
                          org-agenda-files))))

(defun bv-org-roam-find-project ()
  "Find or create a project note."
  (interactive)
  (let ((node (bv-org-roam-node-read
               nil
               (bv-org-roam-filter-by-tag "project")
               nil
               nil)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node)
      ;; Create new project
      (org-roam-capture-
       :node node
       :templates '(("p" "project" plain
                     "* Overview\n\n%?\n\n* Goals\n\n* Tasks\n** TODO \n\n* Resources"
                     :target (file+head "projects/${slug}.org"
                                        "#+title: ${title}\n#+filetags: :project:\n")
                     :unnarrowed t))))
    ;; Add to agenda after creation
    (when (and (not (org-roam-node-file node))
               (eq org-capture-entry '("p" "project")))
      (add-hook 'org-capture-after-finalize-hook
                #'bv-org-roam-update-agenda-files
                nil t))))


;;; Quick capture functions

(defun bv-org-roam-capture-task ()
  "Capture a task to a project."
  (interactive)
  (let ((node (bv-org-roam-node-read
               nil
               (bv-org-roam-filter-by-tag "project")
               nil
               t)))
    (when node
      (let ((org-capture-templates
             `(("t" "task" entry
                (file+headline ,(org-roam-node-file node) "Tasks")
                "** TODO %?\n SCHEDULED: %t"))))
        (org-capture nil "t")))))

(defun bv-org-roam-node-insert-immediate (arg)
  "Insert node link without opening the note.
With prefix ARG, use alternative link type."
  (interactive "P")
  (let ((node (bv-org-roam-node-read)))
    (when node
      (insert (org-link-make-string
               (concat "id:" (org-roam-node-id node))
               (org-roam-node-title node))))))


;;; Sorting functions

(defun bv-org-roam-sort-by-mtime (a b)
  "Sort nodes A and B by modification time (newest first)."
  (let ((time-a (org-roam-node-file-mtime a))
        (time-b (org-roam-node-file-mtime b)))
    (cond
     ((and time-a time-b) (time-less-p time-b time-a))
     (time-a t)
     (time-b nil)
     (t nil))))

(defun bv-org-roam-sort-by-backlinks (a b)
  "Sort nodes A and B by backlink count."
  (let ((count-a (length (org-roam-backlinks-get a)))
        (count-b (length (org-roam-backlinks-get b))))
    (> count-a count-b)))


;;; Search and navigation commands

(defun bv-org-roam-find-by-tag ()
  "Find nodes by tag."
  (interactive)
  (let* ((tags (seq-uniq (seq-mapcat #'org-roam-node-tags (org-roam-node-list))))
         (tag (completing-read "Tag: " tags nil t)))
    (org-roam-node-find nil nil (bv-org-roam-filter-by-tag tag) nil nil)))

(defun bv-org-roam-find-recent ()
  "Find recently modified nodes."
  (interactive)
  (org-roam-node-find nil nil nil nil nil #'bv-org-roam-sort-by-mtime))

(defun bv-org-roam-find-orphan ()
  "Find orphan nodes (no backlinks)."
  (interactive)
  (org-roam-node-find
   nil nil
   (lambda (node)
     (= 0 (length (org-roam-backlinks-get node))))
   nil nil))

(defun bv-org-roam-random ()
  "Visit a random node."
  (interactive)
  (let* ((nodes (org-roam-node-list))
         (node (seq-random-elt nodes)))
    (org-roam-node-visit node)))


;;; Statistics

(defun bv-org-roam-statistics ()
  "Display org-roam statistics."
  (interactive)
  (let* ((nodes (org-roam-node-list))
         (total (length nodes))
         (tags (seq-uniq (seq-mapcat #'org-roam-node-tags nodes)))
         (orphans (seq-filter
                   (lambda (n) (= 0 (length (org-roam-backlinks-get n))))
                   nodes))
         (projects (seq-filter (bv-org-roam-filter-by-tag "project") nodes))
         (recent (seq-filter
                  (lambda (n)
                    (let ((mtime (org-roam-node-file-mtime n)))
                      (and mtime
                           (< (time-to-days (time-subtract (current-time) mtime)) 7))))
                  nodes)))
    (message "Org-Roam: %d nodes | %d tags | %d orphans | %d projects | %d recent"
             total (length tags) (length orphans) (length projects) (length recent))))


;;; Consult-org-roam integration

(use-package consult-org-roam
  :after (org-roam consult)
  :demand t
  :init
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :bind
  (("C-c n s" . consult-org-roam-search)
   ("C-c n l" . consult-org-roam-backlinks)
   ("C-c n L" . consult-org-roam-forward-links)
   ("C-c n F" . consult-org-roam-file-find)))


;;; Keybindings

(bind-keys :prefix-map bv-org-roam-map
           :prefix "C-c n"
           ("n" . org-roam-buffer-toggle)
           ("f" . org-roam-node-find)
           ("i" . org-roam-node-insert)
           ("I" . bv-org-roam-node-insert-immediate)
           ("c" . org-roam-capture)
           ("g" . bv-org-roam-find-by-tag)
           ("p" . bv-org-roam-find-project)
           ("t" . bv-org-roam-capture-task)
           ("r" . bv-org-roam-random)
           ("R" . bv-org-roam-find-recent)
           ("o" . bv-org-roam-find-orphan)
           ("S" . bv-org-roam-statistics)
           ("d" . org-roam-dailies-capture-today)
           ("D" . org-roam-dailies-goto-today)
           ("y" . org-roam-dailies-goto-yesterday)
           ("Y" . org-roam-dailies-goto-tomorrow))


;;; Initialize

(add-hook 'after-init-hook #'bv-org-roam-update-agenda-files)

(provide 'bv-org-roam)
;;; bv-org-roam.el ends here
