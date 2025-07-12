;;; bv-org-roam.el --- Org-roam knowledge base  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Zettelkasten-style knowledge management.

;;; Code:

(eval-when-compile (require 'eieio))

(autoload 'marginalia--time "marginalia")
(autoload 'oref "eieio")

(cl-defstruct emacsql-connection handle)

(defun bv-patch-emacsql-close (connection &rest _args)
  "Prevent emacsql-close errors."
  (when (ignore-errors (oref connection handle))
    t))

(with-eval-after-load 'emacsql
  (advice-add 'emacsql-close :before-while #'bv-patch-emacsql-close))

(eval-when-compile (let ((org-roam-v2-ack t)) (require 'org-roam)))

(when (boundp 'org-roam-v2-ack)
  (setq org-roam-v2-ack t))
(when (boundp 'org-roam-completion-everywhere)
  (setq org-roam-completion-everywhere t))
(when (boundp 'org-roam-directory)
  (setq org-roam-directory "~/documents/slipbox/slips"))
(when (boundp 'org-roam-db-gc-threshold)
  (setq org-roam-db-gc-threshold most-positive-fixnum))

(defun bv-ensure-org-roam-directories ()
  "Ensure org-roam directories exist."
  (let ((roam-dir (expand-file-name (if (boundp 'org-roam-directory) org-roam-directory "~/documents/slipbox/slips")))
        (cache-dir (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs")))
    (unless (file-exists-p roam-dir)
      (make-directory roam-dir t))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))))

(with-eval-after-load 'org-roam
  (when (boundp 'org-roam-db-location)
    (setq org-roam-db-location
          (concat
           (or (getenv "XDG_CACHE_HOME") "~/.cache")
           "/emacs/org-roam.db")))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE, a directory relative to `org-roam-directory'."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name
            (org-roam-node-file node)
            org-roam-directory))))
      (error "")))

  (when (boundp 'org-roam-node-display-template)
    (setq org-roam-node-display-template
          (concat "${type:15} ${title:*}")))
  (when (boundp 'org-roam-node-annotation-function)
    (setq org-roam-node-annotation-function
          (lambda (node) (marginalia--time (org-roam-node-file-mtime node)))))

  (bv-ensure-org-roam-directories)

  (run-with-idle-timer 1 nil
                       (lambda ()
                         (when (fboundp 'org-roam-db-sync)
                           (org-roam-db-sync))
                         (org-roam-db-autosync-enable)))

  (defun bv-org-roam-open-ref ()
    "Open selected ROAM_REF."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (if-let* ((refs (org-property-values "ROAM_REFS"))
                (choices
                 (mapcar
                  (lambda (x) (org-unbracket-string "[[" "]]" x))
                  (split-string
                   (car (org-property-values "ROAM_REFS"))
                   " ")))
                (node-ref
                 (completing-read
                  "Refs: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          (category . org-roam-ref)
                          ,(cons 'display-sort-function 'identity))
                      (complete-with-action action choices string pred)))
                  nil
                  'require-match)))
          node-ref
        (error "No roam refs in this node"))))

  (with-eval-after-load 'org
    (when (boundp 'org-mode-map)
      (let ((map org-mode-map))
      (define-key map (kbd "C-TAB") 'completion-at-point)
      (define-key map (kbd "C-c r r") 'org-roam-ref-add)
      (define-key map (kbd "C-c r R") 'org-roam-ref-remove)
      (define-key map (kbd "C-c r f") 'org-roam-ref-find)
      (define-key map (kbd "C-c r t") 'org-roam-tag-add)
      (define-key map (kbd "C-c r T") 'org-roam-tag-remove)
      (define-key map (kbd "C-c r a") 'org-roam-alias-add)
      (define-key map (kbd "C-c r A") 'org-roam-alias-remove)
      (define-key map (kbd "C-c r O") 'bv-org-roam-open-ref))))

  (when (boundp 'org-roam-capture-templates)
    (setq org-roam-capture-templates
          '(("s" "Slip" plain
             "%?"
             :target (file+head "%<%Y-%m-%d>-${slug}.org"
                               ":PROPERTIES:\n:ID: %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: \n\n")
             :unnarrowed t)
            ("l" "Literature" plain
             "%?"
             :target (file+head "%<%Y-%m-%d>-lit-${slug}.org"
                               ":PROPERTIES:\n:ID: %(org-id-new)\n:ROAM_REFS: \n:END:\n#+title: ${title}\n#+filetags: :literature:\n\n* Summary\n\n* Key Concepts\n\n* Connections\n\n")
             :unnarrowed t)
            ("c" "Concept" plain
             "%?"
             :target (file+head "slips/%<%Y-%m-%d>-concept-${slug}.org"
                               ":PROPERTIES:\n:ID: %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :concept:\n\n")
             :unnarrowed t)
            ("f" "Fleeting" plain
             "%?"
             :target (file+head "slips/%<%Y-%m-%d>-fleeting-${slug}.org"
                               ":PROPERTIES:\n:ID: %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :fleeting:\n\n")
             :unnarrowed t)))))

(with-eval-after-load 'embark
  ;; Actions for org-roam refs
  (defvar-keymap embark-roam-ref-map
    :doc "Keymap for actions on org-roam refs."
    :parent (if (boundp 'embark-url-map) embark-url-map)
    "v" 'bv-mpv-play-url
    "RET" 'browse-url-generic
    "c" 'browse-url-chromium
    "r" 'org-roam-ref-remove)

  ;; Actions for org-roam nodes
  (defvar-keymap embark-org-roam-node-map
    :doc "Keymap for actions on org-roam nodes."
    :parent embark-general-map
    "RET" 'org-roam-node-open
    "o" 'org-roam-node-open
    "v" 'org-roam-node-visit
    "i" 'org-roam-node-insert
    "t" 'org-roam-tag-add
    "T" 'org-roam-tag-remove
    "a" 'org-roam-alias-add
    "A" 'org-roam-alias-remove
    "r" 'org-roam-ref-add
    "R" 'org-roam-ref-remove
    "d" 'org-roam-db-sync)

  (when (boundp 'embark-keymap-alist)
    (add-to-list 'embark-keymap-alist '(org-roam-ref . embark-roam-ref-map))
    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-node-map)))

  (advice-add 'org-roam-ref-add :around 'bv-browse-url-trace-url))

;; Consult integration
(defun bv-org-roam-node-read ()
  "Select org-roam node with consult."
  (org-roam-node-read nil nil nil 'require-match))

(with-eval-after-load 'consult
  ;; Custom org-roam source for consult-buffer
  (defvar bv-consult--source-org-roam
    `(:name "Org-Roam"
      :narrow ?r
      :category org-roam-node
      :face consult-file
      :history org-roam-node-history
      :items ,(lambda () (mapcar #'org-roam-node-title
                                 (org-roam-node-list)))
      :state ,#'consult--file-state
      :new ,(lambda (name) (org-roam-capture- :node (org-roam-node-create :title name)))))

  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'bv-consult--source-org-roam 'append)))

;; Enhanced search function
(defun bv-org-roam-node-find-consult ()
  "Find org-roam node with enhanced preview."
  (interactive)
  (org-roam-node-find nil nil #'bv-org-roam-node-read))

;; Tag-based filtering
(defun bv-org-roam-node-find-by-tag ()
  "Find org-roam nodes filtered by tag."
  (interactive)
  (let* ((tags (seq-uniq (seq-mapcat #'org-roam-node-tags (org-roam-node-list))))
         (selected-tag (completing-read "Tag: " tags))
         (org-roam-node-display-template
          (concat "${type:15} ${title:*} " (propertize "${tags:20}" 'face 'org-tag))))
    (org-roam-node-find nil nil
                        (lambda (&optional initial-input filter)
                          (org-roam-node-read initial-input
                                              (lambda (node)
                                                (and (member selected-tag (org-roam-node-tags node))
                                                     (or (not filter) (funcall filter node))))
                                              nil
                                              'require-match)))))

(defun bv-org-roam-tag-view ()
  "Show all org-roam nodes grouped by tags."
  (interactive)
  (let ((nodes-by-tag (make-hash-table :test 'equal)))
    ;; Group nodes by tags
    (dolist (node (org-roam-node-list))
      (dolist (tag (org-roam-node-tags node))
        (push node (gethash tag nodes-by-tag))))
    ;; Create buffer
    (with-current-buffer (get-buffer-create "*Org-Roam Tags*")
      (read-only-mode -1)
      (erase-buffer)
      (insert "Org-Roam Nodes by Tag\n")
      (insert "=====================\n\n")
      ;; Sort tags and display
      (dolist (tag (sort (hash-table-keys nodes-by-tag) #'string<))
        (insert (propertize (format "◆ %s" tag) 'face 'org-level-1))
        (insert (format " (%d)\n" (length (gethash tag nodes-by-tag))))
        (dolist (node (sort (gethash tag nodes-by-tag)
                           (lambda (a b)
                             (string< (org-roam-node-title a)
                                     (org-roam-node-title b)))))
          (insert-text-button (format "  • %s\n" (org-roam-node-title node))
                              'action (lambda (_) (org-roam-node-visit node))
                              'follow-link t
                              'help-echo (org-roam-node-file node)))
        (insert "\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer "*Org-Roam Tags*")))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "n") 'org-roam-buffer-toggle)
    (define-key bv-app-map (kbd "f") 'bv-org-roam-node-find-consult)
    (define-key bv-app-map (kbd "F") 'bv-org-roam-node-find-by-tag)
    (define-key bv-app-map (kbd "v") 'bv-org-roam-tag-view)
    (define-key bv-app-map (kbd "i") 'org-roam-node-insert)
    (define-key bv-app-map (kbd "r") 'org-roam-ref-find)
    (define-key bv-app-map (kbd "N") 'org-roam-capture)))

(provide 'bv-org-roam)
;;; bv-org-roam.el ends here