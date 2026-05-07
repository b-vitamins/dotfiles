;;; bv-org-slipbox.el --- Slip box implementation using org-slipbox -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.0

;;; Commentary:

;; Slipbox configuration built on org-slipbox.
;;
;; The slipbox owns the hot `C-c n' notes domain on top of the org-slipbox
;; daemon-backed index.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-slipbox)
(require 'bv-completion)

(declare-function consult-org-slipbox-backlinks "consult-org-slipbox" (&optional other-window))
(declare-function consult-org-slipbox-file-find "consult-org-slipbox" (&optional other-window initial-input))
(declare-function consult-org-slipbox-forward-links "consult-org-slipbox" (&optional other-window))
(declare-function consult-org-slipbox-mode "consult-org-slipbox" (&optional arg))
(declare-function consult-org-slipbox--note-buffer-p "consult-org-slipbox" (buffer))
(declare-function consult-org-slipbox-ref-find "consult-org-slipbox" (&optional other-window initial-input))
(declare-function consult-org-slipbox-search "consult-org-slipbox" (&optional other-window initial-input))
(declare-function consult--buffer-pair "consult" (buffer))
(declare-function consult--buffer-query "consult" (&rest args))
(declare-function consult--customize-put "consult" (cmds prop form))
(declare-function nerd-icons-octicon "nerd-icons" (icon-name &rest args))
;; Autoloaded from `org-slipbox-maintenance'.
(declare-function org-slipbox-sync "org-slipbox-maintenance" ())
(autoload 'org-slipbox-sync "org-slipbox-maintenance" nil t)

(defvar consult-org-slipbox-buffer-after-buffers)
(defvar consult-org-slipbox-buffer-narrow-key)

(defgroup bv-org-slipbox nil
  "Slip box configuration for org-slipbox."
  :group 'applications
  :prefix "bv-org-slipbox-")

(defcustom bv-org-slipbox-directory "~/org/myslipbox"
  "Directory for org-slipbox notes."
  :type 'directory
  :group 'bv-org-slipbox)

(defcustom bv-org-slipbox-show-backlinks t
  "Whether to show backlink counts in node annotations."
  :type 'boolean
  :group 'bv-org-slipbox)

(defvar bv-org-slipbox-common-tags
  '("physics" "mathematics" "computation" "quantum" "statistics"
    "deeplearn" "algorithm" "theorem" "proof" "concept"
    "problem" "solution" "idea" "hypothesis" "experiment"
    "review" "summary" "index" "reference" "question")
  "Common tags for slipbox notes.")

(defvar bv-notes-map)

(defvar bv-org-slipbox-map
  (if (boundp 'bv-notes-map)
      bv-notes-map
    (make-sparse-keymap))
  "Keymap for BV slipbox commands.")

(defconst bv-org-slipbox--capture-types
  '(plain entry item checkitem table-line)
  "Capture template content types recognized by org-slipbox.")

(defvar bv-org-slipbox--initial-index-ready nil
  "Non-nil when the slipbox index is known to contain existing notes.")

(defvar bv-org-slipbox--initial-sync-running nil
  "Non-nil while an initial slipbox bootstrap sync is in progress.")

(defconst bv-org-slipbox--tag-icon-alist
  '(("physics" . ("nf-oct-beaker" . bv-icon-science))
    ("experiment" . ("nf-oct-beaker" . bv-icon-science))
    ("problem" . ("nf-oct-question" . bv-icon-warning))
    ("concept" . ("nf-oct-light_bulb" . bv-icon-idea))
    ("idea" . ("nf-oct-light_bulb" . bv-icon-idea))
    ("algorithm" . ("nf-oct-code" . bv-icon-code))
    ("theorem" . ("nf-oct-law" . bv-icon-proof))
    ("proof" . ("nf-oct-law" . bv-icon-proof))
    ("deeplearn" . ("nf-oct-hubot" . bv-icon-system))
    ("review" . ("nf-oct-search" . bv-icon-review))
    ("index" . ("nf-oct-checklist" . bv-icon-index)))
  "Octicon-family slipbox icons keyed by note tag.")

(defun bv-org-slipbox--sequence (value)
  "Normalize JSON-like VALUE into a plain list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun bv-org-slipbox--cache-home ()
  "Return the XDG cache directory."
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name "~/.cache")))

(defun bv-org-slipbox--setup-directory ()
  "Ensure the slipbox directory exists."
  (unless (file-directory-p org-slipbox-directory)
    (make-directory org-slipbox-directory t)))

(defun bv-org-slipbox--eligible-files ()
  "Return eligible slipbox files under `org-slipbox-directory'."
  (when (file-directory-p org-slipbox-directory)
    (org-slipbox-list-files org-slipbox-directory)))

(defun bv-org-slipbox--index-populated-p ()
  "Return non-nil when the current org-slipbox index already has content."
  (let* ((status (org-slipbox-rpc-status))
         (files-indexed (or (plist-get status :files_indexed) 0))
         (nodes-indexed (or (plist-get status :nodes_indexed) 0)))
    (or (> files-indexed 0)
        (> nodes-indexed 0))))

(defun bv-org-slipbox--ensure-initial-index ()
  "Build the initial org-slipbox index when notes exist but the DB is empty."
  (unless (or bv-org-slipbox--initial-index-ready
              bv-org-slipbox--initial-sync-running)
    (when-let ((files (bv-org-slipbox--eligible-files)))
      (condition-case err
          (if (bv-org-slipbox--index-populated-p)
              (setq bv-org-slipbox--initial-index-ready t)
            (let ((file-count (length files)))
              (setq bv-org-slipbox--initial-sync-running t)
              (message "org-slipbox: indexing %d existing note%s..."
                       file-count
                       (if (= file-count 1) "" "s"))
              (unwind-protect
                  (let* ((response (org-slipbox-sync))
                         (files-indexed (or (plist-get response :files_indexed) 0))
                         (nodes-indexed (or (plist-get response :nodes_indexed) 0)))
                    (setq bv-org-slipbox--initial-index-ready
                          (or (> files-indexed 0)
                              (> nodes-indexed 0)))
                    (if bv-org-slipbox--initial-index-ready
                        (message "org-slipbox: initial index ready (%d files, %d nodes)"
                                 files-indexed
                                 nodes-indexed)
                      (display-warning
                       'bv-org-slipbox
                       (format
                        "Initial org-slipbox sync completed but indexed 0 files and 0 nodes under %s"
                        org-slipbox-directory)
                       :warning)))
                (setq bv-org-slipbox--initial-sync-running nil))))
        (error
         (if (ignore-errors (bv-org-slipbox--index-populated-p))
             (setq bv-org-slipbox--initial-index-ready t)
           (display-warning
            'bv-org-slipbox
            (format "Initial org-slipbox sync failed: %s"
                    (error-message-string err))
            :warning)))))))

(defun bv-org-slipbox--with-initial-index (fn &rest args)
  "Ensure the slipbox index exists before calling FN with ARGS."
  (bv-org-slipbox--ensure-initial-index)
  (apply fn args))

(defun bv-org-slipbox--template-prefix-length (template)
  "Return the non-plist prefix length for capture TEMPLATE."
  (if (memq (nth 2 template) bv-org-slipbox--capture-types)
      4
    2))

(defun bv-org-slipbox--template-with-immediate-finish (template)
  "Return TEMPLATE rewritten to finalize without opening a draft."
  (let* ((template (copy-tree template))
         (prefix-length (bv-org-slipbox--template-prefix-length template))
         (prefix (seq-take template prefix-length))
         (options (nthcdr prefix-length template)))
    (append prefix (plist-put options :immediate-finish t))))

(defun bv-org-slipbox--node-icon (node)
  "Return a nerd icon for NODE based on tags."
  (let ((tags (bv-org-slipbox--sequence (plist-get node :tags))))
    (if (and (fboundp 'nerd-icons-octicon)
             (bv-completion-icons-enabled-p))
        (pcase-let* ((`(,_ . (,icon . ,face))
                      (seq-find (lambda (entry)
                                  (member (car entry) tags))
                                bv-org-slipbox--tag-icon-alist
                                '(_ . ("nf-oct-note" . bv-icon-note)))))
          (nerd-icons-octicon icon :face face))
      "•")))

(defun bv-org-slipbox--column (text width face)
  "Return TEXT padded or truncated to WIDTH using FACE."
  (bv-completion-format-field text width face))

(defun bv-org-slipbox--completion-width ()
  "Return the active completion window width."
  (bv-completion-window-width))

(defun bv-org-slipbox--pad-display (text width)
  "Return TEXT truncated or padded to WIDTH, preserving text properties."
  (bv-completion-pad text width))

(defun bv-org-slipbox--node-metadata-width ()
  "Return the width reserved for node completion metadata."
  (if bv-org-slipbox-show-backlinks 20 12))

(defun bv-org-slipbox--node-main-width ()
  "Return the width to use for the flexible node column."
  (max 24 (- (bv-org-slipbox--completion-width)
             4
             (bv-org-slipbox--node-metadata-width))))

(defun bv-org-slipbox--node-modtime (node)
  "Return NODE modification time formatted for the node table."
  (let ((mtime-ns (plist-get node :file_mtime_ns)))
    (when (and (integerp mtime-ns) (> mtime-ns 0))
      (format-time-string
       "%b %d %H:%M"
       (seconds-to-time (/ (float mtime-ns) 1000000000.0))))))

(defun bv-org-slipbox--node-tags (node)
  "Return NODE tags joined for the completion table."
  (let ((tags (bv-org-slipbox--sequence (plist-get node :tags))))
    (when tags
      (propertize
       (string-join
        (cl-remove-duplicates
         (mapcar (lambda (tag) (concat "#" tag)) tags)
         :test #'equal)
        " ")
       'face 'org-tag))))

(defun bv-org-slipbox--node-backlinks (node)
  "Return NODE backlink count formatted for the completion table."
  (when-let ((count (plist-get node :backlink_count)))
    (format "<- %s" count)))

(defun bv-org-slipbox--node-main-column (node)
  "Return the flexible completion column for NODE."
  (let* ((title (or (plist-get node :title) ""))
         (tags (bv-org-slipbox--node-tags node))
         (text (if tags
                   (concat title "  " tags)
                 title)))
    (bv-org-slipbox--pad-display text (bv-org-slipbox--node-main-width))))

(defun bv-org-slipbox-node-display (node)
  "Return the main completion display string for NODE."
  (let ((icon (bv-org-slipbox--node-icon node)))
    (concat
     icon
     " "
     (bv-org-slipbox--node-main-column node))))

(defun bv-org-slipbox-node-annotation (node)
  "Return the annotation string for NODE completions."
  (let* ((modtime (or (bv-org-slipbox--node-modtime node) ""))
         (backlinks (and bv-org-slipbox-show-backlinks
                         (bv-org-slipbox--node-backlinks node)))
         (metadata
          (string-join
           (delq nil
                 (list (bv-org-slipbox--column modtime 12 'marginalia-date)
                       (when backlinks
                         (bv-org-slipbox--column backlinks 6
                                                 'marginalia-number))))
           "  ")))
    (bv-completion-format-annotation metadata)))

(defun bv-org-slipbox-node-insert-immediate (&optional initial-input filter-fn)
  "Insert a slipbox node immediately.
INITIAL-INPUT seeds the minibuffer. FILTER-FN filters indexed nodes."
  (interactive)
  (org-slipbox-node-insert-immediate initial-input filter-fn))

(defun bv-org-slipbox-capture-slip (&optional title)
  "Capture a new slipbox note with TITLE."
  (interactive)
  (if title
      (org-slipbox-capture title)
    (call-interactively #'org-slipbox-capture)))

(defun bv-org-slipbox-quick-capture (&optional title)
  "Capture a new slipbox note and finalize it immediately."
  (interactive)
  (let ((org-slipbox-capture-templates
         (mapcar #'bv-org-slipbox--template-with-immediate-finish
                 org-slipbox-capture-templates)))
    (bv-org-slipbox-capture-slip title)))

(defun bv-org-slipbox-find-by-tag (tag &optional other-window)
  "Select a node limited to TAG.
With OTHER-WINDOW, visit the node in another window."
  (interactive
   (progn
     (bv-org-slipbox--ensure-initial-index)
     (list (completing-read "Tag: " #'org-slipbox-tag-completions nil t)
           current-prefix-arg)))
  (org-slipbox-node-find
   nil
   (lambda (node)
     (member tag (bv-org-slipbox--sequence (plist-get node :tags))))
   other-window))

(defun bv-org-slipbox--consult-non-slipbox-buffer-items ()
  "Return ordinary `consult-buffer' candidates as proper buffer pairs.
`consult-org-slipbox' separates slipbox buffers into its own source; the
default buffer source should still preserve Consult's native
(NAME . BUFFER) shape."
  (consult--buffer-query
   :sort 'visibility
   :as #'consult--buffer-pair
   :predicate (lambda (buffer)
                (not (consult-org-slipbox--note-buffer-p buffer)))))

(setq org-slipbox-directory (expand-file-name bv-org-slipbox-directory)
      org-slipbox-database-file
      (expand-file-name "emacs/org-slipbox.sqlite"
                        (bv-org-slipbox--cache-home))
      org-slipbox-completion-everywhere nil
      org-slipbox-link-auto-replace t
      org-slipbox-node-display-template #'bv-org-slipbox-node-display
      org-slipbox-node-annotation-function #'bv-org-slipbox-node-annotation
      org-slipbox-buffer-sections
      (list #'org-slipbox-buffer-node-section
            #'org-slipbox-buffer-refs-section
            #'org-slipbox-buffer-backlinks-section
            #'org-slipbox-buffer-reflinks-section)
      org-slipbox-buffer-expensive-sections 'dedicated
      org-slipbox-capture-templates
      '(("s" "slip" plain "%?"
         :target (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS:\n\n")
         :unnarrowed t)))

(bv-org-slipbox--setup-directory)

(add-to-list 'display-buffer-alist
             '("\\*org-slipbox\\(?:[:].*\\)?\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(org-slipbox-mode 1)

(dolist (command '(org-slipbox-node-find
                   org-slipbox-node-insert
                   org-slipbox-node-insert-immediate
                   org-slipbox-node-random
                   org-slipbox-node-backlinks
                   org-slipbox-node-forward-links))
  (advice-add command :around #'bv-org-slipbox--with-initial-index))

(with-eval-after-load 'consult
  (require 'consult-org-slipbox)
  (setq consult-org-slipbox-buffer-narrow-key ?r
        consult-org-slipbox-buffer-after-buffers t)
  (dolist (command '(consult-org-slipbox-file-find
                     consult-org-slipbox-ref-find
                     consult-org-slipbox-search
                     consult-org-slipbox-backlinks
                     consult-org-slipbox-forward-links))
    (advice-add command :around #'bv-org-slipbox--with-initial-index))
  (consult-org-slipbox-mode 1)
  (when (fboundp 'consult--customize-put)
    (consult--customize-put
     '(consult-source-buffer)
     :items
     '(function bv-org-slipbox--consult-non-slipbox-buffer-items))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n i") #'org-slipbox-node-insert))

(global-set-key (kbd "C-c n") bv-org-slipbox-map)

(define-key bv-org-slipbox-map (kbd "I") #'bv-org-slipbox-node-insert-immediate)
(define-key bv-org-slipbox-map (kbd "c") #'bv-org-slipbox-capture-slip)
(define-key bv-org-slipbox-map (kbd "C") #'bv-org-slipbox-quick-capture)
(define-key bv-org-slipbox-map (kbd "f") #'org-slipbox-node-find)
(define-key bv-org-slipbox-map (kbd "g") #'bv-org-slipbox-find-by-tag)
(define-key bv-org-slipbox-map (kbd "i") #'org-slipbox-node-insert)
(define-key bv-org-slipbox-map (kbd "r") #'org-slipbox-node-random)
(define-key bv-org-slipbox-map (kbd "R") #'org-slipbox-ref-add)
(define-key bv-org-slipbox-map (kbd "s") #'bv-org-slipbox-capture-slip)
(define-key bv-org-slipbox-map (kbd "b") #'org-slipbox-buffer-toggle)
(define-key bv-org-slipbox-map (kbd "/") #'consult-org-slipbox-search)
(define-key bv-org-slipbox-map (kbd "l") #'consult-org-slipbox-backlinks)
(define-key bv-org-slipbox-map (kbd "L") #'consult-org-slipbox-forward-links)
(define-key bv-org-slipbox-map (kbd "F") #'consult-org-slipbox-file-find)

(provide 'bv-org-slipbox)
;;; bv-org-slipbox.el ends here
