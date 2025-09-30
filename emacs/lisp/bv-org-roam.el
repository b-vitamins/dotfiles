;;; bv-org-roam.el --- Slip box implementation using Org-roam -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (consult "0.20") (consult-org-roam "0.1"))

;;; Commentary:

;; A slip box implementation using Org-roam following the principles from
;; "Communicating with Slip Boxes" by Niklas Luhmann.
;;
;; Usage Guide:
;;
;; This module implements a pure Zettelkasten system with manual numbering,
;; fixed addressing, and emphasis on emergent connections between slips.
;;
;; Key Bindings:
;;
;; All commands are under the `C-c n' prefix:
;;
;;   C-c n f   - Find or create a slip
;;   C-c n c   - Create new slip
;;   C-c n i   - Insert link to slip at point
;;   C-c n I   - Insert link without opening the slip
;;   C-c n b   - Toggle backlinks buffer
;;   C-c n /   - Full-text search across all slips
;;   C-c n l   - View backlinks with context (consult)
;;   C-c n L   - View forward links with context (consult)
;;   C-c n F   - Find slip files (consult)
;;
;; Workflow:
;;
;; 1. Creating Slips:
;;    - `C-c n c' - Create new slip
;;    - `C-c n f' - Find or create slip
;;    - Tags can be added manually in the #+FILETAGS: header
;;
;; 2. Linking Slips:
;;    - Use `C-c n i' while writing to insert links
;;    - Links create the conversation between slips
;;    - Backlinks appear automatically in the side buffer
;;
;; 3. Finding Slips:
;;    - `C-c n f' for title-based search
;;    - `C-c n /' for full-text search
;;    - `C-c n l' for backlinks
;;    - `C-c n L' for forward links
;;
;; 4. Building Threads:
;;    - Use links to create connections between slips
;;    - Backlinks automatically show related content
;;    - Navigate with `C-c n l' and `C-c n L'
;;
;; Display Features:
;;
;; - Slip titles show with academic icons based on tags
;; - Tags are displayed with 20 character width
;; - Modification times shown in marginalia style
;; - Backlink count visible during selection
;;
;; Configuration:
;;
;; Slips are stored in `bv-org-roam-directory' (default: ~/documents/slipbox/slips)
;; Common tags are defined in `bv-org-roam-common-tags'
;; Toggle backlink count display with `bv-org-roam-show-backlinks'

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-roam)
(eval-when-compile (require 'eieio))

(autoload 'oref "eieio")
(cl-defstruct emacsql-connection handle)

(declare-function org-roam-node-file "org-roam-node")
(declare-function org-roam-node-title "org-roam-node")
(declare-function org-roam-node-tags "org-roam-node")
(declare-function org-roam-node-id "org-roam-node")
(declare-function org-roam-node-aliases "org-roam-node")
(declare-function org-roam-node-file-mtime "org-roam-node")
(declare-function org-roam-node-list "org-roam")
(declare-function org-roam-node-visit "org-roam")
(declare-function org-roam-node-at-point "org-roam")
(declare-function org-roam-node-read "org-roam")
(declare-function org-roam-node-find "org-roam")
(declare-function org-roam-node-insert "org-roam")
(declare-function org-roam-node-create "org-roam")
(declare-function org-roam-db-query "org-roam-db")
(declare-function org-roam-capture- "org-roam-capture")
(declare-function org-roam-ref-add "org-roam")
(declare-function marginalia--time "marginalia")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function org-roam-db-autosync-enable "org-roam-db")
(declare-function consult-ripgrep "consult")
(declare-function consult-org-roam-mode "consult-org-roam" (&optional arg))

(defvar org-roam-v2-ack)
(defvar org-roam-directory)
(defvar org-roam-completion-everywhere)
(defvar org-roam-db-gc-threshold)
(defvar org-roam-db-location)
(defvar org-roam-db-node-include-function)
(defvar org-roam-node-display-template)
(defvar org-roam-capture-templates)
(defvar org-roam-mode-sections)
(defvar org-roam-capture--info)
(defvar consult-org-roam-grep-func)
(defvar consult-org-roam-buffer-narrow-key)
(defvar consult-org-roam-buffer-after-buffers)

(defgroup bv-org-roam nil
  "Slip box configuration for Org-roam."
  :group 'org-roam
  :prefix "bv-org-roam-")

(defcustom bv-org-roam-directory "~/projects/slipbox/slips"
  "Directory for org-roam slip box files."
  :type 'directory
  :group 'bv-org-roam)

(defcustom bv-org-roam-show-backlinks t
  "Whether to show backlink count in completions."
  :type 'boolean
  :group 'bv-org-roam)

(defvar bv-org-roam-common-tags
  '("physics" "mathematics" "computation" "quantum" "statistics"
    "deeplearn" "algorithm" "theorem" "proof" "concept"
    "problem" "solution" "idea" "hypothesis" "experiment"
    "review" "summary" "index" "reference" "question")
  "Common tags for org-roam slips.")

(defvar bv-org-roam-map (make-sparse-keymap)
  "Keymap for bv-org-roam commands.")

(defun bv-org-roam-slug-from-title (title)
  "Convert TITLE to a slug using hyphens instead of underscores."
  (let ((slug (downcase title)))
    (setq slug (replace-regexp-in-string "[^[:alnum:][:digit:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-" "" slug))
    (setq slug (replace-regexp-in-string "-$" "" slug))
    slug))

(defun bv-patch-emacsql-close (connection &rest _args)
  "Prevent `emacsql-close' errors for CONNECTION.
Ignore ARGS."
  (when (ignore-errors (oref connection handle))
    t))

(with-eval-after-load 'emacsql
  (advice-add 'emacsql-close :before-while #'bv-patch-emacsql-close))

(defun bv-org-roam-setup-directory ()
  "Ensure org-roam directory exists."
  (unless (file-exists-p bv-org-roam-directory)
    (make-directory bv-org-roam-directory t)))

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-icon ((node org-roam-node))
    "Return nerd icon for NODE based on tags."
    (if (fboundp 'nerd-icons-mdicon)
        (let ((tags (org-roam-node-tags node)))
          (cond
           ((member "physics" tags) (nerd-icons-mdicon "nf-md-atom" :face 'nerd-icons-blue))
           ((member "problem" tags) (nerd-icons-mdicon "nf-md-puzzle" :face 'nerd-icons-orange))
           ((member "concept" tags) (nerd-icons-mdicon "nf-md-lightbulb_on" :face 'nerd-icons-yellow))
           ((member "algorithm" tags) (nerd-icons-codicon "nf-cod-symbol_method" :face 'nerd-icons-purple))
           ((member "theorem" tags) (nerd-icons-mdicon "nf-md-sigma" :face 'nerd-icons-green))
           ((member "idea" tags) (nerd-icons-mdicon "nf-md-creation" :face 'nerd-icons-lblue))
           ((member "deeplearn" tags) (nerd-icons-mdicon "nf-md-brain" :face 'nerd-icons-pink))
           ((member "report" tags) (nerd-icons-mdicon "nf-md-file_document" :face 'nerd-icons-dsilver))
           ((member "review" tags) (nerd-icons-mdicon "nf-md-text_search" :face 'nerd-icons-cyan))
           ((member "index" tags) (nerd-icons-mdicon "nf-md-format_list_numbered" :face 'nerd-icons-silver))
           (t (nerd-icons-mdicon "nf-md-note_text_outline" :face 'nerd-icons-dsilver))))
      "•"))

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

  (cl-defmethod org-roam-node-modtime ((node org-roam-node))
    "Return formatted modification time for NODE in marginalia style."
    (let ((mtime (org-roam-node-file-mtime node)))
      (if (and mtime (fboundp 'marginalia--time))
          (marginalia--time mtime)
        ""))))

(setq org-roam-v2-ack t)

(setq org-roam-directory bv-org-roam-directory
      org-roam-completion-everywhere nil
      org-roam-db-gc-threshold most-positive-fixnum
      org-roam-db-location
      (expand-file-name "org-roam.db"
                        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs")))

(setq org-roam-db-node-include-function
      (lambda ()
        (not (org-entry-get (point) "ROAM_EXCLUDE"))))

(setq org-roam-node-display-template
      (concat (propertize "${icon}" 'face 'nerd-icons-blue)
              " "
              (propertize "${title:28}" 'face 'font-lock-function-name-face)
              " "
              (propertize "${tags:20}" 'face 'org-tag)
              " "
              (propertize "${modtime:12}" 'face 'marginalia-date)))

(bv-org-roam-setup-directory)

(setq org-roam-capture-templates
      '(("s" "slip" plain "%?"
         :target (file+head "%<%Y-%m-%d-%H%M%S>-%(bv-org-roam-slug-from-title \"${title}\").org"
                            "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS:\n\n")
         :unnarrowed t)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n i") 'org-roam-node-insert))

(org-roam-db-autosync-enable)

(setq org-roam-mode-sections
      '(org-roam-backlinks-section
        org-roam-reflinks-section))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(defun bv-org-roam-node-read (&optional initial-input filter-fn sort-fn require-match)
  "Read and return an org-roam node.
INITIAL-INPUT is the initial minibuffer input.
FILTER-FN filters nodes, SORT-FN sorts them.
REQUIRE-MATCH determines if existing node is required."
  (let ((templates (list (cons "" org-roam-capture-templates))))
    (org-roam-node-read initial-input filter-fn sort-fn require-match templates)))

(defun bv-org-roam-node-insert-immediate (arg &rest args)
  "Insert node link without opening the note.
With prefix ARG, allows for selection of capture template.
ARGS are passed to `org-roam-node-insert'."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates
         (list (append (car org-roam-capture-templates)
                       '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(with-eval-after-load 'consult
  (autoload 'consult-org-roam-mode "consult-org-roam" nil t)
  (autoload 'consult-org-roam-search "consult-org-roam" nil t)
  (autoload 'consult-org-roam-backlinks "consult-org-roam" nil t)
  (autoload 'consult-org-roam-forward-links "consult-org-roam" nil t)
  (autoload 'consult-org-roam-file-find "consult-org-roam" nil t)

  (with-eval-after-load 'consult-org-roam
    (setq consult-org-roam-grep-func #'consult-ripgrep
          consult-org-roam-buffer-narrow-key ?r
          consult-org-roam-buffer-after-buffers t)
    (consult-org-roam-mode 1)))

(global-set-key (kbd "C-c n") bv-org-roam-map)

(define-key bv-org-roam-map (kbd "I") 'bv-org-roam-node-insert-immediate)
(define-key bv-org-roam-map (kbd "c") 'org-roam-capture)
(define-key bv-org-roam-map (kbd "f") 'org-roam-node-find)
(define-key bv-org-roam-map (kbd "i") 'org-roam-node-insert)
(define-key bv-org-roam-map (kbd "b") 'org-roam-buffer-toggle)
(define-key bv-org-roam-map (kbd "/") 'consult-org-roam-search)
(define-key bv-org-roam-map (kbd "l") 'consult-org-roam-backlinks)
(define-key bv-org-roam-map (kbd "L") 'consult-org-roam-forward-links)
(define-key bv-org-roam-map (kbd "F") 'consult-org-roam-file-find)

(provide 'bv-org-roam)
;;; bv-org-roam.el ends here
