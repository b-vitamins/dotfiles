;;; bv-elfeed.el --- Advanced RSS/Atom feed reader with academic paper management -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 2.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;
;; Advanced Elfeed configuration for academic paper reading, especially suited for
;; ML/Physics research. Features automatic ArXiv paper downloading, intelligent
;; scoring, and bibliography management.
;;
;; INSTALLATION:
;;   Add to your init.el: (require 'bv-elfeed)
;;
;; DIRECTORY STRUCTURE (created automatically):
;;   ~/documents/slipbox/bibliography/  - BibTeX files
;;   ~/documents/papers/                - Downloaded PDFs
;;   ~/.config/emacs/elfeed.score       - Scoring rules (user-created)
;;   ~/.config/emacs/elfeed.stat        - Scoring statistics (auto-generated)
;;   ~/.local/share/emacs/elfeed/       - Main database directory:
;;     ~/.local/share/emacs/elfeed/db/  - Feed entries database
;;     ~/.local/share/emacs/elfeed/score.el - Score cache/state
;;   ~/.cache/emacs/elfeed/             - Temporary cache:
;;     ~/.cache/emacs/elfeed/enclosures/ - Downloaded enclosures
;;
;; TO COMPLETELY RESET ELFEED:
;;   rm -rf ~/.local/share/emacs/elfeed/
;;   rm -f ~/.config/emacs/elfeed.stat
;;   # Keep ~/.config/emacs/elfeed.score (your scoring rules)
;;
;; ESSENTIAL KEYBINDINGS:
;;   C-x w    - Open Elfeed
;;   g        - Update all feeds
;;   d        - Download ArXiv paper (creates PDF + BibTeX entry)
;;   m        - Toggle star
;;   s        - Show starred entries
;;   S        - Set custom filter
;;   t        - Today's entries
;;   w        - Last week's entries
;;   W        - Last week's entries unfiltered (all papers)
;;   h        - High-scoring entries (score>50)
;;   H        - Very high-scoring entries (score>100)
;;   x        - ArXiv entries only
;;   =        - Elfeed score menu
;;   ?        - Show statistics
;;   R        - Rescore all entries (if scores show 0)
;;   q        - Quit
;;
;; FILTERS:
;;   +score>200 @1-week-ago     - High-scoring recent papers
;;   +star +downloaded          - Starred papers you've downloaded
;;   +transformer @1-day-ago    - Today's transformer papers
;;   +theory -medical           - Theory papers, exclude medical
;;   +arxiv +stat-mech          - Statistical mechanics papers
;;
;; ARXIV INTEGRATION:
;;   Press 'd' on any ArXiv entry to:
;;   1. Download PDF to ~/documents/papers/
;;   2. Create BibTeX entry in ~/documents/slipbox/bibliography/working.bib
;;   3. Tag entry as 'downloaded'
;;
;; SCORING:
;;   Papers are automatically scored based on:
;;   - Title/abstract keywords (statistical physics, ML terms)
;;   - Author reputation
;;   - Scores displayed in rightmost column
;;   - High scores (>200) highlighted
;;
;; AUTO-TAGGING:
;;   Entries automatically tagged based on content:
;;   - stat-mech, ml-theory, transformers, diffusion, etc.
;;   - theory vs applied
;;   - Conference detection (neurips, icml, etc.)
;;
;; MAINTENANCE:
;;   C        - Clean old entries (keeps starred/downloaded)
;;   ?        - Database statistics
;;
;; CUSTOMIZATION:
;;   Edit ~/.config/emacs/elfeed.score to adjust scoring rules
;;   Modify feed list in the bv-elfeed-feeds variable below

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-score)
(require 'org-ref)
(require 'bibtex-completion)

(defgroup bv-elfeed nil
  "Advanced RSS feed reader settings."
  :group 'applications)

;;; XDG Base Directory paths
(defconst bv-xdg-config-home
  (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config"))
  "XDG config directory.")

(defconst bv-xdg-cache-home
  (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache"))
  "XDG cache directory.")

(defconst bv-xdg-data-home
  (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share"))
  "XDG data directory.")

;;; Constants for bibliography management
(defconst bv-bib-libraries
  (list "~/documents/slipbox/bibliography/working.bib"
        "~/documents/slipbox/bibliography/tmp.bib")
  "List of bibliography databases.")

(defconst bv-main-bib-library (nth 0 bv-bib-libraries)
  "Main bibliography database.")

(defconst bv-pdfs-library-paths
  '("~/documents/papers/" "~/documents/papers/tmp/")
  "PDF storage directories.")

(defconst bv-main-pdfs-path (nth 0 bv-pdfs-library-paths)
  "Main PDF storage directory.")

;;; Custom variables
(defcustom bv-elfeed-score-file
  (expand-file-name "emacs/elfeed.score" bv-xdg-config-home)
  "Location of elfeed-score rules file."
  :type 'file
  :group 'bv-elfeed)

(defcustom bv-elfeed-default-filter "@1-week-ago +unread"
  "Default search filter for elfeed."
  :type 'string
  :group 'bv-elfeed)

;;; Face definitions are now in bv-themes.el for theme-aware colors

;;; Feed list embedded directly
(defvar bv-elfeed-feeds
  '(;; Core categories with reasonable volume - using rss endpoint for abstracts
    ("http://export.arxiv.org/rss/cond-mat.dis-nn?version=2.0"
     disorder neural arxiv)

    ("http://export.arxiv.org/rss/cs.LG?version=2.0"
     ml learning arxiv)

    ("http://export.arxiv.org/rss/cs.AI?version=2.0"
     ai artificial-intelligence arxiv)

    ("http://export.arxiv.org/rss/stat.ML?version=2.0"
     ml statistics arxiv))
  "List of RSS/Atom feeds for elfeed.")

;;; Core Elfeed configuration with XDG paths
(setq elfeed-db-directory (expand-file-name "emacs/elfeed/db" bv-xdg-data-home)
      elfeed-enclosure-default-dir (expand-file-name "emacs/elfeed/enclosures" bv-xdg-cache-home)
      elfeed-score-score-file (expand-file-name "emacs/elfeed/score.el" bv-xdg-data-home)
      elfeed-search-filter bv-elfeed-default-filter
      elfeed-feeds bv-elfeed-feeds
      elfeed-show-entry-switch 'pop-to-buffer
      elfeed-search-title-max-width 100
      elfeed-search-title-min-width 30
      elfeed-search-date-format '("%y-%m-%d" 10 :left)
      elfeed-search-trailing-width 10
      elfeed-show-truncate-long-urls t
      elfeed-show-unique-buffers t
      elfeed-use-curl t
      elfeed-curl-max-connections 10
      elfeed-curl-timeout 30
      elfeed-log-level 'warn
      elfeed-search-remain-on-entry t)

;;; Bibliography setup
(setq bibtex-completion-bibliography bv-bib-libraries
      bibtex-completion-library-path bv-pdfs-library-paths
      bibtex-completion-pdf-field "file"
      bibtex-completion-pdf-open-function #'find-file
      org-ref-pdf-directory bv-main-pdfs-path
      bibtex-dialect 'biblatex)

;;; Enhanced author display function
(defun bv-elfeed-concatenate-authors (authors-list)
  "Given AUTHORS-LIST, return formatted string of authors."
  (cond
   ((null authors-list) "")
   ((= 1 (length authors-list))
    (plist-get (car authors-list) :name))
   ((= 2 (length authors-list))
    (format "%s & %s"
            (plist-get (nth 0 authors-list) :name)
            (plist-get (nth 1 authors-list) :name)))
   (t (format "%s et al." (plist-get (car authors-list) :name)))))

;;; Custom search print function with scores
(defun bv-elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer with enhanced formatting."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (entry-authors (bv-elfeed-concatenate-authors
                        (elfeed-meta entry :authors)))
         (entry-score (when (fboundp 'elfeed-score-scoring-get-score-from-entry)
                       (elfeed-score-scoring-get-score-from-entry entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed
                      (or (elfeed-meta feed :title)
                          (elfeed-feed-title feed))))
         (title-width (- (window-width) 75))
         (title-column (elfeed-format-column
                       title (min title-width 100) :left))
         (authors-column (elfeed-format-column
                         entry-authors 40 :left))
         (score-column (if entry-score
                          (elfeed-format-column
                           (format "%4d" entry-score) 5 :right)
                        "     ")))

    ;; Colorize based on score
    (when (and entry-score (> entry-score 200))
      (add-face-text-property 0 (length title-column)
                             'bv-elfeed-high-score-face nil title-column))

    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (insert (propertize authors-column 'face 'bv-elfeed-author-face
                       'kbd-help entry-authors) " ")
    (insert (propertize score-column 'face
                       (if (and entry-score (> entry-score 100))
                           'bv-elfeed-important-face
                         'elfeed-search-filter-face)) " ")
    (when feed-title
      (insert (propertize (elfeed-format-column feed-title 15 :left)
                         'face 'elfeed-search-feed-face)))))

;;; ArXiv paper fetching integration
(defun bv-elfeed-entry-to-arxiv ()
  "Fetch arXiv paper, create BibTeX entry, and add to reading list."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single)))
         (link (elfeed-entry-link entry))
         (title (elfeed-entry-title entry))
         (match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" link))
         (arxiv-id (when match-idx (match-string 1 link))))

    (if arxiv-id
        (if (require 'org-ref nil t)
            (if (fboundp 'arxiv-get-pdf-add-bibtex-entry)
                (progn
                  (message "Fetching arXiv paper %s..." arxiv-id)
                  ;; Ensure directories exist
                  (make-directory bv-main-pdfs-path t)
                  (make-directory (file-name-directory bv-main-bib-library) t)

                  ;; Fetch paper and create BibTeX entry
                  (condition-case err
                      (progn
                        (arxiv-get-pdf-add-bibtex-entry
                         arxiv-id bv-main-bib-library bv-main-pdfs-path)

                        ;; Update BibTeX with file field
                        (bv-elfeed-update-bibtex-file-field arxiv-id)

                        ;; Mark as downloaded
                        (elfeed-tag entry 'downloaded)
                        (elfeed-search-update-entry entry)

                        (message "Successfully downloaded arXiv paper %s" arxiv-id))
                    (error
                     (message "Error downloading arXiv paper %s: %s"
                              arxiv-id (error-message-string err)))))
              (message "org-ref arxiv functions not available"))
          (message "org-ref package not found. Install emacs-org-ref via Guix"))
      (message "Not an arXiv entry"))))

(defun bv-elfeed-update-bibtex-file-field (arxiv-id)
  "Update the BibTeX entry for ARXIV-ID with file field."
  (save-window-excursion
    (find-file bv-main-bib-library)
    (goto-char (point-max))
    (when (search-backward arxiv-id nil t)
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry))
             (key (cdr (assoc "=key=" entry))))
        (when key
          (let ((pdf-path (expand-file-name
                          (concat key ".pdf")
                          bv-main-pdfs-path)))
            (when (file-exists-p pdf-path)
              (bibtex-set-field "file" pdf-path)
              (save-buffer))))))))

(defun bv-elfeed-clean-bibtex-entry ()
  "Clean and format the current BibTeX entry."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((start (point)))
      (bibtex-end-of-entry)
      (let ((end (point)))
        ;; Clean whitespace
        (whitespace-cleanup-region start end)
        ;; Align fields
        (bibtex-fill-entry)
        ;; Validate entry
        (bibtex-validate)))))

(defun bv-elfeed-format-bibtex-file (&optional file)
  "Format and clean up BibTeX FILE (default: main bibliography)."
  (interactive)
  (let ((bib-file (or file bv-main-bib-library)))
    (when (file-exists-p bib-file)
      (save-window-excursion
        (find-file bib-file)
        (goto-char (point-min))
        ;; Format each entry
        (while (bibtex-skip-to-valid-entry)
          (condition-case nil
              (bv-elfeed-clean-bibtex-entry)
            (error nil))
          (bibtex-end-of-entry))
        ;; Sort entries by key
        (bibtex-sort-buffer)
        (save-buffer)
        (message "Formatted bibliography file: %s" bib-file)))))

;;; Elfeed Score setup
(defun bv-elfeed-score-setup ()
  "Setup elfeed-score with configuration file."
  (unless (file-exists-p bv-elfeed-score-file)
    (message "Creating default elfeed score file at %s" bv-elfeed-score-file)
    (make-directory (file-name-directory bv-elfeed-score-file) t))
  (when (file-exists-p bv-elfeed-score-file)
    (elfeed-score-load-score-file bv-elfeed-score-file))
  ;; Enable elfeed-score globally
  (setq elfeed-score-score-default 0)
  (elfeed-score-enable))

;;; Enhanced search functions
(defun bv-elfeed-search-mode-setup ()
  "Setup elfeed search mode."
  (setq elfeed-search-print-entry-function #'bv-elfeed-search-print-entry)
  ;; Auto-update on entry
  (elfeed-update))

(defun bv-elfeed-search-starred ()
  "Show starred entries."
  (interactive)
  (elfeed-search-set-filter "@6-months-ago +star"))

(defun bv-elfeed-search-all ()
  "Show all entries."
  (interactive)
  (elfeed-search-set-filter "@1-month-ago"))

(defun bv-elfeed-search-today ()
  "Show today's entries."
  (interactive)
  (elfeed-search-set-filter "@1-day-ago"))

(defun bv-elfeed-search-week ()
  "Show last week's entries."
  (interactive)
  (elfeed-search-set-filter "@1-week-ago"))

(defun bv-elfeed-search-unfiltered ()
  "Show all unread entries from the last week without score filtering."
  (interactive)
  (elfeed-search-set-filter "@1-week-ago +unread"))

(defun bv-elfeed-search-arxiv ()
  "Show academic/arxiv entries."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +arxiv"))

(defun bv-elfeed-search-high-score ()
  "Show only high-scoring entries (score > 50)."
  (interactive)
  (elfeed-search-set-filter "@1-week-ago +unread +score>50"))

(defun bv-elfeed-search-very-high-score ()
  "Show only very high-scoring entries (score > 100)."
  (interactive)
  (elfeed-search-set-filter "@2-weeks-ago +unread +score>100"))

(defun bv-elfeed-mark-all-read ()
  "Mark all visible entries as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun bv-elfeed-toggle-star ()
  "Toggle starred status of entry at point."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (if (elfeed-tagged-p 'star entry)
          (elfeed-untag entry 'star)
        (elfeed-tag entry 'star)))
    (mapc #'elfeed-search-update-entry entries)))

;;; Auto-tagging for physics/ML papers
(defun bv-elfeed-physics-ml-autotag (entry)
  "Auto-tag physics and ML papers based on content."
  (let ((title (downcase (or (elfeed-entry-title entry) "")))
        (link (elfeed-entry-link entry))
        (categories (elfeed-meta entry :categories)))

    ;; ArXiv papers
    (when (string-match-p "arxiv\\.org" link)
      (elfeed-tag entry 'arxiv))

    ;; Statistical Physics / Complex Systems
    (cond
     ((string-match-p "\\(spin.glass\\|replica\\|mean.field\\|ising\\|potts\\)" title)
      (elfeed-tag entry 'spin-glass))
     ((string-match-p "\\(phase.transition\\|critical\\|universality\\|scaling\\)" title)
      (elfeed-tag entry 'phase-transition))
     ((string-match-p "\\(hopfield\\|associative.memory\\|attractor\\)" title)
      (elfeed-tag entry 'hopfield))
     ((string-match-p "\\(energy.based\\|boltzmann\\|rbm\\|contrastive\\)" title)
      (elfeed-tag entry 'energy-based))
     ((string-match-p "\\(combinatorial\\|tsp\\|satisfiability\\|np.hard\\)" title)
      (elfeed-tag entry 'combinatorial)))

    ;; Modern ML
    (cond
     ((string-match-p "\\(transformer\\|attention\\|bert\\|gpt\\)" title)
      (elfeed-tag entry 'transformers))
     ((string-match-p "\\(diffusion\\|score.based\\|ddpm\\|denoising\\)" title)
      (elfeed-tag entry 'diffusion))
     ((string-match-p "\\(state.space\\|mamba\\|s4\\|ssm\\)" title)
      (elfeed-tag entry 'ssm))
     ((string-match-p "\\(mixture.of.experts\\|moe\\|sparse\\)" title)
      (elfeed-tag entry 'moe))
     ((string-match-p "\\(graph.neural\\|gnn\\|gcn\\|message.passing\\)" title)
      (elfeed-tag entry 'gnn)))

    ;; Theory markers
    (when (string-match-p "\\(theorem\\|proof\\|lemma\\|convergence\\|complexity\\)" title)
      (elfeed-tag entry 'theory))

    ;; Optimization
    (when (string-match-p "\\(optimization\\|convex\\|gradient\\|sgd\\|adam\\)" title)
      (elfeed-tag entry 'optimization))

    ;; Tag based on ArXiv categories
    (when categories
      (dolist (cat categories)
        (cond
         ((string-match-p "cond-mat\\.dis-nn" cat)
          (elfeed-tag entry 'disorder))
         ((string-match-p "cs\\.LG" cat)
          (elfeed-tag entry 'ml-theory))
         ((string-match-p "stat\\.ML" cat)
          (elfeed-tag entry 'ml-theory))
         ((string-match-p "cs\\.AI" cat)
          (elfeed-tag entry 'optimization)))))))

;;; Database maintenance
(defun bv-elfeed-db-cleanup ()
  "Clean up old entries from database."
  (interactive)
  (let ((cutoff-time (- (float-time) (* 90 86400))) ; 90 days ago
        (removed 0))
    (dolist (entry (elfeed-db-get-all-entries))
      (when (and (< (elfeed-entry-date entry) cutoff-time)
                 (not (or (elfeed-tagged-p 'star entry)
                         (elfeed-tagged-p 'downloaded entry))))
        ;; Mark as read and remove from unread
        (elfeed-untag entry 'unread)
        (cl-incf removed)))
    (elfeed-db-save)
    (when (fboundp 'elfeed-db-compact)
      (elfeed-db-compact))
    (message "Cleaned %d old entries (marked as read)" removed)))

(defun bv-elfeed-remove-feed-entries (feed-url-substring)
  "Remove all entries from feeds matching FEED-URL-SUBSTRING."
  (interactive "sRemove entries from feed containing: ")
  (let ((removed 0))
    (with-elfeed-db-visit (entry feed)
      (let ((feed-url (when feed (elfeed-feed-url feed))))
        (when (and feed-url (string-match-p feed-url-substring feed-url))
          (elfeed-untag entry 'unread)
          (cl-incf removed))))
    (elfeed-db-save)
    (elfeed-search-update :force)
    (message "Removed %d entries from feeds matching '%s'" removed feed-url-substring)))

(defun bv-elfeed-statistics ()
  "Show statistics about feeds and entries."
  (interactive)
  (let ((total-entries 0)
        (unread-entries 0)
        (starred-entries 0)
        (downloaded-entries 0)
        (feeds-count (hash-table-count elfeed-db-feeds)))

    (with-elfeed-db-visit (entry feed)
      (cl-incf total-entries)
      (when (elfeed-tagged-p 'unread entry)
        (cl-incf unread-entries))
      (when (elfeed-tagged-p 'star entry)
        (cl-incf starred-entries))
      (when (elfeed-tagged-p 'downloaded entry)
        (cl-incf downloaded-entries)))

    (message "Feeds: %d | Total: %d | Unread: %d | Starred: %d | Downloaded: %d"
             feeds-count total-entries unread-entries
             starred-entries downloaded-entries)))

(defun bv-elfeed-show-entry-score ()
  "Show the score of the current entry in the minibuffer."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single)))
         (score (when (fboundp 'elfeed-score-scoring-get-score-from-entry)
                 (elfeed-score-scoring-get-score-from-entry entry)))
         (title (elfeed-entry-title entry)))
    (if score
        (message "Score: %d | %s" score
                (truncate-string-to-width title 60 nil nil "..."))
      (message "No score available for this entry"))))

(defun bv-elfeed-rescore-all-entries ()
  "Rescore all entries in the database."
  (interactive)
  (if (not (featurep 'elfeed-score))
      (message "elfeed-score not loaded")
    (let ((count 0))
      (with-elfeed-db-visit (entry feed)
        (when (fboundp 'elfeed-score-scoring-score-entry)
          (elfeed-score-scoring-score-entry entry)
          (cl-incf count)))
      (elfeed-db-save)
      (elfeed-search-update :force)
      (message "Rescored %d entries" count))))


;;; Initialize elfeed
(defun bv-elfeed-init ()
  "Initialize elfeed with all customizations."
  ;; Create XDG directories
  (make-directory elfeed-db-directory t)
  (make-directory elfeed-enclosure-default-dir t)
  (make-directory (file-name-directory bv-main-bib-library) t)
  (make-directory bv-main-pdfs-path t)

  ;; Setup elfeed-score
  (bv-elfeed-score-setup)

  ;; Load database
  (elfeed-db-load)

  ;; Apply faces
  (push '(star bv-elfeed-star-face) elfeed-search-face-alist)
  (push '(important bv-elfeed-important-face) elfeed-search-face-alist)
  (push '(priority bv-elfeed-high-score-face) elfeed-search-face-alist)
  (push '(arxiv bv-elfeed-arxiv-face) elfeed-search-face-alist)
  (push '(downloaded elfeed-search-date-face) elfeed-search-face-alist)

  ;; Add hooks
  (add-hook 'elfeed-new-entry-hook #'bv-elfeed-physics-ml-autotag)
  (add-hook 'elfeed-new-entry-hook #'elfeed-score-scoring-score-entry)
  (add-hook 'elfeed-search-mode-hook #'bv-elfeed-search-mode-setup)

  ;; Auto-remove old entries
  (when (fboundp 'elfeed-make-tagger)
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "3 months ago"
                                  :remove 'unread))))

;; Run initialization
(bv-elfeed-init)

;;; Keybindings
(define-key elfeed-search-mode-map (kbd "RET") 'elfeed-search-show-entry)
(define-key elfeed-search-mode-map "q" 'quit-window)
(define-key elfeed-search-mode-map "Q" 'bv-elfeed-save-and-quit)
(define-key elfeed-search-mode-map "s" 'bv-elfeed-search-starred)
(define-key elfeed-search-mode-map "S" 'elfeed-search-set-filter)
(define-key elfeed-search-mode-map "c" 'elfeed-search-clear-filter)
(define-key elfeed-search-mode-map "a" 'bv-elfeed-search-all)
(define-key elfeed-search-mode-map "t" 'bv-elfeed-search-today)
(define-key elfeed-search-mode-map "w" 'bv-elfeed-search-week)
(define-key elfeed-search-mode-map "W" 'bv-elfeed-search-unfiltered)
(define-key elfeed-search-mode-map "x" 'bv-elfeed-search-arxiv)
(define-key elfeed-search-mode-map "h" 'bv-elfeed-search-high-score)
(define-key elfeed-search-mode-map "H" 'bv-elfeed-search-very-high-score)
(define-key elfeed-search-mode-map "m" 'bv-elfeed-toggle-star)
(define-key elfeed-search-mode-map "d" 'bv-elfeed-entry-to-arxiv)
(define-key elfeed-search-mode-map "M" 'bv-elfeed-mark-all-read)
(define-key elfeed-search-mode-map "u" 'elfeed-search-tag-all-unread)
(define-key elfeed-search-mode-map "g" 'elfeed-update)
(define-key elfeed-search-mode-map "G" 'elfeed-search-fetch)
(define-key elfeed-search-mode-map "C" 'bv-elfeed-db-cleanup)
(define-key elfeed-search-mode-map "?" 'bv-elfeed-statistics)
(define-key elfeed-search-mode-map "=" 'elfeed-score-map)
(define-key elfeed-search-mode-map "R" 'bv-elfeed-rescore-all-entries)

(define-key elfeed-show-mode-map "q" 'quit-window)
(define-key elfeed-show-mode-map "=" 'bv-elfeed-show-entry-score)
(define-key elfeed-show-mode-map "m" 'bv-elfeed-toggle-star)
(define-key elfeed-show-mode-map "d" 'bv-elfeed-entry-to-arxiv)
(define-key elfeed-show-mode-map "n" 'elfeed-show-next)
(define-key elfeed-show-mode-map "p" 'elfeed-show-prev)
(define-key elfeed-show-mode-map "b" 'elfeed-show-visit)

(defun bv-elfeed-save-and-quit ()
  "Save database and quit."
  (interactive)
  (elfeed-db-save)
  (quit-window))

;; Global keybinding
(global-set-key (kbd "C-x w") 'elfeed)

;; Save on exit
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (fboundp 'elfeed-db-save)
              (elfeed-db-save))
            (when (fboundp 'elfeed-db-compact)
              (elfeed-db-compact))))

;; Auto-update every 4 hours
(run-with-idle-timer (* 4 60 60) t 'elfeed-update)

(provide 'bv-elfeed)
;;; bv-elfeed.el ends here
