;;; bv-research.el --- Research and knowledge management -*- lexical-binding: t -*-

;;; Commentary:
;; Research environment with Zettelkasten (org-roam), bibliography management (citar),
;; and PDF annotation workflow.

;;; Code:

(require 'bv-core)
(require 'bv-org)

;;;; External variable declarations
(defvar bv-app-map)
(defvar bv-cache-directory)
(defvar org-roam-dailies-directory)
(defvar org-roam-dailies-capture-templates)
(defvar tempel-template-sources)

;;;; External function declarations
;; org-roam functions
(declare-function org-roam-db-autosync-mode "org-roam" (&optional arg))
(declare-function org-roam-db-query "org-roam-db" (sql &rest args))
(declare-function org-roam-node-read "org-roam-node" (&optional initial-input filter-fn sort-fn require-match prompt))
(declare-function org-roam-property-add "org-roam" (property value))
(declare-function org-roam-capture- "org-roam-capture" (&rest props))
(declare-function org-roam-file-p "org-roam" (&optional file))
(declare-function org-roam-list-files "org-roam" ())

;; citar functions
(declare-function citar-indicator-create "citar" (&rest args))
(declare-function citar-has-files "citar" (citekey))
(declare-function citar-has-links "citar" (citekey))
(declare-function citar-has-notes "citar" (citekey))
(declare-function citar-get-files "citar" (citekey))
(declare-function citar-get-notes "citar" (citekey))
(declare-function citar-select-ref "citar" (&optional multiple filter))
(declare-function citar-open "citar" (citekey))
(declare-function citar-org-roam-mode "citar-org-roam" (&optional arg))
(declare-function citar-embark-mode "citar-embark" (&optional arg))

;; all-the-icons functions
(declare-function all-the-icons-faicon "all-the-icons" (icon &rest args))
(declare-function all-the-icons-octicon "all-the-icons" (icon &rest args))
(declare-function all-the-icons-material "all-the-icons" (icon &rest args))

;; other functions
(declare-function pdf-tools-install "pdf-tools" (&optional no-query-p skip-dependencies-p no-error-p force-dependencies-p))
(declare-function marginalia--time "marginalia" (time))

;; Forward declarations for functions defined later
(declare-function bv-research-org-roam-slug "bv-research" (title))
(declare-function bv-research-org-roam-todo-p "bv-research" ())

;;;; Custom Variables
(defgroup bv-research nil
  "Research and knowledge management configuration."
  :group 'bv)

(defcustom bv-research-directory "~/documents/slipbox"
  "Root directory for research files."
  :type 'directory
  :group 'bv-research)

(defcustom bv-research-roam-directory "~/documents/slipbox/notes"
  "Directory for org-roam notes."
  :type 'directory
  :group 'bv-research)

(defcustom bv-research-bibliography-directory "~/documents/references/bibliographies"
  "Directory for bibliography files and PDFs."
  :type 'directory
  :group 'bv-research)

(defcustom bv-research-global-bibliography
  '("~/documents/references/bibliographies/working.bib")
  "List of bibliography files."
  :type '(repeat file)
  :group 'bv-research)

(defcustom bv-research-library-paths
  '("~/documents/papers")
  "Paths to PDF libraries."
  :type '(repeat directory)
  :group 'bv-research)

(defcustom bv-research-notes-paths
  '("~/documents/slipbox")
  "Paths for literature notes."
  :type '(repeat directory)
  :group 'bv-research)

(defcustom bv-research-capture-templates t
  "Use default capture templates for research."
  :type 'boolean
  :group 'bv-research)

(defcustom bv-research-org-roam-todo t
  "Enable org-roam based task management."
  :type 'boolean
  :group 'bv-research)

(defcustom bv-research-bibtex-dialect 'biblatex
  "BibTeX dialect to use."
  :type '(choice (const bibtex) (const biblatex))
  :group 'bv-research)

(defcustom bv-research-cache-dir
  (expand-file-name "research" (or (bound-and-true-p bv-cache-directory)
                                    (expand-file-name "bv-cache" user-emacs-directory)))
  "Cache directory for research data."
  :type 'directory
  :group 'bv-research)

;;;; Org Roam - Zettelkasten
(use-package org-roam
  :custom
  (org-roam-directory bv-research-roam-directory)
  (org-roam-completion-everywhere t)
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-extra-links-elements '(keyword node-property))
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t)
                            org-roam-reflinks-section))
  (org-roam-link-title-format "R:%s")
  (org-roam-tag-sources '(all-directories))
  (org-roam-tag-sort t)
  (org-roam-tag-context-lines 5)
  (org-roam-capture-templates
   (when bv-research-capture-templates
     '(("d" "default" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: %<%Y-%m-%d-%H-%M-%S> ${title}\n")
        :unnarrowed t)

       ("f" "fleeting" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :fleeting:\n\n")
        :unnarrowed t)

       ("F" "fleeting-timed" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :fleeting:\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+END\n\n")
        :unnarrowed t)

       ("c" "concept" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :concept:\n\n* Definition\n\n* Examples\n")
        :unnarrowed t)

       ("C" "concept-timed" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :concept:\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+END\n\n* Definition\n\n* Examples\n")
        :unnarrowed t)

       ("l" "literature" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :literature:\n\n* Summary\n\n* Key Points\n\n* Questions\n\n* Relevance\n")
        :unnarrowed t)

       ("L" "literature-timed" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :literature:\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+END\n\n* Summary\n\n* Key Points\n\n* Questions\n\n* Relevance\n")
        :unnarrowed t)

       ("p" "problem" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :problem:\n#+TEXTBOOK: \n\n* Problem Statement\n\n* Solution\n")
        :unnarrowed t)

       ("P" "problem-timed" plain "%?"
        :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :problem:\n#+TEXTBOOK: \n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+END\n\n* Problem Statement\n\n* Solution\n")
        :unnarrowed t)

       ("r" "reference" plain "%?"
        :target (file+head "references/${citar-citekey}.org"
                          "#+title: ${citar-title}\n#+date: %U\n#+filetags: :reference:\n\n* Notes\n")
        :unnarrowed t)

       ("j" "project" plain "%?"
        :target (file+head "projects/${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n\n* Overview\n\n* Tasks\n")
        :unnarrowed t))))
  :bind (("C-c n n" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n R" . bv-research-org-roam-ref-add)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-tag-remove)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n A" . org-roam-alias-remove)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  ;; Create directories
  (dolist (dir '("" "references" "projects" "concepts" "daily"))
    (let ((path (expand-file-name dir org-roam-directory)))
      (unless (file-directory-p path)
        (make-directory path t))))

  (setq org-roam-db-location
        (expand-file-name "org-roam.db" bv-research-cache-dir))

  ;; Custom slug generation
  (defun bv-research-org-roam-slug (title-or-node)
    "Generate a slug from TITLE-OR-NODE, replacing underscores with hyphens."
    (let* ((title (if (stringp title-or-node)
                      title-or-node
                    ;; If it's a node object, extract the title
                    (org-roam-node-title title-or-node)))
           (slug-trim-chars '(768 769 770 771 772 774 775 776 777 778 779 780
                                  795 803 804 805 807 813 814 816 817)))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                   (string-glyph-compose
                    (apply #'string
                           (seq-remove #'nonspacing-mark-p
                                       (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                   (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs '(("[^[:alnum:][:digit:]]" . "-")
                        ("--*" . "-")
                        ("^-" . "")
                        ("-$" . "")))
               (slug (-reduce-from #'cl-replace
                                   (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

  ;; Override slug generation
  (advice-add 'org-roam-node-slug :override #'bv-research-org-roam-slug)

  ;; Node display customization
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node)
                               org-roam-directory))))
      (error "")))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    "Return the uppercase directory of the NODE's file."
    (if-let ((dirs (file-name-directory
                    (file-relative-name (org-roam-node-file node)
                                        org-roam-directory))))
        (format "%s" (upcase (car (split-string dirs "/"))))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    "Return the count of backlinks to the NODE as a string."
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (setq org-roam-node-display-template
        (concat (propertize "${title:50} " 'face 'italic)
                (propertize "${directories:10} " 'face 'italic)
                (propertize "${tags:40} " 'face 'org-tag)
                (propertize "${backlinkscount:10} " 'face 'italic)))

  ;; Enable database autosync
  (with-eval-after-load 'org-roam
    (org-roam-db-autosync-mode))

  ;; Configure org-roam-dailies
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+date: %U\n\n"))

          ("j" "journal" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+date: %U\n\n* Journal\n"))

          ("t" "task" entry
           "* TODO %?"
           :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n#+date: %U\n\n* Tasks\n"))))

  (defun bv-research-org-roam-ref-add (ref node)
    "Add REF to NODE."
    (interactive
     (list
      (read-string "Ref: ")
      (org-roam-node-read)))
    (if-let ((file (org-roam-node-file node)))
        (with-current-buffer (or (find-buffer-visiting file)
                                 (find-file-noselect file))
          (org-roam-property-add "ROAM_REFS" ref)
          (save-buffer))
      (org-roam-capture-
       :keys "r"
       :node node
       :info `(:ref ,ref)
       :templates org-roam-capture-templates
       :props '(:finalize find-file))))

  ;; TODO management
  (when bv-research-org-roam-todo
    (defun bv-research-org-roam-todo-p ()
      "Return non-nil if the current buffer has any to-do entry."
      (org-element-map
          (org-element-parse-buffer 'headline)
          'headline
        (lambda (h)
          (eq (org-element-property :todo-type h) 'todo))
        nil 'first-match))

    (defun bv-research-org-roam-update-todo-tag ()
      "Update the todo tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (fboundp 'org-roam-file-p)
                 (org-roam-file-p))
        (org-with-point-at 1
          (let* ((tags (org-get-tags))
                 (is-todo (bv-research-org-roam-todo-p)))
            (cond ((and is-todo (not (member "todo" tags)))
                   (org-roam-tag-add '("todo")))
                  ((and (not is-todo) (member "todo" tags))
                   (org-roam-tag-remove '("todo"))))))))

    (add-hook 'org-roam-find-file-hook 'bv-research-org-roam-update-todo-tag)
    (add-hook 'before-save-hook 'bv-research-org-roam-update-todo-tag)))

;;;; Citar - Bibliography Management
(use-package citar
  :custom
  (org-cite-global-bibliography bv-research-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths bv-research-library-paths)
  (citar-notes-paths bv-research-notes-paths)
  (citar-file-extensions '("pdf" "org" "md"))
  (citar-templates
   '((main . "${author editor:40}    ${date year issued:4}   ${title:120}")
     (suffix . "${=type=:12} ${tags keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor}, ${title}")))
  (citar-at-point-function 'embark-act)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (("C-c ]" . org-cite-insert)
   ("C-c C-o C-i" . citar-insert-citation)
   ("C-c C-o C-e" . citar-insert-edit)
   ("C-c C-o C-f" . citar-open)
   ("C-c C-o C-o" . citar-open-files)
   ("C-c C-o C-n" . citar-open-notes)
   ("C-c C-o C-b" . citar-open-entry)
   ("C-c C-o C-d" . citar-org-delete-citation)
   ("C-c C-o C-x" . citar-export-local-bib-file)
   ("C-c n b" . bv-research-find-bibliography))
  :config
  (setq org-cite-export-processors
        '((latex biblatex)
          (t csl)))

  ;; Custom indicators with all-the-icons
  (with-eval-after-load 'all-the-icons
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (all-the-icons-faicon
                "file-o"
                :face 'all-the-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  "
       :tag "has:files"))

    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (all-the-icons-octicon
                "link"
                :face 'all-the-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))

    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (all-the-icons-material
                "speaker_notes"
                :face 'all-the-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "  "
       :tag "has:notes"))

    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons)))

  (unless (file-directory-p bv-research-bibliography-directory)
    (make-directory bv-research-bibliography-directory t))

  (defun bv-research-find-bibliography ()
    "Open main bibliography file."
    (interactive)
    (find-file (car bv-research-global-bibliography))))

;;;; Citar Org Roam - Integration
(use-package citar-org-roam
  :after (citar org-roam)
  :custom
  (citar-org-roam-note-title-template "${author} - ${title}")
  (citar-org-roam-subdir "references")
  :config
  (with-eval-after-load 'citar
    (citar-org-roam-mode 1)))

;;;; Embark Integration
(use-package citar-embark
  :after (citar embark)
  :config
  (with-eval-after-load 'citar
    (citar-embark-mode 1)))

;;;; Org Cite Configuration
(use-package oc
  :after org
  :config
  (require 'oc-biblatex)
  (require 'oc-csl)

  ;; Declare bibtex variables before setting them
  (defvar bibtex-dialect)
  (defvar bibtex-align-at-equal-sign)
  (defvar bibtex-autokey-year-title-separator)
  (defvar bibtex-autokey-year-length)
  (defvar bibtex-autokey-titleword-separator)
  (defvar bibtex-autokey-titlewords)

  (setq bibtex-dialect bv-research-bibtex-dialect
        bibtex-align-at-equal-sign t
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-year-length 4
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 3))

;;;; Zotra - Zotero Integration
(use-package zotra
  :defer t
  :bind (("C-c z a" . zotra-add-entry))
  :custom
  (zotra-backend 'translation-server)
  (zotra-url-retrieve-timeout 10)
  (zotra-default-entry-format (symbol-name bv-research-bibtex-dialect))
  (zotra-default-bibliography (car bv-research-global-bibliography))
  :config
  (unless (executable-find "translation-server")
    (message "Warning: translation-server not found. \
Install from https://github.com/zotero/translation-server")))

;;;; PDF Tools Integration
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)

  (defun bv-research-open-pdf (entry)
    "Open PDF for bibliography ENTRY."
    (interactive)
    (when-let ((files (and (fboundp 'citar-get-files)
                           (citar-get-files entry)))
               (file (car files)))
      (find-file file))))

;;;; Org Noter - PDF Annotations
(use-package org-noter
  :after (:any org pdf-tools)
  :bind (:map org-mode-map
              ("C-c N" . org-noter))
  :custom
  (org-noter-notes-search-path bv-research-notes-paths)
  (org-noter-always-create-frame nil)
  (org-noter-auto-save-last-location t)
  (org-noter-default-notes-file-names '("notes.org"))
  (org-noter-doc-split-fraction '(0.6 . 0.4))
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-notes-window-location 'horizontal-split)
  :config
  (defun bv-research-noter-from-citar ()
    "Open org-noter session from citar selection."
    (interactive)
    (when (fboundp 'citar-select-ref)
      (let* ((entry (citar-select-ref))
             (files (and (fboundp 'citar-get-files)
                         (citar-get-files entry)))
             (notes (and (fboundp 'citar-get-notes)
                         (citar-get-notes entry))))
        (when files
          (if notes
              (progn
                (find-file (car notes))
                (org-noter))
            (message "Create a note first with citar-open-notes")))))))

;;;; Arxiv Mode
(use-package arxiv-mode
  :defer t
  :custom
  (arxiv-default-category "cond-mat.dis-nn")
  (arxiv-entries-per-fetch "25")
  (arxiv-default-download-folder (car bv-research-library-paths))
  (arxiv-default-bibliography (car bv-research-global-bibliography))
  (arxiv-pdf-open-function
   (lambda (fpath)
     (if (executable-find "evince")
         (call-process "evince" nil 0 nil fpath)
       (find-file fpath))))
  :bind (("C-c C-a s" . arxiv-search)
         ("C-c C-a r" . arxiv-read)))

;;;; Research Workflow Commands
(defun bv-research-new-project (name)
  "Create a new research project with NAME."
  (interactive "sProject name: ")
  (org-roam-capture- :keys "j"
                     :node (org-roam-node-create :title name)
                     :props '(:finalize find-file)))

(defun bv-research-literature-review ()
  "Start a literature review session."
  (interactive)
  (let ((topic (read-string "Review topic: ")))
    (org-roam-node-find nil topic)
    (split-window-right)
    (other-window 1)
    (when (fboundp 'citar-open)
      (call-interactively #'citar-open))))

(defun bv-research-export-bibliography ()
  "Export bibliography to various formats."
  (interactive)
  (let ((format (completing-read "Export format: "
                                 '("html" "markdown" "plain"))))
    (cond
     ((string= format "html")
      (shell-command
       (format "pandoc %s -o bibliography.html --citeproc"
               (car bv-research-global-bibliography))))
     ((string= format "markdown")
      (shell-command
       (format "bibtex2md %s > bibliography.md"
               (car bv-research-global-bibliography))))
     ((string= format "plain")
      (shell-command
       (format "bibtex2txt %s > bibliography.txt"
               (car bv-research-global-bibliography)))))))

;;;; Templates
(bv-with-feature tempel
  (with-eval-after-load 'tempel
    (defvar bv-research-tempel-templates
      '(org-mode
        ;; Citations
        (cite "[[cite:@" p "]]")
        (citet "[[citet:@" p "]]")
        (citep "[[citep:@" p "]]")
        (citeauthor "[[citeauthor:@" p "]]")
        (citeyear "[[citeyear:@" p "]]")

        ;; Research notes
        (paper "* " p " [[cite:@" p "]]\n:PROPERTIES:\n:NOTER_DOCUMENT: \n:END:\n** Summary\n** Key Points\n** Questions\n** Relevance")
        (hypothesis "* Hypothesis: " p "\n** Background\n** Prediction\n** Method\n** Evidence")
        (litreview "* Literature Review: " p "\n** Scope\n** Search Strategy\n** Key Papers\n** Synthesis\n** Gaps")
        (researchq "* RQ: " p "\n** Context\n** Question\n** Importance\n** Approach"))
      "Research-specific templates.")

    (defvar tempel-template-sources nil "Template sources for tempel.")
    (add-to-list 'tempel-template-sources 'bv-research-tempel-templates)))

;;;; Keybindings
(with-eval-after-load 'bv-core
  ;; Ensure bv-app-map exists
  (unless (boundp 'bv-app-map)
    (defvar bv-app-map (make-sparse-keymap)
      "Application keymap for bv."))

  ;; Create research map
  (defvar bv-research-map (make-sparse-keymap)
    "Keymap for research commands.")

  (define-key bv-app-map "r" bv-research-map)

  (define-key bv-research-map "p" 'bv-research-new-project)
  (define-key bv-research-map "l" 'bv-research-literature-review)
  (define-key bv-research-map "n" 'bv-research-noter-from-citar)
  (define-key bv-research-map "e" 'bv-research-export-bibliography)
  (define-key bv-research-map "b" 'citar-open)
  (define-key bv-research-map "f" 'citar-open-files)
  (define-key bv-research-map "N" 'citar-open-notes))

;;;; Feature Definition
(defun bv-research-load ()
  "Load research configuration."
  (add-to-list 'bv-enabled-features 'research)

  ;; Create research directories
  (dolist (dir (list bv-research-directory
                     bv-research-roam-directory
                     bv-research-bibliography-directory
                     bv-research-cache-dir
                     (car bv-research-library-paths)
                     (car bv-research-notes-paths)))
    (unless (file-directory-p dir)
      (make-directory dir t)))

  ;; Create initial bibliography file
  (let ((bib-file (car bv-research-global-bibliography)))
    (unless (file-exists-p bib-file)
      (with-temp-file bib-file
        (insert "% Bibliography file\n")
        (insert "% Created by bv-research\n\n"))))

  ;; Update org-agenda-files for roam-todo
  (when bv-research-org-roam-todo
    (defun bv-research-org-roam-update-todo-files (&rest _)
      "Update the value of `org-agenda-files'."
      (when (fboundp 'org-roam-list-files)
        (setq org-agenda-files
              (delete-dups
               (append org-agenda-files
                       (org-roam-list-files))))))

    (advice-add 'org-agenda :before 'bv-research-org-roam-update-todo-files)))

;;;; Node annotation with marginalia
(with-eval-after-load 'marginalia
  (setq org-roam-node-annotation-function
        (lambda (node)
          (marginalia--time (org-roam-node-file-mtime node)))))

(provide 'bv-research)
;;; bv-research.el ends here
