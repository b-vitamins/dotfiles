;;; bv-research.el --- Research and knowledge management -*- lexical-binding: t -*-

;;; Commentary:
;; Research environment with Zettelkasten (org-roam), bibliography management (citar), 
;; and PDF annotation workflow.

;;; Code:

(require 'bv-core)
(require 'bv-org)

;;;; Custom Variables
(defgroup bv-research nil
  "Research and knowledge management configuration."
  :group 'bv)

(defcustom bv-research-directory "~/research"
  "Root directory for research files."
  :type 'directory
  :group 'bv-research)

(defcustom bv-research-roam-directory "~/research/roam"
  "Directory for org-roam notes."
  :type 'directory
  :group 'bv-research)

(defcustom bv-research-bibliography-directory "~/research/bibliography"
  "Directory for bibliography files and PDFs."
  :type 'directory
  :group 'bv-research)

(defcustom bv-research-global-bibliography
  '("~/research/bibliography/references.bib")
  "List of bibliography files."
  :type '(repeat file)
  :group 'bv-research)

(defcustom bv-research-library-paths
  '("~/research/bibliography/pdfs")
  "Paths to PDF libraries."
  :type '(repeat directory)
  :group 'bv-research)

(defcustom bv-research-notes-paths
  '("~/research/roam/references")
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

;;;; Org Roam - Zettelkasten
(use-package org-roam
  :custom
  (org-roam-directory bv-research-roam-directory)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   (when bv-research-capture-templates
     '(("d" "default" plain "%?"
        :target (file+head "${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags:\n\n")
        :unnarrowed t)
       ("r" "reference" plain "%?"
        :target (file+head "references/${citar-citekey}.org"
                          "#+title: ${citar-title}\n#+date: %U\n#+filetags: :reference:\n\n* Notes\n")
        :unnarrowed t)
       ("p" "project" plain "%?"
        :target (file+head "projects/${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n\n* Overview\n\n* Tasks\n")
        :unnarrowed t)
       ("c" "concept" plain "%?"
        :target (file+head "concepts/${slug}.org"
                          "#+title: ${title}\n#+date: %U\n#+filetags: :concept:\n\n* Definition\n\n* Examples\n")
        :unnarrowed t))))
  :bind (("C-c n n" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n R" . bv-research-org-roam-ref-add)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-goto-today)
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
        (expand-file-name "org-roam.db" bv-cache-dir))
  
  ;; Node display
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node)
                               org-roam-directory))))
      (error "")))
  
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:80} "
                (propertize "${tags:20}" 'face 'org-tag)))
  
  (org-roam-db-autosync-enable)
  
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
  (citar-symbols
   `((file . (,(all-the-icons-faicon "file-o" :v-adjust -0.1) . " "))
     (note . (,(all-the-icons-material "speaker_notes" :v-adjust -0.3) . " "))
     (link . (,(all-the-icons-octicon "link" :v-adjust 0.01) . " "))))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map
        ("C-c b" . org-cite-insert)
        :map global-map
        ("C-c n b" . bv-research-find-bibliography))
  :config
  (setq org-cite-export-processors
        '((latex biblatex)
          (t csl)))
  
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
  (citar-org-roam-mode 1))

;;;; Embark Integration
(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode 1))

;;;; Org Cite Configuration
(use-package oc
  :after org
  :config
  (require 'oc-biblatex)
  (require 'oc-csl)
  
  (setq bibtex-dialect bv-research-bibtex-dialect)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-autokey-year-title-separator "-")
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-titleword-separator "-")
  (setq bibtex-autokey-titlewords 3))

;;;; Zotra - Zotero Integration
(use-package zotra
  :defer t
  :bind (("C-c z a" . zotra-add-entry)
         ("C-c z s" . zotra-add-entry-from-search)
         ("C-c z u" . zotra-add-entry-from-url))
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
  (pdf-tools-install)
  
  (defun bv-research-open-pdf (entry)
    "Open PDF for bibliography ENTRY."
    (interactive)
    (when-let ((file (citar-get-files entry)))
      (find-file (car file)))))

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
    (let* ((entry (citar-select-ref))
           (files (citar-get-files entry))
           (notes (citar-get-notes entry)))
      (when files
        (if notes
            (progn
              (find-file (car notes))
              (org-noter))
          (message "Create a note first with citar-open-notes")))))

;;;; Org Ref (Alternative - disabled by default)
(use-package org-ref
  :after org
  :disabled t
  :custom
  (org-ref-bibliography-notes
   (expand-file-name "notes.org" bv-research-bibliography-directory))
  (org-ref-default-bibliography bv-research-global-bibliography)
  (org-ref-pdf-directory (car bv-research-library-paths)))

;;;; Research Workflow Commands
(defun bv-research-new-project (name)
  "Create a new research project with NAME."
  (interactive "sProject name: ")
  (let ((slug (org-roam-node--slug name)))
    (org-roam-capture- :keys "p"
                       :node (org-roam-node-create :title name)
                       :props '(:finalize find-file))))

(defun bv-research-literature-review ()
  "Start a literature review session."
  (interactive)
  (let ((topic (read-string "Review topic: ")))
    (org-roam-node-find nil topic)
    (split-window-right)
    (other-window 1)
    (citar-open)))

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
    
    (add-to-list 'tempel-template-sources 'bv-research-tempel-templates)))

;;;; Keybindings
(with-eval-after-load 'bv-core
  (define-prefix-command 'bv-research-map)
  (define-key bv-app-map "r" 'bv-research-map)
  
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
      (setq org-agenda-files
            (delete-dups
             (append org-agenda-files
                     (org-roam-list-files)))))
    
    (advice-add 'org-agenda :before 'bv-research-org-roam-update-todo-files)))

(provide 'bv-research)
;;; bv-research.el ends here
