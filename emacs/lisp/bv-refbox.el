;;; bv-refbox.el --- Refbox bibliography configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Refbox-backed citation UI and bibliography index configuration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'bibtex)
(require 'refbox)
(require 'refbox-org)
(require 'refbox-latex)
(require 'refbox-markdown)
(require 'refbox-embark)
(require 'oc-csl nil t)

(autoload 'embark-act "embark" nil t)
(autoload 'nerd-icons-faicon "nerd-icons")
(autoload 'nerd-icons-mdicon "nerd-icons")
(autoload 'nerd-icons-octicon "nerd-icons")

(defvar completion-category-overrides)
(defvar org-cite-global-bibliography)
(defvar savehist-additional-variables)
(defvar bv-org-cite-bibliography-roots)
(defvar bv-org-cite-bibliography-exclude-paths)
(defvar refbox-org-slipbox-capture-template-key)
(defvar refbox-org-slipbox-note-title-template)
(defvar refbox-org-slipbox-preload-limit)
(defvar vertico-multiform-categories)
(defvar vertico-multiform-commands)

(defgroup bv-refbox nil
  "Refbox configuration for the BV Emacs setup."
  :group 'applications
  :prefix "bv-refbox-")

(defcustom bv-refbox-bibliography-roots '("~/projects/bibliography")
  "Bibliography root directories indexed by Refbox."
  :type '(repeat directory)
  :group 'bv-refbox)

(defcustom bv-refbox-bibliography-exclude-paths
  '("~/projects/bibliography/collections/_archive")
  "Bibliography file or directory paths excluded from Refbox."
  :type '(repeat (choice file directory))
  :group 'bv-refbox)

(defcustom bv-refbox-library-paths
  '("~/documents" "~/documents/papers" "~/documents/papers/tmp")
  "Directories searched for files associated with references."
  :type '(repeat directory)
  :group 'bv-refbox)

(defcustom bv-refbox-csl-styles-dir "~/documents/styles"
  "Directory containing local CSL style files.
When this directory does not exist, Refbox and Org fall back to Org's bundled
CSL directory."
  :type '(choice (const :tag "Use Org fallback" nil) directory)
  :group 'bv-refbox)

(defcustom bv-refbox-csl-locales-dir nil
  "Directory containing CSL locale files.
When nil, Refbox uses the same fallback CSL directory Org uses."
  :type '(choice (const :tag "Use Org fallback" nil) directory)
  :group 'bv-refbox)

(defcustom bv-refbox-csl-style "chicago-author-date"
  "Default CSL style used for formatted reference insertion."
  :type '(choice (const :tag "Prompt when formatting" nil) string)
  :group 'bv-refbox)

(defcustom bv-refbox-csl-locale "en-US"
  "Default CSL locale used for formatted reference insertion."
  :type 'string
  :group 'bv-refbox)

(defcustom bv-refbox-completion-limit 12
  "Maximum number of Refbox candidates shown in minibuffer completion."
  :type 'natnum
  :group 'bv-refbox)

(defun bv-refbox--cache-home ()
  "Return the XDG cache directory."
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name "~/.cache")))

(defun bv-refbox--cache-file (file)
  "Return FILE under the Emacs cache directory, creating its parent."
  (let* ((path (expand-file-name file (bv-refbox--cache-home)))
         (parent (file-name-directory path)))
    (make-directory parent t)
    path))

(defun bv-refbox--expanded-directories (directories)
  "Return existing or intended DIRECTORIES as expanded path strings."
  (mapcar (lambda (directory)
            (file-name-as-directory (expand-file-name directory)))
          directories))

(defun bv-refbox--expanded-exclude-paths (paths)
  "Return expanded file or directory PATHS used by Refbox excludes."
  (mapcar (lambda (path)
            (let ((expanded (expand-file-name path)))
              (if (or (file-directory-p expanded)
                      (string-suffix-p "/" path))
                  (file-name-as-directory expanded)
                expanded)))
          paths))

(defun bv-refbox--existing-directory (directory)
  "Return expanded DIRECTORY when it exists, otherwise nil."
  (when (and directory (not (string-empty-p directory)))
    (let ((expanded (file-name-as-directory (expand-file-name directory))))
      (when (file-directory-p expanded)
        expanded))))

(defun bv-refbox--templates ()
  "Return Refbox templates for the BV completion surface."
  '((main . "${author editor:30%sn}  ${date year issued:4}  ${title:56}")
    (suffix . "  ${=key= id:18}  ${=type=:14}  ${tags keywords:36}")
    (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
    (note . "Notes on ${author editor:%etal}, ${title}")))

(defun bv-refbox--indicator-symbol
    (function icon fallback face &optional v-adjust)
  "Return an indicator glyph from FUNCTION, or FALLBACK.
ICON is passed to FUNCTION and FACE colors the resulting glyph."
  (if (fboundp function)
      (condition-case nil
          (funcall function icon :face face :v-adjust (or v-adjust -0.1))
        (error (propertize fallback 'face face)))
    (propertize fallback 'face face)))

(defun bv-refbox--indicators ()
  "Return BV-styled Refbox resource indicators."
  (list
   (refbox-indicator-create
    :symbol (bv-refbox--indicator-symbol #'nerd-icons-faicon
                                         "nf-fa-file"
                                         "F" 'nerd-icons-green)
    :function #'refbox-has-files
    :padding "  "
    :width 3
    :tag "has:files")
   (refbox-indicator-create
    :symbol (bv-refbox--indicator-symbol #'nerd-icons-mdicon
                                         "nf-md-notebook"
                                         "N" 'nerd-icons-blue -0.3)
    :function #'refbox-has-notes
    :padding "  "
    :width 3
    :tag "has:notes")
   (refbox-indicator-create
    :symbol (bv-refbox--indicator-symbol #'nerd-icons-octicon
                                         "nf-oct-link"
                                         "L" 'nerd-icons-orange)
    :function #'refbox-has-links
    :padding "  "
    :width 3
    :tag "has:links")
   (refbox-indicator-create
    :symbol (bv-refbox--indicator-symbol #'nerd-icons-octicon
                                         "nf-oct-check_circle"
                                         "C" 'bv-icon-success)
    :function #'refbox-is-cited
    :padding "  "
    :width 3
    :tag "is:cited")))

(defun bv-refbox--bibliography-roots ()
  "Return the bibliography roots shared by Org-cite and Refbox."
  (bv-refbox--expanded-directories
   (if (boundp 'bv-org-cite-bibliography-roots)
       bv-org-cite-bibliography-roots
     bv-refbox-bibliography-roots)))

(defun bv-refbox--bibliography-exclude-paths ()
  "Return bibliography exclude paths shared by Org-cite and Refbox."
  (delete-dups
   (bv-refbox--expanded-exclude-paths
    (append bv-refbox-bibliography-exclude-paths
            (when (boundp 'bv-org-cite-bibliography-exclude-paths)
              bv-org-cite-bibliography-exclude-paths)))))

(defun bv-refbox-at-point ()
  "Run contextual citation actions at point."
  (interactive)
  (if (require 'embark nil t)
      (call-interactively #'embark-act)
    (call-interactively #'refbox-dwim)))

(defun bv-refbox--format-reference-function ()
  "Return the preferred Refbox reference formatter."
  (if (require 'citeproc nil t)
      #'refbox-citeproc-format-reference
    #'refbox-format-reference))

(defun bv-refbox--presets ()
  "Return high-signal Refbox search presets."
  '(":p" ":n" ":l" ":c"
    "has:files" "has:notes" "has:links" "is:cited"
    "type:article" "type:book" "type:inproceedings"))

(defun bv-refbox--setup-completion-category ()
  "Keep Refbox dynamic completion on the native matching path."
  (setf (alist-get 'refbox-reference completion-category-overrides)
        `((styles ,@refbox-completion-category-styles))))

(defun bv-refbox--setup-core ()
  "Apply core Refbox configuration."
  (setq refbox-server-program "refbox"
        refbox-bibliography-roots (bv-refbox--bibliography-roots)
        refbox-bibliography nil
        refbox-bibliography-extensions '("bib" "bibtex")
        refbox-bibliography-include-globs nil
        refbox-bibliography-exclude-globs nil
        refbox-bibliography-exclude-paths
        (bv-refbox--bibliography-exclude-paths)
        refbox-bibliography-include-hidden nil
        refbox-database-file (bv-refbox--cache-file "emacs/refbox.sqlite")
        refbox-autosync-sync-on-enable t
        refbox-library-paths
        (bv-refbox--expanded-directories bv-refbox-library-paths)
        refbox-library-paths-recursive t
        refbox-library-file-extensions '("pdf" "djvu" "epub" "html")
        refbox-file-additional-files-separator "-"
        refbox-file-open-functions '(("pdf" . refbox-file-open-pdf)
                                     ("html" . refbox-file-open-external)
                                     (t . refbox-file-open-in-emacs))
        refbox-open-resources '(:files :links :notes :create-notes)
        refbox-default-action #'refbox-open
        refbox-templates (bv-refbox--templates)
        refbox-indicators (bv-refbox--indicators)
        refbox-completion-limit bv-refbox-completion-limit
        refbox-capf-limit 12
        refbox-ellipsis nil
        refbox-completion-category-styles '(basic)
        refbox-presets (bv-refbox--presets)
        refbox-at-point-function #'bv-refbox-at-point
        refbox-format-reference-function (bv-refbox--format-reference-function)
        refbox-citeproc-csl-styles-dir
        (bv-refbox--existing-directory bv-refbox-csl-styles-dir)
        refbox-citeproc-csl-locales-dir
        (bv-refbox--existing-directory bv-refbox-csl-locales-dir)
        refbox-citeproc-csl-style bv-refbox-csl-style
        refbox-citeproc-csl-locale bv-refbox-csl-locale
        refbox-latex-default-cite-command "cite"
        refbox-latex-prompt-for-cite-style nil
        refbox-latex-prompt-for-extra-arguments nil
        refbox-markdown-prompt-for-extra-arguments nil)
  (bv-refbox--setup-completion-category))

(defun bv-refbox--setup-org ()
  "Register Refbox with Org-cite and org-slipbox."
  (refbox-org-register-processor)
  (when (fboundp 'bv-org-cite-setup-oc)
    (bv-org-cite-setup-oc))
  (when (require 'refbox-org-slipbox nil t)
    (setq refbox-org-slipbox-note-title-template
          "${author editor:%etal}, ${title}"
          refbox-org-slipbox-preload-limit 20000
          refbox-org-slipbox-capture-template-key "s")
    (refbox-org-slipbox-mode 1)))

(defun bv-refbox--setup-capf ()
  "Install Refbox completion-at-point hooks."
  (add-hook 'org-mode-hook #'refbox-org-setup-capf 90)
  (add-hook 'LaTeX-mode-hook #'refbox-latex-setup-capf 90)
  (add-hook 'latex-mode-hook #'refbox-latex-setup-capf 90)
  (add-hook 'markdown-mode-hook #'refbox-markdown-setup-capf 90)
  (add-hook 'gfm-mode-hook #'refbox-markdown-setup-capf 90))

(defun bv-refbox--setup-savehist ()
  "Persist Refbox minibuffer history."
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'refbox-history)))

(defun bv-refbox--setup-embark ()
  "Enable Refbox Embark integration."
  (when (require 'embark nil t)
    (setq refbox-embark-multitarget-limit 250)
    (refbox-embark-setup)))

(defun bv-refbox--setup-vertico ()
  "Keep Refbox completion candidates in daemon-ranked order."
  (with-eval-after-load 'vertico-multiform
    (setf (alist-get 'refbox-reference vertico-multiform-categories)
          '((vertico-sort-function . nil)))
    (dolist (command '(refbox-read-reference
                       refbox-read-references
                       refbox-select-reference
                       refbox-select-references
                       refbox-select-ref
                       refbox-select-refs
                       refbox-insert-citation
                       refbox-insert-keys
                       refbox-insert-reference
                       refbox-copy-reference
                       refbox-open
                       refbox-open-files
                       refbox-open-links
                       refbox-open-notes
                       refbox-open-entry
                       refbox-add-file-to-library
                       refbox-attach-files))
      (setf (alist-get command vertico-multiform-commands)
            '((vertico-sort-function . nil))))))

(defun bv-refbox--setup-autosync ()
  "Enable Refbox index synchronization for interactive Emacs sessions."
  (unless noninteractive
    (refbox-autosync-mode 1)))

(bv-refbox--setup-core)
(bv-refbox--setup-org)
(bv-refbox--setup-capf)
(bv-refbox--setup-savehist)
(bv-refbox--setup-embark)
(bv-refbox--setup-vertico)
(bv-refbox--setup-autosync)

(provide 'bv-refbox)
;;; bv-refbox.el ends here
