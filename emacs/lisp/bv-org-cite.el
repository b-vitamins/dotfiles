;;; bv-org-cite.el --- Org-cite configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Org-cite configuration for academic citation management through Refbox.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(eval-when-compile
  (require 'oc-biblatex)
  (require 'oc-csl))

;; Declare external functions and variables
(autoload 'refbox-org-register-processor "refbox-org")

;; Declare org-cite variables that might not be loaded yet
(defvar org-cite-global-bibliography)
(defvar org-cite-insert-processor)
(defvar org-cite-follow-processor)
(defvar org-cite-activate-processor)
(defvar org-cite-export-processors)
(defvar org-cite-csl-styles-dir)
(defvar org-cite-csl-locales-dir)

(defgroup bv-org-cite nil
  "Org-cite configuration for the BV Emacs setup."
  :group 'org
  :prefix "bv-org-cite-")

(defcustom bv-org-cite-bibliography-roots '("~/projects/bibliography")
  "Bibliography root directories used by Org-cite."
  :type '(repeat directory)
  :group 'bv-org-cite)

(defcustom bv-org-cite-bibliography-exclude-paths
  '("~/projects/bibliography/collections/_archive")
  "Bibliography file or directory paths excluded from Org-cite."
  :type '(repeat (choice file directory))
  :group 'bv-org-cite)

(defcustom bv-org-cite-csl-styles-dir "~/documents/styles"
  "Directory containing local CSL style files for Org export.
When the directory does not exist, Org's bundled CSL fallback remains active."
  :type '(choice (const :tag "Use Org fallback" nil) directory)
  :group 'bv-org-cite)

(defcustom bv-org-cite-csl-locales-dir nil
  "Directory containing CSL locale files for Org export."
  :type '(choice (const :tag "Use Org fallback" nil) directory)
  :group 'bv-org-cite)

(defvar bv-org-cite--bibliography-cache nil
  "Cached bibliography files keyed by expanded bibliography roots.")

(defun bv-org-cite--existing-directory (directory)
  "Return expanded DIRECTORY when it exists, otherwise nil."
  (when (and directory (not (string-empty-p directory)))
    (let ((expanded (file-name-as-directory (expand-file-name directory))))
      (when (file-directory-p expanded)
        expanded))))

(defun bv-org-cite--bibliography-file-p (file)
  "Return non-nil when FILE is a bibliography file."
  (and (file-regular-p file)
       (string-match-p "\\.bib\\(?:tex\\)?\\'" file)))

(defun bv-org-cite--expanded-exclude-paths ()
  "Return expanded bibliography paths excluded from Org-cite scans."
  (mapcar (lambda (path)
            (let ((expanded (expand-file-name path)))
              (if (or (file-directory-p expanded)
                      (string-suffix-p "/" path))
                  (file-name-as-directory expanded)
                expanded)))
          bv-org-cite-bibliography-exclude-paths))

(defun bv-org-cite--path-excluded-p (path exclude-paths)
  "Return non-nil when PATH is covered by EXCLUDE-PATHS."
  (let ((expanded (expand-file-name path)))
    (cl-some
     (lambda (exclude)
       (if (string-suffix-p "/" exclude)
           (string-prefix-p exclude (file-name-as-directory expanded))
         (equal expanded exclude)))
     exclude-paths)))

(defun bv-org-cite--visible-directory-p (directory exclude-paths)
  "Return non-nil when DIRECTORY should be scanned."
  (and (file-directory-p directory)
       (not (file-symlink-p directory))
       (not (string-prefix-p "." (file-name-nondirectory directory)))
       (not (bv-org-cite--path-excluded-p directory exclude-paths))))

(defun bv-org-cite--directory-bibliography-files (directory exclude-paths)
  "Return bibliography files under DIRECTORY, skipping hidden directories."
  (let (files)
    (dolist (entry (directory-files directory t directory-files-no-dot-files-regexp))
      (cond
       ((bv-org-cite--visible-directory-p entry exclude-paths)
        (setq files
              (nconc files
                     (bv-org-cite--directory-bibliography-files
                      entry exclude-paths))))
       ((and (bv-org-cite--bibliography-file-p entry)
             (not (bv-org-cite--path-excluded-p entry exclude-paths)))
        (push entry files))))
    (nreverse files)))

(defun bv-org-cite--setup-csl ()
  "Apply Org CSL directory configuration."
  (when (boundp 'org-cite-csl-styles-dir)
    (setq org-cite-csl-styles-dir
          (bv-org-cite--existing-directory bv-org-cite-csl-styles-dir)))
  (when (boundp 'org-cite-csl-locales-dir)
    (setq org-cite-csl-locales-dir
          (bv-org-cite--existing-directory bv-org-cite-csl-locales-dir))))

(defun bv-org-cite-setup-oc ()
  "Setup org-cite configuration."
  (require 'oc-csl)
  (bv-org-cite--setup-csl)
  (when (featurep 'refbox-org)
    (refbox-org-register-processor))
  (when (boundp 'org-cite-global-bibliography)
    (setq org-cite-global-bibliography
          (bv-org-cite-find-bibliography-files)))
  (when (boundp 'org-cite-insert-processor)
    (setq org-cite-insert-processor 'refbox))
  (when (boundp 'org-cite-follow-processor)
    (setq org-cite-follow-processor 'refbox))
  (when (boundp 'org-cite-activate-processor)
    (setq org-cite-activate-processor 'refbox))
  (when (boundp 'org-cite-export-processors)
    (setq org-cite-export-processors '((latex biblatex) (t csl)))))

(defun bv-org-cite-find-bibliography-files (&optional refresh)
  "Find all bibliography files in `bv-org-cite-bibliography-roots'.
When REFRESH is non-nil, ignore the cached directory scan."
  (let* ((roots (mapcar #'expand-file-name bv-org-cite-bibliography-roots))
         (exclude-paths (bv-org-cite--expanded-exclude-paths))
         (cache-key (list roots exclude-paths)))
    (if (and (not refresh)
             bv-org-cite--bibliography-cache
             (equal cache-key (car bv-org-cite--bibliography-cache)))
        (cdr bv-org-cite--bibliography-cache)
      (let ((files
             (sort
              (delete-dups
               (cl-mapcan
                (lambda (root)
                  (when (file-directory-p root)
                    (bv-org-cite--directory-bibliography-files
                     root exclude-paths)))
                roots))
              #'string-lessp)))
        (setq bv-org-cite--bibliography-cache (cons cache-key files))
        files))))

(defun bv-org-cite-refresh-bibliography ()
  "Refresh Org-cite's cached global bibliography file list."
  (interactive)
  (setq bv-org-cite--bibliography-cache nil)
  (when (boundp 'org-cite-global-bibliography)
    (setq org-cite-global-bibliography
          (bv-org-cite-find-bibliography-files t)))
  (message "Org-cite bibliography files: %d"
           (length (bound-and-true-p org-cite-global-bibliography))))

(with-eval-after-load 'oc
  (bv-org-cite-setup-oc))

(with-eval-after-load 'oc-csl
  (bv-org-cite--setup-csl))

;; Keybindings moved to bv-citation.el for unified citation interface

(provide 'bv-org-cite)
;;; bv-org-cite.el ends here
