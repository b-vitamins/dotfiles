;;; bv-citation.el --- Unified citation management -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Unified citation management system integrating BibTeX, Org-cite, and Refbox.
;; Provides a consistent interface for all citation-related operations.
;;
;; Usage Guide:
;;
;; Insert/Edit:
;; - `C-c c i' - Insert citation
;; - `C-c c m' - Edit citation at point
;; - `C-c c k' - Insert keys
;;
;; Resources:
;; - `C-c c o' - Open files, links, or notes
;; - `C-c c f' - Open files
;; - `C-c c n' - Open notes
;;
;; Index:
;; - `C-c c g' - Sync bibliography roots
;; - `C-c c G' - Sync current bibliography file
;; - `C-c c D' - List parse diagnostics

;;; Code:

(require 'bv-bibtex)
(require 'bv-org-cite)
(require 'bv-refbox)

;; Declare external functions
(declare-function refbox-select-references "refbox" (&rest args))
(declare-function refbox-insert-preset "refbox" ())
(declare-function refbox-insert-citation "refbox" (&optional references arg))
(declare-function refbox-insert-edit "refbox" (&optional arg))
(declare-function refbox-open "refbox" (&optional references))
(declare-function refbox-open-files "refbox" (&optional references))
(declare-function refbox-open-entry "refbox" (&optional reference))
(declare-function refbox-open-source "refbox" (&optional reference))
(declare-function refbox-add-file-to-library "refbox" (&optional reference))
(declare-function refbox-attach-files "refbox" (&optional references))
(declare-function refbox-export-local-bib-file "refbox" (&optional file))
(declare-function refbox-dwim "refbox" ())
(declare-function refbox-at-point "refbox" ())
(declare-function refbox-copy-reference "refbox" (&optional references))
(declare-function refbox-insert-bibtex "refbox" (&optional references))
(declare-function refbox-insert-raw-entry "refbox" (&optional references))
(declare-function refbox-insert-reference "refbox" (&optional references))
(declare-function refbox-open-notes "refbox" (&optional references))
(declare-function refbox-create-note "refbox" (&optional key entry))
(declare-function refbox-open-links "refbox" (&optional references))
(declare-function refbox-insert-keys "refbox" (&optional references))
(declare-function refbox-reference-field "refbox" (candidate field))
(declare-function refbox-sync "refbox" ())
(declare-function refbox-sync-current-file "refbox" ())
(declare-function refbox-status "refbox" ())
(declare-function refbox-ping "refbox" ())
(declare-function refbox-list-diagnostics "refbox" (&optional limit))
(declare-function refbox-list-duplicates "refbox" (&optional limit))
(declare-function org-cite-insert "oc" (&optional arg))

;; Define citation keymap
(defvar bv-citation-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for citation commands.
This keymap provides bindings for all citation-related operations.")

;; Additional functions for enhanced citation workflow
(defun bv-citation-insert-and-open ()
  "Insert a citation and immediately open its PDF.
This function combines citation insertion with PDF opening for quick access."
  (interactive)
  (call-interactively #'refbox-insert-citation)
  (save-excursion
    (goto-char (max (point-min) (1- (point))))
    (call-interactively #'refbox-dwim)))

(defun bv-citation-select-references ()
  "Select references and echo their keys."
  (interactive)
  (let ((references (refbox-select-references)))
    (when references
      (message "Selected: %s"
               (mapconcat (lambda (reference)
                            (or (refbox-reference-field reference "key") ""))
                          references
                          ", ")))
    references))

(defun bv-citation-refresh ()
  "Synchronize Refbox and refresh Org-cite's bibliography file cache."
  (interactive)
  (refbox-sync)
  (when (fboundp 'bv-org-cite-refresh-bibliography)
    (bv-org-cite-refresh-bibliography)))

;; Insert and edit.
(define-key bv-citation-map (kbd "i") #'refbox-insert-citation)
(define-key bv-citation-map (kbd "c") #'refbox-insert-citation)
(define-key bv-citation-map (kbd "I") #'bv-citation-insert-and-open)
(define-key bv-citation-map (kbd "m") #'refbox-insert-edit)
(define-key bv-citation-map (kbd "k") #'refbox-insert-keys)
(define-key bv-citation-map (kbd "p") #'refbox-insert-preset)

;; Open and act at point.
(define-key bv-citation-map (kbd "o") #'refbox-open)
(define-key bv-citation-map (kbd "RET") #'refbox-open)
(define-key bv-citation-map (kbd "f") #'refbox-open-files)
(define-key bv-citation-map (kbd "l") #'refbox-open-links)
(define-key bv-citation-map (kbd "n") #'refbox-open-notes)
(define-key bv-citation-map (kbd "N") #'refbox-create-note)
(define-key bv-citation-map (kbd "e") #'refbox-open-entry)
(define-key bv-citation-map (kbd "E") #'refbox-open-source)
(define-key bv-citation-map (kbd "d") #'refbox-dwim)
(define-key bv-citation-map (kbd ".") #'refbox-at-point)

;; Transfer and export.
(define-key bv-citation-map (kbd "r") #'refbox-copy-reference)
(define-key bv-citation-map (kbd "y") #'refbox-copy-reference)
(define-key bv-citation-map (kbd "R") #'refbox-insert-reference)
(define-key bv-citation-map (kbd "b") #'refbox-insert-bibtex)
(define-key bv-citation-map (kbd "t") #'refbox-insert-bibtex)
(define-key bv-citation-map (kbd "B") #'refbox-insert-raw-entry)
(define-key bv-citation-map (kbd "x") #'refbox-export-local-bib-file)

;; Library and index operations.
(define-key bv-citation-map (kbd "a") #'refbox-add-file-to-library)
(define-key bv-citation-map (kbd "A") #'refbox-attach-files)
(define-key bv-citation-map (kbd "s") #'bv-citation-select-references)
(define-key bv-citation-map (kbd "g") #'bv-citation-refresh)
(define-key bv-citation-map (kbd "G") #'refbox-sync-current-file)
(define-key bv-citation-map (kbd "S") #'refbox-status)
(define-key bv-citation-map (kbd "P") #'refbox-ping)
(define-key bv-citation-map (kbd "D") #'refbox-list-diagnostics)
(define-key bv-citation-map (kbd "U") #'refbox-list-duplicates)

;; Org-specific citation commands.
(with-eval-after-load 'org
  (define-key bv-citation-map (kbd "C") #'org-cite-insert))

(global-set-key (kbd "C-c c") bv-citation-map)

(provide 'bv-citation)
;;; bv-citation.el ends here
