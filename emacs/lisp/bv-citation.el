;;; bv-citation.el --- Unified citation management -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Unified citation management system integrating BibTeX, Org-cite, and Citar.
;; Provides a consistent interface for all citation-related operations.
;;
;; Usage Guide:
;;
;; Basic Operations:
;; - `C-c c i' - Insert citation
;; - `C-c c o' - Open citation (PDF/URL)
;; - `C-c c f' - Find citation
;;
;; Bibliography Management:
;; - `C-c c b' - Browse bibliography
;; - `C-c c r' - Refresh bibliography cache
;; - `C-c c a' - Add new entry to bibliography
;;
;; Advanced:
;; - `C-c c s' - Search citations
;; - `C-c c e' - Export citations
;; - `C-c c l' - List all citations in buffer
;; - `C-c c d' - Show citation details

;;; Code:

(require 'bv-bibtex)
(require 'bv-org-cite)
(require 'bv-citar)

;; Declare external functions
(declare-function citar-select-refs "citar" (&optional arg))
(declare-function citar-insert-preset "citar" (&optional arg))
(declare-function citar-insert-citation "citar" (&optional arg))
(declare-function citar-open "citar" (&optional arg))
(declare-function citar-open-entry "citar" (&optional arg))
(declare-function citar-add-file-to-library "citar" (&optional arg))
(declare-function citar-attach-files "citar" (&optional arg))
(declare-function citar-export-local-bib-file "citar" (&optional arg))
(declare-function citar-dwim "citar" (&optional arg))
(declare-function citar-copy-reference "citar" (&optional arg))
(declare-function citar-insert-bibtex "citar" (&optional arg))
(declare-function citar-insert-reference "citar" (&optional arg))
(declare-function citar-open-notes "citar" (&optional arg))
(declare-function citar-create-note "citar" (&optional arg))
(declare-function citar-open-links "citar" (&optional arg))
(declare-function citar-insert-keys "citar" (&optional arg))
(declare-function org-cite-insert "oc" (&optional arg))

;; Define citation keymap
(defvar bv-citation-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for citation commands.
This keymap provides bindings for all citation-related operations.")

;; Basic citation operations
(define-key bv-citation-map (kbd "i") 'citar-insert-citation)
(define-key bv-citation-map (kbd "o") 'citar-open)
(define-key bv-citation-map (kbd "f") 'citar-open-entry)

;; Bibliography management
(define-key bv-citation-map (kbd "a") 'citar-add-file-to-library)
(define-key bv-citation-map (kbd "A") 'citar-attach-files)

;; Advanced operations
(define-key bv-citation-map (kbd "s") 'citar-select-refs)
(define-key bv-citation-map (kbd "e") 'citar-export-local-bib-file)
(define-key bv-citation-map (kbd "d") 'citar-dwim)
(define-key bv-citation-map (kbd "p") 'citar-insert-preset)
(define-key bv-citation-map (kbd "c") 'citar-copy-reference)
(define-key bv-citation-map (kbd "t") 'citar-insert-bibtex)

;; Org-specific citation commands
(with-eval-after-load 'org
  (define-key bv-citation-map (kbd "C") 'org-cite-insert))

;; Bind the keymap to C-c c
(global-set-key (kbd "C-c c") bv-citation-map)

;; Additional functions for enhanced citation workflow
(defun bv-citation-insert-and-open ()
  "Insert a citation and immediately open its PDF.
This function combines citation insertion with PDF opening for quick access."
  (interactive)
  (call-interactively 'citar-insert-citation)
  (save-excursion
    (backward-char)
    (call-interactively 'citar-open)))

;; Add to citation map
(define-key bv-citation-map (kbd "I") 'bv-citation-insert-and-open)

;; Additional citation helper functions
(defun bv-citation-insert-formatted-reference ()
  "Insert a formatted reference for the selected citation.
This creates a properly formatted bibliographic reference."
  (interactive)
  (call-interactively 'citar-insert-reference))

(defun bv-citation-open-notes ()
  "Open notes for the selected citation.
This opens existing notes associated with a bibliographic entry."
  (interactive)
  (call-interactively 'citar-open-notes))

(defun bv-citation-create-note ()
  "Create a note for the selected citation.
This creates a new note file for a bibliographic entry."
  (interactive)
  (call-interactively 'citar-create-note))

;; Add new functions to keymap
(define-key bv-citation-map (kbd "n") 'bv-citation-open-notes)
(define-key bv-citation-map (kbd "N") 'bv-citation-create-note)
(define-key bv-citation-map (kbd "F") 'bv-citation-insert-formatted-reference)
(define-key bv-citation-map (kbd "l") 'citar-open-links)
(define-key bv-citation-map (kbd "k") 'citar-insert-keys)

(provide 'bv-citation)
;;; bv-citation.el ends here