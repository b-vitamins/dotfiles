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
;; - `C-c c n' - Open/create citation note
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

;; Define citation keymap
(defvar-keymap bv-citation-map
  :doc "Keymap for citation commands.")

;; Basic citation operations
(define-key bv-citation-map (kbd "i") 'citar-insert-citation)
(define-key bv-citation-map (kbd "o") 'citar-open)
(define-key bv-citation-map (kbd "n") 'citar-open-notes)
(define-key bv-citation-map (kbd "f") 'citar-open-entry)

;; Bibliography management
(define-key bv-citation-map (kbd "b") 'citar-browse-bibliography)
(define-key bv-citation-map (kbd "r") 'citar-refresh)
(define-key bv-citation-map (kbd "a") 'citar-add-file-to-library)

;; Advanced operations
(define-key bv-citation-map (kbd "s") 'citar-search)
(define-key bv-citation-map (kbd "e") 'citar-export-local-bib-file)
(define-key bv-citation-map (kbd "l") 'citar-list-keys)
(define-key bv-citation-map (kbd "d") 'citar-dwim)

;; Org-specific citation commands
(with-eval-after-load 'org
  (define-key bv-citation-map (kbd "c") 'org-cite-insert)
  (define-key bv-citation-map (kbd "k") 'citar-kill-citation))

;; Bind the keymap to C-c c
(global-set-key (kbd "C-c c") bv-citation-map)

;; Additional functions for enhanced citation workflow
(defun bv-citation-insert-and-open ()
  "Insert a citation and immediately open its PDF."
  (interactive)
  (call-interactively 'citar-insert-citation)
  (save-excursion
    (backward-char)
    (citar-open)))

(defun bv-citation-create-note ()
  "Create a new note for a citation using org-roam."
  (interactive)
  (citar-open-notes))

;; Add to citation map
(define-key bv-citation-map (kbd "I") 'bv-citation-insert-and-open)
(define-key bv-citation-map (kbd "N") 'bv-citation-create-note)

(provide 'bv-citation)
;;; bv-citation.el ends here