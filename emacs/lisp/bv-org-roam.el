;;; bv-org-roam.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/lisp/bv-org-roam.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; "What are we to do with what we have written down? Certainly at first, we will
;; produce mostly garbage.  But we have been educated to expect something useful
;; from our activities, and soon lose confidence if nothing useful seems to
;; result.  We should therefore reflect on whether and how we arrange our notes so
;; they are available for later access." - Niklas Luhmann

;;; Code:

(require 'org)
(require 'marginalia)
(require 'org-roam)

(defvar bv-org-roam-capture-templates
  '(
    ("d" "default" entry
     "* %?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+title: %<%Y-%m-%d-%H-%M-%S> ${title}\n"))

    ("f" "fleeting" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :fleeting:\n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n")
     :unnarrowed t)

    ("F" "fleeting-timed" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :fleeting:\n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+CAPTION:\n#+END\n")
     :unnarrowed t)

    ("c" "concept" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :concept:\n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n")
     :unnarrowed t)

    ("C" "concept-timed" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :concept:\n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+CAPTION:\n#+END\n")
     :unnarrowed t)

    ("l" "literature" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :literature:\n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n")
     :unnarrowed t)

    ("L" "literature-timed" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :literature:\n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+CAPTION:\n#+END\n")
     :unnarrowed t)

    ("p" "problem" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :problem:\n#+TEXTBOOK: \n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n")
     :unnarrowed t)

    ("P" "problem-timed" plain "%?"
     :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :problem:\n#+TEXTBOOK: \n#+SETUPFILE: ~/.config/emacs/setup/setupfile.org\n#+BEGIN: clocktable :maxlevel 2 :scope nil :emphasize nil\n#+CAPTION:\n#+END\n")
     :unnarrowed t)
    ))

(defun bv-org-roam-slug (title)
  "Generate a slug from TITLE, replacing underscores with hyphens."
  (let ((slug-trim-chars '(;; Combining Diacritical Marks
                           768 769 770 771 772 774 775 776 777 778 779 780
                           795 803 804 805 807 813 814 816 817)))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs '(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric to "-"
                      ("--*" . "-")                   ;; remove sequential hyphens
                      ("^-" . "")                     ;; remove starting hyphen
                      ("-$" . "")))                   ;; remove ending hyphen
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defun bv-org-roam-node-slug-advice (orig-fun &rest args)
  "Advice to replace underscores with hyphens in Org-roam slugs."
  (let ((title (org-roam-node-title (car args))))
    (bv-org-roam-slug title)))

;; Apply the advice to override the slug generation
(advice-add 'org-roam-node-slug :around #'bv-org-roam-node-slug-advice)

(defcustom bv-org-roam-node-display-template
  (concat
   (propertize "" 'face 'org-todo)
   (propertize " ${hierarchy:100} " 'face 'italic)
   (propertize " ${directories:20} " 'face 'italic)
   (propertize " ${tags:60} " 'face 'org-tag)
   (propertize " ${backlinkscount:6} " 'face 'italic))
  "Template for displaying nodes in Org Roam."
  :type 'string
  :group 'bv-org-roam)

(defcustom bv-org-roam-node-annotation-function
  (lambda (node)
    (marginalia--time (org-roam-node-file-mtime node)))
  "Function to annotate Org Roam nodes with their modification time."
  :type 'function
  :group 'bv-org-roam)

(cl-defmethod org-roam-node-directories ((node org-roam-node))
  "Return the uppercase directory of the NODE's file.
The directory is relative to org-roam-directory'."
  (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
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

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  "Return the hierarchy string for the NODE."
  (let ((level (org-roam-node-level node)))
    (concat
     (when (> level 0) (concat (org-roam-node-file-title node) ">"))
     (when (> level 1) (concat (string-join (org-roam-node-olp node) ">") ">"))
     (org-roam-node-title node))))

(provide 'bv-org-roam)
;;; bv-org-roam.el ends here