;;; bv-treesit.el --- Tree-sitter ergonomics and audit tools  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Tree-sitter ergonomics helpers.
;; Provides a small audit command to show which grammars are available and which
;; are missing for the languages implied by the current configuration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)

;; External variables to avoid warnings
(defvar major-mode-remap-alist)
(defvar treesit-extra-load-path)
(defvar treesit-language-source-alist)

(defgroup bv-treesit nil
  "Tree-sitter ergonomics."
  :group 'bv
  :prefix "bv-treesit-")

(defcustom bv-treesit-audit-languages nil
  "Additional tree-sitter languages to include in `bv-treesit-audit'.

When nil, languages are inferred from `major-mode-remap-alist'."
  :type '(repeat symbol)
  :group 'bv-treesit)

(defcustom bv-treesit-audit-include-sources nil
  "Whether `bv-treesit-audit' should include all entries from
`treesit-language-source-alist'."
  :type 'boolean
  :group 'bv-treesit)

(defconst bv-treesit--audit-buffer "*BV Treesit Audit*"
  "Name of the treesit audit buffer.")

(defun bv-treesit--available-p ()
  "Return non-nil if this Emacs has tree-sitter support enabled."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun bv-treesit--language-from-ts-mode (mode)
  "Infer a tree-sitter language symbol from MODE, a *-ts-mode symbol."
  (let ((name (and (symbolp mode) (symbol-name mode))))
    (when (and name (string-suffix-p "-ts-mode" name))
      (let ((base (string-remove-suffix "-ts-mode" name)))
        (pcase base
          ("c++" 'cpp)
          ("js" 'javascript)
          ("typescript" 'typescript)
          ("tsx" 'tsx)
          (_ (intern base)))))))

(defun bv-treesit--remapped-languages ()
  "Return an alist of (LANG . MODES) inferred from `major-mode-remap-alist'."
  (let ((table (make-hash-table :test #'eq)))
    (when (boundp 'major-mode-remap-alist)
      (dolist (pair major-mode-remap-alist)
        (let* ((to (cdr pair))
               (lang (bv-treesit--language-from-ts-mode to)))
          (when lang
            (push to (gethash lang table))))))
    (let (alist)
      (maphash (lambda (lang modes)
                 (push (cons lang (delete-dups (delq nil modes))) alist))
               table)
      (sort alist (lambda (a b)
                    (string< (symbol-name (car a))
                             (symbol-name (car b))))))))

(defun bv-treesit--audit-language-set ()
  "Return the ordered set of languages to audit."
  (let* ((remapped (mapcar #'car (bv-treesit--remapped-languages)))
         (langs (append bv-treesit-audit-languages remapped)))
    (when (and bv-treesit-audit-include-sources
               (boundp 'treesit-language-source-alist)
               (listp treesit-language-source-alist))
      (setq langs (append langs (mapcar #'car treesit-language-source-alist))))
    (delete-dups (delq nil langs))))

(defun bv-treesit--source-summary (lang)
  "Return a concise source summary for LANG from `treesit-language-source-alist'."
  (when (and (boundp 'treesit-language-source-alist)
             (listp treesit-language-source-alist))
    (when-let* ((entry (assq lang treesit-language-source-alist))
                (url (cadr entry)))
      (let ((tail (file-name-nondirectory (directory-file-name url))))
        (if (string-empty-p tail) url tail)))))

(define-derived-mode bv-treesit-audit-mode tabulated-list-mode "BV-Treesit"
  "Major mode for `bv-treesit-audit' buffers."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-format
        [("Language" 16 t)
         ("Status" 10 t)
         ("Modes" 36 t)
         ("Source" 24 t)])
  (tabulated-list-init-header))

(defun bv-treesit-audit (&optional include-sources)
  "Audit installed tree-sitter grammars.

With prefix argument INCLUDE-SOURCES, include all languages from
`treesit-language-source-alist' in addition to inferred languages."
  (interactive "P")
  (unless (bv-treesit--available-p)
    (user-error "Tree-sitter is not available in this Emacs build"))
  (require 'treesit)
  (let* ((bv-treesit-audit-include-sources
          (or bv-treesit-audit-include-sources include-sources))
         (remapped (bv-treesit--remapped-languages))
         (langs (bv-treesit--audit-language-set))
         (entries
          (mapcar
           (lambda (lang)
             (let* ((available (and (fboundp 'treesit-language-available-p)
                                    (treesit-language-available-p lang)))
                    (status (if available
                                (propertize "OK" 'face 'success)
                              (propertize "MISSING" 'face 'warning)))
                    (modes (mapconcat #'symbol-name
                                      (or (alist-get lang remapped) '())
                                      ", "))
                    (source (or (bv-treesit--source-summary lang) "")))
               (list lang (vector (symbol-name lang) status modes source))))
           langs)))
    (with-current-buffer (get-buffer-create bv-treesit--audit-buffer)
      (bv-treesit-audit-mode)
      (setq tabulated-list-entries entries)
      (setq-local header-line-format
                  (format "treesit-extra-load-path: %s"
                          (if (boundp 'treesit-extra-load-path)
                              (mapconcat #'identity (mapcar #'expand-file-name treesit-extra-load-path) "  |  ")
                            "nil")))
      (tabulated-list-print t)
      (pop-to-buffer (current-buffer)))))

(provide 'bv-treesit)
;;; bv-treesit.el ends here
