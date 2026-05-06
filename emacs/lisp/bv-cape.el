;;; bv-cape.el --- Cape completion-at-point pipeline -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Cape provides the candidates Corfu displays.  This module keeps the CAPF
;; pipeline deterministic and context-aware: code, comments, prose, shells,
;; Org, LaTeX, and Eglot-managed buffers each get a small, explicit set of
;; completion functions.

;;; Code:

(require 'cape)
(require 'cl-lib)
(require 'subr-x)

;;; Declarations

(defvar completion-at-point-functions)
(defvar cape-tex-prefix-required)
(defvar yas-minor-mode)

(declare-function eglot-completion-at-point "eglot" ())
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-files "project" (project &optional dirs))
(declare-function project-root "project" (project))
(declare-function yas--all-templates "yasnippet" (&optional tables))
(declare-function yas--get-snippet-tables "yasnippet" (&optional mode))
(declare-function yas-expand-snippet "yasnippet" (template &optional start end expand-env))
(declare-function yas-lookup-snippet "yasnippet" (name &optional mode noerror))
(declare-function bv-cape--eglot-capf "bv-cape" ())
(declare-function bv-cape-setup-eglot "bv-cape" ())

;;; Cache

(defconst bv-cape-cache-duration 30
  "Cache duration in seconds for short-lived Cape data.")

(defconst bv-cape-project-cache-duration 60
  "Cache duration in seconds for project file data.")

(defvar bv-cape--dict-cache nil
  "Cached dictionary file path.")

(defvar bv-cape--project-files-cache (make-hash-table :test #'equal :weakness 'key)
  "Project file cache keyed by project root.")

(defvar bv-cape--project-cache-time (make-hash-table :test #'equal)
  "Project file cache timestamps.")

(defvar bv-cape--snippet-cache (make-hash-table :test #'eq)
  "Snippet trigger cache keyed by major mode.")

(defvar bv-cape--snippet-cache-time (make-hash-table :test #'eq)
  "Snippet cache timestamps.")

(defun bv-cape--cache-expired-p (key table duration)
  "Return non-nil when KEY in TABLE is older than DURATION seconds."
  (> (- (float-time) (gethash key table 0)) duration))

(defun bv-cape--find-dict ()
  "Find a readable dictionary file."
  (or bv-cape--dict-cache
      (setq bv-cape--dict-cache
            (or (getenv "WORDLIST")
                (cl-find-if #'file-readable-p
                            '("/usr/share/dict/words"
                              "/usr/share/dict/american-english"
                              "/usr/share/dict/british-english"
                              "/usr/share/dict/cracklib-small"))))))

(setq cape-dict-file #'bv-cape--find-dict
      cape-dict-limit 100
      cape-dict-case-fold 'case-fold-search
      cape-dabbrev-buffer-function #'cape-same-mode-buffers
      cape-file-directory-must-exist t)

;;; Custom CAPFs

(defun bv-cape-yasnippet ()
  "Yasnippet completion as a CAPF."
  (when (and (featurep 'yasnippet)
             (bound-and-true-p yas-minor-mode))
    (condition-case nil
        (let* ((mode major-mode)
               (bounds (bounds-of-thing-at-point 'symbol))
               (start (or (car bounds) (point)))
               (end (or (cdr bounds) (point))))
          (when (or (not (gethash mode bv-cape--snippet-cache))
                    (bv-cape--cache-expired-p mode bv-cape--snippet-cache-time
                                              bv-cape-cache-duration))
            (when (fboundp 'yas--all-templates)
              (puthash mode
                       (mapcar #'car (yas--all-templates (yas--get-snippet-tables)))
                       bv-cape--snippet-cache)
              (puthash mode (float-time) bv-cape--snippet-cache-time)))
          (when-let ((snippets (gethash mode bv-cape--snippet-cache)))
            `(,start ,end
              ,snippets
              :annotation-function ,(lambda (_)
                                      (propertize " Snippet"
                                                  'face 'marginalia-type))
              :company-kind ,(lambda (_) 'snippet)
              :category cape-yasnippet
              :exclusive no
              :exit-function
              ,(lambda (str status)
                 (when (and (eq status 'finished)
                            (fboundp 'yas-expand-snippet)
                            (fboundp 'yas-lookup-snippet))
                   (when-let ((snippet (yas-lookup-snippet str)))
                     (yas-expand-snippet snippet)))))))
      (error nil))))

(defun bv-cape-project-file ()
  "Project file completion with bounded cache."
  (when-let ((project (and (fboundp 'project-current) (project-current))))
    (condition-case nil
        (let* ((root (project-root project))
               (bounds (or (bounds-of-thing-at-point 'filename)
                           (cons (point) (point))))
               files)
          (when (bv-cape--cache-expired-p root bv-cape--project-cache-time
                                          bv-cape-project-cache-duration)
            (remhash root bv-cape--project-files-cache))
          (setq files
                (or (gethash root bv-cape--project-files-cache)
                    (let ((project-files (when (fboundp 'project-files)
                                           (mapcar (lambda (file)
                                                     (file-relative-name file root))
                                                   (project-files project)))))
                      (puthash root (float-time) bv-cape--project-cache-time)
                      (puthash root project-files bv-cape--project-files-cache)
                      project-files)))
          (when files
            `(,(car bounds) ,(cdr bounds)
              ,files
              :annotation-function ,(lambda (_)
                                      (propertize " Project"
                                                  'face 'marginalia-file-name))
              :company-kind ,(lambda (_) 'file)
              :category project-file
              :exclusive no)))
      (error nil))))

;;; CAPF Profiles

(defun bv-cape--valid-elisp-symbol-p (sym)
  "Return non-nil if SYM is useful in Elisp completion."
  (and (not (keywordp sym))
       (or (boundp sym)
           (fboundp sym)
           (featurep sym)
           (facep sym))))

(defconst bv-cape--file-capf
  (cape-capf-prefix-length #'cape-file 3)
  "Conservative file CAPF.")

(defconst bv-cape--prog-code-capf
  (cape-capf-super
   (cape-capf-prefix-length #'cape-keyword 2)
   :with
   (cape-capf-prefix-length #'cape-dabbrev 3))
  "Programming completion for code context.")

(defconst bv-cape--prog-comment-capf
  (cape-capf-super
   (cape-capf-prefix-length #'cape-dabbrev 3)
   :with
   (cape-capf-prefix-length #'cape-dict 4))
  "Programming completion for comment context.")

(defconst bv-cape--elisp-code-capf
  (cape-capf-super
   (cape-capf-predicate #'cape-elisp-symbol #'bv-cape--valid-elisp-symbol-p)
   :with
   #'cape-keyword
   #'bv-cape-yasnippet)
  "Emacs Lisp completion for code context.")

(defconst bv-cape--elisp-comment-capf
  (cape-capf-super
   (cape-capf-prefix-length #'cape-dabbrev 3)
   :with
   (cape-capf-prefix-length #'cape-dict 4))
  "Emacs Lisp completion for comments.")

(defconst bv-cape--text-capfs
  (list (cape-capf-prefix-length #'cape-dabbrev 3)
        (cape-capf-prefix-length #'cape-dict 4))
  "Text-mode CAPFs.")

(defvar bv-cape--org-capfs nil
  "Lazy Org CAPF list.")

(defun bv-cape--org-capfs ()
  "Return Org CAPFs."
  (or bv-cape--org-capfs
      (setq bv-cape--org-capfs
            (list #'cape-elisp-block
                  (cape-capf-prefix-length #'cape-dabbrev 3)
                  (cape-capf-prefix-length #'cape-tex 2)
                  bv-cape--file-capf))))

(defvar bv-cape--latex-capfs nil
  "Lazy LaTeX CAPF list.")

(defun bv-cape--latex-capfs ()
  "Return LaTeX CAPFs."
  (or bv-cape--latex-capfs
      (setq bv-cape--latex-capfs
            (list (cape-capf-prefix-length #'cape-tex 1)
                  (cape-capf-prefix-length #'cape-dabbrev 3)
                  (cape-capf-prefix-length #'cape-dict 4)
                  bv-cape--file-capf))))

(defun bv-cape--set-capfs (&rest capfs)
  "Set buffer-local CAPFS as non-exclusive completion functions."
  (setq-local completion-at-point-functions
              (mapcar (lambda (capf)
                        (cape-capf-properties capf :exclusive 'no))
                      (delq nil capfs))))

(defun bv-cape-setup-prog-mode ()
  "Install programming-mode CAPFs."
  (bv-cape--set-capfs
   (cape-capf-inside-code bv-cape--prog-code-capf)
   (cape-capf-inside-comment bv-cape--prog-comment-capf)
   bv-cape--file-capf))

(defun bv-cape-setup-elisp-mode ()
  "Install Emacs Lisp CAPFs."
  (bv-cape--set-capfs
   (cape-capf-inside-code bv-cape--elisp-code-capf)
   (cape-capf-inside-comment bv-cape--elisp-comment-capf)
   (cape-capf-inside-string #'cape-file)))

(defun bv-cape-setup-text-mode ()
  "Install text-mode CAPFs."
  (apply #'bv-cape--set-capfs bv-cape--text-capfs))

(defun bv-cape-setup-git-commit-mode ()
  "Keep completion available but quiet in `git-commit-mode'."
  (apply #'bv-cape--set-capfs bv-cape--text-capfs))

(defun bv-cape-setup-org-mode ()
  "Install Org CAPFs."
  (apply #'bv-cape--set-capfs (bv-cape--org-capfs)))

(defun bv-cape-setup-shell-mode ()
  "Install shell and REPL CAPFs."
  (bv-cape--set-capfs
   #'cape-history
   #'cape-file
   (cape-capf-prefix-length #'cape-dabbrev 3)))

(defun bv-cape-setup-latex-mode ()
  "Install LaTeX CAPFs."
  (setq-local cape-tex-prefix-required nil)
  (apply #'bv-cape--set-capfs (bv-cape--latex-capfs)))

;;; Eglot

(defvar bv-cape--eglot-capf nil
  "Lazy Eglot CAPF.")

(with-eval-after-load 'eglot
  (defun bv-cape--eglot-capf ()
    "Return the Eglot super CAPF."
    (or bv-cape--eglot-capf
        (setq bv-cape--eglot-capf
              (cape-capf-buster
               (cape-capf-super
                (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)
                :with
                #'bv-cape-yasnippet
                (cape-capf-prefix-length #'cape-dabbrev 3))))))

  (defun bv-cape-setup-eglot ()
    "Install Eglot-first CAPFs."
    (setq-local completion-at-point-functions
                (list (bv-cape--eglot-capf))))

  (add-hook 'eglot-managed-mode-hook #'bv-cape-setup-eglot))

;;; Interactive Commands

(defun bv-cape-complete-file-or-project ()
  "Complete a project file when in a project, otherwise a file."
  (interactive)
  (if (and (fboundp 'project-current) (project-current))
      (cape-interactive #'bv-cape-project-file #'cape-file)
    (cape-interactive #'cape-file)))

(defun bv-cape-dabbrev-all-buffers ()
  "Complete dabbrev from all buffers."
  (interactive)
  (let ((cape-dabbrev-buffer-function #'buffer-list))
    (cape-interactive #'cape-dabbrev)))

(defun bv-cape-complete-in-string ()
  "Force file completion inside a string."
  (interactive)
  (cape-interactive (cape-capf-inside-string #'cape-file)))

(defun bv-cape-complete-elisp-anywhere ()
  "Complete Elisp symbols regardless of context."
  (interactive)
  (cape-interactive (cape-capf-accept-all #'cape-elisp-symbol)))

(defun bv-cape-clear-caches ()
  "Clear BV Cape caches."
  (interactive)
  (setq bv-cape--project-files-cache (make-hash-table :test #'equal :weakness 'key)
        bv-cape--project-cache-time (make-hash-table :test #'equal)
        bv-cape--snippet-cache (make-hash-table :test #'eq)
        bv-cape--snippet-cache-time (make-hash-table :test #'eq))
  (message "Cape caches cleared"))

(defun bv-cape-cache-info ()
  "Display BV Cape cache information."
  (interactive)
  (message "Cape caches: %d projects, %d snippet modes"
           (hash-table-count bv-cape--project-files-cache)
           (hash-table-count bv-cape--snippet-cache)))

;;; Keymap

(defvar-keymap bv-cape-map
  :doc "Cape completion commands"
  "TAB" #'completion-at-point
  "d"   #'cape-dabbrev
  "D"   #'bv-cape-dabbrev-all-buffers
  "f"   #'bv-cape-complete-file-or-project
  "h"   #'cape-history
  "k"   #'cape-keyword
  "s"   #'cape-elisp-symbol
  "S"   #'bv-cape-complete-elisp-anywhere
  "e"   #'cape-elisp-block
  "l"   #'cape-line
  "a"   #'cape-abbrev
  "w"   #'cape-dict
  "'"   #'bv-cape-complete-in-string
  ":"   #'cape-emoji
  "\\"  #'cape-tex
  "&"   #'cape-sgml
  "r"   #'cape-rfc1345
  "x"   #'bv-cape-clear-caches
  "?"   #'bv-cape-cache-info)

;;; Setup

(defun bv-cape-setup-global ()
  "Install the conservative global fallback CAPF."
  (add-hook 'completion-at-point-functions bv-cape--file-capf 90))

(defun bv-cape--setup-hooks ()
  "Install Cape mode hooks."
  (add-hook 'prog-mode-hook #'bv-cape-setup-prog-mode)
  (add-hook 'emacs-lisp-mode-hook #'bv-cape-setup-elisp-mode)
  (add-hook 'lisp-interaction-mode-hook #'bv-cape-setup-elisp-mode)
  (add-hook 'ielm-mode-hook #'bv-cape-setup-elisp-mode)
  (add-hook 'text-mode-hook #'bv-cape-setup-text-mode)
  (add-hook 'shell-mode-hook #'bv-cape-setup-shell-mode)
  (add-hook 'eshell-mode-hook #'bv-cape-setup-shell-mode)
  (add-hook 'comint-mode-hook #'bv-cape-setup-shell-mode)
  (add-hook 'git-commit-mode-hook #'bv-cape-setup-git-commit-mode)
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'bv-cape-setup-org-mode))
  (with-eval-after-load 'tex-mode
    (add-hook 'latex-mode-hook #'bv-cape-setup-latex-mode))
  (with-eval-after-load 'latex
    (add-hook 'LaTeX-mode-hook #'bv-cape-setup-latex-mode))
  (with-eval-after-load 'project
    (advice-add 'project-switch-project :after
                (lambda (&rest _) (bv-cape-clear-caches)))))

(defun bv-cape-initialize ()
  "Initialize Cape integration."
  (bv-cape-setup-global)
  (bv-cape--setup-hooks)
  (global-set-key (kbd "C-c p") bv-cape-map)
  (global-set-key (kbd "M-p") bv-cape-map))

(bv-cape-initialize)

(provide 'bv-cape)
;;; bv-cape.el ends here
