;;; bv-cape.el --- High-performance Cape completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (cape "2.0"))

;;; Commentary:

;; This package provides an optimized Cape configuration with intelligent
;; caching, lazy evaluation, and context-aware completion.  It follows Cape
;; best practices while focusing on performance and usability.
;;
;; Features:
;; - Smart caching with expiration for project files and snippets
;; - Context-sensitive completion (comments, strings, code)
;; - Mode-specific configurations
;; - Lazy loading of heavy completions
;; - Robust error handling

;;; Code:

(require 'cape)
(require 'cl-lib)

;; External function declarations
(declare-function yas--all-templates "yasnippet" (&optional tables))
(declare-function yas--get-snippet-tables "yasnippet" (&optional mode))
(declare-function yas-lookup-snippet "yasnippet" (name &optional mode noerror))
(declare-function yas-expand-snippet "yasnippet" (template &optional start end expand-env))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function project-files "project" (project &optional dirs))
(declare-function eglot-completion-at-point "eglot" ())
(declare-function cape-keyword "cape" (&optional arg))
(declare-function cape-tex "cape" (&optional arg))
(declare-function cape-emoji "cape" (&optional arg))
(declare-function cape-sgml "cape" (&optional arg))
(declare-function cape-rfc1345 "cape" (&optional arg))

;; External variable declarations
(defvar yas-minor-mode)
(defvar cape-tex-prefix-required)

;;; Constants

(defconst bv-cape-cache-duration 30
  "Cache duration in seconds for time-sensitive data.")

(defconst bv-cape-project-cache-duration 60
  "Cache duration in seconds for project files.")

;;; Cache Variables

(defvar bv-cape--dict-cache nil
  "Cached dictionary file path.")

(defvar bv-cape--project-files-cache (make-hash-table :test #'equal :weakness 'key)
  "Cache for project files by project root.")

(defvar bv-cape--project-cache-time (make-hash-table :test #'equal)
  "Cache timestamps for project files.")

(defvar bv-cape--snippet-cache (make-hash-table :test #'eq)
  "Cache for yasnippet completions by mode.")

(defvar bv-cape--snippet-cache-time (make-hash-table :test #'eq)
  "Cache timestamps for snippets.")

;;; Utility Functions

(defun bv-cape--cache-expired-p (key time-table duration)
  "Check if cache for KEY in TIME-TABLE is older than DURATION seconds."
  (let ((cache-time (gethash key time-table 0))
        (current-time (float-time)))
    (> (- current-time cache-time) duration)))

(defun bv-cape--find-dict ()
  "Find dictionary file with caching."
  (or bv-cape--dict-cache
      (setq bv-cape--dict-cache
            (or (getenv "WORDLIST")
                (cl-find-if #'file-exists-p
                            '("/usr/share/dict/words"
                              "/usr/share/dict/american-english"
                              "/usr/share/dict/british-english"
                              "/usr/share/dict/cracklib-small"))))))

;;; Core Settings

(setq cape-dict-file #'bv-cape--find-dict
      cape-dict-limit 100
      cape-dict-case-fold 'case-fold-search
      cape-dabbrev-buffer-function #'cape-same-mode-buffers
      cape-file-directory-must-exist t)

;;; Custom Capfs

(defun bv-cape-yasnippet ()
  "Yasnippet completion with mode-specific caching and error handling."
  (when (and (featurep 'yasnippet)
             (bound-and-true-p yas-minor-mode))
    (condition-case nil
        (let* ((mode major-mode)
               (bounds (bounds-of-thing-at-point 'symbol))
               (start (or (car bounds) (point)))
               (end (or (cdr bounds) (point))))
          ;; Update cache if expired or missing
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
              :annotation-function ,(lambda (_) " Snippet")
              :company-kind ,(lambda (_) 'snippet)
              :category cape-yasnippet
              :exit-function
              ,(lambda (str status)
                 (when (and (eq status 'finished)
                            (fboundp 'yas-expand-snippet)
                            (fboundp 'yas-lookup-snippet))
                   (when-let ((snippet (yas-lookup-snippet str)))
                     (yas-expand-snippet snippet))))
              :exclusive no)))
      (error nil))))

(defun bv-cape-project-file ()
  "Project file completion with time-based cache invalidation."
  (when-let ((project (and (fboundp 'project-current) (project-current))))
    (condition-case nil
        (let* ((root (project-root project))
               (bounds (or (bounds-of-thing-at-point 'filename)
                          (cons (point) (point))))
               files)
          ;; Refresh cache if expired
          (when (bv-cape--cache-expired-p root bv-cape--project-cache-time
                                          bv-cape-project-cache-duration)
            (remhash root bv-cape--project-files-cache))

          ;; Get or compute files
          (setq files
                (or (gethash root bv-cape--project-files-cache)
                    (let ((pfiles (when (fboundp 'project-files)
                                    (mapcar (lambda (f)
                                              (file-relative-name f root))
                                            (project-files project)))))
                      (puthash root (float-time) bv-cape--project-cache-time)
                      (puthash root pfiles bv-cape--project-files-cache)
                      pfiles)))

          (when files
            `(,(car bounds) ,(cdr bounds)
              ,files
              :annotation-function ,(lambda (_) " Project")
              :company-kind ,(lambda (_) 'file)
              :category project-file
              :exclusive no)))
      (error nil))))

;;; Predicates

(defun bv-cape--not-keyword-p (sym)
  "Return non-nil if SYM is not a keyword."
  (not (keywordp sym)))

(defun bv-cape--valid-elisp-symbol-p (sym)
  "Return non-nil if SYM is a valid Elisp symbol for completion."
  (and (not (keywordp sym))
       (or (boundp sym)
           (fboundp sym)
           (featurep sym)
           (facep sym))))

;;; Context-Aware Super Capfs

(defconst bv-cape--elisp-code-super
  (cape-capf-super
   (cape-capf-predicate #'cape-elisp-symbol #'bv-cape--valid-elisp-symbol-p)
   :with
   #'cape-keyword)
  "Elisp completion for code context.")

(defconst bv-cape--elisp-comment-super
  (cape-capf-super
   #'cape-dabbrev
   :with
   #'cape-dict)
  "Elisp completion for comments.")

(defconst bv-cape--prog-code-super
  (cape-capf-super
   (cape-capf-prefix-length #'cape-keyword 2)
   :with
   #'cape-dabbrev)
  "Programming mode completion for code.")

(defconst bv-cape--prog-comment-super
  (cape-capf-super
   #'cape-dabbrev
   :with
   (cape-capf-prefix-length #'cape-dict 3))
  "Programming mode completion for comments.")

(defconst bv-cape--text-super
  (cape-capf-super
   #'cape-dabbrev
   :with
   (cape-capf-prefix-length #'cape-dict 3))
  "Text mode completion.")

;;; Lazy-Loaded Super Capfs

(defvar bv-cape--org-super nil)
(defun bv-cape--get-org-super ()
  "Get or create Org mode super-capf."
  (or bv-cape--org-super
      (setq bv-cape--org-super
            (cape-capf-super
             #'cape-elisp-block
             :with
             #'cape-dabbrev
             (cape-capf-prefix-length #'cape-tex 2)))))

(defvar bv-cape--shell-super nil)
(defun bv-cape--get-shell-super ()
  "Get or create shell mode super-capf."
  (or bv-cape--shell-super
      (setq bv-cape--shell-super
            (cape-capf-super
             #'cape-history
             :with
             #'cape-file
             #'cape-dabbrev))))

(defvar bv-cape--latex-super nil)
(defun bv-cape--get-latex-super ()
  "Get or create LaTeX mode super-capf."
  (or bv-cape--latex-super
      (setq bv-cape--latex-super
            (cape-capf-super
             (cape-capf-prefix-length #'cape-tex 1)
             :with
             #'cape-dabbrev
             (cape-capf-prefix-length #'cape-dict 4)))))

;;; Mode Setup Functions

(defun bv-cape-setup-with-capf (capf)
  "Setup completion with CAPF, making it non-exclusive."
  (setq-local completion-at-point-functions
              (list (cape-capf-properties capf :exclusive 'no))))

(defun bv-cape-setup-prog-mode ()
  "Setup Cape for programming modes with context awareness."
  (setq-local completion-at-point-functions
              (list (cape-capf-inside-code bv-cape--prog-code-super)
                    (cape-capf-inside-comment bv-cape--prog-comment-super)
                    (cape-capf-prefix-length #'cape-file 3))))

(defun bv-cape-setup-elisp-mode ()
  "Setup Cape for Emacs Lisp mode with context awareness."
  (setq-local completion-at-point-functions
              (list (cape-capf-inside-code bv-cape--elisp-code-super)
                    (cape-capf-inside-comment bv-cape--elisp-comment-super)
                    (cape-capf-inside-string #'cape-file))))

(defun bv-cape-setup-text-mode ()
  "Setup Cape for text modes."
  (bv-cape-setup-with-capf bv-cape--text-super))

(defun bv-cape-setup-org-mode ()
  "Setup Cape for Org mode."
  (bv-cape-setup-with-capf (bv-cape--get-org-super)))

(defun bv-cape-setup-shell-mode ()
  "Setup Cape for shell modes."
  (bv-cape-setup-with-capf (bv-cape--get-shell-super)))

(defun bv-cape-setup-latex-mode ()
  "Setup Cape for LaTeX mode with TeX-specific settings."
  (setq-local cape-tex-prefix-required nil)
  (setq-local completion-at-point-functions
              (list (cape-capf-properties (bv-cape--get-latex-super)
                                          :exclusive 'no))))

;;; Eglot Integration

(defvar bv-cape--eglot-super nil)

;; Forward declaration for byte compiler
(declare-function bv-cape--get-eglot-super "bv-cape" ())
(declare-function bv-cape-setup-eglot "bv-cape" ())

(with-eval-after-load 'eglot
  (defun bv-cape--get-eglot-super ()
    "Get or create Eglot super-capf with yasnippet support."
    (or bv-cape--eglot-super
        (setq bv-cape--eglot-super
              (cape-capf-super
               (cape-capf-properties #'eglot-completion-at-point :exclusive 'no)
               :with
               #'bv-cape-yasnippet
               (cape-capf-prefix-length #'cape-dabbrev 3)))))

  (defun bv-cape-setup-eglot ()
    "Setup Cape for Eglot-managed buffers."
    (setq-local completion-at-point-functions
                (list (cape-capf-buster
                       (cape-capf-properties (bv-cape--get-eglot-super)
                                             :exclusive 'no)))))

  (add-hook 'eglot-managed-mode-hook #'bv-cape-setup-eglot))

;;; Interactive Commands

(defun bv-cape-complete-file-or-project ()
  "Complete file with project awareness."
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
  "Force completion inside string."
  (interactive)
  (cape-interactive (cape-capf-inside-string #'cape-file)))

(defun bv-cape-complete-elisp-anywhere ()
  "Complete Elisp symbol anywhere."
  (interactive)
  (cape-interactive (cape-capf-accept-all #'cape-elisp-symbol)))

;;; Cache Management

(defun bv-cape-clear-caches ()
  "Clear all Cape caches."
  (interactive)
  (setq bv-cape--project-files-cache (make-hash-table :test #'equal :weakness 'key)
        bv-cape--project-cache-time (make-hash-table :test #'equal)
        bv-cape--snippet-cache (make-hash-table :test #'eq)
        bv-cape--snippet-cache-time (make-hash-table :test #'eq))
  (message "Cape caches cleared"))

(defun bv-cape-cache-info ()
  "Display information about Cape caches."
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

;;; Global Setup

(defun bv-cape-setup-global ()
  "Minimal global setup - only file completion."
  (add-hook 'completion-at-point-functions
            (cape-capf-prefix-length #'cape-file 3)
            10))  ; Low priority

;;; Mode Hooks

(defun bv-cape--setup-hooks ()
  "Setup all mode hooks."
  ;; Programming modes
  (add-hook 'prog-mode-hook #'bv-cape-setup-prog-mode)

  ;; Elisp modes
  (add-hook 'emacs-lisp-mode-hook #'bv-cape-setup-elisp-mode)
  (add-hook 'lisp-interaction-mode-hook #'bv-cape-setup-elisp-mode)
  (add-hook 'ielm-mode-hook #'bv-cape-setup-elisp-mode)

  ;; Text modes
  (add-hook 'text-mode-hook #'bv-cape-setup-text-mode)

  ;; Org mode
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'bv-cape-setup-org-mode))

  ;; Shell modes
  (add-hook 'shell-mode-hook #'bv-cape-setup-shell-mode)
  (add-hook 'eshell-mode-hook #'bv-cape-setup-shell-mode)
  (add-hook 'comint-mode-hook #'bv-cape-setup-shell-mode)

  ;; LaTeX modes
  (with-eval-after-load 'tex-mode
    (add-hook 'latex-mode-hook #'bv-cape-setup-latex-mode))
  (with-eval-after-load 'latex
    (add-hook 'LaTeX-mode-hook #'bv-cape-setup-latex-mode))

  ;; Project switch hook
  (with-eval-after-load 'project
    (when (fboundp 'project-switch-project)
      (advice-add 'project-switch-project :after
                  (lambda (&rest _) (bv-cape-clear-caches))))))

;;; Initialization

(defun bv-cape-initialize ()
  "Initialize bv-cape configuration."
  ;; Setup global configuration
  (bv-cape-setup-global)

  ;; Bind keys
  (global-set-key (kbd "C-c p") bv-cape-map)
  (global-set-key (kbd "M-p") bv-cape-map)

  ;; Setup hooks after a short delay
  (run-with-idle-timer 0.5 nil #'bv-cape--setup-hooks))

;; Initialize on load
(bv-cape-initialize)

(provide 'bv-cape)
;;; bv-cape.el ends here
