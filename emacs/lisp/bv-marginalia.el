;;; bv-marginalia.el --- Enhanced Marginalia configuration  -*- lexical-binding: t -*-
;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;;; Commentary:
;; Enhanced configuration for marginalia - rich, intelligent annotations
;; in the minibuffer that provide context-aware information exactly when
;; you need it.
;;; Code:

(require 'marginalia)
(require 'cl-lib)

;; Declare external variables to avoid warnings
(defvar marginalia-cache-size)
(defvar marginalia-annotators)
(defvar completion-list-mode-map)
(defvar vertico-buffer-mode)

;; Declare external functions
(declare-function marginalia-annotate-file "marginalia")
(declare-function marginalia-annotate-buffer "marginalia")
(declare-function marginalia-annotate-symbol "marginalia")
(declare-function marginalia--affixate "marginalia")
(declare-function marginalia--remote-file-p "marginalia")
(declare-function marginalia--full-candidate "marginalia")
(declare-function customize-save-variable "cus-edit")

;;; Core Settings - Optimized defaults

(setq marginalia-align 'left              ; Left align is cleaner with Vertico
      marginalia-align-offset 0            ; No additional offset needed
      marginalia-field-width 80            ; Reasonable field width
      marginalia-separator "  "            ; Clean separator
      marginalia-max-relative-age (* 60 60 24 30) ; Show relative age up to 30 days
      marginalia-cache-size 200)           ; Larger cache for smoother scrolling

;;; Visual Enhancements - Better faces for dark themes

(with-eval-after-load 'marginalia
  ;; Make key bindings more visible
  (set-face-attribute 'marginalia-key nil
                      :foreground "#7aa2f7"
                      :weight 'bold)

  ;; Subdued documentation
  (set-face-attribute 'marginalia-documentation nil
                      :foreground "#565f89"
                      :slant 'italic)

  ;; File permissions with better contrast
  (set-face-attribute 'marginalia-file-priv-read nil :foreground "#9ece6a")
  (set-face-attribute 'marginalia-file-priv-write nil :foreground "#f7768e")
  (set-face-attribute 'marginalia-file-priv-exec nil :foreground "#7aa2f7")
  (set-face-attribute 'marginalia-file-priv-dir nil :foreground "#bb9af7")

  ;; Better file owner visibility
  (set-face-attribute 'marginalia-file-owner nil
                      :foreground "#e0af68"
                      :weight 'normal))

;;; Smart Prompt Categories - More intuitive classification

(setq marginalia-prompt-categories
      '(;; Development
        ("\\<branch\\>" . branch)
        ("\\<tag\\>" . tag)
        ("\\<commit\\>" . commit)

        ;; Enhanced defaults
        ("\\<customize group\\>" . customize-group)
        ("\\<M-x\\>" . command)
        ("\\<package\\>" . package)
        ("\\<bookmark\\>" . bookmark)
        ("\\<color\\>" . color)
        ("\\<face\\>" . face)
        ("\\<environment variable\\>" . environment-variable)
        ("\\<function\\|\\(?:hook\\|advice\\) to remove\\>" . function)
        ("\\<variable\\>" . variable)
        ("\\<input method\\>" . input-method)
        ("\\<charset\\>" . charset)
        ("\\<coding system\\>" . coding-system)
        ("\\<minor mode\\>" . minor-mode)
        ("\\<kill-ring\\>" . kill-ring)
        ("\\<tab by name\\>" . tab)
        ("\\<library\\>" . library)
        ("\\<theme\\>" . theme)

        ;; Project/directory related
        ("\\<director\\(y\\|ies\\)\\>" . file)
        ("\\<file\\>" . file)
        ("\\<project\\>" . project-file)))

;;; Command Categories - Better classification

(setq marginalia-command-categories
      '((imenu . imenu)
        (recentf-open . file)
        (where-is . command)
        (describe-face . face)
        (describe-variable . variable)
        (describe-function . function)
        (describe-command . command)
        (describe-symbol . symbol)
        (helpful-function . function)
        (helpful-macro . function)
        (helpful-command . command)
        (helpful-variable . variable)
        (projectile-find-file . project-file)
        (projectile-recentf . project-file)
        (projectile-switch-to-buffer . buffer)
        (projectile-switch-project . project)))

;;; Custom Annotators - Enhanced information display

(defun bv-marginalia-annotate-file (cand)
  "Enhanced file annotator with git status for CAND."
  (when-let ((attrs (marginalia-annotate-file cand)))
    ;; Add git status if in a git repo
    (when (and (not (marginalia--remote-file-p cand))
               (executable-find "git")
               (locate-dominating-file cand ".git"))
      (let* ((file (expand-file-name (marginalia--full-candidate cand)))
             (git-status (ignore-errors
                           (string-trim
                            (shell-command-to-string
                             (format "git status --porcelain -- %s 2>/dev/null"
                                     (shell-quote-argument file)))))))
        (when (and git-status (not (string-empty-p git-status)))
          (let ((status-char (substring git-status 0 2)))
            (setq attrs
                  (concat
                   (propertize
                    (cond
                     ((string-match-p "^??" status-char) " [?]")
                     ((string-match-p "^A" status-char) " [+]")
                     ((string-match-p "^M" status-char) " [*]")
                     ((string-match-p "^D" status-char) " [-]")
                     ((string-match-p "^R" status-char) " [→]")
                     (t (format " [%s]" (string-trim status-char))))
                    'face (cond
                           ((string-match-p "^??" status-char) 'marginalia-documentation)
                           ((string-match-p "^A" status-char) 'marginalia-on)
                           ((string-match-p "^M" status-char) 'marginalia-modified)
                           ((string-match-p "^D" status-char) 'marginalia-off)
                           (t 'marginalia-documentation)))
                   attrs))))))
    attrs))

(defun bv-marginalia-annotate-buffer (cand)
  "Enhanced buffer annotator with process info and file path for CAND."
  (let ((base-annotation (marginalia-annotate-buffer cand)))
    (when base-annotation
      ;; Add parent directory for file buffers
      (when-let* ((buffer (get-buffer cand))
                  (file (buffer-file-name buffer))
                  (dir (file-name-directory file)))
        (setq base-annotation
              (concat base-annotation
                      (propertize
                       (format " [%s]"
                               (file-name-nondirectory
                                (directory-file-name dir)))
                       'face 'marginalia-file-name))))
      base-annotation)))

(defun bv-marginalia-annotate-symbol (cand)
  "Enhanced symbol annotator with better formatting for CAND."
  (let ((base (marginalia-annotate-symbol cand)))
    ;; Make the symbol class more readable
    (when base
      (setq base (replace-regexp-in-string
                  "\\`\\(......\\)"
                  (lambda (match)
                    (propertize match 'face
                                '(:weight bold :foreground "#7dcfff")))
                  base)))
    base))

;; Register enhanced annotators
(setq marginalia-annotators
      (mapcar
       (lambda (x)
         (cond ((eq (car x) 'file)
                (append x '(bv-marginalia-annotate-file builtin none)))
               ((eq (car x) 'buffer)
                (append x '(bv-marginalia-annotate-buffer marginalia-annotate-buffer builtin none)))
               ((eq (car x) 'symbol)
                (append x '(bv-marginalia-annotate-symbol marginalia-annotate-symbol builtin none)))
               (t (append x '(builtin none)))))
       '((command marginalia-annotate-command marginalia-annotate-binding)
         (embark-keybinding marginalia-annotate-embark-keybinding)
         (customize-group marginalia-annotate-customize-group)
         (variable marginalia-annotate-variable)
         (function marginalia-annotate-function)
         (face marginalia-annotate-face)
         (color marginalia-annotate-color)
         (unicode-name marginalia-annotate-char)
         (minor-mode marginalia-annotate-minor-mode)
         (symbol marginalia-annotate-symbol)
         (environment-variable marginalia-annotate-environment-variable)
         (input-method marginalia-annotate-input-method)
         (coding-system marginalia-annotate-coding-system)
         (charset marginalia-annotate-charset)
         (package marginalia-annotate-package)
         (imenu marginalia-annotate-imenu)
         (bookmark marginalia-annotate-bookmark)
         (file marginalia-annotate-file)
         (project-file marginalia-annotate-project-file)
         (buffer marginalia-annotate-buffer)
         (library marginalia-annotate-library)
         (theme marginalia-annotate-theme)
         (tab marginalia-annotate-tab)
         (multi-category marginalia-annotate-multi-category))))

;;; Truncation Settings - Smart truncation for different categories

(defun bv-marginalia-truncate-file-name (file-name)
  "Intelligently truncate FILE-NAME to fit available space."
  (let* ((max-width (/ marginalia-field-width 2))
         (len (length file-name)))
    (if (<= len max-width)
        file-name
      ;; Smart truncation: show beginning and end
      (let* ((keep-start (/ max-width 2))
             (keep-end (- max-width keep-start 3)))
        (concat (substring file-name 0 keep-start)
                "..."
                (substring file-name (- len keep-end)))))))

;;; Integration Helpers

(defun bv-marginalia-in-minibuffer-p ()
  "Check if we're in a minibuffer."
  (minibufferp))

(defun bv-marginalia-toggle ()
  "Toggle between all annotators and no annotators."
  (interactive)
  (if (cl-some (lambda (x) (eq (cadr x) 'none)) marginalia-annotators)
      ;; Currently disabled, enable all
      (progn
        (mapc (lambda (x)
                (setcdr x (remq 'none (cdr x))))
              marginalia-annotators)
        (message "Marginalia annotations enabled"))
    ;; Currently enabled, disable all
    (progn
      (mapc (lambda (x)
              (setcdr x (cons 'none (remq 'none (cdr x)))))
            marginalia-annotators)
      (message "Marginalia annotations disabled"))))

;;; Keybindings

(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
(define-key minibuffer-local-map (kbd "M-T") #'bv-marginalia-toggle)

;; Also make available in completion list buffer
(with-eval-after-load 'simple
  (define-key completion-list-mode-map (kbd "M-A") #'marginalia-cycle)
  (define-key completion-list-mode-map (kbd "M-T") #'bv-marginalia-toggle))

;;; Auto-save preference after cycling

(defun bv-marginalia-save-preferences ()
  "Save marginalia preferences after cycling."
  (when (bound-and-true-p marginalia-mode)
    (let ((inhibit-message t))
      (customize-save-variable 'marginalia-annotators marginalia-annotators))))

(advice-add #'marginalia-cycle :after #'bv-marginalia-save-preferences)

;;; Performance optimizations for large collections

(defun bv-marginalia-inhibit-for-large-collections (orig-fun &rest args)
  "Inhibit annotations for very large candidate collections.
Applies ORIG-FUN to ARGS only if collection is reasonably sized."
  (if (and (> (length (car args)) 10000)
           (not (eq this-command 'consult-line))) ; Keep for consult-line
      nil
    (apply orig-fun args)))

(advice-add #'marginalia--affixate :around #'bv-marginalia-inhibit-for-large-collections)

;;; Mode activation

(marginalia-mode 1)

;; Ensure marginalia works well with our Vertico enhancements
(with-eval-after-load 'vertico
  ;; Adjust field width based on window width when using vertico-buffer
  (declare-function bv-marginalia-adjust-field-width "bv-marginalia")
  (defun bv-marginalia-adjust-field-width ()
    "Adjust marginalia field width for vertico-buffer display."
    (when (bound-and-true-p vertico-buffer-mode)
      (setq-local marginalia-field-width
                  (min 120 (- (window-width) 20)))))

  (add-hook 'vertico-buffer-mode-hook #'bv-marginalia-adjust-field-width))

(provide 'bv-marginalia)
;;; bv-marginalia.el ends here
