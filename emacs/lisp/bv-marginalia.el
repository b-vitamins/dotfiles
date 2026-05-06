;;; bv-marginalia.el --- Width-aware Marginalia configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Marginalia annotations for the BV completion surface.
;;
;; This module keeps the minibuffer primary by making annotations adaptive:
;; compact surfaces show only high-signal metadata, normal surfaces show useful
;; context, and wide/buffer surfaces can afford richer file and buffer hints.

;;; Code:

(require 'marginalia)
(require 'cl-lib)
(require 'subr-x)
(require 'bv-completion)

;;; Declarations

(defvar completion-list-mode-map)
(defvar marginalia-annotators)
(defvar marginalia-cache-size)
(defvar marginalia-command-categories)
(defvar marginalia-field-width)
(defvar marginalia-mode)
(defvar marginalia-prompt-categories)
(defvar marginalia-separator)

(declare-function marginalia-annotate-buffer "marginalia" (cand))
(declare-function marginalia-annotate-file "marginalia" (cand))
(declare-function marginalia-annotate-symbol "marginalia" (cand))
(declare-function marginalia-cycle "marginalia" ())
(declare-function marginalia-mode "marginalia" (&optional arg))
(declare-function marginalia--affixate "marginalia" (metadata annotator cands))

;;; Settings

(defcustom bv-marginalia-large-collection-keep-commands
  '(consult-line consult-outline consult-imenu consult-imenu-multi)
  "Commands where annotations remain enabled for very large collections."
  :type '(repeat symbol)
  :group 'marginalia)

(setq marginalia-align 'right
      marginalia-align-offset 0
      marginalia-field-width 96
      marginalia-separator "  "
      marginalia-max-relative-age (* 60 60 24 30)
      marginalia-cache-size 240)

;;; Categories

(defun bv-marginalia--put-category (alist-var key value)
  "Set KEY to VALUE in ALIST-VAR, preserving unrelated package defaults."
  (let ((alist (symbol-value alist-var)))
    (setf (alist-get key alist nil nil #'equal) value)
    (set alist-var alist)))

(dolist (entry '(("\\<branch\\>" . branch)
                 ("\\<tag\\>" . tag)
                 ("\\<commit\\>" . commit)
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
                 ("\\<director\\(y\\|ies\\)\\>" . file)
                 ("\\<file\\>" . file)
                 ("\\<project\\>" . project-file)))
  (bv-marginalia--put-category 'marginalia-prompt-categories
                               (car entry)
                               (cdr entry)))

(dolist (entry '((imenu . imenu)
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
                 (projectile-switch-to-buffer . buffer)))
  (bv-marginalia--put-category 'marginalia-command-categories
                               (car entry)
                               (cdr entry)))

;;; Formatting

(defun bv-marginalia--field (text width face)
  "Return TEXT truncated to WIDTH and propertized with FACE."
  (when (and text (not (string-empty-p text)))
    (propertize (bv-completion-truncate text width) 'face face)))

(defun bv-marginalia--annotation (&rest fields)
  "Build a Marginalia annotation from FIELDS with an alignment marker."
  (when-let ((body (string-join (delq nil fields) marginalia-separator)))
    (unless (string-empty-p body)
      (concat (propertize " " 'marginalia--align t) body))))

(defun bv-marginalia--buffer-for-candidate (cand)
  "Return the buffer represented by CAND, if any."
  (or (and (stringp cand)
           (get-text-property 0 'uniquify-orig-buffer cand))
      (and (stringp cand) (get-buffer cand))))

(defun bv-marginalia--buffer-status (buffer)
  "Return compact status text for BUFFER."
  (cond
   ((buffer-modified-p buffer) "*")
   ((get-buffer-process buffer) "proc")
   (t "-")))

(defun bv-marginalia--buffer-path (buffer width)
  "Return BUFFER path context truncated to WIDTH."
  (when-let ((file (or (buffer-file-name buffer)
                       (buffer-local-value 'default-directory buffer))))
    (let ((path (abbreviate-file-name file)))
      (if (eq (bv-completion-width-class) 'wide)
          (bv-completion-truncate path width)
        (file-name-nondirectory
         (directory-file-name
          (or (file-name-directory (directory-file-name path)) path)))))))

(defun bv-marginalia-annotate-buffer (cand)
  "Width-aware buffer annotation for CAND."
  (when-let ((buffer (bv-marginalia--buffer-for-candidate cand)))
    (when (buffer-live-p buffer)
      (let* ((width (bv-completion-annotation-width))
             (class (bv-completion-width-class))
             (mode (string-remove-suffix
                    "-mode"
                    (symbol-name (buffer-local-value 'major-mode buffer))))
             (status (bv-marginalia--buffer-status buffer))
             (path-width (pcase class
                           ('compact 0)
                           ('wide (max 24 (- width 28)))
                           (_ (max 12 (- width 22)))))
             (path (and (> path-width 0)
                        (bv-marginalia--buffer-path buffer path-width))))
        (bv-marginalia--annotation
         (bv-marginalia--field status 4
                               (if (string= status "*")
                                   'marginalia-modified
                                 'marginalia-documentation))
         (bv-marginalia--field mode
                               (pcase class
                                 ('compact 12)
                                 ('wide 24)
                                 (_ 18))
                               'marginalia-mode)
         (bv-marginalia--field path path-width 'marginalia-file-name))))))

(defun bv-marginalia-annotate-file (cand)
  "File annotation for CAND using Marginalia's native file mechanics."
  (marginalia-annotate-file cand))

(defun bv-marginalia-annotate-symbol (cand)
  "Symbol annotation for CAND with theme-friendly emphasis."
  (when-let ((base (marginalia-annotate-symbol cand)))
    (if (string-match "\\`[[:space:]]*\\([^[:space:]]+\\)" base)
        (progn
          (add-face-text-property (match-beginning 1) (match-end 1)
                                  'marginalia-key t base)
          base)
      base)))

;;; Annotator Registry

(defun bv-marginalia--prepend-annotator (category function)
  "Add FUNCTION as the first annotator for CATEGORY."
  (when-let ((entry (assq category marginalia-annotators)))
    (setcdr entry
            (cl-remove-duplicates
             (cons function (cdr entry))
             :test #'eq))))

(defun bv-marginalia-install-annotators ()
  "Install BV annotators without replacing Marginalia's registry."
  (bv-marginalia--prepend-annotator 'file #'bv-marginalia-annotate-file)
  (bv-marginalia--prepend-annotator 'project-file #'bv-marginalia-annotate-file)
  (bv-marginalia--prepend-annotator 'buffer #'bv-marginalia-annotate-buffer)
  (bv-marginalia--prepend-annotator 'project-buffer #'bv-marginalia-annotate-buffer)
  (bv-marginalia--prepend-annotator 'symbol #'bv-marginalia-annotate-symbol))

(bv-marginalia-install-annotators)

;;; Width and Performance

(defun bv-marginalia--setup-minibuffer-width ()
  "Make Marginalia annotation width local to this minibuffer."
  (setq-local marginalia-field-width
              (bv-completion-annotation-width
               (bv-completion-window-width))))

(defun bv-marginalia--plain-affixation (cands)
  "Return CANDS without annotations in affixation shape."
  (mapcar (lambda (cand) (list cand "" "")) cands))

(defun bv-marginalia-affixate-advice (orig-fun metadata annotator cands)
  "Apply width policy around Marginalia ORIG-FUN.
METADATA, ANNOTATOR and CANDS are the arguments to `marginalia--affixate'."
  (let ((marginalia-field-width
         (bv-completion-annotation-width
          (bv-completion-window-width))))
    (if (and (> (length cands) bv-completion-large-collection-limit)
             (not (memq this-command
                        bv-marginalia-large-collection-keep-commands)))
        (bv-marginalia--plain-affixation cands)
      (funcall orig-fun metadata annotator cands))))

(advice-add #'marginalia--affixate :around #'bv-marginalia-affixate-advice)
(add-hook 'minibuffer-setup-hook #'bv-marginalia--setup-minibuffer-width)

;;; Commands and Keys

(defun bv-marginalia-toggle ()
  "Toggle Marginalia annotations globally."
  (interactive)
  (marginalia-mode (if marginalia-mode -1 1))
  (message "Marginalia annotations %s"
           (if marginalia-mode "enabled" "disabled")))

(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
(define-key minibuffer-local-map (kbd "M-T") #'bv-marginalia-toggle)

(with-eval-after-load 'simple
  (define-key completion-list-mode-map (kbd "M-A") #'marginalia-cycle)
  (define-key completion-list-mode-map (kbd "M-T") #'bv-marginalia-toggle))

(marginalia-mode 1)

(provide 'bv-marginalia)
;;; bv-marginalia.el ends here
