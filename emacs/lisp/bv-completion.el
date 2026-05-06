;;; bv-completion.el --- Core completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Core minibuffer and completion policy.
;;
;; This file owns the shared surface rules used by Vertico, Consult,
;; Marginalia, Corfu, Cape, Embark, and Orderless.  Package-specific files
;; should ask this layer about width, truncation, icons, and surface policy
;; instead of hard-coding their own numbers.

;;; Code:

(require 'seq)
(require 'subr-x)

;;; Declarations

(defvar minibuffer-history-variable)
(defvar tab-always-indent)
(defvar minibuffer-prompt-properties)
(defvar completion-show-help)
(defvar completions-format)
(defvar completions-header-format)
(defvar minibuffer-mode-map)
(defvar completion-in-region-mode-map)
(defvar file-name-shadow-properties)
(defvar file-name-shadow-tty-properties)
(defvar minibuffer-local-completion-map)
(defvar minibuffer-local-map)
(defvar enable-recursive-minibuffers)
(defvar history-length)

(declare-function minibufferp "subr" (&optional buffer))
(declare-function minibuffer-next-completion "minibuffer")
(declare-function minibuffer-previous-completion "minibuffer")
(declare-function file-name-shadow-mode "rfn-eshadow" (&optional arg))
(declare-function minibuffer-default-add-completions "minibuffer")
(declare-function abbreviate-file-name "files" (filename))
(declare-function exit-minibuffer "minibuffer")
(declare-function cursor-intangible-mode "simple" (&optional arg))

;;; Shared Policy

(defgroup bv-completion nil
  "Shared completion surface policy."
  :group 'minibuffer
  :prefix "bv-completion-")

(defcustom bv-completion-width-breakpoints
  '(:compact 90 :wide 140)
  "Completion width breakpoints in columns.
Widths below :compact are compact, widths at or above :wide are wide,
and the range between them is normal."
  :type '(plist :key-type symbol :value-type integer)
  :group 'bv-completion)

(defcustom bv-completion-icon-policy 'always
  "Icon policy for completion surfaces.
The default, `always', treats icons as part of candidate identity and keeps
them visible even in compact minibuffers.  `adaptive' hides icons below
`bv-completion-icon-min-width'.  `never' disables completion icons."
  :type '(choice (const :tag "Always show icons" always)
                 (const :tag "Hide icons on compact surfaces" adaptive)
                 (const :tag "Never show icons" never))
  :group 'bv-completion)

(defcustom bv-completion-icon-min-width 96
  "Minimum completion surface width for icons when policy is `adaptive'."
  :type 'integer
  :group 'bv-completion)

(defcustom bv-completion-surface-policy
  '((buffer . (consult-ripgrep
               consult-grep
               consult-git-grep
               consult-xref
               consult-compile-error
               consult-flymake))
    (minibuffer . (consult-buffer
                   consult-line
                   consult-line-multi
                   consult-imenu
                   consult-imenu-multi
                   consult-outline
                   consult-history
                   consult-yank-pop
                   consult-bookmark
                   consult-recent-file
                   execute-extended-command)))
  "Preferred display surface for completion commands.
The default is minibuffer-first.  Commands listed under `buffer' are
row-heavy enough to start in a Vertico buffer display."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'bv-completion)

(defcustom bv-completion-large-collection-limit 10000
  "Candidate count above which expensive annotations are suppressed."
  :type 'integer
  :group 'bv-completion)

(defcustom bv-completion-truncation-marker "  ->"
  "Inline marker used when completion annotation text is truncated."
  :type 'string
  :group 'bv-completion)

(defun bv-completion-window-width (&optional window)
  "Return the width of the active completion WINDOW in columns."
  (let ((win (or window
                 (active-minibuffer-window)
                 (and (minibufferp) (selected-window))
                 (selected-window))))
    (max 20 (window-width win))))

(defun bv-completion-width-class (&optional width)
  "Return the completion width class for WIDTH.
The return value is one of `compact', `normal', or `wide'."
  (let* ((width (or width (bv-completion-window-width)))
         (compact (plist-get bv-completion-width-breakpoints :compact))
         (wide (plist-get bv-completion-width-breakpoints :wide)))
    (cond
     ((< width compact) 'compact)
     ((>= width wide) 'wide)
     (t 'normal))))

(defun bv-completion-annotation-width (&optional width)
  "Return an annotation budget for completion surface WIDTH."
  (let* ((width (or width (bv-completion-window-width)))
         (class (bv-completion-width-class width)))
    (pcase class
      ('compact (max 18 (min 36 (/ width 3))))
      ('wide (max 72 (min 120 (- width 32))))
      (_ (max 42 (min 80 (/ width 2)))))))

(defun bv-completion-icons-enabled-p (&optional width)
  "Return non-nil if icons should be shown at completion WIDTH."
  (pcase bv-completion-icon-policy
    ('always t)
    ('never nil)
    ('adaptive
     (>= (or width (bv-completion-window-width)) bv-completion-icon-min-width))
    (_ t)))

(defun bv-completion-command-prefers-buffer-p (&optional command)
  "Return non-nil if COMMAND should default to a buffer completion surface."
  (memq (or command this-command)
        (alist-get 'buffer bv-completion-surface-policy)))

(defun bv-completion-truncate (text width &optional ellipsis)
  "Truncate TEXT to WIDTH columns, preserving text properties where possible.
When ELLIPSIS is nil, use the BV completion continuation marker."
  (truncate-string-to-width (or text "") (max 0 width) 0 nil
                            (if (null ellipsis)
                                bv-completion-truncation-marker
                              ellipsis)))

(defun bv-completion-pad (text width &optional face)
  "Return TEXT padded or truncated to WIDTH columns.
FACE, when non-nil, is applied to the resulting field."
  (let ((field (truncate-string-to-width
                (or text "") (max 0 width) 0 ?\s
                bv-completion-truncation-marker)))
    (if face
        (propertize field 'face face)
      field)))

(defun bv-completion-format-field (text width &optional face)
  "Return TEXT as a fixed WIDTH completion field with optional FACE."
  (bv-completion-pad text width face))

(defun bv-completion-join-fields (&rest fields)
  "Join completion FIELDS with a clean separator.
Each field is either a string or a list (TEXT WIDTH FACE)."
  (string-join
   (delq nil
         (mapcar
          (lambda (field)
            (pcase field
              (`(,text ,width ,face)
               (bv-completion-format-field text width face))
              (`(,text ,width)
               (bv-completion-format-field text width))
              ((pred stringp) (unless (string-empty-p field) field))
              (_ nil)))
          fields))
   "  "))

(defun bv-completion-format-annotation (&rest fields)
  "Return a width-aware annotation built from completion FIELDS.
Each field is accepted by `bv-completion-join-fields'.  The returned string
includes the leading completion annotation gap expected by minibuffer UIs."
  (let* ((width (bv-completion-annotation-width))
         (annotation (apply #'bv-completion-join-fields fields)))
    (unless (string-empty-p annotation)
      (concat "  " (bv-completion-truncate
                    annotation width bv-completion-truncation-marker)))))

;;; History

(defun bv-completion-sort-by-history (candidates)
  "Sort CANDIDATES by minibuffer history, putting recent items first."
  (let ((hist (and (minibufferp)
                   (symbol-value minibuffer-history-variable))))
    (if hist
        (seq-sort (lambda (a b)
                    (let ((a-pos (seq-position hist a))
                          (b-pos (seq-position hist b)))
                      (cond ((and a-pos b-pos) (< a-pos b-pos))
                            (a-pos t)
                            (b-pos nil)
                            (t (string< a b)))))
                  candidates)
      candidates)))

;;; Diagnostics

(defun bv-completion-report ()
  "Show the active completion surface policy and width diagnostics."
  (interactive)
  (let* ((width (bv-completion-window-width))
         (class (bv-completion-width-class width)))
    (with-help-window "*BV Completion Report*"
      (princ "BV Completion Report\n\n")
      (princ (format "width: %s\n" width))
      (princ (format "class: %s\n" class))
      (princ (format "annotation width: %s\n"
                     (bv-completion-annotation-width width)))
      (princ (format "icons enabled: %s\n"
                     (if (bv-completion-icons-enabled-p width) "yes" "no")))
      (princ (format "icon policy: %s\n" bv-completion-icon-policy))
      (princ (format "current command: %s\n" this-command))
      (princ (format "buffer surface: %s\n"
                     (if (bv-completion-command-prefers-buffer-p)
                         "preferred"
                       "not preferred"))))))

;;; Core minibuffer configuration

(with-eval-after-load 'minibuffer
  (setq tab-always-indent 'complete)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq completion-show-help nil
        completions-format 'one-column
        completions-header-format nil)

  (let ((map minibuffer-mode-map))
    (define-key map (vector 'remap 'next-line) #'minibuffer-next-completion)
    (define-key map (vector 'remap 'previous-line) #'minibuffer-previous-completion))

  (let ((map completion-in-region-mode-map))
    (define-key map (kbd "C-n") #'minibuffer-next-completion)
    (define-key map (kbd "C-p") #'minibuffer-previous-completion))

  (file-name-shadow-mode 1)
  (setq file-name-shadow-properties '(face file-name-shadow field shadow)
        file-name-shadow-tty-properties '(before-string "{" after-string "}"))

  (add-hook 'rfn-eshadow-setup-minibuffer-hook
            (lambda ()
              (when (eq this-command 'find-file)
                (setq-local minibuffer-default-add-function
                            (lambda ()
                              (let ((def (minibuffer-default-add-completions)))
                                (if (listp def)
                                    (mapcar #'abbreviate-file-name def)
                                  (abbreviate-file-name def))))))))

  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil))

  (define-key minibuffer-local-map (kbd "s-b") #'exit-minibuffer)

  (setq enable-recursive-minibuffers t))

(setq history-length 10000)

(provide 'bv-completion)
;;; bv-completion.el ends here
