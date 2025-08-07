;;; bv-vertico.el --- Enhanced Vertico completion configuration  -*- lexical-binding: t -*-
;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;;; Commentary:
;; Enhanced vertical completion with Vertico - a DWIM configuration that
;; incorporates the best ideas from the ecosystem for an intuitive,
;; proactively helpful completion experience.
;;; Code:

(require 'cl-lib)
(require 'vertico)
(require 'vertico-directory)
(require 'vertico-multiform)
(require 'vertico-repeat)
(require 'vertico-quick)
(autoload 'consult-completion-in-region "consult")

;; Declare external variables and functions to avoid warnings
(defvar completion-in-region-function)
(defvar minor-mode-list)
(defvar vertico--index)
(defvar vertico--candidates)
(defvar vertico-map)
(defvar vertico-multiform-map)

(declare-function vertico--metadata-get "vertico" (key))
(declare-function vertico--candidate "vertico" (&optional n))
(declare-function vertico-insert "vertico")
(declare-function vertico-next "vertico" (&optional arg))
(declare-function vertico-previous "vertico" (&optional arg))
(declare-function vertico-next-group "vertico" (&optional arg))
(declare-function vertico-previous-group "vertico" (&optional arg))
(declare-function vertico-multiform-vertical "vertico-multiform")
(declare-function vertico-multiform-grid "vertico-multiform")
(declare-function vertico-multiform-buffer "vertico-multiform")
(declare-function vertico-quick-jump "vertico-quick")
(declare-function vertico-repeat "vertico-repeat")
(declare-function vertico-repeat-save "vertico-repeat")
(declare-function vertico-repeat-select "vertico-repeat")
(declare-function vertico-directory-enter "vertico-directory")
(declare-function vertico-directory-delete-char "vertico-directory")
(declare-function vertico-directory-delete-word "vertico-directory")
(declare-function vertico-directory-up "vertico-directory")
(declare-function vertico-directory-tidy "vertico-directory")
(declare-function vertico--update "vertico")
(declare-function vertico--setup "vertico")
(declare-function vertico--format-candidate "vertico")
(declare-function vertico-sort-history-length-alpha "vertico")
(declare-function vertico-sort-alpha "vertico")

;;; Core Settings - Sensible defaults that work everywhere

(setq vertico-cycle t                    ; Enable cycling for continuous flow
      vertico-resize t                   ; Adapt to content
      vertico-count 12                   ; Goldilocks zone - not too many, not too few
      vertico-scroll-margin 4            ; Comfortable scroll margin
      vertico-preselect 'directory       ; Smart: prompt for dirs, first for files
      ;; Preserve default history/recency sorting - this is crucial!
      vertico-sort-function 'vertico-sort-history-length-alpha
      completion-in-region-function      ; Use consult for in-region completion
      (lambda (&rest args)
        (apply (if vertico-mode
                   'consult-completion-in-region
                 'completion--in-region)
               args)))

;;; Visual Enhancements - Make it beautiful and informative

;; Move mode-line to header for cleaner look (your original idea - keeping it!)
(defun bv-vertico--prepare-header-line ()
  "Move mode line to header line for less visual clutter."
  (setq-local header-line-format mode-line-format
              mode-line-format nil))

;; Highlight directories
(defun bv-vertico-highlight-directory (file)
  "Highlight FILE as directory if it ends with slash."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'dired-directory)
    file))

;; Highlight enabled modes in M-x
(defun bv-vertico-highlight-enabled-mode (cmd)
  "Highlight CMD if it's an enabled mode."
  (let ((sym (intern-soft cmd)))
    (if (and sym (or (eq sym major-mode)
                     (and (memq sym minor-mode-list)
                          (boundp sym)
                          (symbol-value sym))))
        (propertize cmd 'face 'font-lock-constant-face)
      cmd)))

;;; Directory Navigation - Ido/Ivy-like behavior

;; Previous directory tracking for smart navigation
(defvar bv-vertico--previous-directory nil
  "The directory that was just exited.")

(defun bv-vertico-set-previous-directory (&rest _)
  "Remember the directory we're leaving."
  (when (and (< (minibuffer-prompt-end) (point))
             (eq 'file (vertico--metadata-get 'category)))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (setq bv-vertico--previous-directory
              (buffer-substring (1+ (point)) (point-max)))
        (unless (string-suffix-p "/" bv-vertico--previous-directory)
          (setq bv-vertico--previous-directory nil))
        t))))

(defun bv-vertico-smart-slash ()
  "Insert / or select directory, Ido-style."
  (interactive)
  (let* ((mb (minibuffer-contents-no-properties))
         (lc (if (string= mb "") mb (substring mb -1))))
    (cond
     ;; Special paths - just insert /
     ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
     ;; Current candidate is directory - select it
     ((and (>= vertico--index 0)
           (file-directory-p (vertico--candidate)))
      (vertico-insert))
     ;; Otherwise insert /
     (t (self-insert-command 1 ?/)))))

;; Smart directory preselection
(define-advice vertico--update (:after (&rest _) bv-choose-previous-dir)
  "Select previous directory when navigating up."
  (when bv-vertico--previous-directory
    (setq vertico--index (or (seq-position vertico--candidates
                                           bv-vertico--previous-directory)
                             vertico--index))
    (setq bv-vertico--previous-directory nil)))

;;; Smart Features - Quality of life improvements

;; Restrict candidates to current matches (like Ivy's S-SPC)
(defun bv-vertico-restrict-to-matches ()
  "Restrict future matches to current candidate set."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert " ")
    (add-text-properties (minibuffer-prompt-end) (point-max)
                         '(invisible t read-only t cursor-intangible t rear-nonsticky t))))

;; Kill region DWIM - from your original config
(defun bv-vertico-kill-region-dwim (&optional count)
  "Kill region or delete word/directory intelligently.
COUNT specifies the number of words to delete when no region is active."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) 'region)
    (vertico-directory-delete-word count)))

;; Commands from outside minibuffer
(defun bv-vertico-next-from-outside ()
  "Move to next candidate from outside minibuffer."
  (interactive)
  (when-let ((win (active-minibuffer-window)))
    (with-selected-window win
      (vertico-next))))

(defun bv-vertico-previous-from-outside ()
  "Move to previous candidate from outside minibuffer."
  (interactive)
  (when-let ((win (active-minibuffer-window)))
    (with-selected-window win
      (vertico-previous))))

;;; Multiform Configuration - Conservative and smart

(setq vertico-multiform-categories
      '(;; Files stay in normal vertical mode with directory highlighting
        (file (bv-vertico-transform-functions . bv-vertico-highlight-directory))

        ;; Info menus benefit from buffer display
        (info-menu buffer)

        ;; Symbols: alphabetical sorting
        (symbol (vertico-sort-function . vertico-sort-alpha))))

(setq vertico-multiform-commands
      '(;; Your original buffer commands - these make sense!
        (consult-grep buffer)
        (consult-ripgrep buffer)
        (consult-git-grep buffer)
        (consult-line buffer)
        (consult-outline buffer)
        (consult-imenu buffer)
        (consult-org-heading buffer)
        (consult-history buffer)
        (consult-xref buffer)
        (consult-yank-pop buffer)
        (consult-bookmark buffer)
        (consult-recent-file buffer)

        ;; M-x gets mode highlighting
        (execute-extended-command
         (bv-vertico-transform-functions . bv-vertico-highlight-enabled-mode))

        ;; Theme selection in grid
        (consult-theme grid)))

;; Transform function support (simplified)
(defvar bv-vertico-transform-functions nil
  "List of functions to transform candidates in vertico.
Each function should take a candidate string and return a modified string.")

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start)
  "Apply transform functions to candidate CAND if configured.
PREFIX, SUFFIX, INDEX, and START are passed to the next method."
  (when bv-vertico-transform-functions
    (dolist (fun (ensure-list bv-vertico-transform-functions))
      (setq cand (funcall fun cand))))
  (cl-call-next-method cand prefix suffix index start))

;;; Keybindings - Intuitive and consistent

(define-key vertico-map (kbd "C-j") #'vertico-directory-enter)
(define-key vertico-map (kbd "RET") #'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "C-w") #'bv-vertico-kill-region-dwim)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(define-key vertico-map (kbd "C-l") #'vertico-directory-up)
(define-key vertico-map (kbd "/") #'bv-vertico-smart-slash)
(define-key vertico-map (kbd "S-SPC") #'bv-vertico-restrict-to-matches)
(define-key vertico-map (kbd "M-q") #'vertico-quick-jump)
(define-key vertico-map (kbd "C-M-n") #'vertico-next-group)
(define-key vertico-map (kbd "C-M-p") #'vertico-previous-group)

;; Quick multiform switching (when you need it)
(define-key vertico-multiform-map (kbd "M-V") #'vertico-multiform-vertical)
(define-key vertico-multiform-map (kbd "M-G") #'vertico-multiform-grid)
(define-key vertico-multiform-map (kbd "M-B") #'vertico-multiform-buffer)

;; Global keys
(global-set-key (kbd "s-s") #'vertico-repeat)
(global-set-key (kbd "M-R") #'vertico-repeat)
(global-set-key (kbd "C-c C-r") #'vertico-repeat-select)
(global-set-key (kbd "C-M-n") #'bv-vertico-next-from-outside)
(global-set-key (kbd "C-M-p") #'bv-vertico-previous-from-outside)

;;; Setup and Activation

;; Install advice
(advice-add 'vertico--setup :after #'bv-vertico--prepare-header-line)
(advice-add 'vertico-directory-up :before #'bv-vertico-set-previous-directory)

;; Hooks
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; Enable modes
(vertico-mode 1)
(vertico-multiform-mode 1)

(provide 'bv-vertico)
;;; bv-vertico.el ends here
