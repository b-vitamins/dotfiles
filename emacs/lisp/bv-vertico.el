;;; bv-vertico.el --- Vertico completion surface configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Vertico is the minibuffer surface.  The policy here is deliberately
;; minibuffer-first, with buffer display reserved for commands whose rows need
;; more room than the minibuffer should spend by default.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-completion)
(require 'vertico)
(require 'vertico-directory)
(require 'vertico-multiform)
(require 'vertico-repeat)
(require 'vertico-quick)
(autoload 'consult-completion-in-region "consult")

;;; Declarations

(defvar completion-in-region-function)
(defvar minor-mode-list)
(defvar vertico--candidates)
(defvar vertico--index)
(defvar vertico-map)
(defvar vertico-mode)
(defvar vertico-multiform-map)

(declare-function completion--in-region "minibuffer" (start end collection predicate))
(declare-function consult-completion-in-region "consult" (start end collection &optional predicate))
(declare-function vertico--candidate "vertico" (&optional n))
(declare-function vertico--format-candidate "vertico")
(declare-function vertico--metadata-get "vertico" (key))
(declare-function vertico--setup "vertico")
(declare-function vertico--update "vertico")
(declare-function vertico-directory-delete-char "vertico-directory")
(declare-function vertico-directory-delete-word "vertico-directory")
(declare-function vertico-directory-enter "vertico-directory")
(declare-function vertico-directory-tidy "vertico-directory")
(declare-function vertico-directory-up "vertico-directory")
(declare-function vertico-insert "vertico")
(declare-function vertico-multiform-buffer "vertico-multiform")
(declare-function vertico-multiform-grid "vertico-multiform")
(declare-function vertico-multiform-vertical "vertico-multiform")
(declare-function vertico-next "vertico" (&optional arg))
(declare-function vertico-next-group "vertico" (&optional arg))
(declare-function vertico-previous "vertico" (&optional arg))
(declare-function vertico-previous-group "vertico" (&optional arg))
(declare-function vertico-quick-jump "vertico-quick")
(declare-function vertico-repeat "vertico-repeat")
(declare-function vertico-repeat-save "vertico-repeat")
(declare-function vertico-repeat-select "vertico-repeat")
(declare-function vertico-sort-alpha "vertico")
(declare-function vertico-sort-history-length-alpha "vertico")

;;; Core Settings

(setq vertico-cycle t
      vertico-resize t
      vertico-count 12
      vertico-scroll-margin 4
      vertico-preselect 'directory
      vertico-count-format '("%-6s " . "%s/%s ")
      vertico-sort-function #'vertico-sort-history-length-alpha
      completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;;; Visual Transformations

(defun bv-vertico--prepare-header-line ()
  "Move the minibuffer mode line to the header line."
  (setq-local header-line-format mode-line-format
              mode-line-format nil))

(defun bv-vertico-highlight-directory (file)
  "Highlight FILE as a directory when it ends with slash."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'dired-directory)
    file))

(defun bv-vertico-highlight-enabled-mode (cmd)
  "Highlight CMD when it names an enabled major or minor mode."
  (let ((sym (intern-soft cmd)))
    (if (and sym
	     (or (eq sym major-mode)
	         (and (memq sym minor-mode-list)
	              (boundp sym)
	              (symbol-value sym))))
	(propertize cmd 'face 'bv-face-salient)
      cmd)))

(defvar bv-vertico-transform-functions nil
  "Candidate transform functions active for the current Vertico session.")

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start)
  "Apply `bv-vertico-transform-functions' before formatting CAND."
  (when bv-vertico-transform-functions
    (dolist (fun (ensure-list bv-vertico-transform-functions))
      (setq cand (funcall fun cand))))
  (cl-call-next-method cand prefix suffix index start))

;;; Directory Navigation

(defvar bv-vertico--previous-directory nil
  "Directory just left by `vertico-directory-up'.")

(defun bv-vertico-set-previous-directory (&rest _)
  "Remember the directory being exited."
  (when (and (< (minibuffer-prompt-end) (point))
             (eq 'file (vertico--metadata-get 'category)))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (setq bv-vertico--previous-directory
              (buffer-substring (1+ (point)) (point-max)))
        (unless (string-suffix-p "/" bv-vertico--previous-directory)
          (setq bv-vertico--previous-directory nil))))))

(defun bv-vertico-smart-slash ()
  "Insert / or descend into the selected directory."
  (interactive)
  (let ((candidate (and (>= vertico--index 0) (vertico--candidate))))
    (cond
     ((and candidate (string-suffix-p "/" candidate))
      (vertico-insert))
     ((and candidate (ignore-errors (file-directory-p candidate)))
      (vertico-insert))
     (t
      (self-insert-command 1 ?/)))))

(define-advice vertico--update (:after (&rest _) bv-choose-previous-dir)
  "Re-select the previous directory after navigating up."
  (when bv-vertico--previous-directory
    (setq vertico--index (or (seq-position vertico--candidates
                                           bv-vertico--previous-directory)
                             vertico--index)
          bv-vertico--previous-directory nil)))

;;; Session Commands

(defun bv-vertico-restrict-to-matches ()
  "Restrict future matches to the current candidate set."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert " ")
    (add-text-properties (minibuffer-prompt-end) (point-max)
                         '(invisible t read-only t cursor-intangible t
				     rear-nonsticky t))))

(defun bv-vertico-kill-region-dwim (&optional count)
  "Kill region or delete a path component.
COUNT is passed to `vertico-directory-delete-word'."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) 'region)
    (vertico-directory-delete-word count)))

(defun bv-vertico-next-from-outside ()
  "Move to the next Vertico candidate from outside the minibuffer."
  (interactive)
  (when-let ((win (active-minibuffer-window)))
    (with-selected-window win
      (vertico-next))))

(defun bv-vertico-previous-from-outside ()
  "Move to the previous Vertico candidate from outside the minibuffer."
  (interactive)
  (when-let ((win (active-minibuffer-window)))
    (with-selected-window win
      (vertico-previous))))

;;; Multiform

(setq vertico-multiform-categories
      '((file
         (:keymap . vertico-directory-map)
         (bv-vertico-transform-functions . bv-vertico-highlight-directory))
        (project-file
         (:keymap . vertico-directory-map)
         (bv-vertico-transform-functions . bv-vertico-highlight-directory))
        (buffer (vertico-sort-function . nil))
        (symbol (vertico-sort-function . vertico-sort-alpha))
        (command (vertico-sort-function . vertico-sort-history-length-alpha))
        (info-menu (vertico-sort-function . vertico-sort-alpha))))

(setq vertico-multiform-commands
      (append
       (mapcar (lambda (command) (list command 'buffer))
               (alist-get 'buffer bv-completion-surface-policy))
       '((execute-extended-command
          (bv-vertico-transform-functions . bv-vertico-highlight-enabled-mode))
         (consult-theme grid))))

;;; Keybindings

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

(define-key vertico-multiform-map (kbd "M-V") #'vertico-multiform-vertical)
(define-key vertico-multiform-map (kbd "M-G") #'vertico-multiform-grid)
(define-key vertico-multiform-map (kbd "M-B") #'vertico-multiform-buffer)

(global-set-key (kbd "s-s") #'vertico-repeat)
(global-set-key (kbd "M-R") #'vertico-repeat)
(global-set-key (kbd "C-c C-r") #'vertico-repeat-select)
(global-set-key (kbd "C-M-n") #'bv-vertico-next-from-outside)
(global-set-key (kbd "C-M-p") #'bv-vertico-previous-from-outside)

;;; Activation

(advice-add #'vertico--setup :after #'bv-vertico--prepare-header-line)
(advice-add #'vertico-directory-up :before #'bv-vertico-set-previous-directory)

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(vertico-mode 1)
(vertico-multiform-mode 1)

(provide 'bv-vertico)
;;; bv-vertico.el ends here
