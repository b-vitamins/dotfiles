;;; bv-tempel.el --- Template system configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Template expansion with tempel.

;;; Code:

(require 'tempel)
(declare-function tempel-complete "tempel")
(declare-function tempel-expand "tempel")
(declare-function tempel-insert "tempel" (template-or-name))
(declare-function tempel--templates "tempel")
(defvar user-emacs-directory)
(defvar completion-at-point-functions)

(defgroup bv-tempel nil
  "Template expansion helpers."
  :group 'convenience)

(defcustom bv-tempel-trigger-prefix ";"
  "Prefix string used to trigger Tempel expansion on TAB."
  :type 'string
  :group 'bv-tempel)
(when (boundp 'tempel-path)
  (setq tempel-path
        (list (expand-file-name "templates/*.eld" user-emacs-directory)
              ;; Optionally include external latex-templates
              ;; (expand-file-name "~/projects/latex-templates/*.eld")
              )))

;; Enable automatic reloading when template files change
(when (boundp 'tempel-auto-reload)
  (setq tempel-auto-reload t))

;; Set annotation width for completion
(when (boundp 'tempel-complete-annotation)
  (setq tempel-complete-annotation 30))

(defun bv-tempel-setup-capf ()
  "Add tempel to completion-at-point."
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    (cons #'tempel-expand
                          completion-at-point-functions))))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'bv-tempel-setup-capf))

;; Add specific support for org-mode LaTeX templates
(add-hook 'org-mode-hook #'bv-tempel-setup-capf)

(global-set-key (kbd "M-+") #'tempel-insert)
(global-set-key (kbd "M-*") #'tempel-expand)

(when (boundp 'tempel-map)
  (define-key tempel-map (kbd "TAB") #'bv-tempel-expand-trigger-or-next)
  (define-key tempel-map (kbd "<tab>") #'bv-tempel-expand-trigger-or-next)
  (define-key tempel-map (kbd "S-TAB") #'tempel-previous)
  (define-key tempel-map (kbd "<backtab>") #'tempel-previous)
  (define-key tempel-map (kbd "C-g") #'tempel-abort)
  (define-key tempel-map (kbd "RET") #'tempel-done))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "t") #'tempel-insert)
    (define-key bv-app-map (kbd "T") #'tempel-expand)))

;;;; Trigger expansion via TAB

(defun bv-tempel--trigger-at-point ()
  "Return (NAME BEG END) for a Tempel trigger at point, or nil.

This matches snippets written as `bv-tempel-trigger-prefix' followed by a
template name, e.g. \";im\"."
  (let ((prefix bv-tempel-trigger-prefix))
    (when (and (stringp prefix) (> (length prefix) 0))
      (let ((end (point)))
        (save-excursion
          (skip-syntax-backward "w_")
          (let* ((beg (point))
                 (token (and (< beg end)
                             (buffer-substring-no-properties beg end))))
            (cond
             ;; Case 1: prefix is part of the symbol/word.
             ((and (stringp token) (string-prefix-p prefix token))
              (let ((name (substring token (length prefix))))
                (and (not (string-empty-p name))
                     (list name beg end))))
             ;; Case 2: prefix is punctuation before the symbol/word.
             ((and (stringp token)
                   (>= (- beg (length prefix)) (point-min))
                   (string= prefix
                            (buffer-substring-no-properties
                             (- beg (length prefix)) beg)))
              (list token (- beg (length prefix)) end))
             (t nil))))))))

(defun bv-tempel-expand-trigger ()
  "Expand a prefixed Tempel template at point.

Returns non-nil when a template expands successfully."
  (interactive)
  (when-let* ((trigger (bv-tempel--trigger-at-point))
              (name (nth 0 trigger))
              (beg (nth 1 trigger))
              (end (nth 2 trigger))
              (templates (tempel--templates))
              (sym (intern-soft name)))
    (when (assq sym templates)
      (delete-region beg end)
      (tempel-insert sym)
      t)))

(defun bv-tempel-expand-trigger-or-next (arg)
  "Expand a prefixed Tempel template at point, or move to next field.

ARG is passed to `tempel-next'."
  (interactive "p")
  (unless (bv-tempel-expand-trigger)
    (tempel-next arg)))

(with-eval-after-load 'org
  (add-hook 'org-tab-first-hook #'bv-tempel-expand-trigger))

(defvar-local bv-tempel--tab-fallback nil
  "Original TAB command captured for a buffer-local wrapper.")

(defun bv-tempel-expand-trigger-or-tab ()
  "Expand a prefixed Tempel template, or fall back to TAB."
  (interactive)
  (unless (bv-tempel-expand-trigger)
    (call-interactively (or bv-tempel--tab-fallback #'indent-for-tab-command))))

(defun bv-tempel--wrap-tab ()
  "Wrap the current buffer's TAB binding with Tempel trigger expansion."
  (unless (eq (key-binding (kbd "TAB")) #'bv-tempel-expand-trigger-or-tab)
    (setq bv-tempel--tab-fallback (key-binding (kbd "TAB")))
    (local-set-key (kbd "TAB") #'bv-tempel-expand-trigger-or-tab)
    (local-set-key (kbd "<tab>") #'bv-tempel-expand-trigger-or-tab)))

(add-hook 'LaTeX-mode-hook #'bv-tempel--wrap-tab)
(add-hook 'latex-mode-hook #'bv-tempel--wrap-tab)

(provide 'bv-tempel)
;;; bv-tempel.el ends here
