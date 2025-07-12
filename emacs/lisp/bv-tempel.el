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

(when (boundp 'tempel-trigger-prefix)
  (setq tempel-trigger-prefix ";"))
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
  (define-key tempel-map (kbd "TAB") #'tempel-next)
  (define-key tempel-map (kbd "<tab>") #'tempel-next)
  (define-key tempel-map (kbd "S-TAB") #'tempel-previous)
  (define-key tempel-map (kbd "<backtab>") #'tempel-previous)
  (define-key tempel-map (kbd "C-g") #'tempel-abort)
  (define-key tempel-map (kbd "RET") #'tempel-done))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "t") #'tempel-insert)
    (define-key bv-app-map (kbd "T") #'tempel-expand)))

;; Enable abbrev mode globally for trigger expansion
(global-tempel-abbrev-mode 1)

;; Hook to ensure LaTeX math templates work in org-mode
(defun bv-tempel-org-mode-setup ()
  "Additional setup for tempel in org-mode."
  ;; Ensure ; doesn't conflict with org-mode
  (modify-syntax-entry ?\; "w" org-mode-syntax-table))

(add-hook 'org-mode-hook #'bv-tempel-org-mode-setup)

(provide 'bv-tempel)
;;; bv-tempel.el ends here