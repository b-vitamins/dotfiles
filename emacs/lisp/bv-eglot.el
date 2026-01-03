;;; bv-eglot.el --- Eglot LSP configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (eglot "1.0"))
;; Keywords: tools, languages

;;; Commentary:

;; This package provides configuration for Eglot, the built-in Language Server
;; Protocol client in Emacs.  It automatically starts Eglot for programming
;; modes that have LSP servers configured, with sensible defaults and key
;; bindings.

;;; Code:

(require 'eglot)
(require 'cl-lib)
(autoload 'consult-eglot-symbols "consult-eglot" nil t)
(autoload 'bv-flymake-quickfix "bv-flymake" nil t)

;; External variables
(defvar goto-map)
(defvar eglot-server-programs)
(defvar eldoc-echo-area-use-multiline-p)
(defvar eglot-confirm-server-initiated-edits)
(defvar eglot-extend-to-xref)
(defvar eglot-autoshutdown)
(defvar eglot-sync-connect)
(defvar eglot-events-buffer-size)
(defvar eglot-report-progress)
(defvar eglot-send-changes-idle-time)
(defvar eglot-mode-map)
(defvar bv-app-map)

;; External functions
(declare-function eglot-ensure "eglot")
(declare-function eglot-rename "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-format "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function xref-find-definitions "xref" (&optional identifier))
(declare-function xref-find-definitions-other-window "xref" (&optional identifier))
(declare-function xref-find-references "xref" (&optional identifier))
(declare-function eldoc-doc-buffer "eldoc" (&optional interactive))

(defun bv-eglot-peek-definition ()
  "Peek definition at point via Xref (Consult provides the UI)."
  (interactive)
  (call-interactively #'xref-find-definitions))

(defun bv-eglot-peek-definition-other-window ()
  "Peek definition at point in other window."
  (interactive)
  (call-interactively #'xref-find-definitions-other-window))

(defun bv-eglot-peek-references ()
  "Peek references at point via Xref (Consult provides the UI)."
  (interactive)
  (call-interactively #'xref-find-references))

(defun bv-eglot-peek-diagnostics (&optional project)
  "Peek diagnostics via Consult/Flymake.

With prefix argument PROJECT, show project diagnostics."
  (interactive "P")
  (cond
   ((fboundp 'bv-flymake-quickfix) (bv-flymake-quickfix project))
   ((fboundp 'consult-flymake) (consult-flymake project))
   (project (flymake-show-project-diagnostics))
   (t (flymake-show-buffer-diagnostics))))

(defun bv-eglot-doc-buffer ()
  "Show full documentation at point in a dedicated buffer."
  (interactive)
  (if (fboundp 'eldoc-doc-buffer)
      (call-interactively #'eldoc-doc-buffer)
    (user-error "Eldoc doc buffer is unavailable in this Emacs")))

(when (boundp 'goto-map)
  (define-key goto-map (kbd "s") 'consult-eglot-symbols))

(when (boundp 'eldoc-echo-area-use-multiline-p)
  (setq eldoc-echo-area-use-multiline-p nil))
(when (boundp 'eglot-confirm-server-initiated-edits)
  (setq eglot-confirm-server-initiated-edits nil))
(when (boundp 'eglot-extend-to-xref)
  (setq eglot-extend-to-xref t))
(when (boundp 'eglot-autoshutdown)
  (setq eglot-autoshutdown t))
(when (boundp 'eglot-sync-connect)
  (setq eglot-sync-connect nil))
(when (boundp 'eglot-events-buffer-size)
  ;; Keep the events buffer essentially off unless explicitly debugging.
  (setq eglot-events-buffer-size 0))
(when (boundp 'eglot-report-progress)
  ;; Avoid noisy minibuffer progress spam from some servers.
  (setq eglot-report-progress nil))
(when (boundp 'eglot-send-changes-idle-time)
  ;; Slight batching for fewer RPCs on fast typists.
  (setq eglot-send-changes-idle-time 0.2))

(defun bv-eglot-has-lsp-server-p ()
  "Check if current major mode has an LSP server configured."
  (cl-some (lambda (entry)
             (let ((modes (car entry)))
               (cond
                ((symbolp modes) (eq modes major-mode))
                ((listp modes) (memq major-mode modes))
                (t nil))))
           eglot-server-programs))

(defun bv-eglot-ensure ()
  "Start eglot in programming modes with LSP support."
  (when (and (derived-mode-p 'prog-mode)
             ;; Only start if there's a server configured for this mode
             (bv-eglot-has-lsp-server-p))
    (unless (or (derived-mode-p 'emacs-lisp-mode)
                (eq major-mode 'lisp-interaction-mode))
      (condition-case err
          (eglot-ensure)
        (error
         (message "Eglot failed to start: %s" (error-message-string err)))))))

(add-hook 'prog-mode-hook 'bv-eglot-ensure)

(with-eval-after-load 'eglot
  (when (boundp 'eglot-mode-map)
    (define-key eglot-mode-map (kbd "C-c l d") #'bv-eglot-peek-definition)
    (define-key eglot-mode-map (kbd "C-c l D") #'bv-eglot-peek-definition-other-window)
    (define-key eglot-mode-map (kbd "C-c l R") #'bv-eglot-peek-references)
    (define-key eglot-mode-map (kbd "C-c l e") #'bv-eglot-peek-diagnostics)
    (define-key eglot-mode-map (kbd "C-c l s") #'consult-eglot-symbols)
    (define-key eglot-mode-map (kbd "C-c l h") #'bv-eglot-doc-buffer)
    (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c l F") 'eglot-format-buffer)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "l") 'eglot)
    (define-key bv-app-map (kbd "L") 'eglot-shutdown)))

(provide 'bv-eglot)
;;; bv-eglot.el ends here
