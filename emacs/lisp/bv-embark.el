;;; bv-embark.el --- Embark contextual actions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Embark provides contextual actions for minibuffer candidates and things at
;; point.  This configuration stays close to Embark's public extension points:
;; action keymaps, target finders, indicators, collect/live/export, and
;; embark-consult integration.

;;; Code:

(require 'embark)
(require 'embark-consult)
(require 'subr-x)
(require 'url-util)

;;; Declarations

(defvar embark-buffer-map)
(defvar embark-command-map)
(defvar embark-expression-map)
(defvar embark-file-map)
(defvar embark-general-map)
(defvar embark-indicators)
(defvar bv-action-map)
(defvar embark-keymap-alist)
(defvar embark-mixed-indicator-delay)
(defvar embark-symbol-map)
(defvar embark-target-finders)
(defvar embark-url-map)
(defvar embark-verbose-indicator-display-action)

(declare-function which-key--hide-popup-ignore-command "which-key" ())
(declare-function which-key--show-keymap
                  "which-key"
                  (keymap-name keymap &optional prior-args all no-paging filter))

(autoload 'helpful-symbol "helpful" nil t)
(autoload 'magit-file-dispatch "magit" nil t)
(autoload 'magit-status "magit" nil t)

;;; Indicators

(defun bv-embark--which-key-target-title (targets)
  "Return a compact which-key title for Embark TARGETS."
  (let* ((target (car targets))
         (type (plist-get target :type))
         (value (substring-no-properties
                 (format "%s" (plist-get target :target)))))
    (if (eq type 'embark-become)
        "Become"
      (format "Act on %s '%s'"
              type
              (truncate-string-to-width value 60 nil nil t)))))

(defun bv-embark-which-key-indicator ()
  "Return an Embark indicator that displays action maps with which-key."
  (if (and (require 'which-key nil t)
           (fboundp 'which-key--show-keymap)
           (fboundp 'which-key--hide-popup-ignore-command))
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (let ((map (if prefix
                         (pcase (lookup-key keymap prefix 'accept-default)
                           ((and (pred keymapp) km) km)
                           (_ (key-binding prefix 'accept-default)))
                       keymap)))
            (when (keymapp map)
              (which-key--show-keymap
               (bv-embark--which-key-target-title targets)
               map
               nil
               nil
               t
               (lambda (binding)
                 (not (string-suffix-p "-argument"
                                       (or (cdr-safe binding) "")))))))))
    (embark-mixed-indicator)))

;;; Core Settings

(setq embark-quit-after-action
      '((embark-copy-as-kill . t)
        (bv-embark-smart-copy . t)
        (bv-embark-copy-as-markdown-link . t)
        (kill-buffer . t)
        (embark-kill-buffer-and-window . t)
        (t . nil))
      embark-indicators
      '(bv-embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator)
      embark-cycle-key "."
      embark-help-key "C-h"
      embark-confirm-act-all t
      embark-mixed-indicator-delay 0.45
      embark-verbose-indicator-display-action
      '(display-buffer-below-selected
        (window-height . fit-window-to-buffer))
      prefix-help-command #'embark-prefix-help-command)

;;; Actions

(defun bv-embark-smart-copy (target)
  "Copy TARGET without text properties."
  (interactive "sCopy: ")
  (kill-new (substring-no-properties target))
  (message "Copied: %s" target))

(defun bv-embark-copy-as-markdown-link (target)
  "Copy TARGET as a Markdown link."
  (interactive "sTarget: ")
  (let* ((plain (substring-no-properties target))
         (label (if (file-exists-p plain)
                    (file-name-nondirectory (directory-file-name plain))
                  plain))
         (link (if (string-match-p "\\`https?://" plain)
                   (format "[%s](%s)" label plain)
                 (format "[%s](%s)" label (abbreviate-file-name plain)))))
    (kill-new link)
    (message "Copied Markdown link: %s" link)))

(defun bv-embark-google (target)
  "Search Google for TARGET."
  (interactive "sGoogle: ")
  (browse-url
   (concat "https://www.google.com/search?q="
           (url-hexify-string (substring-no-properties target)))))

(defun bv-embark-github-search (target)
  "Search GitHub for TARGET."
  (interactive "sGitHub: ")
  (browse-url
   (concat "https://github.com/search?q="
           (url-hexify-string (substring-no-properties target)))))

;;; Keymaps

(define-key embark-general-map "w" #'bv-embark-smart-copy)
(define-key embark-general-map "W" #'bv-embark-copy-as-markdown-link)
(define-key embark-general-map "G" #'bv-embark-google)
(define-key embark-general-map "/" #'bv-embark-github-search)

(define-key embark-file-map "g" #'magit-file-dispatch)
(define-key embark-file-map "=" #'ediff-files)

(define-key embark-buffer-map "g" #'magit-status)
(define-key embark-buffer-map "=" #'ediff-buffers)
(define-key embark-buffer-map "M-k" #'kill-buffer-and-window)

(define-key embark-symbol-map "h" #'helpful-symbol)
(define-key embark-symbol-map "." #'xref-find-definitions)
(define-key embark-symbol-map "?" #'xref-find-references)

(define-key embark-url-map "w" #'bv-embark-smart-copy)
(define-key embark-url-map "y" #'bv-embark-smart-copy)

(with-eval-after-load 'project
  (defvar-keymap bv-embark-project-file-map
    :doc "Embark actions for project files."
    :parent embark-file-map
    "p" #'project-find-file
    "d" #'project-dired
    "s" #'project-shell
    "c" #'project-compile)
  (add-to-list 'embark-keymap-alist
               '(project-file . bv-embark-project-file-map)))

;;; Targets

(defun bv-embark-target-sexp-at-point ()
  "Return an expression target for the sexp at point."
  (when-let ((bounds (ignore-errors (bounds-of-thing-at-point 'sexp))))
    (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      `(expression ,text ,(car bounds) . ,(cdr bounds)))))

(add-to-list 'embark-target-finders #'bv-embark-target-sexp-at-point 'append)

;;; Mouse

(defun bv-embark-mouse-act (event)
  "Run `embark-act' at mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (embark-act))

;;; Global Keys

(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)
(global-set-key (kbd "C-c C-.") #'embark-act-all)
(global-set-key (kbd "s-.") #'embark-act)
(global-set-key (kbd "s->") #'embark-become)
(global-set-key (kbd "C-h B") #'embark-bindings)
(global-set-key (kbd "<mouse-3>") #'bv-embark-mouse-act)

(define-key minibuffer-local-map (kbd "C-.") #'embark-act)
(define-key minibuffer-local-map (kbd "C-;") #'embark-dwim)
(define-key minibuffer-local-map (kbd "C-c C-.") #'embark-act-all)
(define-key minibuffer-local-map (kbd "s-g") #'embark-become)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-action-map)
    (define-key bv-action-map (kbd "a") #'embark-act-all)
    (define-key bv-action-map (kbd "c") #'embark-collect)
    (define-key bv-action-map (kbd "l") #'embark-live)
    (define-key bv-action-map (kbd "e") #'embark-export)))

;;; Collect Buffers

(add-hook 'embark-collect-mode-hook
          (lambda ()
            (setq-local truncate-lines t)
            (when (fboundp 'embark-collect-direct-action-minor-mode)
              (embark-collect-direct-action-minor-mode 1))))

(provide 'bv-embark)
;;; bv-embark.el ends here
