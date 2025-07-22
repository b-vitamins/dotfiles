;;; bv-embark.el --- Enhanced Embark configuration  -*- lexical-binding: t -*-
;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;;; Commentary:
;; Enhanced configuration for embark - contextual actions everywhere.
;; This configuration provides intuitive contextual actions with visual
;; feedback, smart defaults, and quality-of-life improvements that make
;; Embark feel like a natural extension of Emacs.
;;; Code:

(require 'embark)
(require 'embark-consult)

;;; Core Settings - Optimized defaults

(setq embark-quit-after-action nil         ; Stay in minibuffer by default
      embark-indicators                    ; Clean, informative indicators
      '(embark-minimal-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator)
      embark-cycle-key "."                 ; Easy cycling through targets
      embark-help-key "?"                  ; More intuitive than C-h
      embark-confirm-act-all t             ; Safety first for act-all
      prefix-help-command #'embark-prefix-help-command) ; Better prefix help

;;; Enhanced Indicators - Better visual feedback

(defun bv-embark-which-key-indicator ()
  "An embark indicator that displays keybindings in a which-key style.
This is a more sophisticated version that shows the target info too."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (let* ((current-target (car (or targets '("No target"))))
             (formatted-target (if (stringp current-target)
                                   (truncate-string-to-width current-target 40)
                                 "No target"))
             (which-key-separator " → ")
             (which-key-prefix-prefix "+"))
        (which-key--show-keymap
         (format "Embark on '%s'" formatted-target)
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding)))))))))

;; Use which-key style if available
(when (require 'which-key nil t)
  (setq embark-indicators
        '(bv-embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

;;; Smart Actions - Context-aware enhancements

(defun bv-embark-smart-copy (str)
  "Copy STR to kill ring with smart formatting.
Remove common prefixes/suffixes, clean up whitespace."
  (let* ((clean-str (string-trim str))
         ;; Remove common Emacs prefixes
         (clean-str (replace-regexp-in-string "^\\(function\\|variable\\|command\\): " "" clean-str))
         ;; Remove file extensions for certain contexts
         (clean-str (if (and (eq (embark--target-type) 'file)
                             (not (string-match-p "\\." (file-name-nondirectory clean-str))))
                        clean-str
                      clean-str)))
    (kill-new clean-str)
    (message "Copied: %s" clean-str)))

(defun bv-embark-google (term)
  "Google TERM."
  (browse-url (format "https://www.google.com/search?q=%s"
                      (url-encode-url term))))

(defun bv-embark-github-search (term)
  "Search TERM on GitHub."
  (browse-url (format "https://github.com/search?q=%s"
                      (url-encode-url term))))

(defun bv-embark-insert-relative-path ()
  "Insert relative path to target file."
  (interactive)
  (if-let* ((target (embark--target))
            (file (plist-get target :target))
            ((file-exists-p file)))
      (insert (file-relative-name file default-directory))
    (user-error "No file target found")))

(defun bv-embark-copy-as-markdown-link ()
  "Copy target as markdown link."
  (interactive)
  (let* ((target (embark--target))
         (str (plist-get target :target))
         (type (plist-get target :type)))
    (kill-new
     (pcase type
       ('url (format "[%s](%s)" (read-string "Link text: " str) str))
       ('file (format "[%s](./%s)"
                      (file-name-nondirectory str)
                      (file-relative-name str)))
       (_ (format "[%s]" str))))))

(defun bv-embark-open-externally (file)
  "Open FILE with system's default application."
  (interactive "f")
  (if (eq system-type 'darwin)
      (shell-command (format "open %s" (shell-quote-argument file)))
    (shell-command (format "xdg-open %s" (shell-quote-argument file)))))

;;; Enhanced Keymaps - More actions, better bindings

;; Enhanced general map
(define-key embark-general-map "w" #'bv-embark-smart-copy)
(define-key embark-general-map "W" #'bv-embark-copy-as-markdown-link)
(define-key embark-general-map "G" #'bv-embark-google)
(define-key embark-general-map "/" #'bv-embark-github-search)

;; Enhanced file map
(define-key embark-file-map "o" #'bv-embark-open-externally)
(define-key embark-file-map "I" #'bv-embark-insert-relative-path)
(define-key embark-file-map "=" #'ediff-files)
(define-key embark-file-map "g" #'magit-file-dispatch)

;; Enhanced buffer map
(define-key embark-buffer-map "g" #'magit-status)
(define-key embark-buffer-map "=" #'ediff-buffers)
(define-key embark-buffer-map "M-k" #'kill-buffer-and-window)

;; Enhanced symbol map
(define-key embark-symbol-map "h" #'helpful-symbol)
(define-key embark-symbol-map "." #'xref-find-definitions)
(define-key embark-symbol-map "?" #'xref-find-references)

;; URL map improvements
(define-key embark-url-map "w" #'bv-embark-smart-copy)
(define-key embark-url-map "y" #'bv-embark-smart-copy) ; Alternative binding

;;; Project Integration - Smart project actions

(with-eval-after-load 'project
  (defvar-keymap embark-project-map
    :doc "Keymap for Embark project actions."
    :parent embark-file-map
    "p" #'project-find-file
    "g" #'project-find-regexp
    "r" #'project-query-replace-regexp
    "d" #'project-dired
    "v" #'project-vc-dir
    "s" #'project-shell
    "e" #'project-eshell
    "c" #'project-compile
    "!" #'project-shell-command
    "&" #'project-async-shell-command)

  (defun embark-project-file-p (file)
    "Check if FILE is part of a project."
    (and (file-exists-p file)
         (project-current nil (file-name-directory file))))

  (add-to-list 'embark-keymap-alist '(project-file . embark-project-map))
  (add-to-list 'embark-target-injection-hooks
               '(project-find-file embark--allow-edit embark--project-file-completion)))

;;; Live Occur Improvements

(defun bv-embark-live-occur-buffer-p ()
  "Check if there's a live occur buffer for current minibuffer."
  (and (minibufferp)
       (get-buffer "*Embark Live Occur*")))

(defun bv-embark-toggle-live-occur ()
  "Toggle Embark Live Occur buffer."
  (interactive)
  (if (bv-embark-live-occur-buffer-p)
      (kill-buffer "*Embark Live Occur*")
    (embark-live)))

;;; Better Quit Behavior - Context-aware

(setq embark-quit-after-action
      '((kill-buffer . t)
        (kill-buffers . t)
        (embark-kill-buffer-and-window . t)
        (bookmark-delete . t)
        (package-delete . t)
        (t . nil))) ; Don't quit by default

;;; Minibuffer Actions Enhancement

(defun bv-embark-act-on-minibuffer-contents ()
  "Act on the entire minibuffer contents as a single target."
  (interactive)
  (let ((contents (minibuffer-contents-no-properties)))
    (embark--act (list :target contents :type 'general))))

;;; Integration with Consult

(with-eval-after-load 'consult
  ;; Make consult-ripgrep and friends work better as actions
  (cl-pushnew #'embark--mark-target
              (alist-get 'consult-ripgrep embark-around-action-hooks))
  (cl-pushnew #'embark--mark-target
              (alist-get 'consult-git-grep embark-around-action-hooks))
  (cl-pushnew #'embark--mark-target
              (alist-get 'consult-grep embark-around-action-hooks))

  ;; Preview when collecting
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Mouse Support - Right-click context menu

(defun bv-embark-mouse-act (event)
  "Run embark-act at EVENT position."
  (interactive "e")
  (let ((window (posn-window (event-start event)))
        (position (posn-point (event-start event))))
    (with-selected-window window
      (goto-char position)
      (embark-act))))

;;; Org Mode Integration

(with-eval-after-load 'org
  (defvar-keymap embark-org-heading-map
    :doc "Keymap for Embark actions on Org headings."
    :parent embark-general-map
    "RET" #'org-open-at-point
    "TAB" #'org-cycle
    "t" #'org-todo
    ":" #'org-set-tags
    "d" #'org-deadline
    "s" #'org-schedule
    "a" #'org-archive-subtree
    "r" #'org-refile
    "n" #'org-narrow-to-subtree
    "x" #'org-cut-subtree
    "w" #'org-copy-subtree)

  (add-to-list 'embark-keymap-alist '(org-heading . embark-org-heading-map)))

;;; Keybindings - Comprehensive and intuitive

;; Main bindings
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)
(global-set-key (kbd "C-c C-.") #'embark-act-all)
(global-set-key (kbd "s-.") #'embark-act)      ; Keep your super binding
(global-set-key (kbd "s->") #'embark-become)
(global-set-key (kbd "C-h B") #'embark-bindings)

;; Mouse binding
(global-set-key (kbd "<mouse-3>") #'bv-embark-mouse-act)

;; Minibuffer-specific bindings
(define-key minibuffer-local-map (kbd "C-.") #'embark-act)
(define-key minibuffer-local-map (kbd "C-;") #'embark-dwim)
(define-key minibuffer-local-map (kbd "C-c C-.") #'embark-act-all)
(define-key minibuffer-local-map (kbd "s-g") #'embark-become)
(define-key minibuffer-local-map (kbd "C-l") #'bv-embark-toggle-live-occur)
(define-key minibuffer-local-map (kbd "M-.") #'bv-embark-act-on-minibuffer-contents)

;; Alternative collect/export bindings that don't require embark-act first
(global-set-key (kbd "C-c e c") #'embark-collect)
(global-set-key (kbd "C-c e l") #'embark-live)
(global-set-key (kbd "C-c e e") #'embark-export)

;;; Display Configuration

;; Hide mode line in Embark buffers for cleaner look
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;; Use completing-read for action selection when requested
(defun bv-embark-keymap-help ()
  "Use completion for action selection with preview."
  (interactive)
  (embark-completing-read-prompter))

(define-key embark-general-map "?" #'bv-embark-keymap-help)

;;; Target Finders - Find more things

(defun bv-embark-target-sexp-at-point ()
  "Target the sexp at point."
  (when (or (looking-at "(")
            (looking-back ")" 1))
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (when bounds
        (list :type 'expression
              :target (buffer-substring (car bounds) (cdr bounds))
              :bounds bounds)))))

(add-to-list 'embark-target-finders #'bv-embark-target-sexp-at-point)

;;; Smart Dwim Configuration

(defun bv-embark-smart-eval-dwim ()
  "Evaluate expression intelligently based on context."
  (interactive)
  (cond
   ;; In minibuffer, exit with current input
   ((minibufferp) (exit-minibuffer))
   ;; On expression, evaluate it
   ((thing-at-point 'sexp) (eval-last-sexp nil))
   ;; Otherwise, run original dwim
   (t (embark-dwim))))

(global-set-key (kbd "C-;") #'bv-embark-smart-eval-dwim)

;;; Mode Hook Configuration

(add-hook 'embark-collect-mode-hook
          (lambda ()
            (setq-local truncate-lines t)
            (setq-local cursor-type 'box)))

;;; Performance optimization

;; Don't compute targets until needed
(setq embark-mixed-indicator-delay 0.5)

;; Faster candidate collection for large sets
(setq embark-candidate-collectors
      '(embark-sorted-minibuffer-candidates
        embark-collect-snapshot
        embark-completions-buffer-candidates
        embark-dired-candidates
        embark-ibuffer-candidates
        embark-embark-collect-candidates
        embark-consult-outline-candidates))

(provide 'bv-embark)
;;; bv-embark.el ends here
