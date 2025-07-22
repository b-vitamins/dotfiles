;;; bv-consult.el --- Enhanced Consult configuration (Fixed) -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.1
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Fixed version of enhanced Consult configuration.
;; Main fixes:
;; - Removed invalid :enabled key from sources
;; - Fixed uniquification to prevent infinite loops
;; - Simplified dynamic preview key
;; - Added error handling for XDG files
;; - Fixed source ordering

;;; Code:

(require 'consult)
(require 'recentf)
(require 'project)

;;;; Custom Variables

(defgroup bv-consult nil
  "Enhanced Consult configuration."
  :group 'consult
  :prefix "bv-consult-")

(defcustom bv-consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `bv-consult-ripgrep-or-line'."
  :type 'integer
  :group 'bv-consult)

(defcustom bv-consult-preview-excluded-modes
  '(exwm-mode image-mode doc-view-mode pdf-view-mode)
  "Major modes excluded from preview."
  :type '(repeat symbol)
  :group 'bv-consult)

(defcustom bv-consult-auto-narrow-modes
  '((consult-buffer . ((emacs-lisp-mode . ?e)
                      (lisp-interaction-mode . ?e)
                      (python-mode . ?p)
                      (python-ts-mode . ?p)
                      (rust-mode . ?r)
                      (rust-ts-mode . ?r)
                      (javascript-mode . ?j)
                      (javascript-ts-mode . ?j)
                      (typescript-mode . ?t)
                      (typescript-ts-mode . ?t)
                      (c-mode . ?c)
                      (c-ts-mode . ?c)
                      (c++-mode . ?C)
                      (c++-ts-mode . ?C)
                      (java-mode . ?J)
                      (java-ts-mode . ?J)
                      (go-mode . ?g)
                      (go-ts-mode . ?g)
                      (ruby-mode . ?R)
                      (ruby-ts-mode . ?R)
                      (scheme-mode . ?s)
                      (org-mode . ?o)
                      (markdown-mode . ?m)
                      (dired-mode . ?d)
                      (magit-status-mode . ?G)
                      (prog-mode . ?P)))
    (consult-imenu . ((prog-mode . ?f))))
  "Auto-narrowing configuration per command and mode."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type character))
  :group 'bv-consult)

(defcustom bv-consult-enable-xdg-recent nil
  "Whether to include XDG recent files in candidates.
Disabled by default for performance."
  :type 'boolean
  :group 'bv-consult)

(defcustom bv-consult-max-uniquify-depth 3
  "Maximum directory depth for file name uniquification."
  :type 'integer
  :group 'bv-consult)

;;;; Narrowing System

(defun bv-consult--get-narrow-key ()
  "Get narrow key based on current command and context."
  (when-let* ((config (alist-get this-command bv-consult-auto-narrow-modes))
              (window (minibuffer-selected-window))
              (buffer (and window (window-buffer window)))
              (mode (and buffer (buffer-local-value 'major-mode buffer))))
    (or (alist-get mode config)
        (seq-some (pcase-lambda (`(,parent . ,key))
                   (and (provided-mode-derived-p mode parent) key))
                 config))))

(defun bv-consult-initial-narrow ()
  "Auto-narrow based on context."
  (when-let ((key (bv-consult--get-narrow-key)))
    (setq unread-command-events
          (append unread-command-events (list key 32)))))

(defun bv-consult-narrow-cycle-forward ()
  "Cycle forward through narrowing keys."
  (interactive)
  (when-let ((keys (plist-get consult--narrow-config :keys)))
    (consult-narrow
     (if consult--narrow
         (let ((idx (seq-position keys (assq consult--narrow keys))))
           (car (nth (mod (1+ idx) (length keys)) keys)))
       (caar keys)))))

(defun bv-consult-narrow-cycle-backward ()
  "Cycle backward through narrowing keys."
  (interactive)
  (when-let ((keys (plist-get consult--narrow-config :keys)))
    (consult-narrow
     (if consult--narrow
         (let ((idx (seq-position keys (assq consult--narrow keys))))
           (car (nth (mod (1- idx) (length keys)) keys)))
       (caar (last keys))))))

;;;; Orderless Integration

(defun bv-consult--orderless-regexp-compiler (input type &rest _config)
  "Orderless regexp compiler for consult."
  (when (require 'orderless nil t)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str)))))

(defun bv-consult--with-orderless (&rest args)
  "Use orderless for the current command if available."
  (if (require 'orderless nil t)
      (minibuffer-with-setup-hook
          (lambda ()
            (setq-local consult--regexp-compiler #'bv-consult--orderless-regexp-compiler))
        (apply args))
    (apply args)))

;;;; Toggle Preview

(defvar-local bv-consult-toggle-preview-orig nil
  "Original preview function.")

(defun bv-consult-toggle-preview ()
  "Toggle preview on/off."
  (interactive)
  (if bv-consult-toggle-preview-orig
      (progn
        (setq consult--preview-function bv-consult-toggle-preview-orig
              bv-consult-toggle-preview-orig nil)
        (message "Preview enabled"))
    (setq bv-consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)
    (message "Preview disabled")))

;;;; Recent Files Uniquification (Simplified)

(defun bv-consult--buffer-file-hash ()
  "Create hash table of all buffer file names."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (buffer (buffer-list))
      (when-let ((file (buffer-file-name buffer)))
        (puthash file t ht)))
    ht))

(defun bv-consult--simple-uniquify (files)
  "Simple uniquification - add parent dir to duplicates only."
  (let ((name-count (make-hash-table :test 'equal))
        (result nil))
    ;; Count occurrences
    (dolist (file files)
      (let ((name (file-name-nondirectory file)))
        (puthash name (1+ (gethash name name-count 0)) name-count)))
    ;; Build result with uniquified names
    (dolist (file files)
      (let* ((name (file-name-nondirectory file))
             (count (gethash name name-count)))
        (push (cons (if (> count 1)
                        ;; Add parent directory for duplicates
                        (let ((parent (file-name-nondirectory
                                      (directory-file-name
                                       (file-name-directory file)))))
                          (if (string-empty-p parent)
                              name
                            (format "%s (%s)" name parent)))
                      name)
                    file)
              result)))
    (nreverse result)))

;;;; XDG Recent Files (with error handling)

(defun bv-consult--xdg-recent-files-safe ()
  "Get recent files from XDG recently-used.xbel with error handling."
  (when (and bv-consult-enable-xdg-recent
             (eq system-type 'gnu/linux))
    (ignore-errors
      (let ((xbel-file (expand-file-name
                        "recently-used.xbel"
                        (or (getenv "XDG_DATA_HOME") "~/.local/share"))))
        (when (file-readable-p xbel-file)
          (with-temp-buffer
            (insert-file-contents xbel-file)
            (let ((dom (libxml-parse-xml-region (point-min) (point-max)))
                  files)
              (dolist (bookmark (dom-by-tag dom 'bookmark))
                (when-let* ((href (dom-attr bookmark 'href))
                            (file (url-unhex-string (string-remove-prefix "file://" href))))
                  (when (file-exists-p file)
                    (push file files))))
              (nreverse files))))))))

;;;; Enhanced Buffer Sources

(defvar bv-consult--source-recent-file-uniquified
  `(:name "Recent File"
    :narrow ?f
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'consult--file-state
    :new ,#'consult--file-action
    :items
    ,(lambda ()
       (when recentf-mode
         (let* ((ht (bv-consult--buffer-file-hash))
                (recent-files (seq-filter #'file-exists-p recentf-list))
                (xdg-files (or (bv-consult--xdg-recent-files-safe) '()))
                (all-files (seq-uniq (append recent-files xdg-files)))
                (files (seq-remove (lambda (f) (gethash f ht)) all-files))
                (unique-files (bv-consult--simple-uniquify files)))
           (mapcar (lambda (pair)
                     (propertize (car pair) 'multi-category `(file . ,(cdr pair))))
                   unique-files)))))
  "Recent file source with uniquified names.")

(defvar bv-consult--source-file-in-dir
  `(:name "File in Dir"
    :narrow ?.
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'consult--file-state
    :new ,#'consult--file-action
    :items
    ,(lambda ()
       (when default-directory
         (let ((ht (bv-consult--buffer-file-hash)))
           (mapcar (lambda (f)
                     (propertize f 'multi-category `(file . ,(expand-file-name f))))
                   (seq-filter
                    (lambda (f)
                      (and (not (string-prefix-p "." f))
                           (file-regular-p f)
                           (not (gethash (expand-file-name f) ht))))
                    (ignore-errors (directory-files default-directory nil nil t))))))))
  "Files in current directory source.")

(defvar bv-consult--source-tab
  `(:name "Tab"
    :narrow ?T
    :category tab
    :face font-lock-function-name-face
    :history tab-bar-history
    :action ,#'tab-bar-select-tab-by-name
    :items ,(lambda ()
              (when (and (fboundp 'tab-bar-tabs)
                         (cdr (tab-bar-tabs)))
                (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar-tabs)))))
  "Tab-bar source.")

;;;; Enhanced Commands

(defun bv-consult-ripgrep-or-line ()
  "Use `consult-line' for small buffers, `consult-ripgrep' for large."
  (interactive)
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (file-remote-p buffer-file-name)
          (< (buffer-size)
             (/ bv-consult-ripgrep-or-line-limit
                (if (eq major-mode 'org-mode) 4 1))))
      (consult-line)
    (when (buffer-modified-p)
      (save-buffer))
    (consult-ripgrep default-directory)))

(defun bv-consult-line-symbol-at-point ()
  "Search for symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun bv-consult-ripgrep-at-point (&optional dir)
  "Ripgrep with thing at point or region."
  (interactive "P")
  (let ((initial (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol))))
    (consult-ripgrep dir initial)))

(defun bv-consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  "Find file with preview support."
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal
                   :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred
                   :category 'file
                   :history 'file-name-history)))

;;;; Setup & Configuration

(defun bv-consult-setup ()
  "Setup consult configuration."
  ;; Set narrowing keys
  (define-key consult-narrow-map (kbd "TAB") #'bv-consult-narrow-cycle-forward)
  (define-key consult-narrow-map (kbd "<backtab>") #'bv-consult-narrow-cycle-backward)

  ;; Setup auto-narrowing
  (add-hook 'minibuffer-setup-hook #'bv-consult-initial-narrow)

  ;; Apply orderless to grep commands if available
  (when (require 'orderless nil t)
    (advice-add #'consult-ripgrep :around #'bv-consult--with-orderless)
    (advice-add #'consult-grep :around #'bv-consult--with-orderless)
    (advice-add #'consult-git-grep :around #'bv-consult--with-orderless))

  ;; Configure buffer sources - start with default and add custom
  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          bv-consult--source-file-in-dir
          bv-consult--source-recent-file-uniquified
          consult--source-bookmark
          bv-consult--source-tab
          consult--source-project-buffer-hidden
          consult--source-project-recent-file-hidden))

  ;; File reading with preview - only set if you want it globally
  ;; (setq read-file-name-function #'bv-consult-find-file-with-preview)

  ;; Performance settings
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.3
        consult-async-input-debounce 0.1)

  ;; Grep configuration
  (setq consult-grep-args
        (concat "grep --null --line-buffered --color=never --ignore-case "
                "--exclude-dir=.git --exclude-dir=node_modules "
                "--exclude-dir=.venv --exclude-dir=.cache "
                "--with-filename --line-number -I -r"))

  (setq consult-ripgrep-args
        (concat "rg --null --line-buffered --color=never --max-columns=1000 "
                "--path-separator / --smart-case --no-heading "
                "--with-filename --line-number --search-zip "
                "--hidden --glob !.git/ --glob !node_modules/"))

  ;; Project function
  (setq consult-project-function
        (lambda (may-prompt)
          (when-let ((pr (project-current may-prompt)))
            (project-root pr))))

  ;; Preview settings
  (setq consult-preview-partial-size (* 1024 1024)
        consult-preview-max-count 10)

  ;; Xref integration
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Enable preview at point
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  ;; Preview exclusions - simplified
  (setq consult-preview-excluded-buffers
        (lambda (buf)
          (with-current-buffer buf
            (or (memq major-mode bv-consult-preview-excluded-modes)
                (and buffer-file-name
                     (file-remote-p buffer-file-name))))))

  ;; Customizations
  (consult-customize
   ;; Disable preview for theme selection
   consult-theme :preview-key nil

   ;; Simple preview for buffers
   consult-buffer :preview-key '(:debounce 0.2 any)

   ;; Delayed preview for grep commands
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)

   ;; Configure specific commands
   consult-goto-line :prompt "Go to line: "
   consult-line :prompt "Search: " :inherit-input-method t
   consult-outline :prompt "Outline: "
   consult-history :category nil))

;;;; Key Bindings

(defun bv-consult-setup-keybindings ()
  "Setup consult keybindings."
  ;; Global bindings
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
  (global-set-key (kbd "C-x t b") #'consult-buffer-other-tab)
  (global-set-key (kbd "C-x r b") #'consult-bookmark)
  (global-set-key (kbd "C-x p b") #'consult-project-buffer)
  (global-set-key (kbd "M-y") #'consult-yank-pop)
  (global-set-key (kbd "M-g g") #'consult-goto-line)
  (global-set-key (kbd "M-g M-g") #'consult-goto-line)
  (global-set-key (kbd "M-g o") #'consult-outline)
  (global-set-key (kbd "M-g m") #'consult-mark)
  (global-set-key (kbd "M-g k") #'consult-global-mark)
  (global-set-key (kbd "M-g i") #'consult-imenu)
  (global-set-key (kbd "M-g I") #'consult-imenu-multi)
  (global-set-key (kbd "M-g e") #'consult-compile-error)
  (global-set-key (kbd "M-g f") #'consult-flymake)
  (global-set-key (kbd "C-s") #'consult-line)
  (global-set-key (kbd "C-M-s") #'bv-consult-ripgrep-or-line)
  (global-set-key (kbd "C-S-s") #'bv-consult-line-symbol-at-point)

  ;; Minibuffer bindings
  (define-key minibuffer-local-map (kbd "M-s") #'consult-history)
  (define-key minibuffer-local-map (kbd "M-r") #'consult-history)

  ;; Search map bindings
  (when (boundp 'search-map)
    (define-key search-map (kbd "d") #'consult-find)
    (define-key search-map (kbd "D") #'consult-fd)
    (define-key search-map (kbd "g") #'consult-grep)
    (define-key search-map (kbd "G") #'consult-git-grep)
    (define-key search-map (kbd "r") #'consult-ripgrep)
    (define-key search-map (kbd "R") #'bv-consult-ripgrep-at-point)
    (define-key search-map (kbd "l") #'consult-line)
    (define-key search-map (kbd "L") #'consult-line-multi)
    (define-key search-map (kbd "k") #'consult-keep-lines)
    (define-key search-map (kbd "u") #'consult-focus-lines)
    (define-key search-map (kbd "e") #'consult-isearch-history))

  ;; Isearch integration
  (with-eval-after-load 'isearch
    (define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-s l") #'consult-line)))

;;;; Initialize

(defun bv-consult-init ()
  "Initialize bv-consult configuration."
  (bv-consult-setup)
  (bv-consult-setup-keybindings))

;; Initialize when loaded
(bv-consult-init)

(provide 'bv-consult)
;;; bv-consult.el ends here
