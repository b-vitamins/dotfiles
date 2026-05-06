;;; bv-consult.el --- Consult navigation and search configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Consult owns high-value navigation commands and source composition.
;; The default philosophy is minibuffer-first with explicit narrowing and
;; preview controls; only row-heavy commands are promoted by Vertico policy.

;;; Code:

(require 'consult)
(require 'dom)
(require 'project)
(require 'recentf)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'bv-completion)

(autoload 'consult-compile-error "consult-compile" nil t)
(autoload 'consult-flymake "consult-flymake" nil t)
(autoload 'consult-imenu "consult-imenu" nil t)
(autoload 'consult-imenu-multi "consult-imenu" nil t)
(autoload 'consult-xref "consult-xref" nil nil)

;;; Declarations

(defvar consult--narrow)
(defvar consult--narrow-config)
(defvar consult--preview-function)
(defvar consult-buffer-sources)
(defvar consult-narrow-map)
(defvar isearch-mode-map)
(defvar recentf-list)
(defvar search-map)
(defvar xref-show-definitions-function)
(defvar xref-show-xrefs-function)

(declare-function bv-flymake-quickfix "bv-flymake" (&optional project))
(declare-function consult--convert-regexp "consult" (regexp type))
(declare-function consult--default-regexp-compiler "consult" (input type &optional ignore-case))
(declare-function consult--file-action "consult" (file))
(declare-function consult--file-state "consult" ())
(declare-function consult--read "consult" (table &rest args))
(declare-function consult-completion-in-region "consult" (start end collection &optional predicate))
(declare-function consult-flymake "consult-flymake" (&optional project))
(declare-function consult-imenu "consult-imenu" (&optional options))
(declare-function consult-imenu-multi "consult-imenu" (&optional options))
(declare-function consult-line-multi "consult" (&optional initial))
(declare-function consult-xref "consult-xref" (fetcher &optional alist))
(declare-function consult--file-preview "consult" ())
(declare-function consult-narrow "consult" (&optional narrow))
(declare-function orderless--highlight "orderless" (regexps ignore-case str))
(declare-function orderless-compile "orderless" (pattern &optional styles dispatchers))

;;; Customization

(defgroup bv-consult nil
  "BV Consult configuration."
  :group 'consult
  :prefix "bv-consult-")

(defcustom bv-consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `bv-consult-ripgrep-or-line'."
  :type 'integer
  :group 'bv-consult)

(defcustom bv-consult-preview-excluded-modes
  '(exwm-mode image-mode doc-view-mode pdf-view-mode)
  "Major modes excluded from Consult buffer/file preview."
  :type '(repeat symbol)
  :group 'bv-consult)

(defcustom bv-consult-auto-narrow-modes
  '((consult-imenu . ((prog-mode . ?f))))
  "Auto-narrowing configuration per command and selected-window mode."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type character))
  :group 'bv-consult)

(defcustom bv-consult-narrow-cycle-order
  '(?b ?. ?f ?p ?B ?F ?m ?T ?* ?\s)
  "Preferred Consult narrowing key order for TAB cycling."
  :type '(repeat character)
  :group 'bv-consult)

(defcustom bv-consult-narrow-echo t
  "Whether to echo the active Consult narrowing source."
  :type 'boolean
  :group 'bv-consult)

(defcustom bv-consult-enable-xdg-recent nil
  "Whether to include XDG recent files in `consult-buffer'."
  :type 'boolean
  :group 'bv-consult)

(defcustom bv-consult-max-uniquify-depth 3
  "Maximum parent directory depth used to uniquify file labels."
  :type 'integer
  :group 'bv-consult)

;;; Narrowing

(defun bv-consult--get-narrow-key ()
  "Return the configured initial narrow key for the current command."
  (when-let* ((config (alist-get this-command bv-consult-auto-narrow-modes))
              (window (minibuffer-selected-window))
              (buffer (window-buffer window))
              (mode (buffer-local-value 'major-mode buffer)))
    (or (alist-get mode config)
        (seq-some (pcase-lambda (`(,parent . ,key))
                    (and (provided-mode-derived-p mode parent) key))
                  config))))

(defun bv-consult--narrow-key-valid-p (key)
  "Return non-nil when KEY is valid in the active Consult minibuffer."
  (and (characterp key)
       (plist-get consult--narrow-config :keys)
       (assq key (plist-get consult--narrow-config :keys))))

(defun bv-consult--narrow-keys-ordered (keys)
  "Return KEYS ordered by `bv-consult-narrow-cycle-order'."
  (let (result)
    (dolist (key bv-consult-narrow-cycle-order)
      (when-let ((pair (assq key keys)))
        (push pair result)))
    (setq result (nreverse result))
    (dolist (pair keys)
      (unless (assq (car pair) result)
        (setq result (append result (list pair)))))
    result))

(defvar-local bv-consult--narrow-last-key :unset
  "Last echoed Consult narrow key in this minibuffer.")

(defun bv-consult--narrow-echo (&rest _)
  "Echo the active Consult narrowing source."
  (when (and bv-consult-narrow-echo (minibufferp))
    (let ((key consult--narrow))
      (unless (eq key bv-consult--narrow-last-key)
        (setq bv-consult--narrow-last-key key)
        (let* ((keys (plist-get consult--narrow-config :keys))
               (label (and key (alist-get key keys))))
          (message "Narrow: %s"
                   (cond
                    ((and key label)
                     (format "%s (%s)" label (key-description (vector key))))
                    (key (key-description (vector key)))
                    (t "All"))))))))

(defun bv-consult-initial-narrow ()
  "Apply context-sensitive Consult initial narrowing."
  (when-let ((key (bv-consult--get-narrow-key)))
    (when (bv-consult--narrow-key-valid-p key)
      (consult-narrow key))))

(defun bv-consult-narrow-cycle-forward ()
  "Cycle forward through Consult narrowing keys."
  (interactive)
  (when-let ((keys (plist-get consult--narrow-config :keys)))
    (setq keys (bv-consult--narrow-keys-ordered keys))
    (consult-narrow
     (if consult--narrow
         (let ((idx (seq-position keys (assq consult--narrow keys))))
           (car (nth (mod (1+ idx) (length keys)) keys)))
       (caar keys)))))

(defun bv-consult-narrow-cycle-backward ()
  "Cycle backward through Consult narrowing keys."
  (interactive)
  (when-let ((keys (plist-get consult--narrow-config :keys)))
    (setq keys (bv-consult--narrow-keys-ordered keys))
    (consult-narrow
     (if consult--narrow
         (let ((idx (seq-position keys (assq consult--narrow keys))))
           (car (nth (mod (1- idx) (length keys)) keys)))
       (caar (last keys))))))

;;; Orderless Search Integration

(defun bv-consult--orderless-regexp-compiler (input type &rest _config)
  "Compile Consult async INPUT using Orderless for regexp TYPE."
  (if (require 'orderless nil t)
      (condition-case nil
          (let ((regexps (cdr (orderless-compile input))))
            (cons
             (mapcar (lambda (regexp)
                       (consult--convert-regexp regexp type))
                     regexps)
             (lambda (str) (orderless--highlight regexps t str))))
        (error (consult--default-regexp-compiler input type)))
    (consult--default-regexp-compiler input type)))

(defun bv-consult--with-orderless (orig-fun &rest args)
  "Run ORIG-FUN with an Orderless Consult regexp compiler."
  (if (require 'orderless nil t)
      (minibuffer-with-setup-hook
          (lambda ()
            (setq-local consult--regexp-compiler
                        #'bv-consult--orderless-regexp-compiler))
        (apply orig-fun args))
    (apply orig-fun args)))

;;; Preview Toggle

(defvar-local bv-consult-toggle-preview-orig nil
  "Original Consult preview function for `bv-consult-toggle-preview'.")

(defun bv-consult-toggle-preview ()
  "Toggle Consult preview in the active minibuffer."
  (interactive)
  (if bv-consult-toggle-preview-orig
      (progn
        (setq consult--preview-function bv-consult-toggle-preview-orig
              bv-consult-toggle-preview-orig nil)
        (message "Consult preview enabled"))
    (setq bv-consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)
    (message "Consult preview disabled")))

;;; Consult Buffer Sources

(defun bv-consult--buffer-file-hash ()
  "Return a hash table of live buffer file names."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (buffer (buffer-list))
      (when-let ((file (buffer-file-name buffer)))
        (puthash (expand-file-name file) t ht)))
    ht))

(defun bv-consult--name-with-parents (file depth)
  "Return FILE name with DEPTH parent components."
  (let ((name (file-name-nondirectory file))
        (dir (file-name-directory (directory-file-name file)))
        parents)
    (dotimes (_ depth)
      (when (and dir (not (string-empty-p dir)))
        (let ((parent (file-name-nondirectory (directory-file-name dir))))
          (unless (string-empty-p parent)
            (push parent parents)))
        (setq dir (file-name-directory (directory-file-name dir)))))
    (if parents
        (format "%s (%s)" name (string-join parents "/"))
      name)))

(defun bv-consult--label-counts (files depth)
  "Return hash table of label counts for FILES at DEPTH."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (file files)
      (let ((label (bv-consult--name-with-parents file depth)))
        (puthash label (1+ (gethash label counts 0)) counts)))
    counts))

(defun bv-consult--uniquify-files (files)
  "Return (LABEL . FILE) pairs for FILES with useful unique labels."
  (let* ((depth 0)
         (counts (bv-consult--label-counts files depth))
         labels)
    (while (and (< depth bv-consult-max-uniquify-depth)
                (seq-some (lambda (file)
                            (> (gethash (bv-consult--name-with-parents file depth)
                                        counts 0)
                               1))
                          files))
      (setq depth (1+ depth)
            counts (bv-consult--label-counts files depth)))
    (dolist (file files (nreverse labels))
      (push (cons (bv-consult--name-with-parents file depth) file)
            labels))))

(defun bv-consult--xdg-recent-files-safe ()
  "Return XDG recent files when enabled and readable."
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
                            (file (url-unhex-string
                                   (string-remove-prefix "file://" href))))
                  (when (file-exists-p file)
                    (push (expand-file-name file) files))))
              (nreverse files))))))))

(defun bv-consult--candidate-file (candidate)
  "Return the file represented by CANDIDATE."
  (or (cdr (get-text-property 0 'multi-category candidate))
      candidate))

(defun bv-consult--file-action (candidate)
  "Open the file represented by CANDIDATE."
  (consult--file-action (bv-consult--candidate-file candidate)))

(defun bv-consult--file-state ()
  "Return a file preview state that understands BV virtual labels."
  (let ((state (consult--file-state)))
    (lambda (action candidate)
      (funcall state action
               (and candidate (bv-consult--candidate-file candidate))))))

(defun bv-consult--file-candidate (label file)
  "Return a Consult virtual file candidate LABEL for FILE."
  (propertize label 'multi-category `(file . ,file)))

(defvar bv-consult--source-recent-file
  `(:name "Recent File"
    :narrow ?f
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'bv-consult--file-state
    :action ,#'bv-consult--file-action
    :new ,#'consult--file-action
    :enabled ,(lambda () recentf-mode)
    :items
    ,(lambda ()
       (let* ((open (bv-consult--buffer-file-hash))
              (recent (seq-filter #'file-exists-p
                                  (mapcar #'expand-file-name
                                          (bound-and-true-p recentf-list))))
              (xdg (or (bv-consult--xdg-recent-files-safe) nil))
              (files (seq-remove (lambda (file) (gethash file open))
                                 (seq-uniq (append recent xdg)))))
         (mapcar (pcase-lambda (`(,label . ,file))
                   (bv-consult--file-candidate label file))
                 (bv-consult--uniquify-files files)))))
  "Recent file source with unique labels and full-path actions.")

(defvar bv-consult--source-file-in-dir
  `(:name "Current Dir"
    :narrow ((?. . "Current Dir"))
    :hidden t
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'bv-consult--file-state
    :action ,#'bv-consult--file-action
    :new ,#'consult--file-action
    :items
    ,(lambda ()
       (when (and default-directory
                  (file-directory-p default-directory))
         (let ((open (bv-consult--buffer-file-hash)))
           (mapcar
            (lambda (file)
              (bv-consult--file-candidate
               (file-name-nondirectory file)
               file))
            (seq-filter
             (lambda (file)
               (and (file-regular-p file)
                    (not (string-prefix-p "." (file-name-nondirectory file)))
                    (not (gethash (expand-file-name file) open))))
             (ignore-errors
               (directory-files default-directory t nil t))))))))
  "Hidden source for regular files in `default-directory'.")

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
  "Tab-bar source for `consult-buffer'.")

;;; Commands

(defun bv-consult-ripgrep-or-line ()
  "Use `consult-line' in small buffers and `consult-ripgrep' otherwise."
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
    (consult-ripgrep)))

(defun bv-consult-search ()
  "DWIM search: project ripgrep when possible, otherwise line search."
  (interactive)
  (cond
   ((or (minibufferp)
        (buffer-narrowed-p)
        (and buffer-file-name (file-remote-p buffer-file-name)))
    (consult-line))
   ((project-current nil)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (consult-ripgrep))
   ((and buffer-file-name (> (buffer-size) bv-consult-ripgrep-or-line-limit))
    (when (buffer-modified-p)
      (save-buffer))
    (consult-ripgrep default-directory))
   (t
    (consult-line-multi))))

(defun bv-consult-line-symbol-at-point ()
  "Search the current buffer for the symbol at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun bv-consult-ripgrep-at-point (&optional dir)
  "Ripgrep for the region or symbol at point in DIR."
  (interactive "P")
  (let ((initial (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol))))
    (consult-ripgrep dir initial)))

(defun bv-consult-find-file-with-preview (prompt &optional dir _default mustmatch initial pred)
  "Find file with Consult preview support.
PROMPT, DIR, MUSTMATCH, INITIAL and PRED follow `read-file-name-function'."
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

;;; Setup

(defun bv-consult--preview-excluded-buffer-p (buffer)
  "Return non-nil if BUFFER should not be previewed."
  (with-current-buffer buffer
    (or (memq major-mode bv-consult-preview-excluded-modes)
        (and buffer-file-name
             (file-remote-p buffer-file-name)))))

(defun bv-consult-setup ()
  "Configure Consult."
  (define-key consult-narrow-map (kbd "TAB") #'bv-consult-narrow-cycle-forward)
  (define-key consult-narrow-map (kbd "<backtab>") #'bv-consult-narrow-cycle-backward)
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  (add-hook 'minibuffer-setup-hook #'bv-consult-initial-narrow)

  (when (require 'orderless nil t)
    (advice-add #'consult-ripgrep :around #'bv-consult--with-orderless)
    (advice-add #'consult-grep :around #'bv-consult--with-orderless)
    (advice-add #'consult-git-grep :around #'bv-consult--with-orderless))

  (setq consult-buffer-sources
        '(consult-source-buffer
          bv-consult--source-file-in-dir
          consult-source-project-buffer-hidden
          consult-source-project-recent-file-hidden
          bv-consult--source-recent-file
          consult-source-bookmark
          bv-consult--source-tab
          consult-source-modified-buffer
          consult-source-hidden-buffer))

  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.3
        consult-async-input-debounce 0.1
        consult-preview-partial-size (* 1024 1024)
        consult-preview-max-count 10
        consult-preview-excluded-buffers #'bv-consult--preview-excluded-buffer-p
        consult-grep-args
        (concat "grep --null --line-buffered --color=never --ignore-case "
                "--exclude-dir=.git --exclude-dir=node_modules "
                "--exclude-dir=.venv --exclude-dir=.cache "
                "--with-filename --line-number -I -r")
        consult-ripgrep-args
        (concat "rg --null --line-buffered --color=never --max-columns=1000 "
                "--path-separator / --smart-case --no-heading "
                "--with-filename --line-number --search-zip "
                "--hidden --glob !.git/ --glob !node_modules/")
        consult-project-function
        (lambda (may-prompt)
          (when-let ((project (project-current may-prompt)))
            (project-root project)))
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-theme :preview-key nil
   consult-buffer :preview-key '(:debounce 0.2 any)
   consult-imenu consult-imenu-multi consult-xref
   consult-compile-error consult-flymake
   :preview-key '(:debounce 0.15 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)
   consult-goto-line :prompt "Go to line: "
   consult-line :prompt "Search: " :inherit-input-method t
   :preview-key '(:debounce 0.15 any)
   consult-outline :prompt "Outline: "
   :preview-key '(:debounce 0.15 any)
   consult-history :category nil)

  (unless (advice-member-p #'bv-consult--narrow-echo #'consult-narrow)
    (advice-add #'consult-narrow :after #'bv-consult--narrow-echo)))

;;; Keybindings

(defun bv-consult-setup-keybindings ()
  "Install Consult keybindings."
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
  (global-set-key (kbd "M-g f") #'bv-flymake-quickfix)
  (global-set-key (kbd "C-s") #'consult-line)
  (global-set-key (kbd "C-M-s") #'bv-consult-search)
  (global-set-key (kbd "C-S-s") #'bv-consult-line-symbol-at-point)

  (define-key minibuffer-local-map (kbd "M-s") #'consult-history)
  (define-key minibuffer-local-map (kbd "M-r") #'consult-history)
  (define-key minibuffer-local-map (kbd "C-c p") #'bv-consult-toggle-preview)

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

  (with-eval-after-load 'isearch
    (define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history)
    (define-key isearch-mode-map (kbd "M-s l") #'consult-line)))

(defun bv-consult-init ()
  "Initialize `bv-consult'."
  (bv-consult-setup)
  (bv-consult-setup-keybindings))

(bv-consult-init)

(provide 'bv-consult)
;;; bv-consult.el ends here
