;;; bv-consult.el --- Enhanced Consult configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced configuration for consult with custom sources, actions, and utilities.
;; Features:
;; - Smart auto-narrowing with context awareness
;; - Custom buffer sources with better filtering
;; - Enhanced grep/ripgrep integration
;; - Project-aware commands
;; - Custom actions and embark integration
;; - Better preview handling
;; - Performance optimizations

;;; Code:

(require 'consult)

;;;; Custom Variables

(defgroup bv-consult nil
  "Enhanced Consult configuration."
  :group 'consult)

(defcustom bv-consult-ripgrep-glob-patterns
  '(("el" . "*.el")
    ("org" . "*.org")
    ("py" . "*.py")
    ("js" . "*.{js,jsx,ts,tsx}")
    ("web" . "*.{html,css,scss,js,jsx,ts,tsx}")
    ("code" . "*.{el,py,js,jsx,ts,tsx,c,cpp,h,hpp,rs,go}")
    ("doc" . "*.{org,md,tex,txt}"))
  "Alist of quick glob patterns for ripgrep."
  :type '(alist :key-type string :value-type string)
  :group 'bv-consult)

(defcustom bv-consult-buffer-filter-groups
  '(("Hidden" . "\\` ")
    ("Temp" . "\\`\\*.*\\*\\'")
    ("Magit" . "\\`magit")
    ("Help" . "\\`\\*[Hh]elp"))
  "Buffer filter groups for quick filtering."
  :type '(alist :key-type string :value-type string)
  :group 'bv-consult)

;;;; Enhanced Auto-narrowing System

(defvar bv-consult-auto-narrow-rules
  '((consult-buffer . bv-consult--buffer-narrow-key)
    (consult-imenu . bv-consult--imenu-narrow-key)
    (consult-line . bv-consult--line-narrow-key))
  "Rules for automatic narrowing based on context.")

(defvar bv-consult-mode-narrow-alist
  '((emacs-lisp-mode . ?e)
    (lisp-interaction-mode . ?e)
    (python-mode . ?p)
    (python-ts-mode . ?p)
    (org-mode . ?o)
    (markdown-mode . ?m)
    (dired-mode . ?d)
    (magit-status-mode . ?g))
  "Mode to narrowing key mapping.")

(defun bv-consult--buffer-narrow-key ()
  "Determine buffer narrow key based on context."
  (let ((buf (window-buffer (minibuffer-selected-window))))
    (with-current-buffer buf
      (or (alist-get major-mode bv-consult-mode-narrow-alist)
          (cdr (seq-find (lambda (mode)
                          (derived-mode-p (car mode)))
                        bv-consult-mode-narrow-alist))))))

(defun bv-consult--imenu-narrow-key ()
  "Determine imenu narrow key based on context."
  (when (derived-mode-p 'prog-mode)
    ?f)) ; Functions by default in programming modes

(defun bv-consult--line-narrow-key ()
  "Determine line narrow key based on context."
  ;; Could return different keys based on file type or mode
  nil)

(defun bv-consult-auto-narrow ()
  "Enhanced auto-narrowing with context awareness."
  (when-let* ((rule (alist-get this-command bv-consult-auto-narrow-rules))
              (key (funcall rule)))
    (setq unread-command-events
          (append unread-command-events (list key 32)))))

(add-hook 'minibuffer-setup-hook #'bv-consult-auto-narrow)

;;;; Custom Buffer Sources

(defvar bv-consult--source-hidden-buffer
  `(:name "Hidden Buffer"
    :narrow ?\s
    :hidden t
    :category buffer
    :face consult-buffer
    :history buffer-name-history
    :action ,#'consult--buffer-action
    :items ,(lambda ()
              (consult--buffer-query
               :sort 'visibility
               :filter 'invert
               :exclude consult-buffer-filter
               :include '("\\` ")
               :as #'consult--buffer-pair)))
  "Hidden buffer source, showing space-prefixed buffers.")

(defvar bv-consult--source-project-file
  `(:name "Project File"
    :narrow ?f
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'consult--file-state
    :enabled ,(lambda () (and consult-project-function
                             (consult--project-root)))
    :items ,(lambda ()
              (when-let (root (consult--project-root))
                (let ((len (length root))
                      (inv-root (propertize root 'invisible t)))
                  (mapcar (lambda (x)
                           (concat inv-root (substring x len)))
                          (bv-consult--project-files root))))))
  "Enhanced project file source with better performance.")

(defun bv-consult--project-files (root)
  "Get project files for ROOT with caching."
  (let* ((cache-key (expand-file-name root))
         (cache-file (expand-file-name
                     (concat ".consult-project-cache-"
                            (secure-hash 'md5 cache-key))
                     temporary-file-directory))
         (cache-valid (and (file-exists-p cache-file)
                          (< (- (float-time)
                                (float-time (file-attribute-modification-time
                                           (file-attributes cache-file))))
                             300)))) ; 5 minute cache
    (if cache-valid
        (with-temp-buffer
          (insert-file-contents cache-file)
          (read (current-buffer)))
      (let ((files (bv-consult--find-project-files root)))
        (with-temp-file cache-file
          (prin1 files (current-buffer)))
        files))))

(defun bv-consult--find-project-files (root)
  "Find files in project ROOT."
  (let* ((default-directory root)
         (cmd (cond
               ((executable-find "fd")
                "fd --type f --hidden --exclude .git")
               ((executable-find "rg")
                "rg --files --hidden")
               (t "find . -type f -name '*' | grep -v '^\\.git'"))))
    (mapcar (lambda (f) (expand-file-name f root))
            (split-string (shell-command-to-string cmd) "\n" t))))

;;;; Enhanced Sources List

(defcustom bv-consult-buffer-sources
  '(consult--source-hidden-buffer
    consult--source-modified-buffer
    consult--source-buffer
    consult--source-recent-file
    bv-consult--source-project-file
    consult--source-bookmark
    consult--source-project-buffer-hidden
    consult--source-project-recent-file-hidden)
  "Custom buffer sources."
  :type '(repeat symbol)
  :group 'bv-consult)

;;;; Custom Actions

(defun bv-consult-buffer-kill-action (cand)
  "Kill buffer CAND without switching to it."
  (when-let ((buf (get-buffer cand)))
    (kill-buffer buf)))

(defun bv-consult-buffer-save-action (cand)
  "Save buffer CAND without switching to it."
  (when-let ((buf (get-buffer cand)))
    (with-current-buffer buf
      (save-buffer))))

(defvar bv-consult-buffer-actions
  '(("k" bv-consult-buffer-kill-action "Kill")
    ("s" bv-consult-buffer-save-action "Save")
    ("o" consult--buffer-action "Open")))

;;;; Enhanced Commands

(defun bv-consult-buffer ()
  "Enhanced `consult-buffer' with custom sources."
  (interactive)
  (consult-buffer bv-consult-buffer-sources))

(defun bv-consult-ripgrep-glob (&optional dir initial)
  "Ripgrep with quick glob selection."
  (interactive "P")
  (let* ((glob (completing-read "File pattern: "
                               bv-consult-ripgrep-glob-patterns
                               nil nil nil 'bv-consult-glob-history))
         (pattern (or (alist-get glob bv-consult-ripgrep-glob-patterns
                                nil nil #'string=)
                     glob))
         (consult-ripgrep-args
          (concat consult-ripgrep-args " --glob '" pattern "'")))
    (consult-ripgrep dir initial)))

(defun bv-consult-line-multi-all ()
  "Search all open buffers with consult-line-multi."
  (interactive)
  (consult-line-multi t))

(defun bv-consult-line-project ()
  "Search project buffers with consult-line-multi."
  (interactive)
  (consult-line-multi '(:sort alpha-current :directory project)))

(defun bv-consult-imenu-all ()
  "Jump to imenu item in all project buffers."
  (interactive)
  (consult-imenu-multi t))

(defun bv-consult-mark-ring ()
  "Enhanced mark ring browser with preview."
  (interactive)
  (consult-mark))

(defun bv-consult-outline-up ()
  "Jump to parent outline heading."
  (interactive)
  (let ((start-level (save-excursion
                      (outline-back-to-heading t)
                      (funcall outline-level))))
    (outline-up-heading 1)
    (consult-outline start-level)))

;;;; Embark Integration

(with-eval-after-load 'embark
  (defvar-keymap embark-consult-async-search-map
    :doc "Keymap for async search results"
    :parent embark-general-map
    "w" #'embark-save
    "e" #'embark-export
    "f" #'embark-find-definition)

  (add-to-list 'embark-keymap-alist '(consult-grep . embark-consult-async-search-map))
  (add-to-list 'embark-keymap-alist '(consult-ripgrep . embark-consult-async-search-map)))

;;;; Preview Configuration

(defun bv-consult--preview-key ()
  "Dynamic preview key based on context."
  (cond
   ((< (buffer-size) 100000) 'any)     ; Small buffers: always preview
   ((< (buffer-size) 1000000) "M-.")   ; Medium buffers: manual preview
   (t nil)))                           ; Large buffers: no preview

(defun bv-consult-toggle-preview ()
  "Toggle preview for current session."
  (interactive)
  (setq-local consult-preview-key
              (if consult-preview-key nil 'any)))

;;;; Enhanced Key Bindings

;; Global bindings
(global-set-key (kbd "C-x b") #'bv-consult-buffer)
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
(global-set-key (kbd "C-x t b") #'consult-buffer-other-tab)
(global-set-key (kbd "C-x C-r") #'consult-recent-file)
(global-set-key (kbd "M-y") #'consult-yank-pop)
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "C-r") #'consult-line)
(global-set-key (kbd "C-M-s") #'bv-consult-line-multi-all)
(global-set-key (kbd "C-M-r") #'bv-consult-line-project)

;; Enhanced goto-map
(with-eval-after-load 'goto-map
  (let ((map goto-map))
    (define-key map (kbd "g") #'consult-goto-line)
    (define-key map (kbd "M-g") #'consult-goto-line)
    (define-key map (kbd "l") #'consult-line)
    (define-key map (kbd "L") #'bv-consult-line-multi-all)
    (define-key map (kbd "o") #'consult-outline)
    (define-key map (kbd "O") #'bv-consult-outline-up)
    (define-key map (kbd "i") #'consult-imenu)
    (define-key map (kbd "I") #'bv-consult-imenu-all)
    (define-key map (kbd "m") #'consult-mark)
    (define-key map (kbd "M") #'consult-global-mark)
    (define-key map (kbd "k") #'consult-bookmark)
    (define-key map (kbd "e") #'consult-compile-error)))

;; Enhanced search-map
(with-eval-after-load 'search-map
  (let ((map search-map))
    (define-key map (kbd "g") #'consult-grep)
    (define-key map (kbd "G") #'consult-git-grep)
    (define-key map (kbd "r") #'consult-ripgrep)
    (define-key map (kbd "R") #'bv-consult-ripgrep-glob)
    (define-key map (kbd "f") #'consult-find)
    (define-key map (kbd "F") #'consult-fd)
    (define-key map (kbd "l") #'consult-locate)
    (define-key map (kbd "L") #'consult-line-multi)
    (define-key map (kbd "k") #'consult-keep-lines)
    (define-key map (kbd "u") #'consult-focus-lines)
    (define-key map (kbd "e") #'consult-isearch-history)
    (define-key map (kbd "m") #'consult-man)))

;; Minibuffer local map
(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "M-r") #'consult-history)
  (define-key minibuffer-local-map (kbd "C-M-r") #'consult-history))

;; Better isearch integration
(with-eval-after-load 'isearch
  (let ((map isearch-mode-map))
    (define-key map (kbd "M-e") #'consult-isearch-history)
    (define-key map (kbd "M-s e") #'consult-isearch-history)
    (define-key map (kbd "M-s l") #'consult-line)
    (define-key map (kbd "M-s L") #'consult-line-multi)))

;;;; Customizations

(consult-customize
 ;; Preview customizations
 consult-buffer :preview-key #'bv-consult--preview-key
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file
 consult--source-recent-file consult--source-project-recent-file
 consult--source-bookmark :preview-key "M-."

 ;; Disable preview for these
 consult-theme :preview-key nil

 ;; Better sorting
 consult-buffer :sort nil  ; Keep source order
 consult-line :inherit-input-method t :prompt "Search line: "
 consult-line-multi :inherit-input-method t

 ;; Completion requirements
 consult-grep consult-ripgrep :require-match t
 consult-line :require-match nil) ; Allow jumping to non-matching lines

;; Performance settings
(setq consult-async-min-input 2
      consult-async-refresh-delay 0.2
      consult-async-input-throttle 0.5
      consult-async-input-debounce 0.2)

;; Better grep defaults
(setq consult-grep-args
      (concat "grep --null --line-buffered --color=never --ignore-case "
              "--exclude-dir=.git --exclude-dir=node_modules "
              "--with-filename --line-number -I -r"))

(setq consult-ripgrep-args
      (concat "rg --null --line-buffered --color=never --max-columns=1000 "
              "--path-separator / --smart-case --no-heading --with-filename "
              "--line-number --search-zip --hidden"))

;; Project function
(setq consult-project-function #'bv-consult--project-root-default)

(defun bv-consult--project-root-default (&optional may-prompt)
  "Enhanced project root function."
  (or (when-let ((project (project-current may-prompt)))
        (if (fboundp 'project-root)
            (project-root project)
          (car (project-roots project))))
      (when may-prompt
        (read-directory-name "Project: " nil nil t))))

;; Better xref integration
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Enable preview in completion list mode
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

;;;; Utility Functions

(defun bv-consult-info ()
  "Search all Info manuals."
  (interactive)
  (consult-info Info-directory-list))

(defun bv-consult-project-extra-find ()
  "Find file with preview in project or any directory."
  (interactive)
  (let ((consult-find-args
         (if (executable-find "fd")
             "fd --color=never --type f --hidden --exclude .git"
           consult-find-args)))
    (consult-find (consult--project-root t))))

(defun bv-consult-org-heading ()
  "Jump to Org heading with better preview."
  (interactive)
  (let ((consult-preview-key 'any))
    (consult-org-heading)))

;;;; Mode-specific configurations

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-j") #'bv-consult-org-heading))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map (kbd "C-c C-j") #'consult-imenu))

;;;; Integration with bv-app-map

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "b") #'bv-consult-buffer)
    (define-key bv-app-map (kbd "B") #'consult-project-buffer)
    (define-key bv-app-map (kbd "f") #'consult-find)
    (define-key bv-app-map (kbd "F") #'bv-consult-project-extra-find)
    (define-key bv-app-map (kbd "g") #'consult-ripgrep)
    (define-key bv-app-map (kbd "G") #'bv-consult-ripgrep-glob)
    (define-key bv-app-map (kbd "h") #'consult-history)
    (define-key bv-app-map (kbd "i") #'bv-consult-info)
    (define-key bv-app-map (kbd "k") #'consult-kmacro)
    (define-key bv-app-map (kbd "l") #'consult-line)
    (define-key bv-app-map (kbd "L") #'bv-consult-line-project)
    (define-key bv-app-map (kbd "m") #'consult-mode-command)
    (define-key bv-app-map (kbd "M") #'consult-minor-mode-menu)
    (define-key bv-app-map (kbd "o") #'consult-outline)
    (define-key bv-app-map (kbd "r") #'consult-register)
    (define-key bv-app-map (kbd "t") #'consult-theme)
    (define-key bv-app-map (kbd "T") #'bv-consult-toggle-preview)
    (define-key bv-app-map (kbd "y") #'consult-yank-from-kill-ring)))

(provide 'bv-consult)
;;; bv-consult.el ends here
