;;; bv-navigation.el --- Navigation and window management -*- lexical-binding: t -*-

;;; Commentary:
;; Navigation and window management features
;; Project.el, Dired, ace-window, focus mode, perspectives

;;; Code:

(require 'bv-core)

;;;; External Variable Declarations
(defvar compile-command)
(defvar compilation-buffer-name-function)
(defvar project-compilation-buffer-name-function)
(defvar project-find-functions)
(defvar consult-project-function)
(defvar dired-omit-files)
(defvar dired-omit-verbose)
(defvar dired-summary-regexp)
(defvar dired-find-file-other-window)
(defvar ls-lisp-use-insert-directory-program)
(defvar all-the-icons-dired-monochrome)
(defvar dired-rsync-options)
(defvar bv-var-dir)
(defvar persp-state-default-file)
(defvar persp-show-modestring)
(defvar persp-modestring-dividers)
(defvar consult-buffer-sources)

;;;; Function Declarations
;; Project functions
(declare-function project-root "project" (project))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-prefixed-buffer-name "project" (mode))
(declare-function project-dired "project" ())
(declare-function project-compile "project" (&optional dir))
(declare-function compilation--default-buffer-name "compile" (mode))
(declare-function compilation-read-command "compile" (command))
(declare-function compile "compile" (command &optional comint))
(declare-function project-prompt-project-dir "project" ())
(declare-function project-switch-project "project" (dir))
(declare-function project-find-functions "project" ())
;; Dired functions
(declare-function dired-get-marked-files "dired" (&optional localp arg filter distinguish-one-marked error))
(declare-function dired-hide-details-mode "dired" (&optional arg))
(declare-function dired-omit-mode "dired-x" (&optional arg))
;; Consult functions
(declare-function consult-find "consult" (&optional dir initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function consult-project-buffer "consult" ())
(declare-function consult--buffer-state "consult" ())
;; Olivetti and hide-mode-line
(declare-function global-olivetti-mode "olivetti" (&optional arg))
(declare-function global-hide-mode-line-mode "hide-mode-line" (&optional arg))
(declare-function olivetti-mode "olivetti" (&optional arg))
(declare-function hide-mode-line-mode "hide-mode-line" (&optional arg))
;; Perspective functions
(declare-function persp-switch "perspective" (name))
(declare-function persp-mode "perspective" (&optional arg))

;;;; Custom Variables

(defgroup bv-navigation nil
  "Navigation and window management configuration."
  :group 'bv)

;; Project settings
(bv-defcustom bv-project-root-files
  '(".project" ".projectile" ".dir-locals.el" ".envrc" "Makefile" "package.json")
  "Files that indicate a project root."
  :type '(repeat string)
  :group 'bv-navigation)

(bv-defcustom bv-project-switch-commands '((project-dired "Dired"))
  "Default actions when switching projects."
  :type '(repeat (choice (list :tag "Entry" function string)
                         function))
  :group 'bv-navigation)

;; Dired settings
(bv-defcustom bv-dired-listing-switches
  "-ltu --time=access --format=long --no-group --group-directories-first --almost-all --ignore=.DS_Store"
  "Switches passed to ls for Dired listings."
  :type 'string
  :group 'bv-navigation)

(bv-defcustom bv-dired-kill-when-opening-new-buffer nil
  "Kill Dired buffer when opening a new Dired buffer."
  :type 'boolean
  :group 'bv-navigation)

(bv-defcustom bv-dired-omit-files "^\\.[^.].*"
  "Regexp for files to omit in Dired."
  :type 'string
  :group 'bv-navigation)

(bv-defcustom bv-dired-enable-rainbow t
  "Enable rainbow colors for different file types in Dired."
  :type 'boolean
  :group 'bv-navigation)

(bv-defcustom bv-dired-enable-nerd-icons t
  "Enable nerd icons in Dired."
  :type 'boolean
  :group 'bv-navigation)

;; Window management
(bv-defcustom bv-ace-window-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Keys used for `ace-window' selection."
  :type '(repeat character)
  :group 'bv-navigation)

;; Focus mode settings
(bv-defcustom bv-focus-mode-width 85
  "Width of text body in focus mode."
  :type 'integer
  :group 'bv-navigation)

;; Perspective settings
(bv-defcustom bv-enable-perspectives nil
  "Enable perspective-based workspace management."
  :type 'boolean
  :group 'bv-navigation)

;;;; Helper Functions (defined before use)

;; Custom project root detection
(defun bv-project-try-local (dir)
  "Find project root in DIR by looking for specific files."
  (let ((root (cl-find-if
               (lambda (file)
                 (locate-dominating-file dir file))
               bv-project-root-files)))
    (when root
      (cons 'transient (locate-dominating-file dir root))))

;; Compilation buffer naming
(defun bv-project-compilation-buffer-name (mode)
  "Generate a project-specific compilation buffer name for MODE."
  (if (project-current)
      (project-prefixed-buffer-name mode)
    (compilation--default-buffer-name mode)))

;; Project-specific compile command
(defun bv-project-compile (&optional comint)
  "Run project compilation with optional COMINT mode."
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
              compilation-buffer-name-function)))
    (compile (compilation-read-command compile-command) comint)))

;; Project TODO finder
(defun bv-project-todo ()
  "Find TODO items in current project."
  (interactive)
  (consult-ripgrep (project-root (project-current t))
                   "\\b(TODO|FIXME|HACK|NOTE)\\b"))

;; Project in perspective
(defun bv-project-switch-project-in-perspective (dir)
  "Switch to project DIR in its own perspective."
  (interactive (list (project-prompt-project-dir)))
  (let ((name (file-name-nondirectory
               (directory-file-name
                (file-name-directory dir)))))
    (persp-switch name)
    (project-switch-project dir)))

;; Dired external open
(defun bv-dired-open-externally ()
  "Open marked files in external application."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (call-process "xdg-open" nil 0 nil file))))

;;;; Project.el Configuration

(use-package project
  :ensure nil
  :bind (("s-p" . project-prefix-map)
         :map project-prefix-map
         ("P" . bv-project-switch-project-in-perspective)
         ("F" . consult-find)
         ("R" . consult-ripgrep)
         ("b" . consult-project-buffer)
         ("d" . project-dired)
         ("c" . project-compile)
         ("t" . bv-project-todo))
  :config
  ;; Add custom project finder
  (add-hook 'project-find-functions #'bv-project-try-local 90)

  ;; Better project switching
  (setq project-switch-commands bv-project-switch-commands)
  (setq project-switch-use-entire-map t)

  ;; XDG-compliant project list
  (setq project-list-file
        (expand-file-name "emacs/projects"
                          (or (getenv "XDG_CACHE_HOME") "~/.cache")))

  (setq project-compilation-buffer-name-function
        #'bv-project-compilation-buffer-name)

  (advice-add 'project-compile :override #'bv-project-compile)

  ;; Integration with completion system
  (with-eval-after-load 'consult
    (setq consult-project-function #'project-current)))

;;;; Dired Configuration

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("s-d" . dired-jump)
         :map dired-mode-map
         ("q" . kill-current-buffer)
         ("V" . bv-dired-open-externally)
         ("C-c C-r" . dired-rsync)
         ("M-n" . dired-next-subdir)
         ("M-p" . dired-prev-subdir)
         ("M-u" . dired-up-directory))
  :config
  ;; Better defaults
  (setq dired-dwim-target t
        dired-listing-switches bv-dired-listing-switches
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer bv-dired-kill-when-opening-new-buffer
        delete-by-moving-to-trash t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-hide-details-hide-symlink-targets nil)

  ;; Find file in other window (if variable exists)
  (when (boundp 'dired-find-file-other-window)
    (setq dired-find-file-other-window t))

  ;; Auto-refresh
  (setq dired-auto-revert-buffer t)

  ;; Hide details by default
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; Line truncation
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)

  ;; Use ls-lisp on non-Linux platforms
  (when (or (eq system-type 'windows-nt)
            (eq system-type 'darwin))
    (setq ls-lisp-use-insert-directory-program nil)
    (require 'ls-lisp)))

;; Dired enhancements
(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-x-hands-off-my-keys t)
  (setq dired-clean-up-buffers-too t)
  ;; Omit configuration
  (setq dired-omit-files bv-dired-omit-files
        dired-omit-verbose nil)
  ;; Summary line regexp (remove colons)
  (setq dired-summary-regexp
        (replace-regexp-in-string ":" "" "^.*: "))
  ;; Enable omit mode by default
  (add-hook 'dired-mode-hook #'dired-omit-mode))

;; Dired rainbow - colorize files by type
(when bv-dired-enable-rainbow
  (use-package dired-rainbow
    :after dired
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286"
      ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024"
      ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2"
      ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a"
      ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd"
      ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f"
      ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b"
      ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f"
      ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172"
      ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5"
      ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a"
      ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63"
      ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a"
      ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb"
      ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f"
      ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9"
      ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

;; Nerd icons in dired
(when bv-dired-enable-nerd-icons
  (use-package nerd-icons-dired
    :after (dired nerd-icons)
    :hook (dired-mode . nerd-icons-dired-mode)))

;; Icons in dired (all-the-icons - kept for compatibility)
(bv-when-feature bv-ui
  (when (and (bv-get-value 'ui-enable-icons)
             (not bv-dired-enable-nerd-icons))
    (use-package all-the-icons-dired
      :hook (dired-mode . all-the-icons-dired-mode)
      :config
      (when (boundp 'all-the-icons-dired-monochrome)
        (setq all-the-icons-dired-monochrome nil)))))

;; Dired rsync
(use-package dired-rsync
  :after dired
  :config
  (setq dired-rsync-options
        "--archive --verbose --compress --human-readable --progress --delete"))

;;;; Ace Window - Quick Window Switching

(use-package ace-window
  :bind (("M-o" . ace-window)
         ("s-o" . ace-window))
  :config
  (setq aw-keys bv-ace-window-keys
        aw-scope 'frame
        aw-background nil
        aw-display-mode-overlay t
        aw-ignore-current nil)

  ;; Custom actions
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?e aw-execute-command-other-window "Execute Command")
          (?F aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help)))

  ;; Face customization
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

;;;; Focus Mode (Monocle)

(use-package olivetti
  :commands olivetti-mode
  :config
  (setq olivetti-body-width bv-focus-mode-width
        olivetti-margin-width 0
        olivetti-minimum-body-width 40
        olivetti-recall-visual-line-mode-entry-state t))

;; Hide mode line
(use-package hide-mode-line
  :commands hide-mode-line-mode)

;; Focus mode toggle
(defvar-local bv--focus-mode-previous-config nil
  "Store previous window configuration for focus mode.")

(defun bv-toggle-focus-mode (&optional arg)
  "Toggle focus mode for distraction-free editing.
With prefix ARG, toggle globally."
  (interactive "P")
  (if arg
      ;; Global toggle
      (progn
        (if (and (bound-and-true-p global-olivetti-mode)
                 (bound-and-true-p global-hide-mode-line-mode))
            (progn
              (global-hide-mode-line-mode -1)
              (global-olivetti-mode -1))
          (progn
            (global-hide-mode-line-mode 1)
            (global-olivetti-mode 1))))
    ;; Local toggle with window configuration
    (if (one-window-p)
        (when bv--focus-mode-previous-config
          (set-window-configuration bv--focus-mode-previous-config)
          (setq bv--focus-mode-previous-config nil))
      (setq bv--focus-mode-previous-config (current-window-configuration))
      (delete-other-windows))))

;; Bind the toggle function
(global-set-key (kbd "s-f") #'bv-toggle-focus-mode)

;; Define focus mode minor mode
(define-minor-mode bv-focus-mode
  "Minor mode for focused editing."
  :init-value nil
  :lighter " Focus"
  (if bv-focus-mode
      (progn
        (olivetti-mode 1)
        (hide-mode-line-mode 1))
    (olivetti-mode -1)
    (hide-mode-line-mode -1)))

;;;; Window Management Utilities

;; Better window splitting
(defun bv-split-window-below-and-switch ()
  "Split window below and switch to it."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun bv-split-window-right-and-switch ()
  "Split window right and switch to it."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") #'bv-split-window-below-and-switch)
(global-set-key (kbd "C-x 3") #'bv-split-window-right-and-switch)

;; Window resizing
(defun bv-window-resize (key)
  "Interactively resize windows with KEY."
  (interactive "cPress {/} to shrink/enlarge horizontally, [/] vertically")
  (let ((horizontal (memq key '(?{ ?})))
        (shrink (memq key '(?{ ?\[))))
    (enlarge-window (if shrink -3 3) horizontal)))

(global-set-key (kbd "C-x }") #'bv-window-resize)
(global-set-key (kbd "C-x {") #'bv-window-resize)

;;;; Perspective (Optional Workspace Management)

(when bv-enable-perspectives
  (use-package perspective
    :bind (("C-x x s" . persp-switch)
           ("C-x x k" . persp-kill)
           ("C-x x r" . persp-rename)
           ("C-x x a" . persp-add-buffer)
           ("C-x x A" . persp-set-buffer)
           ("C-x x i" . persp-import)
           ("C-x x n" . persp-next)
           ("C-x x p" . persp-prev)
           ("C-x x c" . persp-kill-buffer))
    :custom
    (persp-mode-prefix-key (kbd "C-x x"))
    :init
    (when (boundp 'bv-var-dir)
      (setq persp-state-default-file
            (expand-file-name "perspective-state" bv-var-dir)))
    :config
    (setq persp-show-modestring t
          persp-modestring-dividers '("[" "]" "|"))

    (persp-mode)))

;;;; Better Buffer Management

(defun bv-kill-current-buffer ()
  "Kill current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun bv-kill-buffer-and-window ()
  "Kill current buffer and delete window."
  (interactive)
  (kill-current-buffer)
  (when (not (one-window-p))
    (delete-window)))

;; Key bindings
(global-set-key (kbd "s-w") #'bv-kill-current-buffer)
(global-set-key (kbd "s-W") #'bv-kill-buffer-and-window)

;;;; Integration with Completion System

;; Consult buffer source for project buffers
(with-eval-after-load 'consult
  (defvar bv-consult-source-project-buffer
    `(:name "Project Buffer"
            :narrow ?p
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :default t
            :items
            ,(lambda ()
               (when-let (project (project-current))
                 (mapcar #'buffer-name
                         (seq-filter
                          (lambda (b)
                            (when-let (file (buffer-file-name b))
                              (file-in-directory-p file (project-root project))))
                          (buffer-list))))))
    "Project buffer candidate source for `consult-buffer'.")

  ;; Add to consult sources
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'bv-consult-source-project-buffer 'append)))

;;;; Feature Registration

(bv-register-feature 'bv-navigation)

(provide 'bv-navigation)
;;; bv-navigation.el ends here
