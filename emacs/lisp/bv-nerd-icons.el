;;; bv-nerd-icons.el --- Nerd Icons configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module configures nerd-icons for displaying icons throughout Emacs.
;; It replaces all-the-icons with nerd-icons for better terminal support
;; and consistent icon display.

;;; Code:

(require 'nerd-icons)
(require 'nerd-icons-completion)
(require 'nerd-icons-dired)
(require 'nerd-icons-ibuffer)

;; Declare external variables to avoid warnings
(defvar nerd-icons-completion-mode-hooks)
(defvar which-key-prefix-prefix)
(defvar marginalia-mode)

;; Declare external functions
(declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
(declare-function nerd-icons-completion-completion-metadata-get "nerd-icons-completion" (metadata category))
(declare-function treemacs-load-theme "treemacs" (theme-name))
(declare-function nerd-icons-icon-for-mode "nerd-icons" (mode))
(declare-function nerd-icons-insert "nerd-icons")
(declare-function nerd-icons-icon-for-file "nerd-icons" (file))
(declare-function nerd-icons-install-fonts "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))
(declare-function bv-completion-icons-enabled-p "bv-completion" (&optional width))

(defgroup bv-nerd-icons nil
  "Nerd Icons configuration."
  :group 'bv)

(defface bv-icon-default
  '((t :inherit default))
  "Default BV icon face."
  :group 'bv-nerd-icons)

(defface bv-icon-muted
  '((t :inherit shadow))
  "Muted BV icon face."
  :group 'bv-nerd-icons)

(defface bv-icon-file
  '((t :inherit bv-icon-default))
  "BV icon face for plain files."
  :group 'bv-nerd-icons)

(defface bv-icon-directory
  '((t :inherit font-lock-function-name-face))
  "BV icon face for directories and project containers."
  :group 'bv-nerd-icons)

(defface bv-icon-note
  '((t :inherit bv-icon-default))
  "BV icon face for notes and prose documents."
  :group 'bv-nerd-icons)

(defface bv-icon-code
  '((t :inherit font-lock-function-name-face))
  "BV icon face for code."
  :group 'bv-nerd-icons)

(defface bv-icon-science
  '((t :inherit success))
  "BV icon face for research/science material."
  :group 'bv-nerd-icons)

(defface bv-icon-idea
  '((t :inherit font-lock-keyword-face))
  "BV icon face for ideas and concepts."
  :group 'bv-nerd-icons)

(defface bv-icon-proof
  '((t :inherit success))
  "BV icon face for theorems and proofs."
  :group 'bv-nerd-icons)

(defface bv-icon-review
  '((t :inherit font-lock-doc-face))
  "BV icon face for review/search surfaces."
  :group 'bv-nerd-icons)

(defface bv-icon-index
  '((t :inherit shadow))
  "BV icon face for indexes and lists."
  :group 'bv-nerd-icons)

(defface bv-icon-system
  '((t :inherit font-lock-builtin-face))
  "BV icon face for tools, systems, and infrastructure."
  :group 'bv-nerd-icons)

(defface bv-icon-warning
  '((t :inherit warning))
  "BV icon face for cautionary or high-attention items."
  :group 'bv-nerd-icons)

(defface bv-icon-success
  '((t :inherit success))
  "BV icon face for affirmative status."
  :group 'bv-nerd-icons)

(defface bv-icon-special
  '((t :inherit font-lock-keyword-face))
  "BV icon face for special-purpose items."
  :group 'bv-nerd-icons)

(defface bv-icon-info
  '((t :inherit font-lock-doc-face))
  "BV icon face for informational surfaces."
  :group 'bv-nerd-icons)

(defface bv-icon-salient
  '((t :inherit font-lock-function-name-face))
  "BV icon face for prominent navigational marks."
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-font-family "Symbols Nerd Font Mono"
  "The Nerd Font family to use for icons."
  :type 'string
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-scale-factor 1.0
  "Scaling factor for nerd icons."
  :type 'number
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-color-icons t
  "Whether to use colored icons."
  :type 'boolean
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-font-ranges
  '((#xe000 . #xf8ff)
    (#xf0000 . #xffffd)
    (#x100000 . #x10fffd))
  "Unicode private-use ranges assigned to the Nerd Font icon family.

Keeping Nerd Fonts inside private-use ranges prevents the icon font from
stealing ordinary text, math, CJK, or symbol glyphs from the main font stack."
  :type '(repeat (cons integer integer))
  :group 'bv-nerd-icons)

;;; Core nerd-icons configuration
(defun bv-nerd-icons-configure-core ()
  "Configure core nerd-icons settings."
  ;; Set the font family for GUI mode
  (setq nerd-icons-font-family bv-nerd-icons-font-family)

  ;; Enable colored icons
  (setq nerd-icons-color-icons bv-nerd-icons-color-icons)

  ;; Set default scale
  (setq nerd-icons-scale-factor bv-nerd-icons-scale-factor)

  ;; Add custom file associations
  (dolist (ext-icon '(("scm" nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-code)
                      ("rkt" nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-code)
                      ("fnl" nerd-icons-sucicon "nf-seti-lua" :face bv-icon-code)
                      ("janet" nerd-icons-sucicon "nf-custom-clojure" :face bv-icon-code)
                      ("epub" nerd-icons-faicon "nf-fa-book" :face bv-icon-note)
                      ("guix" nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-system)
                      ("nix" nerd-icons-mdicon "nf-md-nix" :face bv-icon-system)
                      ("direnv" nerd-icons-octicon "nf-oct-file_directory" :face bv-icon-directory)))
    (add-to-list 'nerd-icons-extension-icon-alist ext-icon))

  ;; Add custom regex associations
  (dolist (regex-icon '(("\\.?guix" nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-system)
                        ("Dockerfile" nerd-icons-devicon "nf-dev-docker" :face bv-icon-system)
                        ("docker-compose" nerd-icons-devicon "nf-dev-docker" :face bv-icon-system)
                        ("\\.envrc" nerd-icons-octicon "nf-oct-file_directory" :face bv-icon-directory)
                        ("Makefile" nerd-icons-devicon "nf-dev-gnu" :face bv-icon-system)
                        ("CMakeLists.txt" nerd-icons-devicon "nf-dev-cmake" :face bv-icon-warning)
                        ("LICENSE" nerd-icons-octicon "nf-oct-law" :face bv-icon-muted)
                        ("README" nerd-icons-octicon "nf-oct-book" :face bv-icon-note)))
    (add-to-list 'nerd-icons-regexp-icon-alist regex-icon))

  ;; Add mode icons
  (dolist (mode-icon '((emacs-lisp-mode nerd-icons-sucicon "nf-custom-emacs" :face bv-icon-code)
                       (scheme-mode nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-code)
                       (geiser-mode nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-code)
                       (racket-mode nerd-icons-sucicon "nf-custom-scheme" :face bv-icon-code)
                       (clojure-mode nerd-icons-devicon "nf-dev-clojure" :face bv-icon-code)
                       (fennel-mode nerd-icons-sucicon "nf-seti-lua" :face bv-icon-code)
                       (python-mode nerd-icons-devicon "nf-dev-python" :face bv-icon-code)
                       (rust-mode nerd-icons-devicon "nf-dev-rust" :face bv-icon-code)
                       (go-mode nerd-icons-devicon "nf-dev-go" :face bv-icon-code)
                       (js-mode nerd-icons-devicon "nf-dev-javascript_badge" :face bv-icon-code)
                       (typescript-mode nerd-icons-devicon "nf-dev-typescript" :face bv-icon-code)
                       (web-mode nerd-icons-devicon "nf-dev-html5" :face bv-icon-code)
                       (org-mode nerd-icons-octicon "nf-oct-note" :face bv-icon-note)
                       (markdown-mode nerd-icons-octicon "nf-oct-markdown" :face bv-icon-note)
                       (latex-mode nerd-icons-faicon "nf-fa-file_pdf_o" :face bv-icon-warning)
                       (pdf-view-mode nerd-icons-faicon "nf-fa-file_pdf_o" :face bv-icon-warning)
                       (image-mode nerd-icons-faicon "nf-fa-file_image_o" :face bv-icon-special)
                       (magit-mode nerd-icons-devicon "nf-dev-git" :face bv-icon-system)
                       (dired-mode nerd-icons-octicon "nf-oct-file_directory" :face bv-icon-directory)
                       (ibuffer-mode nerd-icons-faicon "nf-fa-list" :face bv-icon-system)
                       (help-mode nerd-icons-faicon "nf-fa-question_circle" :face bv-icon-info)
                       (Info-mode nerd-icons-faicon "nf-fa-info_circle" :face bv-icon-info)
                       (eshell-mode nerd-icons-devicon "nf-dev-terminal" :face bv-icon-system)
                       (term-mode nerd-icons-devicon "nf-dev-terminal" :face bv-icon-system)
                       (vterm-mode nerd-icons-devicon "nf-dev-terminal" :face bv-icon-system)))
    (add-to-list 'nerd-icons-mode-icon-alist mode-icon)))

;;; Completion configuration
(defun bv-nerd-icons-configure-completion ()
  "Configure nerd-icons for completion frameworks."
  ;; Enable nerd-icons-completion globally
  (nerd-icons-completion-mode 1)

  ;; Set up marginalia integration
  (with-eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
    ;; If marginalia-mode is already active, set it up immediately
    (when (bound-and-true-p marginalia-mode)
      (nerd-icons-completion-marginalia-setup)))

  ;; Configure completion categories
  (when (boundp 'nerd-icons-completion-mode-hooks)
    (setq nerd-icons-completion-mode-hooks
          '(marginalia-mode-hook
            selectrum-mode-hook
            icomplete-mode-hook
            vertico-mode-hook)))

  (with-eval-after-load 'bv-completion
    (advice-add #'nerd-icons-completion-get-icon
                :around #'bv-nerd-icons-completion-get-icon)))

(defun bv-nerd-icons-completion-get-icon (orig-fun cand category)
  "Return completion icon from ORIG-FUN for CAND and CATEGORY when policy allows."
  (if (and (fboundp 'bv-completion-icons-enabled-p)
           (not (bv-completion-icons-enabled-p)))
      ""
    (funcall orig-fun cand category)))

;;; Dired configuration
(defun bv-nerd-icons-configure-dired ()
  "Configure nerd-icons for Dired mode."
  ;; Enable nerd-icons in dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

  ;; Configure dired to work better with icons
  (with-eval-after-load 'dired
    ;; Ensure proper alignment.  Icon colors are owned by the BV theme adapter.
    (setq dired-listing-switches "-alh --group-directories-first")))

;;; Ibuffer configuration
(defun bv-nerd-icons-configure-ibuffer ()
  "Configure nerd-icons for ibuffer mode."
  ;; Enable nerd-icons in ibuffer
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

  ;; Configure ibuffer settings
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon bv-nerd-icons-color-icons)
  (setq nerd-icons-ibuffer-icon-size 1.0)
  (setq nerd-icons-ibuffer-human-readable-size t)

  ;; Use custom ibuffer formats with icons
  (setq nerd-icons-ibuffer-formats
        '((mark " " modified " " read-only
                "  " nerd-icons-ibuffer-icon
                "  " (name 32 32 :left :elide)
                "  " (size-h 9 -1 :right)
                "  " (mode+ 16 16 :left :elide)
                "  " filename-and-process+)
          (mark "  " (name 32 -1)
                "  " filename))))

;;; Additional integrations
(defun bv-nerd-icons-configure-integrations ()
  "Configure nerd-icons for various other packages."
  ;; Treemacs integration
  (with-eval-after-load 'treemacs
    (when (fboundp 'treemacs-load-theme)
      (treemacs-load-theme "nerd-icons")))
  
  ;; Company integration
  (with-eval-after-load 'company
    (when (fboundp 'nerd-icons-icon-for-mode)
      (advice-add 'company-fill-propertize :filter-args
                  (lambda (args)
                    (let ((candidate (car args)))
                      (cons (bv-nerd-icons-company-format candidate)
                            (cdr args)))))))
  
  ;; Which-key integration
  (with-eval-after-load 'which-key
    (when (boundp 'which-key-prefix-prefix)
      (setq which-key-prefix-prefix
	    (concat (nerd-icons-faicon "nf-fa-caret_right"
	                               :face 'bv-icon-salient)
	            " ")))))

;;; Helper functions
(defun bv-nerd-icons-company-format (candidate)
  "Add icons to company CANDIDATE."
  (let ((icon (nerd-icons-icon-for-mode
               (get-text-property 0 'company-backend candidate))))
    (if (not (eq icon major-mode))
        (concat icon " " candidate)
      candidate)))

(defun bv-nerd-icons-insert-icon ()
  "Interactively insert a nerd icon."
  (interactive)
  (nerd-icons-insert))

(defun bv-nerd-icons-insert-icon-for-file ()
  "Insert an icon for a file."
  (interactive)
  (let ((file (read-file-name "File: ")))
    (insert (nerd-icons-icon-for-file file))))

(defun bv-nerd-icons-install-fonts ()
  "Install the Symbols Nerd Font."
  (interactive)
  (nerd-icons-install-fonts))

;;; Font setup
(defun bv-nerd-icons--font-available-p (family)
  "Return non-nil when icon font FAMILY is available."
  (and (stringp family)
       (> (length family) 0)
       (find-font (font-spec :family family))))

(defun bv-nerd-icons-setup-fonts ()
  "Ensure nerd fonts are properly configured."
  ;; Set font for icons in GUI
  (when (and (display-graphic-p)
             (bv-nerd-icons--font-available-p bv-nerd-icons-font-family))
    (dolist (range bv-nerd-icons-font-ranges)
      (set-fontset-font t range
                        (font-spec :family bv-nerd-icons-font-family)
                        nil 'prepend))))

;;; Main initialization
(defun bv-nerd-icons-init ()
  "Initialize nerd-icons configuration."
  (bv-nerd-icons-configure-core)
  (bv-nerd-icons-configure-completion)
  (bv-nerd-icons-configure-dired)
  (bv-nerd-icons-configure-ibuffer)
  (bv-nerd-icons-configure-integrations)
  (bv-nerd-icons-setup-fonts)
  
  ;; Set up keybindings
  (global-set-key (kbd "C-c i i") #'bv-nerd-icons-insert-icon)
  (global-set-key (kbd "C-c i f") #'bv-nerd-icons-insert-icon-for-file)
  
  ;; Optimize performance
  (setq inhibit-compacting-font-caches t))

;;; Initialize nerd-icons when the module is loaded
(bv-nerd-icons-init)

;;; Feature provision
(provide 'bv-nerd-icons)

;;; bv-nerd-icons.el ends here
