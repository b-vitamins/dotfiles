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

(defgroup bv-nerd-icons nil
  "Nerd Icons configuration."
  :group 'bv)

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
  (dolist (ext-icon '(("scm" nerd-icons-sucicon "nf-seti-scheme" :face nerd-icons-red)
                      ("rkt" nerd-icons-sucicon "nf-seti-scheme" :face nerd-icons-red-alt)
                      ("fnl" nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lblue)
                      ("janet" nerd-icons-sucicon "nf-custom-janet" :face nerd-icons-purple)
                      ("epub" nerd-icons-faicon "nf-fa-book" :face nerd-icons-green)
                      ("guix" nerd-icons-sucicon "nf-custom-scheme" :face nerd-icons-orange)
                      ("nix" nerd-icons-mdicon "nf-md-nix" :face nerd-icons-blue)
                      ("direnv" nerd-icons-octicon "nf-oct-file_directory" :face nerd-icons-yellow)))
    (add-to-list 'nerd-icons-extension-icon-alist ext-icon))
  
  ;; Add custom regex associations
  (dolist (regex-icon '(("\\.?guix" nerd-icons-sucicon "nf-custom-scheme" :face nerd-icons-orange)
                        ("Dockerfile" nerd-icons-devicon "nf-dev-docker" :face nerd-icons-cyan)
                        ("docker-compose" nerd-icons-devicon "nf-dev-docker" :face nerd-icons-cyan)
                        ("\\.envrc" nerd-icons-octicon "nf-oct-file_directory" :face nerd-icons-yellow)
                        ("Makefile" nerd-icons-devicon "nf-dev-gnu" :face nerd-icons-dorange)
                        ("CMakeLists.txt" nerd-icons-devicon "nf-dev-cmake" :face nerd-icons-red)
                        ("LICENSE" nerd-icons-octicon "nf-oct-law" :face nerd-icons-dgray)
                        ("README" nerd-icons-octicon "nf-oct-book" :face nerd-icons-lcyan)))
    (add-to-list 'nerd-icons-regexp-icon-alist regex-icon))
  
  ;; Add mode icons
  (dolist (mode-icon '((emacs-lisp-mode nerd-icons-sucicon "nf-custom-emacs" :face nerd-icons-purple)
                       (scheme-mode nerd-icons-sucicon "nf-seti-scheme" :face nerd-icons-red)
                       (geiser-mode nerd-icons-sucicon "nf-seti-scheme" :face nerd-icons-red)
                       (racket-mode nerd-icons-sucicon "nf-seti-scheme" :face nerd-icons-red-alt)
                       (clojure-mode nerd-icons-devicon "nf-dev-clojure" :face nerd-icons-green)
                       (fennel-mode nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lblue)
                       (python-mode nerd-icons-devicon "nf-dev-python" :face nerd-icons-dblue)
                       (rust-mode nerd-icons-devicon "nf-dev-rust" :face nerd-icons-maroon)
                       (go-mode nerd-icons-devicon "nf-dev-go" :face nerd-icons-blue)
                       (js-mode nerd-icons-devicon "nf-dev-javascript_badge" :face nerd-icons-yellow)
                       (typescript-mode nerd-icons-devicon "nf-dev-typescript" :face nerd-icons-blue-alt)
                       (web-mode nerd-icons-devicon "nf-dev-html5" :face nerd-icons-orange)
                       (org-mode nerd-icons-faicon "nf-fa-file_text_o" :face nerd-icons-lgreen)
                       (markdown-mode nerd-icons-octicon "nf-oct-markdown" :face nerd-icons-lblue)
                       (latex-mode nerd-icons-faicon "nf-fa-file_pdf_o" :face nerd-icons-maroon)
                       (pdf-view-mode nerd-icons-faicon "nf-fa-file_pdf_o" :face nerd-icons-dred)
                       (image-mode nerd-icons-faicon "nf-fa-file_image_o" :face nerd-icons-orange)
                       (magit-mode nerd-icons-devicon "nf-dev-git" :face nerd-icons-red)
                       (dired-mode nerd-icons-octicon "nf-oct-file_directory" :face nerd-icons-dblue)
                       (ibuffer-mode nerd-icons-faicon "nf-fa-list" :face nerd-icons-cyan)
                       (help-mode nerd-icons-faicon "nf-fa-question_circle" :face nerd-icons-purple)
                       (Info-mode nerd-icons-faicon "nf-fa-info_circle" :face nerd-icons-blue)
                       (eshell-mode nerd-icons-devicon "nf-dev-terminal" :face nerd-icons-purple)
                       (term-mode nerd-icons-devicon "nf-dev-terminal" :face nerd-icons-purple)
                       (vterm-mode nerd-icons-devicon "nf-dev-terminal" :face nerd-icons-purple)))
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
  (setq nerd-icons-completion-mode-hooks
        '(marginalia-mode-hook
          selectrum-mode-hook
          icomplete-mode-hook
          vertico-mode-hook))
  
  ;; Ensure icons appear in all completion UIs
  (advice-add #'completion-metadata-get :around #'nerd-icons-completion-completion-metadata-get))

;;; Dired configuration
(defun bv-nerd-icons-configure-dired ()
  "Configure nerd-icons for dired mode."
  ;; Enable nerd-icons in dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  
  ;; Configure dired to work better with icons
  (with-eval-after-load 'dired
    ;; Ensure proper alignment
    (setq dired-listing-switches "-alh --group-directories-first")
    
    ;; Custom face for directories
    (set-face-attribute 'nerd-icons-dired-dir-face nil
                        :foreground (face-attribute 'dired-directory :foreground))))

;;; Ibuffer configuration
(defun bv-nerd-icons-configure-ibuffer ()
  "Configure nerd-icons for ibuffer mode."
  ;; Enable nerd-icons in ibuffer
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  
  ;; Configure ibuffer settings
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-icon-size 1.0)
  (setq nerd-icons-ibuffer-human-readable-size t)
  
  ;; Use custom ibuffer formats with icons
  (setq nerd-icons-ibuffer-formats
        '((mark modified read-only nerd-icons-ibuffer-icon
                " " (name 30 30 :left :elide)
                " " (size-h 9 -1 :right)
                " " (mode+ 16 16 :left :elide)
                " " filename-and-process+)
          (mark " " (name 30 -1)
                " " filename))))

;;; Modeline configuration
(defun bv-nerd-icons-configure-modeline ()
  "Configure nerd-icons for the modeline."
  ;; Add icon to mode name in modeline
  (defun bv-nerd-icons-mode-line-mode-icon ()
    "Return an icon for the current major mode."
    (let ((icon (nerd-icons-icon-for-mode major-mode)))
      (if (not (eq icon major-mode))
          (concat icon " ")
        "")))
  
  ;; Advise mode-line-mode-name to include icon
  (advice-add 'format-mode-line :around
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  (if (and (stringp result)
                           (string-match-p "%" result))
                      result
                    (if (stringp result)
                        (concat (bv-nerd-icons-mode-line-mode-icon) result)
                      result))))))

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
      (defun bv-nerd-icons-company-format (candidate)
        "Add icons to company candidates."
        (let ((icon (nerd-icons-icon-for-mode
                     (get-text-property 0 'company-backend candidate))))
          (if (not (eq icon major-mode))
              (concat icon " " candidate)
            candidate)))
      
      (advice-add 'company-fill-propertize :filter-args
                  (lambda (args)
                    (let ((candidate (car args)))
                      (cons (bv-nerd-icons-company-format candidate)
                            (cdr args)))))))
  
  ;; Which-key integration
  (with-eval-after-load 'which-key
    (setq which-key-prefix-prefix
          (concat (nerd-icons-faicon "nf-fa-caret_right"
                                     :face 'nerd-icons-green)
                  " "))))

;;; Helper functions
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
(defun bv-nerd-icons-setup-fonts ()
  "Ensure nerd fonts are properly configured."
  ;; Set font for icons in GUI
  (when (display-graphic-p)
    (set-fontset-font t 'unicode
                      (font-spec :family bv-nerd-icons-font-family)
                      nil 'prepend)))

;;; Main initialization
(defun bv-nerd-icons-init ()
  "Initialize nerd-icons configuration."
  (bv-nerd-icons-configure-core)
  (bv-nerd-icons-configure-completion)
  (bv-nerd-icons-configure-dired)
  (bv-nerd-icons-configure-ibuffer)
  (bv-nerd-icons-configure-modeline)
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