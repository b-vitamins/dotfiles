;;; bv-nerd-icons.el --- Role-based Nerd Icons configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Icons are semantic punctuation, not decoration.  This module keeps Nerd Font
;; glyphs behind a small BV role registry so completion rows, Dired, Ibuffer,
;; Which-Key, and mode/file icons share one visual grammar.

;;; Code:

(require 'cl-lib)
(require 'nerd-icons)
(require 'nerd-icons-completion)
(require 'nerd-icons-dired)
(require 'nerd-icons-ibuffer)

;;; Declarations

(defvar bv-fonts-icon-family "Symbols Nerd Font Mono")
(defvar marginalia-mode)
(defvar nerd-icons-completion-mode-hooks)
(defvar which-key-prefix-prefix)

(declare-function bv-completion-icons-enabled-p "bv-completion" (&optional width))
(declare-function bv-completion-width-class "bv-completion" (&optional width))
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-completion-get-icon "nerd-icons-completion" (cand category))
(declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
(declare-function nerd-icons-devicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-icon-for-file "nerd-icons" (file &rest arg-overrides))
(declare-function nerd-icons-icon-for-mode "nerd-icons" (mode &rest arg-overrides))
(declare-function nerd-icons-insert "nerd-icons" (&optional arg glyph-set))
(declare-function nerd-icons-install-fonts "nerd-icons" (&optional pfx))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-octicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-sucicon "nerd-icons" (icon-name &rest args))
(declare-function treemacs-load-theme "treemacs" (theme-name))

;;; Faces

(defgroup bv-nerd-icons nil
  "Role-based Nerd Icons configuration."
  :group 'bv
  :prefix "bv-nerd-icons-")

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

;;; Policy

(defcustom bv-nerd-icons-font-family nil
  "Explicit Nerd Font family override.

When nil, use `bv-fonts-icon-family' so typography remains the source of truth
for icon font selection."
  :type '(choice (const :tag "Use bv-fonts-icon-family" nil)
                 string)
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-scale-factor 1.0
  "Scaling factor for Nerd Font icons."
  :type 'number
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-style 'semantic
  "Color policy for BV-owned icon roles.

`semantic' uses the role face from `bv-nerd-icons-role-alist'.
`muted' renders all BV-owned roles with `bv-icon-muted'.
`monochrome' renders all BV-owned roles with `bv-icon-default'.
`native' keeps upstream Nerd Icons colors where a native face is specified."
  :type '(choice (const :tag "Semantic BV role colors" semantic)
                 (const :tag "Muted BV role colors" muted)
                 (const :tag "Monochrome BV role colors" monochrome)
                 (const :tag "Native Nerd Icons colors" native))
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

(defcustom bv-nerd-icons-enable-company-integration nil
  "Non-nil enables legacy Company icon advice.

Corfu is the primary in-buffer completion surface in this configuration, so the
Company integration is deliberately opt-in."
  :type 'boolean
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-enable-which-key-prefix t
  "Non-nil uses the BV icon registry for Which-Key group prefixes."
  :type 'boolean
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-completion-category-policy
  '((file . always)
    (project-file . always)
    (buffer . adaptive)
    (command . wide)
    (symbol . wide)
    (function . wide)
    (variable . wide)
    (consult-location . adaptive)
    (org-slipbox-node . always)
    (ssh-host . adaptive)
    (cape-yasnippet . wide)
    (t . adaptive))
  "Category-aware icon policy for completion candidates.

Values are `always', `adaptive', `wide', or `never'.  File, project, and note
surfaces keep icons as identity.  Command and symbol-heavy surfaces show icons
only when there is enough horizontal room."
  :type '(alist :key-type symbol
                :value-type (choice (const always)
                                    (const adaptive)
                                    (const wide)
                                    (const never)))
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-role-alist
  '((file :set octicon :icon "nf-oct-file" :face bv-icon-file
          :native-face nerd-icons-silver :family ui)
    (directory :set octicon :icon "nf-oct-file_directory"
               :face bv-icon-directory :native-face nerd-icons-blue
               :family ui)
    (project :set octicon :icon "nf-oct-repo" :face bv-icon-directory
             :native-face nerd-icons-blue :family ui)
    (note :set octicon :icon "nf-oct-note" :face bv-icon-note
          :native-face nerd-icons-cyan :family document)
    (book :set octicon :icon "nf-oct-book" :face bv-icon-note
          :native-face nerd-icons-cyan :family document)
    (markdown :set octicon :icon "nf-oct-markdown" :face bv-icon-note
              :native-face nerd-icons-blue :family document)
    (pdf :set codicon :icon "nf-cod-file_pdf" :face bv-icon-warning
         :native-face nerd-icons-red :family document)
    (image :set octicon :icon "nf-oct-file_media" :face bv-icon-special
           :native-face nerd-icons-purple :family document)
    (code :set octicon :icon "nf-oct-code" :face bv-icon-code
          :native-face nerd-icons-blue :family ui)
    (scheme :set sucicon :icon "nf-custom-scheme" :face bv-icon-code
            :native-face nerd-icons-red :family language-logo)
    (clojure :set devicon :icon "nf-dev-clojure" :face bv-icon-code
             :native-face nerd-icons-green :family language-logo)
    (fennel :set sucicon :icon "nf-seti-lua" :face bv-icon-code
            :native-face nerd-icons-blue :family language-logo)
    (python :set devicon :icon "nf-dev-python" :face bv-icon-code
            :native-face nerd-icons-dblue :family language-logo)
    (rust :set devicon :icon "nf-dev-rust" :face bv-icon-code
          :native-face nerd-icons-maroon :family language-logo)
    (go :set sucicon :icon "nf-seti-go2" :face bv-icon-code
        :native-face nerd-icons-blue :family language-logo)
    (javascript :set devicon :icon "nf-dev-javascript_badge"
                :face bv-icon-code :native-face nerd-icons-yellow
                :family language-logo)
    (typescript :set devicon :icon "nf-dev-typescript"
                :face bv-icon-code :native-face nerd-icons-blue
                :family language-logo)
    (web :set devicon :icon "nf-dev-html5" :face bv-icon-code
         :native-face nerd-icons-orange :family language-logo)
    (latex :set codicon :icon "nf-cod-file_pdf" :face bv-icon-warning
           :native-face nerd-icons-red :family document)
    (git :set devicon :icon "nf-dev-git" :face bv-icon-system
         :native-face nerd-icons-red :family tool)
    (terminal :set devicon :icon "nf-dev-terminal" :face bv-icon-system
              :native-face nerd-icons-purple :family tool)
    (dired :set octicon :icon "nf-oct-file_directory" :face bv-icon-directory
           :native-face nerd-icons-blue :family ui)
    (ibuffer :set faicon :icon "nf-fa-list" :face bv-icon-system
             :native-face nerd-icons-silver :family ui)
    (help :set faicon :icon "nf-fa-question_circle" :face bv-icon-info
          :native-face nerd-icons-blue :family ui)
    (info :set faicon :icon "nf-fa-info_circle" :face bv-icon-info
          :native-face nerd-icons-blue :family ui)
    (review :set octicon :icon "nf-oct-checklist" :face bv-icon-review
            :native-face nerd-icons-blue :family ui)
    (science :set mdicon :icon "nf-md-flask" :face bv-icon-science
             :native-face nerd-icons-green :family domain)
    (idea :set octicon :icon "nf-oct-light_bulb" :face bv-icon-idea
          :native-face nerd-icons-yellow :family domain)
    (proof :set octicon :icon "nf-oct-shield_check" :face bv-icon-proof
           :native-face nerd-icons-green :family domain)
    (index :set octicon :icon "nf-oct-list_unordered" :face bv-icon-index
           :native-face nerd-icons-silver :family ui)
    (warning :set octicon :icon "nf-oct-alert" :face bv-icon-warning
             :native-face nerd-icons-yellow :family state)
    (success :set octicon :icon "nf-oct-check_circle" :face bv-icon-success
             :native-face nerd-icons-green :family state)
    (special :set octicon :icon "nf-oct-star" :face bv-icon-special
             :native-face nerd-icons-purple :family state)
    (system :set octicon :icon "nf-oct-tools" :face bv-icon-system
            :native-face nerd-icons-silver :family tool)
    (license :set octicon :icon "nf-oct-law" :face bv-icon-muted
             :native-face nerd-icons-silver :family document)
    (docker :set devicon :icon "nf-dev-docker" :face bv-icon-system
            :native-face nerd-icons-blue :family tool)
    (make :set devicon :icon "nf-dev-gnu" :face bv-icon-system
          :native-face nerd-icons-orange :family tool)
    (cmake :set devicon :icon "nf-dev-cmake" :face bv-icon-warning
           :native-face nerd-icons-green :family tool)
    (nix :set mdicon :icon "nf-md-nix" :face bv-icon-system
         :native-face nerd-icons-blue :family tool)
    (direnv :set octicon :icon "nf-oct-file_directory"
            :face bv-icon-directory :native-face nerd-icons-blue
            :family tool)
    (navigation-caret :set faicon :icon "nf-fa-caret_right"
                      :face bv-icon-salient :native-face nerd-icons-blue
                      :family ui))
  "BV icon role registry.

Every BV-owned icon lookup should go through a role here.  Roles keep package
integrations coherent even when individual glyphs come from different Nerd Font
sets."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-extension-role-alist
  '(("scm" . scheme)
    ("rkt" . scheme)
    ("fnl" . fennel)
    ("janet" . scheme)
    ("epub" . book)
    ("guix" . scheme)
    ("nix" . nix)
    ("direnv" . direnv))
  "File extension to BV icon role mappings."
  :type '(alist :key-type string :value-type symbol)
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-regexp-role-alist
  '(("\\.?guix" . scheme)
    ("Dockerfile" . docker)
    ("docker-compose" . docker)
    ("\\.envrc" . direnv)
    ("Makefile" . make)
    ("CMakeLists.txt" . cmake)
    ("LICENSE" . license)
    ("README" . book))
  "Filename regexp to BV icon role mappings."
  :type '(alist :key-type string :value-type symbol)
  :group 'bv-nerd-icons)

(defcustom bv-nerd-icons-mode-role-alist
  '((emacs-lisp-mode . scheme)
    (scheme-mode . scheme)
    (geiser-mode . scheme)
    (racket-mode . scheme)
    (clojure-mode . clojure)
    (fennel-mode . fennel)
    (python-mode . python)
    (rust-mode . rust)
    (go-mode . go)
    (js-mode . javascript)
    (typescript-mode . typescript)
    (web-mode . web)
    (org-mode . note)
    (markdown-mode . markdown)
    (latex-mode . latex)
    (pdf-view-mode . pdf)
    (image-mode . image)
    (magit-mode . git)
    (dired-mode . dired)
    (ibuffer-mode . ibuffer)
    (help-mode . help)
    (Info-mode . info)
    (eshell-mode . terminal)
    (term-mode . terminal)
    (vterm-mode . terminal))
  "Major mode to BV icon role mappings."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'bv-nerd-icons)

;;; Role mechanics

(defun bv-nerd-icons-effective-font-family ()
  "Return the active Nerd Font family."
  (or bv-nerd-icons-font-family
      (and (boundp 'bv-fonts-icon-family) bv-fonts-icon-family)
      "Symbols Nerd Font Mono"))

(defun bv-nerd-icons--set-function (set)
  "Return the Nerd Icons function for glyph SET."
  (pcase set
    ('codicon #'nerd-icons-codicon)
    ('devicon #'nerd-icons-devicon)
    ('faicon #'nerd-icons-faicon)
    ('mdicon #'nerd-icons-mdicon)
    ('octicon #'nerd-icons-octicon)
    ('sucicon #'nerd-icons-sucicon)
    (_ (error "Unknown BV icon glyph set: %S" set))))

(defun bv-nerd-icons--role-spec (role)
  "Return the icon spec for ROLE."
  (or (alist-get role bv-nerd-icons-role-alist)
      (error "Unknown BV icon role: %S" role)))

(defun bv-nerd-icons--role-face (spec)
  "Return the face for icon SPEC under `bv-nerd-icons-style'."
  (pcase bv-nerd-icons-style
    ('semantic (plist-get spec :face))
    ('muted 'bv-icon-muted)
    ('monochrome 'bv-icon-default)
    ('native (plist-get spec :native-face))
    (_ (plist-get spec :face))))

(defun bv-nerd-icons--icon-entry (role)
  "Return a Nerd Icons alist entry tail for ROLE."
  (let* ((spec (bv-nerd-icons--role-spec role))
         (face (bv-nerd-icons--role-face spec))
         (height (plist-get spec :height))
         (v-adjust (plist-get spec :v-adjust)))
    (append (list (bv-nerd-icons--set-function (plist-get spec :set))
                  (plist-get spec :icon))
            (when face (list :face face))
            (when height (list :height height))
            (when v-adjust (list :v-adjust v-adjust)))))

(defun bv-nerd-icons-icon (role &rest overrides)
  "Return the rendered icon for ROLE.
OVERRIDES are Nerd Icons plist arguments such as `:face' or `:height'."
  (pcase-let ((`(,function ,icon . ,args) (bv-nerd-icons--icon-entry role)))
    (apply function icon (append overrides args))))

(defun bv-nerd-icons--set-icon-alist-entry (variable key role)
  "Install in VARIABLE a KEY mapped to icon ROLE."
  (when (boundp variable)
    (let* ((entry (cons key (bv-nerd-icons--icon-entry role)))
           (current (symbol-value variable)))
      (set variable (cl-remove key current :key #'car :test #'equal))
      (add-to-list variable entry))))

(defun bv-nerd-icons--install-role-alists ()
  "Install BV role mappings into Nerd Icons lookup tables."
  (dolist (entry bv-nerd-icons-extension-role-alist)
    (bv-nerd-icons--set-icon-alist-entry
     'nerd-icons-extension-icon-alist (car entry) (cdr entry)))
  (dolist (entry bv-nerd-icons-regexp-role-alist)
    (bv-nerd-icons--set-icon-alist-entry
     'nerd-icons-regexp-icon-alist (car entry) (cdr entry)))
  (dolist (entry bv-nerd-icons-mode-role-alist)
    (bv-nerd-icons--set-icon-alist-entry
     'nerd-icons-mode-icon-alist (car entry) (cdr entry))))

(defun bv-nerd-icons--color-icons-p ()
  "Return non-nil when Nerd Icons should apply face color properties."
  ;; Even `monochrome' needs face properties so BV can force the default icon
  ;; face instead of letting upstream package colors leak through.
  t)

;;; Completion policy

(defun bv-nerd-icons--completion-width-class (&optional width)
  "Return completion width class for WIDTH without requiring completion eagerly."
  (if (fboundp 'bv-completion-width-class)
      (bv-completion-width-class width)
    (let ((width (or width (window-width))))
      (cond ((< width 90) 'compact)
            ((>= width 140) 'wide)
            (t 'normal)))))

(defun bv-nerd-icons-completion-category-enabled-p (category &optional width)
  "Return non-nil when completion CATEGORY should show icons at WIDTH."
  (let ((policy (or (alist-get category bv-nerd-icons-completion-category-policy
                               nil nil #'eq)
                    (alist-get t bv-nerd-icons-completion-category-policy))))
    (pcase policy
      ('always t)
      ('never nil)
      ('wide (eq (bv-nerd-icons--completion-width-class width) 'wide))
      ('adaptive
       (if (fboundp 'bv-completion-icons-enabled-p)
           (bv-completion-icons-enabled-p width)
         t))
      (_ t))))

(defun bv-nerd-icons-completion-get-icon (orig-fun cand category)
  "Return completion icon from ORIG-FUN for CAND and CATEGORY when policy allows."
  (if (not (bv-nerd-icons-completion-category-enabled-p category))
      ""
    (funcall orig-fun cand category)))

(defun bv-nerd-icons-configure-completion ()
  "Configure Nerd Icons for completion frameworks."
  (nerd-icons-completion-mode 1)
  (with-eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
    (when (bound-and-true-p marginalia-mode)
      (nerd-icons-completion-marginalia-setup)))
  (when (boundp 'nerd-icons-completion-mode-hooks)
    (setq nerd-icons-completion-mode-hooks
          '(marginalia-mode-hook
            icomplete-mode-hook
            vertico-mode-hook)))
  (advice-add #'nerd-icons-completion-get-icon
              :around #'bv-nerd-icons-completion-get-icon))

;;; Surface integrations

(defun bv-nerd-icons-configure-core ()
  "Configure the core Nerd Icons package."
  (setq nerd-icons-font-family (bv-nerd-icons-effective-font-family)
        nerd-icons-color-icons (bv-nerd-icons--color-icons-p)
        nerd-icons-scale-factor bv-nerd-icons-scale-factor)
  (bv-nerd-icons--install-role-alists))

(defun bv-nerd-icons-configure-dired ()
  "Configure Nerd Icons for Dired mode."
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(defun bv-nerd-icons-configure-ibuffer ()
  "Configure Nerd Icons for Ibuffer mode."
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  (setq nerd-icons-ibuffer-icon t
        nerd-icons-ibuffer-color-icon (bv-nerd-icons--color-icons-p)
        nerd-icons-ibuffer-icon-size 1.0
        nerd-icons-ibuffer-human-readable-size t)
  (setq nerd-icons-ibuffer-formats
        '((mark " " modified " " read-only
                "  " nerd-icons-ibuffer-icon
                "  " (name 32 32 :left :elide)
                "  " (size-h 9 -1 :right)
                "  " (mode+ 16 16 :left :elide)
                "  " filename-and-process+)
          (mark "  " (name 32 -1)
                "  " filename))))

(defun bv-nerd-icons-company-format (candidate)
  "Return CANDIDATE with a mode icon when legacy Company integration is enabled."
  (let* ((backend (get-text-property 0 'company-backend candidate))
         (icon (and backend
                    (fboundp 'nerd-icons-icon-for-mode)
                    (nerd-icons-icon-for-mode backend))))
    (if (and icon (not (eq icon backend)))
        (concat icon " " candidate)
      candidate)))

(defun bv-nerd-icons--which-key-prefix ()
  "Return the Which-Key prefix marker."
  (if bv-nerd-icons-enable-which-key-prefix
      (concat (bv-nerd-icons-icon 'navigation-caret) " ")
    "› "))

(defun bv-nerd-icons-configure-integrations ()
  "Configure secondary Nerd Icons integrations."
  (with-eval-after-load 'treemacs
    (when (fboundp 'treemacs-load-theme)
      (treemacs-load-theme "nerd-icons")))
  (with-eval-after-load 'company
    (when (and bv-nerd-icons-enable-company-integration
               (fboundp 'nerd-icons-icon-for-mode))
      (advice-add 'company-fill-propertize :filter-args
                  (lambda (args)
                    (let ((candidate (car args)))
                      (cons (bv-nerd-icons-company-format candidate)
                            (cdr args)))))))
  (with-eval-after-load 'which-key
    (when (boundp 'which-key-prefix-prefix)
      (setq which-key-prefix-prefix (bv-nerd-icons--which-key-prefix)))))

;;; Commands and font setup

(defun bv-nerd-icons-insert-icon ()
  "Interactively insert a Nerd Font icon."
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

(defun bv-nerd-icons--font-available-p (family)
  "Return non-nil when icon font FAMILY is available."
  (and (stringp family)
       (> (length family) 0)
       (fboundp 'find-font)
       (find-font (font-spec :family family))))

(defun bv-nerd-icons--range-valid-p (range)
  "Return non-nil when RANGE is a valid fontset character range."
  (and (consp range)
       (integerp (car range))
       (integerp (cdr range))
       (<= (car range) (cdr range))))

(defun bv-nerd-icons-font-ready-p ()
  "Return non-nil when the configured icon font is available or displayless."
  (or (not (display-graphic-p))
      (bv-nerd-icons--font-available-p
       (bv-nerd-icons-effective-font-family))))

(defun bv-nerd-icons-setup-fonts ()
  "Install Nerd Font private-use ranges into the active fontset."
  (let ((family (bv-nerd-icons-effective-font-family)))
    (when (and (display-graphic-p)
               (bv-nerd-icons--font-available-p family))
      (dolist (range bv-nerd-icons-font-ranges)
        (when (bv-nerd-icons--range-valid-p range)
          (set-fontset-font t range
                            (font-spec :family family)
                            nil 'prepend))))))

;;; Initialization

(defun bv-nerd-icons-init ()
  "Initialize BV icon configuration."
  (bv-nerd-icons-configure-core)
  (bv-nerd-icons-configure-completion)
  (bv-nerd-icons-configure-dired)
  (bv-nerd-icons-configure-ibuffer)
  (bv-nerd-icons-configure-integrations)
  (bv-nerd-icons-setup-fonts)
  (global-set-key (kbd "C-c i i") #'bv-nerd-icons-insert-icon)
  (global-set-key (kbd "C-c i f") #'bv-nerd-icons-insert-icon-for-file)
  (setq inhibit-compacting-font-caches t))

(bv-nerd-icons-init)

(provide 'bv-nerd-icons)
;;; bv-nerd-icons.el ends here
