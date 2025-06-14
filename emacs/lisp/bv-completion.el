;;; bv-completion.el --- Modern completion system -*- lexical-binding: t -*-

;;; Commentary:
;; State-of-the-art completion system
;; Features: orderless fuzzy matching, marginalia annotations,
;; embark actions, vertico UI, corfu in-buffer completion

;;; Code:

(require 'bv-core)

;;;; External Variable Declarations
(defvar crm-separator)
(defvar corfu--extra)
(defvar savehist-additional-variables)
(defvar xref-show-xrefs-function)
(defvar xref-show-definitions-function)
(defvar marginalia-annotators)
(defvar vertico-grid-separator)
(defvar vertico-grid-lookahead)

;;;; Function Declarations
;; Marginalia
(declare-function marginalia-mode "marginalia" (&optional arg))
(declare-function marginalia-cycle "marginalia" ())
;; Nerd Icons
(declare-function nerd-icons-completion-mode "nerd-icons-completion" (&optional arg))
;; Consult
(declare-function consult-grep "consult" (&optional dir initial))
(declare-function consult-register-format "consult" (reg))
(declare-function consult-xref "consult-xref" (fetcher &optional alist))
(declare-function consult-narrow-help "consult" ())
(declare-function consult-completion-in-region "consult" (start end collection &optional predicate))
(declare-function corfu-enable-in-minibuffer "bv-completion" ())
(declare-function corfu-move-to-minibuffer "bv-completion" ())
;; Embark
(declare-function embark--truncate-target "embark" (target))
;; Which-key
(declare-function which-key--hide-popup-ignore-command "which-key" ())
(declare-function which-key--show-keymap "which-key" (keymap-name keymap &optional prior-args all no-paging filter))
;; Vertico
(declare-function crm-indicator "bv-completion" (args))
(declare-function vertico-mode "vertico" (&optional arg))
(declare-function vertico-multiform-mode "vertico-multiform" (&optional arg))
;; Corfu
(declare-function global-corfu-mode "corfu" (&optional arg))
(declare-function corfu-terminal-mode "corfu-terminal" (&optional arg))
(declare-function corfu-mode "corfu" (&optional arg))
(declare-function corfu-history-mode "corfu-history" (&optional arg))
(declare-function consult--project-root "consult" (&optional may-prompt))
;; Cape
(declare-function cape-wrap-silent "cape" (capf))
(declare-function cape-wrap-purify "cape" (capf))
;; Kind Icon
(declare-function kind-icon-margin-formatter "kind-icon" (metadata))

;;;; Custom Variables

(defgroup bv-completion nil
  "Modern completion configuration."
  :group 'bv)

(bv-defcustom bv-completion-style 'orderless
  "Completion style to use."
  :type '(choice (const orderless) (const flex) (const substring))
  :group 'bv-completion)

(bv-defcustom bv-completion-cycle t
  "Enable cycling through completion candidates."
  :type 'boolean
  :group 'bv-completion)

(bv-defcustom bv-completion-auto t
  "Enable automatic completion in buffers."
  :type 'boolean
  :group 'bv-completion)

(bv-defcustom bv-completion-auto-delay 0.1
  "Delay before automatic completion starts."
  :type 'number
  :group 'bv-completion)

(bv-defcustom bv-completion-auto-prefix 2
  "Minimum prefix length for automatic completion."
  :type 'integer
  :group 'bv-completion)

(bv-defcustom bv-completion-annotations 'left
  "Position of completion annotations."
  :type '(choice (const left) (const right))
  :group 'bv-completion)

;;;; Core Completion Infrastructure

(use-package emacs
  :ensure nil
  :init
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Performance
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold (* 100 1024 1024))

  ;; Better defaults
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq completions-detailed t)

  ;; Ignore case
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  (setq completion-show-help nil)

  :config
  ;; Minibuffer improvements
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Better default UI
  (setq completions-format 'one-column)
  (setq completions-header-format nil))

;;;; Orderless - Fuzzy Matching

(use-package orderless
  :demand t
  :config
  ;; Matching styles
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-regexp
          orderless-initialism
          orderless-flex))

  ;; Allow SPC in minibuffer
  (setq orderless-component-separator 'orderless-escapable-split-on-space)

  ;; Style dispatchers
  (defun bv-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using = suffix."
    (cond
     ((equal "=" pattern) '(orderless-literal . "="))
     ((string-suffix-p "=" pattern)
      (cons 'orderless-literal (substring pattern 0 -1)))))

  (defun bv-orderless-flex-dispatcher (pattern _index _total)
    "Flex style dispatcher using ~ suffix."
    (cond
     ((equal "~" pattern) '(orderless-literal . "~"))
     ((string-suffix-p "~" pattern)
      (cons 'orderless-flex (substring pattern 0 -1)))))

  (defun bv-orderless-initialism-dispatcher (pattern _index _total)
    "Initialism style dispatcher using , suffix."
    (cond
     ((equal "," pattern) '(orderless-literal . ","))
     ((string-suffix-p "," pattern)
      (cons 'orderless-initialism (substring pattern 0 -1)))))

  (defun bv-orderless-without-literal-dispatcher (pattern _index _total)
    "Exclude matches using ! suffix."
    (cond
     ((equal "!" pattern) '(orderless-literal . "!"))
     ((string-suffix-p "!" pattern)
      (cons 'orderless-without-literal (substring pattern 0 -1)))))

  (setq orderless-style-dispatchers
        '(bv-orderless-literal-dispatcher
          bv-orderless-flex-dispatcher
          bv-orderless-initialism-dispatcher
          bv-orderless-without-literal-dispatcher))

  ;; Set as default
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (orderless partial-completion)))
          (buffer (styles . (orderless flex)))
          (project-file (styles . (orderless partial-completion))))))

;;;; Marginalia - Rich Annotations

(use-package marginalia
  :demand t
  :config
  ;; Annotation levels - can cycle through heavy, light, or none
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light
                                nil))

  ;; Alignment configuration
  (setq marginalia-align bv-completion-annotations)  ; Configurable via custom var
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align-offset 1)

  ;; Custom annotators for better prompt detection
  (add-to-list 'marginalia-prompt-categories '("\\<buffer\\>" . buffer))
  (add-to-list 'marginalia-prompt-categories '("\\<file\\>" . file))

  ;; Enable marginalia
  (marginalia-mode 1)

  ;; Optional: Add cycling command for annotation levels
  (defun bv-marginalia-cycle ()
    "Cycle between annotation levels."
    (interactive)
    (marginalia-cycle))

  :bind (("M-A" . bv-marginalia-cycle)))

;;;; Nerd Icons Completion

(use-package nerd-icons-completion
  :after marginalia
  :demand t
  :config
  (nerd-icons-completion-mode))

;;;; Consult - Advanced Commands

(use-package consult
  :bind (;; C-c bindings
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; Remapped bindings
         ([remap switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap recentf-open-files] . consult-recent-file)
         ;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x C-r" . consult-recent-file)
         ;; Register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings
         ("M-g e" . consult-compile-error)
         ("M-g k" . consult-global-mark)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . (lambda () (interactive)
                      (if (executable-find "rg")
                          (consult-ripgrep)
                        (consult-grep))))
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s m" . consult-mark)
         ("M-s o" . consult-outline)
         ("M-s f" . consult-flymake)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("C-r" . consult-history))

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Preview configuration
  (setq consult-preview-key '(:debounce 0.2 any))
  (setq consult-narrow-key "<")

  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (setq completion-in-region-function #'consult-completion-in-region)

  ;; Fix for project.el compatibility - handle new project format
  ;; This overrides consult's internal function to properly extract project roots
  (defun consult--project-root (&optional may-prompt)
    "Return project root as string, properly handling new project.el format."
    (when-let ((proj (project-current may-prompt)))
      (cond
       ;; If project-root function exists (newer project.el), use it
       ((fboundp 'project-root)
        (project-root proj))
       ;; Handle new format: (transient . "/path/to/project/")
       ((and (consp proj) (stringp (cdr proj)))
        (cdr proj))
       ;; Handle old format: just a string
       ((stringp proj)
        proj)
       ;; Fallback
       (t nil))))

  ;; Also set the public project function for other consult commands
  (setq consult-project-function #'consult--project-root)

  ;; Buffer sources
  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          consult--source-recent-file
          consult--source-file-register
          consult--source-bookmark
          consult--source-project-buffer-hidden
          consult--source-project-recent-file)))

;;;; Embark - Contextual Actions

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-collect))

  :init
  ;; Which-key integration
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  :config
  ;; Hide mode line in Embark buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(?:Live\\|Completions\\)\\*\\'"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Which-key indicator
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if prefix (format " (prefix: %s)" prefix) "")))
         (lookup-key keymap prefix 'accept-default)))))

  (setq embark-mixed-indicator-delay 0.5)
  (setq embark-verbose-indicator-display-action nil))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Vertico - Vertical Minibuffer UI

(use-package vertico
  :demand t
  :bind (:map vertico-map
         ;; Navigation
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("M-<" . vertico-first)
         ("M->" . vertico-last)
         ;; Actions
         ("RET" . vertico-directory-enter)
         ("TAB" . vertico-insert)
         ;; Quick selection
         ("M-q" . vertico-quick-insert)
         ("C-M-n" . vertico-next-group)
         ("C-M-p" . vertico-previous-group))

  :init
  (setq vertico-cycle bv-completion-cycle)
  (setq vertico-resize nil)
  (setq vertico-count 12)
  (when (boundp 'vertico-grid-separator)
    (setq vertico-grid-separator "       "))
  (when (boundp 'vertico-grid-lookahead)
    (setq vertico-grid-lookahead 50))

  :config
  (vertico-mode 1)

  (setq vertico-scroll-margin 0)
  (setq vertico-count 15)
  (setq vertico-resize t)
  (setq vertico-cycle t)

  ;; CRM indicator
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Vertico extensions
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :bind ("M-R" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-line buffer)
          (consult-imenu buffer)
          (consult-buffer flat)
          (consult-project-buffer flat)
          (consult-yank-pop buffer)))
  (setq vertico-multiform-categories
        '((file grid)
          (buffer flat)
          (consult-grep buffer))))

;;;; Corfu - In-buffer Completion

(use-package corfu
  :custom
  (corfu-cycle bv-completion-cycle)
  (corfu-auto bv-completion-auto)
  (corfu-auto-delay bv-completion-auto-delay)
  (corfu-auto-prefix bv-completion-auto-prefix)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 4)
  (corfu-count 12)
  (corfu-max-width 100)
  (corfu-min-width 40)
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("M-d" . corfu-doc-toggle)
         ("M-l" . corfu-show-location)
         ("M-q" . corfu-quick-complete)
         ("C-q" . corfu-quick-insert))

  :init
  (global-corfu-mode)

  :config
  ;; Terminal support
  (unless (display-graphic-p)
    (when (fboundp 'corfu-terminal-mode)
      (corfu-terminal-mode +1)))

  ;; Minibuffer support
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  ;; Move to minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when (boundp 'corfu--extra)
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer))

;; Corfu extensions
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-history
  :ensure nil
  :after corfu
  :config
  (with-eval-after-load 'savehist
    (when (boundp 'savehist-additional-variables)
      (add-to-list 'savehist-additional-variables 'corfu-history)))
  (corfu-history-mode))

(use-package corfu-quick
  :ensure nil
  :after corfu
  :bind (:map corfu-map
         ("M-q" . corfu-quick-complete)
         ("C-q" . corfu-quick-insert)))

;;;; Cape - Completion At Point Extensions

(use-package cape
  :init
  ;; Add completion functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  :config
  ;; Silence pcomplete
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  :bind (("C-c p p" . completion-at-point)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)))

;;;; Kind Icon - Icons in Completion

(use-package kind-icon
  :after corfu
  :demand t
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  (kind-icon-use-icons t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; Additional Optimizations

(use-package emacs
  :ensure nil
  :config
  ;; Fast path expansion
  (setq file-name-shadow-properties '(invisible t intangible t))
  (file-name-shadow-mode +1)

  (setq insert-default-directory t)
  (setq completion-auto-help 'lazy)
  (setq completions-detailed t))

;; Performance tuning
(setq-default cache-long-scans t)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Register feature
(bv-register-feature 'bv-completion)
(bv-set-value 'completion-system 'vertico-corfu)

(provide 'bv-completion)
;;; bv-completion.el ends here
