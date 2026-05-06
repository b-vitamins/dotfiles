;;; bv-themes.el --- BV next-generation Emacs theme system -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; BV themes are compiled from perceptual token profiles, semantic roles, and
;; package adapters.  This library owns theme declaration, installation,
;; lifecycle hooks, inspection commands, and audit integration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-themes-compile)
(require 'bv-themes-audit)

(declare-function bv-themes-regression-run "bv-themes-regression" (&optional directory))
(declare-function bv-themes-inventory-report "bv-themes-inventory" (&optional inventory))
(declare-function bv-themes-workloads-inventory "bv-themes-workloads" (&optional theme report))
(declare-function bv-themes-workloads-report "bv-themes-workloads" (&optional report))
(declare-function bv-themes-workloads-run "bv-themes-workloads" ())

(defconst bv-themes--library-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the BV theme runtime.")

(defgroup bv-themes nil
  "BV theme compiler and runtime."
  :group 'faces
  :prefix "bv-themes-")

(defcustom bv-themes-variants nil
  "Theme symbols managed by the BV theme runtime."
  :type '(repeat symbol)
  :group 'bv-themes)

(defcustom bv-themes-default-theme nil
  "Default BV theme used by inspection commands when no theme is active."
  :type '(choice (const nil) symbol)
  :group 'bv-themes)

(defcustom bv-themes-toggle-themes nil
  "Ordered BV theme pair used by `bv-themes-toggle'."
  :type '(repeat symbol)
  :group 'bv-themes)

(defcustom bv-themes-theme-directories
  (let ((directory
         (expand-file-name "../themes" bv-themes--library-directory)))
    (if (file-directory-p directory)
        (list directory)
      nil))
  "Theme specification directories searched by BV runtime commands."
  :type '(repeat directory)
  :group 'bv-themes)

(defcustom bv-themes-intensity 'balanced
  "Global chroma intensity for generated theme tokens."
  :type '(choice (const :tag "Faint" faint)
                 (const :tag "Balanced" balanced)
                 (const :tag "Vivid" vivid)
                 (const :tag "High chroma" high-chroma))
  :group 'bv-themes)

(defcustom bv-themes-bold-constructs t
  "Whether syntax and structural emphasis may use stronger weights."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-italic-constructs t
  "Whether comments, documentation, and italic roles may use slant."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-no-underlines t
  "When non-nil, remove underlines after loading a BV theme."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-audit-on-load nil
  "When non-nil, run strict BV audit whenever a BV theme is enabled."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-font-family-monospaced nil
  "Monospaced family used when compiling theme typography."
  :type '(choice (const nil) string)
  :group 'bv-themes)

(defcustom bv-themes-font-family-proportional nil
  "Proportional family used when compiling theme typography."
  :type '(choice (const nil) string)
  :group 'bv-themes)

(defcustom bv-themes-font-size 120
  "Default face height in 1/10 pt units used during theme compilation."
  :type 'integer
  :group 'bv-themes)

(defvar bv-themes-after-load-theme-hook nil
  "Hook run after a BV theme is enabled.")

(defvar bv-themes--current-theme nil
  "Currently enabled BV theme.")

(defvar bv-themes--current-artifact nil
  "Compiled artifact for the current BV theme.")

(defvar bv-themes--artifact-cache (make-hash-table :test 'equal)
  "Cache of compiled theme artifacts keyed by theme and options.")

(defvar bv-themes-gallery nil
  "Registered BV theme metadata for gallery and inspection commands.")

(defvar bv-themes--inhibit-load-theme-advice nil
  "Non-nil while BV load-theme advice is delegating to the original loader.")

(defun bv-themes--cache-key (theme)
  "Return cache key for THEME and active options."
  (list theme
        bv-themes-intensity
        bv-themes-bold-constructs
        bv-themes-italic-constructs
        bv-themes-font-family-monospaced
        bv-themes-font-family-proportional
        bv-themes-font-size))

(defun bv-themes-clear-cache ()
  "Clear cached compiled BV theme artifacts."
  (interactive)
  (clrhash bv-themes--artifact-cache))

(defun bv-themes--compiled (theme)
  "Return compiled artifact for THEME."
  (bv-themes-ensure-theme theme)
  (let ((key (bv-themes--cache-key theme)))
    (or (gethash key bv-themes--artifact-cache)
        (puthash key (bv-themes-compile theme) bv-themes--artifact-cache))))

(defun bv-themes--managed-theme-p (theme)
  "Return non-nil if THEME is managed by BV."
  (or (memq theme bv-themes-variants)
      (assq theme bv-themes-token-profiles)
      (bv-themes--theme-file theme)))

(defun bv-themes--theme-path-entry (entry)
  "Return normalized theme path ENTRY, or nil."
  (cond
   ((stringp entry)
    entry)
   ((and (symbolp entry)
         (not (eq entry t))
         (boundp entry)
         (stringp (symbol-value entry)))
    (symbol-value entry))
   (t nil)))

(defun bv-themes--theme-search-path ()
  "Return the BV theme specification search path."
  (let (paths)
    (dolist (entry bv-themes-theme-directories)
      (when-let ((path (bv-themes--theme-path-entry entry)))
        (when (file-directory-p path)
          (push path paths))))
    (delete-dups (nreverse paths))))

(defun bv-themes--theme-file (theme)
  "Return the Custom theme file for THEME, or nil."
  (locate-file (format "%s-theme.el" theme)
               (bv-themes--theme-search-path)))

(defun bv-themes--install-theme-directories ()
  "Install BV theme directories into `custom-theme-load-path'."
  (dolist (directory bv-themes-theme-directories)
    (when (and (stringp directory)
               (file-directory-p directory))
      (add-to-list 'custom-theme-load-path directory t))))

(defun bv-themes--theme-file-symbols ()
  "Return theme symbols discovered from theme specification files."
  (let (themes)
    (dolist (directory (bv-themes--theme-search-path))
      (dolist (file (directory-files directory nil
                                     "\\`.+-theme\\.el\\'"))
        (when (string-match "\\`\\(.+\\)-theme\\.el\\'" file)
          (push (intern (match-string 1 file)) themes))))
    (delete-dups (nreverse themes))))

(defun bv-themes-known-themes ()
  "Return registered and discoverable BV theme symbols."
  (delete-dups
   (append bv-themes-variants
           (mapcar #'car bv-themes-token-profiles)
           (bv-themes--theme-file-symbols))))

(defun bv-themes--resolve-theme (theme)
  "Return THEME or a current/discoverable BV theme."
  (or theme
      (bv-themes-current)
      bv-themes-default-theme
      (car (bv-themes-known-themes))
      (error "No BV theme is active or discoverable")))

(defun bv-themes--profile-registered-p (theme)
  "Return non-nil when THEME already has a token profile."
  (assq theme bv-themes-token-profiles))

(defun bv-themes--load-theme-definition (theme)
  "Load THEME specification without enabling it."
  (unless (bv-themes--profile-registered-p theme)
    (if-let ((file (bv-themes--theme-file theme)))
        (load file nil t)
      (error "Unknown BV theme specification: %S" theme)))
  (unless (bv-themes--profile-registered-p theme)
    (error "BV theme file did not register a profile: %S" theme))
  theme)

(defun bv-themes-ensure-theme (theme)
  "Ensure THEME is registered and return THEME."
  (bv-themes--load-theme-definition theme))

(defun bv-themes--definition-key (name)
  "Return keyword for DSL block NAME."
  (intern (format ":%s" name)))

(defun bv-themes--plist-remove (plist property)
  "Return PLIST without PROPERTY."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (unless (eq key property)
          (setq result (plist-put result key value)))))
    result))

(defun bv-themes--dsl-value (value)
  "Return normalized literal DSL VALUE."
  (pcase value
    (`(quote ,quoted)
     quoted)
    (_ value)))

(defun bv-themes--normalize-dsl-plist-values (plist)
  "Return PLIST with literal DSL values normalized."
  (let (result)
    (while plist
      (setq result
            (plist-put result
                       (pop plist)
                       (bv-themes--dsl-value (pop plist)))))
    result))

(defun bv-themes--dsl-plist (theme block body)
  "Return BODY as a plist for THEME BLOCK."
  (unless (or (null body) (keywordp (car body)))
    (error "BV theme %S block %S must use keyword arguments: %S"
           theme block body))
  (when (cl-oddp (length body))
    (error "BV theme %S block %S has an odd keyword list: %S"
           theme block body))
  (bv-themes--normalize-dsl-plist-values body))

(defun bv-themes--normalize-anchor-color (theme entry value)
  "Return OKLCH triplet VALUE for THEME anchor ENTRY."
  (pcase value
    (`(oklch ,l ,chroma ,hue)
     (list l chroma hue))
    (`(:oklch ,l ,chroma ,hue)
     (list l chroma hue))
    (`(,l ,chroma ,hue)
     (list l chroma hue))
    (_
     (error "Invalid BV theme %S anchor color in %S: %S"
            theme entry value))))

(defun bv-themes--normalize-anchor (theme entry)
  "Return an anchor cons from DSL ENTRY for THEME."
  (pcase entry
    (`(,name ,l ,chroma ,hue)
     (unless (and (symbolp name)
                  (numberp l)
                  (numberp chroma)
                  (numberp hue))
       (error "Invalid BV theme %S anchor: %S" theme entry))
     (cons name (list l chroma hue)))
    (`(,name ,value)
     (unless (symbolp name)
       (error "Invalid BV theme %S anchor name: %S" theme entry))
     (cons name (bv-themes--normalize-anchor-color theme entry value)))
    (`(,name :oklch ,l ,chroma ,hue)
     (unless (symbolp name)
       (error "Invalid BV theme %S anchor name: %S" theme entry))
     (cons name (list l chroma hue)))
    (_
     (error "Invalid BV theme %S anchor: %S" theme entry))))

(defun bv-themes--anchor-entry-p (entry)
  "Return non-nil if ENTRY looks like an anchor entry."
  (and (consp entry)
       (symbolp (car entry))
       (or (numberp (cadr entry))
           (keywordp (cadr entry))
           (and (consp (cadr entry))
                (memq (caadr entry) '(oklch :oklch))))))

(defun bv-themes--normalize-anchor-group (theme group)
  "Return flattened anchors from DSL GROUP for THEME."
  (if (and (consp group)
           (symbolp (car group))
           (not (bv-themes--anchor-entry-p group)))
      (mapcar (lambda (entry)
                (bv-themes--normalize-anchor theme entry))
              (cdr group))
    (list (bv-themes--normalize-anchor theme group))))

(defun bv-themes--normalize-anchors (theme groups)
  "Return flattened anchor alist from DSL GROUPS for THEME."
  (let ((anchors (apply #'append
                        (mapcar (lambda (group)
                                  (bv-themes--normalize-anchor-group
                                   theme group))
                                groups))))
    (dolist (required bv-themes-tokens-required-anchor-names)
      (unless (assq required anchors)
        (error "BV theme %S anchors are missing %S" theme required)))
    anchors))

(defun bv-themes--normalize-pair-list (theme block entries)
  "Return an alist from DSL pair ENTRIES for THEME BLOCK."
  (mapcar (lambda (entry)
            (pcase entry
              (`(,name ,value)
               (cons name value))
              (`(,name . ,value)
               (cons name value))
              (_
               (error "Invalid BV theme %S block %S pair: %S"
                      theme block entry))))
          entries))

(defun bv-themes--normalize-control-list (theme block entries)
  "Return numeric control alist from THEME BLOCK ENTRIES."
  (mapcar (lambda (entry)
            (pcase entry
              (`(,name ,amount)
               (cons name amount))
              (`(,name . ,plist)
               (unless (keywordp (car plist))
                 (error "Invalid BV theme %S %S control: %S"
                        theme block entry))
               (let ((amount (plist-get plist :amount)))
                 (unless amount
                   (error "BV theme %S %S control %S lacks :amount"
                          theme block name))
                 (cons name amount)))
              (_
               (error "Invalid BV theme %S %S control: %S"
                      theme block entry))))
          entries))

(defconst bv-themes--role-attribute-keys
  '(:inherit :fg :bg :family :family-var :height :height-var :weight :slant
    :width :extend :inverse-video :strike-through :stipple
    :distant-foreground :overline :underline :wave :box)
  "Face role attribute keys accepted by the BV theme DSL.")

(defun bv-themes--validate-attribute-plist
    (theme block name plist &optional extra-keys)
  "Validate THEME BLOCK NAME attribute PLIST."
  (unless (or (null plist) (keywordp (car plist)))
    (error "BV theme %S %S entry %S must use keyword attributes: %S"
           theme block name plist))
  (when (cl-oddp (length plist))
    (error "BV theme %S %S entry %S has an odd attribute list: %S"
           theme block name plist))
  (let ((cursor plist))
    (while cursor
      (let ((key (pop cursor)))
        (pop cursor)
        (unless (memq key (append extra-keys
                                  bv-themes--role-attribute-keys))
          (error "BV theme %S %S entry %S has unknown attribute %S"
                 theme block name key)))))
  plist)

(defun bv-themes--normalize-role-entry (theme entry)
  "Return a role override from THEME role ENTRY."
  (pcase entry
    (`(,role . ,plist)
     (unless (symbolp role)
       (error "BV theme %S has invalid role name: %S" theme role))
     (setq plist
           (bv-themes--validate-attribute-plist
            theme 'roles role plist '(:define)))
     (unless (or (bv-themes-roles-known-p role)
                 (plist-get plist :define))
       (error "BV theme %S role %S is not known; use :define t for new roles"
              theme role))
     (cons role (bv-themes--plist-remove plist :define)))
    (_
     (error "BV theme %S has invalid role entry: %S" theme entry))))

(defun bv-themes--normalize-face-adapter (theme block entry)
  "Return a face adapter for THEME BLOCK ENTRY."
  (unless (and (consp entry) (symbolp (car entry)))
    (error "BV theme %S %S entry has invalid face: %S"
           theme block entry))
  (let* ((face (car entry))
         (tail (cdr entry))
         role
         plist)
    (cond
     ((null tail)
      (error "BV theme %S %S entry %S lacks a role or attributes"
             theme block face))
     ((keywordp (car tail))
      (setq role nil
            plist tail))
     (t
      (setq role (car tail)
            plist (cdr tail))
      (unless (or (null role) (symbolp role))
        (error "BV theme %S %S entry %S has invalid role %S"
               theme block face role))))
    (setq plist
          (bv-themes--validate-attribute-plist theme block face plist))
    (cons face (cons role plist))))

(defun bv-themes--normalize-face-adapters (theme block body)
  "Return face adapters from THEME BLOCK BODY."
  (mapcar (lambda (entry)
            (bv-themes--normalize-face-adapter theme block entry))
          body))

(defun bv-themes--normalize-variant (theme body)
  "Return profile entries for variant DSL BODY in THEME."
  (let ((kind nil)
        (plist body))
    (when (and plist (not (keywordp (car plist))))
      (setq kind (car plist)
            plist (cdr plist)))
    (setq plist (bv-themes--dsl-plist theme 'variant plist))
    (when (plist-get plist :kind)
      (setq kind (plist-get plist :kind)))
    (unless kind
      (setq kind (plist-get plist :variant)))
    (append (list :variant kind)
            (when (plist-member plist :polarity)
              (list :polarity (plist-get plist :polarity)))
            (when (plist-member plist :modes)
              (list :modes (plist-get plist :modes))))))

(defconst bv-themes--domain-token-aliases
  '((syntax
     (comment . syntax-comment)
     (doc . syntax-doc)
     (string . syntax-string)
     (regexp . syntax-regexp)
     (escape . syntax-escape)
     (keyword . syntax-keyword)
     (builtin . syntax-builtin)
     (function . syntax-function)
     (variable . syntax-variable)
     (constant . syntax-constant)
     (number . syntax-number)
     (type . syntax-type)
     (operator . syntax-operator)
     (preprocessor . syntax-preprocessor)
     (property . syntax-property)
     (bracket . syntax-bracket)
     (delimiter . syntax-delimiter))
    (prose
     (heading-1 . prose-heading-1)
     (heading-2 . prose-heading-2)
     (heading-3 . prose-heading-3)
     (heading-4 . prose-heading-4)
     (heading-5 . prose-heading-5)
     (heading-6 . prose-heading-6)
     (heading-7 . prose-heading-7)
     (heading-8 . prose-heading-8)
     (link . prose-link)
     (visited . prose-link-visited)
     (code . prose-code)
     (verbatim . prose-verbatim)
     (metadata . prose-metadata)
     (metadata-value . prose-metadata-value)
     (table . prose-table)
     (todo . prose-todo)
     (done . prose-done))
    (states
     (success . success)
     (success-subtle . success-subtle)
     (success-strong . success-strong)
     (warning . warning)
     (warning-subtle . warning-subtle)
     (warning-strong . warning-strong)
     (error . error)
     (error-subtle . error-subtle)
     (error-strong . error-strong)
     (info . info)
     (info-subtle . info-subtle)
     (info-strong . info-strong)
     (critical . critical)
     (critical-bg . critical-bg)
     (critical-fg . critical-fg))
    (ui
     (salient . fg-salient)
     (special . fg-special)
     (special-mild . fg-special-mild)
     (header . fg-header)
     (header-muted . fg-header-muted)
     (header-inverse . fg-header-inverse)
     (cursor . cursor)
     (prompt . prompt)
     (keybind . keybind)
     (border . border)
     (border-subtle . border-subtle)
     (border-strong . border-strong)
     (accent-0 . accent-0)
     (accent-1 . accent-1)
     (accent-2 . accent-2)
     (accent-3 . accent-3))
    (search
     (background . bg-search)
     (foreground . fg-search)
     (current-background . bg-search-current)
     (current-foreground . fg-search-current)
     (lazy-background . bg-search-lazy)
     (match-background . bg-match)
     (paren-background . bg-paren)
     (paren-foreground . fg-paren))
    (diff
     (added-background . bg-added)
     (added-refine . bg-added-refine)
     (added-foreground . fg-added)
     (removed-background . bg-removed)
     (removed-refine . bg-removed-refine)
     (removed-foreground . fg-removed)
     (changed-background . bg-changed)
     (changed-refine . bg-changed-refine)
     (changed-foreground . fg-changed))
    (modeline
     (active-background . modeline-bg-active)
     (active-foreground . modeline-fg-active)
     (inactive-background . modeline-bg-inactive)
     (inactive-foreground . modeline-fg-inactive)
     (active-border . modeline-border-active)
     (inactive-border . modeline-border-inactive)
     (accent-background . modeline-bg-accent)
     (accent-foreground . modeline-fg-accent)
     (critical-background . modeline-bg-critical)
     (critical-foreground . modeline-fg-critical))
    (links
     (normal . fg-link)
     (faint . fg-link-faint)
     (visited . fg-link-visited))
    (terminal
     (black . term-black)
     (red . term-red)
     (green . term-green)
     (yellow . term-yellow)
     (blue . term-blue)
     (magenta . term-magenta)
     (cyan . term-cyan)
     (white . term-white)
     (bright-black . term-bright-black)
     (bright-red . term-bright-red)
     (bright-green . term-bright-green)
     (bright-yellow . term-bright-yellow)
     (bright-blue . term-bright-blue)
     (bright-magenta . term-bright-magenta)
     (bright-cyan . term-bright-cyan)
     (bright-white . term-bright-white)))
  "Token aliases used by domain-specific DSL blocks.")

(defun bv-themes--domain-token (theme domain name)
  "Return concrete token for THEME DOMAIN alias NAME."
  (or (alist-get name (alist-get domain bv-themes--domain-token-aliases))
      (and (memq name bv-themes-known-token-names) name)
      (error "BV theme %S has unknown %S token alias %S"
             theme domain name)))

(defun bv-themes--normalize-domain-overrides (theme domain body)
  "Return token overrides for THEME DOMAIN BODY."
  (mapcar (lambda (entry)
            (let* ((pair (car
                          (bv-themes--normalize-pair-list
                           theme domain (list entry))))
                   (token (bv-themes--domain-token theme domain (car pair))))
              (cons token (cdr pair))))
          body))

(defun bv-themes--append-profile (profile key values)
  "Append VALUES to PROFILE KEY."
  (plist-put profile key (append (plist-get profile key) values)))

(defun bv-themes--normalize-theme-definition (theme blocks)
  "Return compiler profile plist from theme DSL BLOCKS."
  (let (profile token-overrides)
    (dolist (block blocks)
      (pcase block
        (`(metadata . ,body)
         (setq profile
               (append profile
                       (bv-themes--dsl-plist theme 'metadata body))))
        (`(variant . ,body)
         (setq profile
               (append profile (bv-themes--normalize-variant theme body))))
        (`(anchors . ,body)
         (setq profile
               (append profile
                       (list :anchor-groups body
                             :anchors
                             (bv-themes--normalize-anchors theme body)))))
        (`(surfaces . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :mixes
                (bv-themes--normalize-control-list theme 'surfaces body))))
        (`(mixes . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :mixes
                (bv-themes--normalize-control-list theme 'mixes body))))
        (`(tones . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :tone-curve
                (bv-themes--normalize-control-list theme 'tones body))))
        (`(tone-curve . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :tone-curve
                (bv-themes--normalize-control-list theme 'tone-curve body))))
        (`(semantic-tokens . ,body)
         (setq token-overrides
               (append token-overrides
                       (bv-themes--normalize-pair-list
                        theme 'semantic-tokens body))))
        (`(tokens . ,body)
         (setq token-overrides
               (append token-overrides
                       (bv-themes--normalize-pair-list
                        theme 'tokens body))))
        (`(roles . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :role-overrides
                (mapcar (lambda (entry)
                          (bv-themes--normalize-role-entry theme entry))
                        body))))
        (`(faces . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :face-adapters
                (bv-themes--normalize-face-adapters
                 theme 'faces body))))
        (`(replace-faces . ,body)
         (setq profile
               (bv-themes--append-profile
                profile :face-adapter-replacements
                (bv-themes--normalize-face-adapters
                 theme 'replace-faces body))))
        (`(gallery . ,body)
         (setq profile
               (append profile
                       (list :gallery
                             (bv-themes--dsl-plist theme 'gallery body)))))
        (`(policy . ,body)
         (setq profile
               (append profile
                       (list :policy
                             (bv-themes--dsl-plist theme 'policy body)))))
        (`(samples . ,body)
         (setq profile (append profile (list :samples body))))
        (`(notes . ,body)
         (setq profile (append profile (list :notes body))))
        (`(,name . ,body)
         (if (assq name bv-themes--domain-token-aliases)
             (setq token-overrides
                   (append token-overrides
                           (bv-themes--normalize-domain-overrides
                            theme name body)))
           (setq profile
                 (append profile
                         (list (bv-themes--definition-key name) body)))))))
    (when token-overrides
      (setq profile
            (append profile (list :token-overrides token-overrides))))
    profile))

(defun bv-themes--duplicate-cars (entries)
  "Return duplicate cars in ENTRIES."
  (let (seen duplicates)
    (dolist (entry entries)
      (if (memq (car entry) seen)
          (cl-pushnew (car entry) duplicates)
        (push (car entry) seen)))
    (nreverse duplicates)))

(defun bv-themes--validate-face-adapters (theme roles adapters)
  "Validate THEME ADAPTERS against ROLES."
  (dolist (adapter adapters)
    (let ((role (cadr adapter)))
      (unless (or (null role)
                  (bv-themes-roles-known-p role roles))
        (error "BV theme %S face adapter %S uses unknown role %S"
               theme (car adapter) role)))))

(defun bv-themes--validate-theme-extensions (theme profile)
  "Validate non-token THEME PROFILE extensions."
  (let* ((role-overrides (plist-get profile :role-overrides))
         (roles (bv-themes-roles-build role-overrides))
         (adapters (plist-get profile :face-adapters))
         (replacements (plist-get profile :face-adapter-replacements)))
    (when-let ((duplicates (bv-themes--duplicate-cars role-overrides)))
      (error "BV theme %S has duplicate role definitions: %S"
             theme duplicates))
    (when-let ((duplicates (bv-themes--duplicate-cars adapters)))
      (error "BV theme %S has duplicate face adapters: %S"
             theme duplicates))
    (when-let ((duplicates (bv-themes--duplicate-cars replacements)))
      (error "BV theme %S has duplicate replacement face adapters: %S"
             theme duplicates))
    (bv-themes--validate-face-adapters theme roles adapters)
    (bv-themes--validate-face-adapters theme roles replacements)))

(defun bv-themes-register-theme (theme &rest profile)
  "Register THEME from a declarative PROFILE plist.
PROFILE accepts token profile keys such as :display-name, :variant,
:polarity, and :anchors.  Gallery-facing keys such as :summary and
:tags are preserved in `bv-themes-gallery'."
  (bv-themes--validate-theme-extensions theme profile)
  (bv-themes-tokens-register-profile theme profile)
  (add-to-list 'bv-themes-variants theme t)
  (setq bv-themes-gallery
        (cons (cons theme profile)
              (assq-delete-all theme bv-themes-gallery)))
  (clrhash bv-themes--artifact-cache)
  theme)

(defmacro bv-themes-define-theme (theme &rest blocks)
  "Define THEME from structured DSL BLOCKS.
Supported blocks include `metadata', `variant', `anchors',
`surfaces', `tones', `syntax', `prose', `states', `ui', `search',
`diff', `modeline', `links', `terminal', `semantic-tokens',
`tokens', `roles', `faces', `replace-faces', `gallery', `policy',
`samples', and `notes'."
  (declare (indent 1))
  `(let ((profile (bv-themes--normalize-theme-definition
                   ',theme ',blocks)))
     (apply #'bv-themes-register-theme ',theme profile)
     (bv-themes-apply-theme ',theme)))

(defun bv-themes--theme-doc (artifact)
  "Return a Custom theme doc string for ARTIFACT."
  (let* ((theme (plist-get artifact :theme))
         (profile (cdr (assq theme bv-themes-token-profiles)))
         (display-name (or (plist-get artifact :display-name)
                           (symbol-name theme)))
         (summary (plist-get profile :summary))
         (tags (plist-get profile :tags)))
    (string-join
     (delq nil
           (list display-name
                 summary
                 (and tags
                      (format "Tags: %s"
                              (string-join
                               (mapcar (lambda (tag)
                                         (format "%s" tag))
                                       tags)
                               ", ")))))
     "\n\n")))

(defun bv-themes--declare-theme (theme artifact)
  "Declare THEME for ARTIFACT when it is not already known."
  (unless (memq theme custom-known-themes)
    (custom-declare-theme
     theme
     (custom-make-theme-feature theme)
     (bv-themes--theme-doc artifact))))

(defun bv-themes-apply-theme (theme)
  "Install generated faces and variables for THEME.
This declares THEME, installs generated faces, and provides it to Custom."
  (let* ((artifact (bv-themes--compiled theme))
         (faces (plist-get artifact :faces))
         (variables (plist-get artifact :variables)))
    (bv-themes--declare-theme theme artifact)
    (apply #'custom-theme-set-faces theme faces)
    (apply #'custom-theme-set-variables theme variables)
    (provide-theme theme)
    artifact))

(defun bv-themes-current ()
  "Return the currently enabled BV theme, or nil."
  (cl-find-if #'bv-themes--managed-theme-p custom-enabled-themes))

(defun bv-themes-current-artifact ()
  "Return the compiled artifact for the current BV theme."
  (or bv-themes--current-artifact
      (when-let ((theme (bv-themes-current)))
        (bv-themes--compiled theme))))

(defun bv-themes-current-tokens ()
  "Return color tokens for the current BV theme."
  (or (plist-get (bv-themes-current-artifact) :tokens)
      (error "No BV theme is active")))

(defun bv-themes-color (name &optional tokens)
  "Return token NAME from TOKENS or the current BV theme."
  (bv-themes-tokens-get name (or tokens (bv-themes-current-tokens))))

(defun bv-themes-load-gallery ()
  "Load all discoverable BV theme specifications without enabling them."
  (interactive)
  (dolist (theme (bv-themes-known-themes))
    (bv-themes-ensure-theme theme))
  (when (called-interactively-p 'interactive)
    (message "BV theme gallery loaded: %d themes"
             (length bv-themes-gallery)))
  bv-themes-gallery)

(defun bv-themes-variant ()
  "Return `light' or `dark' for the current BV theme."
  (when-let ((theme (bv-themes-current)))
    (plist-get (bv-themes--compiled theme) :variant)))

(defun bv-themes--activate (theme)
  "Record THEME as active and run lifecycle hooks."
  (when (bv-themes--managed-theme-p theme)
    (setq bv-themes--current-theme theme
          bv-themes--current-artifact (bv-themes--compiled theme))
    (when bv-themes-no-underlines
      (bv-themes-enforce-no-underlines))
    (when bv-themes-audit-on-load
      (bv-themes-audit theme t))
    (run-hooks 'bv-themes-after-load-theme-hook)))

(defun bv-themes--after-enable-theme (theme)
  "Advice run after enabling THEME."
  (bv-themes--activate theme))

(advice-add 'enable-theme :after #'bv-themes--after-enable-theme)

(defun bv-themes--after-disable-theme (theme)
  "Advice run after disabling THEME."
  (when (bv-themes--managed-theme-p theme)
    (let ((current (cl-find-if #'bv-themes--managed-theme-p
                               custom-enabled-themes)))
      (setq bv-themes--current-theme current
            bv-themes--current-artifact
            (and current (bv-themes--compiled current))))))

(advice-add 'disable-theme :after #'bv-themes--after-disable-theme)

(defun bv-themes--disable-sibling-themes (theme)
  "Disable enabled BV themes other than THEME."
  (dolist (enabled custom-enabled-themes)
    (when (and (not (eq enabled theme))
               (bv-themes--managed-theme-p enabled))
      (disable-theme enabled))))

(defun bv-themes--load-theme (theme &optional no-enable)
  "Compile THEME and optionally enable it.
When NO-ENABLE is non-nil, only declare and install the generated
theme data."
  (bv-themes-apply-theme theme)
  (unless no-enable
    (enable-theme theme))
  t)

(defun bv-themes--around-load-theme (fn theme &optional no-confirm no-enable)
  "Advice around `load-theme' for BV managed THEME.
FN, NO-CONFIRM, and NO-ENABLE match `load-theme'."
  (bv-themes--install-theme-directories)
  (if bv-themes--inhibit-load-theme-advice
      (funcall fn theme no-confirm no-enable)
    (if (bv-themes--managed-theme-p theme)
        (let ((bv-themes--inhibit-load-theme-advice t))
          (unless no-enable
            (bv-themes--disable-sibling-themes theme))
          (if (bv-themes--theme-file theme)
              (funcall fn theme no-confirm no-enable)
            (bv-themes--load-theme theme no-enable)))
      (funcall fn theme no-confirm no-enable))))

(advice-add 'load-theme :around #'bv-themes--around-load-theme)

(bv-themes--install-theme-directories)

(defun bv-themes-load-theme (theme)
  "Disable other BV variants, compile THEME, and enable it."
  (interactive
   (list (intern (completing-read "BV theme: "
                                  (mapcar #'symbol-name
                                          (bv-themes-known-themes))
                                  nil t))))
  (unless (bv-themes--managed-theme-p theme)
    (user-error "Unknown BV theme: %S" theme))
  (dolist (enabled custom-enabled-themes)
    (when (bv-themes--managed-theme-p enabled)
      (disable-theme enabled)))
  (load-theme theme t))

(defun bv-themes-toggle ()
  "Toggle between the themes in `bv-themes-toggle-themes'."
  (interactive)
  (let ((themes (or bv-themes-toggle-themes
                    (bv-themes-known-themes))))
    (unless (= (length themes) 2)
      (user-error "`bv-themes-toggle-themes' must contain exactly two themes"))
    (let* ((current (bv-themes-current))
           (first (car themes))
           (second (cadr themes))
           (next (if (eq current first) second first)))
      (bv-themes-load-theme next))))

(defun bv-themes-enforce-no-underlines ()
  "Remove underlines from every live face."
  (interactive)
  (dolist (face (face-list))
    (when (facep face)
      (set-face-attribute face nil :underline nil))))

(defmacro bv-themes-with-colors (&rest body)
  "Evaluate BODY with current BV token names bound as symbol macros."
  (declare (indent 0) (debug t))
  `(let ((bv-themes--colors (bv-themes-current-tokens)))
     (cl-symbol-macrolet
         ,(mapcar (lambda (name)
                    `(,name (bv-themes-color ',name bv-themes--colors)))
                  bv-themes-known-token-names)
       ,@body)))

(defun bv-themes-list-colors (&optional theme)
  "Display the compiled color tokens for THEME or the current theme."
  (interactive)
  (let* ((theme (bv-themes--resolve-theme theme))
         (artifact (bv-themes--compiled theme))
         (tokens (plist-get artifact :tokens))
         (buffer (get-buffer-create "*BV Theme Colors*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# BV Theme Colors: %S\n\n" theme))
        (dolist (entry tokens)
          (let ((name (car entry))
                (value (cdr entry)))
            (insert (propertize "    " 'face `(:background ,value)))
            (insert (format " %-28s %s\n" name value))))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun bv-themes-list-all-faces (&optional theme)
  "Display generated face specs for THEME or the current theme."
  (interactive)
  (let* ((theme (bv-themes--resolve-theme theme))
         (artifact (bv-themes--compiled theme))
         (faces (plist-get artifact :faces))
         (buffer (get-buffer-create "*BV Theme Faces*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# BV Theme Faces: %S\n\n" theme))
        (dolist (spec faces)
          (insert (format "- %S %S\n" (car spec)
                          (bv-themes-audit--face-attrs spec))))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun bv-themes-gallery-report ()
  "Display registered BV theme gallery metadata."
  (interactive)
  (bv-themes-load-gallery)
  (let ((buffer (get-buffer-create "*BV Theme Gallery*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# BV Theme Gallery\n\n")
        (dolist (entry (sort (copy-sequence bv-themes-gallery)
                             (lambda (left right)
                               (string< (symbol-name (car left))
                                        (symbol-name (car right))))))
          (let* ((theme (car entry))
                 (profile (cdr entry))
                 (gallery (plist-get profile :gallery)))
            (insert (format "## %s\n" (plist-get profile :display-name)))
            (insert (format "- symbol: %S\n" theme))
            (when-let ((summary (plist-get profile :summary)))
              (insert (format "- summary: %s\n" summary)))
            (when-let ((tags (plist-get profile :tags)))
              (insert (format "- tags: %s\n"
                              (string-join
                               (mapcar (lambda (tag)
                                         (format "%s" tag))
                                       tags)
                                           ", "))))
            (when gallery
              (insert (format "- gallery: %S\n" gallery)))
            (insert "\n")))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun bv-themes-contrast-report (&optional theme)
  "Display contrast audit for THEME or the current theme."
  (interactive)
  (bv-themes-audit-report
   (bv-themes-audit (bv-themes--resolve-theme theme))))

(defun bv-themes-live-face-inventory (&optional theme)
  "Display live face coverage for THEME or the current theme."
  (interactive)
  (require 'bv-themes-inventory)
  (let* ((theme (bv-themes--resolve-theme theme))
         (artifact (bv-themes--compiled theme)))
    (bv-themes-inventory-report
     (bv-themes-inventory-scan artifact))))

(defun bv-themes-workload-report ()
  "Run workflow probes and display the workload report."
  (interactive)
  (require 'bv-themes-workloads)
  (bv-themes-workloads-report
   (bv-themes-workloads-run)))

(defun bv-themes-workload-face-inventory (&optional theme)
  "Run workflow probes and display observed face coverage for THEME."
  (interactive)
  (require 'bv-themes-inventory)
  (require 'bv-themes-workloads)
  (let* ((theme (bv-themes--resolve-theme theme))
         (report (bv-themes-workloads-run)))
    (bv-themes-inventory-report
     (bv-themes-workloads-inventory theme report))))

(defun bv-themes-write-regression-artifacts (&optional directory)
  "Write deterministic visual regression artifacts to DIRECTORY."
  (interactive "DArtifact directory: ")
  (bv-themes-load-gallery)
  (require 'bv-themes-regression)
  (bv-themes-regression-run
   (if (and directory (not (string-empty-p directory)))
       directory
     nil)))

(defun bv-themes-preview-colors (&optional theme)
  "Alias for `bv-themes-list-colors' for quick token inspection."
  (interactive)
  (bv-themes-list-colors theme))

(provide 'bv-themes)
;;; bv-themes.el ends here
