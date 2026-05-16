;;; bv-themes-compile.el --- BV theme compiler -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; The compiler turns profile tokens and adapter specs into Emacs theme face
;; and variable declarations.  This is intentionally data-first so audits can
;; reason about the generated artifact before it is installed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-themes-tokens)
(require 'bv-themes-roles)
(require 'bv-themes-adapters)

(defvar bv-themes-intensity 'balanced)
(defvar bv-themes-bold-constructs t)
(defvar bv-themes-italic-constructs t)
(defvar bv-themes-font-family-monospaced nil)
(defvar bv-themes-font-family-proportional nil)
(defvar bv-themes-font-size 120)

(defconst bv-themes-compile-contrast-pairs
  '((default fg-main bg-main 7.0)
    (muted fg-muted bg-main 4.5)
    (dim fg-dim bg-main 3.0)
    (header fg-header bg-header 4.5)
    (header-muted fg-header-muted bg-header 3.0)
    (region fg-main bg-region 4.5)
    (completion fg-main bg-completion 4.5)
    (completion-current fg-main bg-completion-current 4.5)
    (search fg-search bg-search 4.5)
    (search-current fg-search-current bg-search-current 4.5)
    (prose-block fg-main bg-prose-block-contents 4.5)
    (added fg-added bg-added 4.5)
    (removed fg-removed bg-removed 4.5)
    (changed fg-changed bg-changed 4.5)
    (modeline-active modeline-fg-active modeline-bg-active 4.5)
    (modeline-inactive modeline-fg-inactive modeline-bg-inactive 3.0)
    (critical critical-fg critical-bg 4.5))
  "Contrast pairs measured by the BV theme audit.")

(defconst bv-themes-compile--xterm-color-steps [0 95 135 175 215 255]
  "RGB channel values used by the terminal 256-color cube.")

(defun bv-themes-compile--plist-merge (&rest plists)
  "Return a plist produced by merging PLISTS left to right."
  (let (result)
    (dolist (plist plists)
      (while plist
        (setq result (plist-put result (pop plist) (pop plist)))))
    result))

(defun bv-themes-compile--hex-color (red green blue)
  "Return an sRGB hex color for RED, GREEN, and BLUE channel values."
  (format "#%02x%02x%02x" red green blue))

(defun bv-themes-compile--xterm-color (index tokens)
  "Return terminal 256-color INDEX using TOKENS for the first 16 colors."
  (let ((base '(term-black term-red term-green term-yellow term-blue
                term-magenta term-cyan term-white term-bright-black
                term-bright-red term-bright-green term-bright-yellow
                term-bright-blue term-bright-magenta term-bright-cyan
                term-bright-white)))
    (cond
     ((not (and (integerp index) (<= 0 index 255)))
      (error "Invalid terminal color index: %S" index))
     ((< index 16)
      (bv-themes-tokens-get (nth index base) tokens))
     ((< index 232)
      (let* ((offset (- index 16))
             (red (aref bv-themes-compile--xterm-color-steps
                        (/ offset 36)))
             (green (aref bv-themes-compile--xterm-color-steps
                          (% (/ offset 6) 6)))
             (blue (aref bv-themes-compile--xterm-color-steps
                         (% offset 6))))
        (bv-themes-compile--hex-color red green blue)))
     (t
      (let ((level (+ 8 (* 10 (- index 232)))))
        (bv-themes-compile--hex-color level level level))))))

(defun bv-themes-compile--token (value tokens)
  "Resolve VALUE against TOKENS when VALUE names a color token."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((and (consp value) (eq (car value) 'xterm-256))
    (bv-themes-compile--xterm-color (cadr value) tokens))
   ((and (symbolp value) (assq value tokens))
    (bv-themes-tokens-get value tokens))
   ((symbolp value) value)
   (t value)))

(defun bv-themes-compile--color-token (value tokens)
  "Resolve VALUE to a color string from TOKENS."
  (let ((resolved (bv-themes-compile--token value tokens)))
    (cond
     ((stringp resolved) resolved)
     ((null resolved) nil)
     (t (error "Expected a color token or string, got %S" value)))))

(defun bv-themes-compile--variable-value (symbol)
  "Return SYMBOL's value if it is bound and meaningful."
  (when (and symbol (boundp symbol))
    (let ((value (symbol-value symbol)))
      (unless (or (null value)
                  (and (stringp value) (string-empty-p value)))
        value))))

(defun bv-themes-compile--construct-weight (role weight)
  "Return effective WEIGHT for ROLE."
  (cond
   ((null weight) nil)
   ((not bv-themes-bold-constructs)
    (if (memq role
              '(syntax-keyword syntax-builtin syntax-function syntax-type
                syntax-constant syntax-preprocessor syntax-regexp
                syntax-escape syntax-property))
        'normal
      weight))
   (t weight)))

(defun bv-themes-compile--construct-slant (role slant)
  "Return effective SLANT for ROLE."
  (cond
   ((null slant) nil)
   ((and (not bv-themes-italic-constructs)
         (memq role '(syntax-comment syntax-doc italic)))
    'normal)
   (t slant)))

(defun bv-themes-compile--resolve-underline (value tokens)
  "Resolve underline VALUE with TOKENS."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((symbolp value)
    (list :style 'line
          :color (bv-themes-compile--color-token value tokens)))
   ((and (consp value) (plist-get value :color))
    (let ((copy (copy-sequence value)))
      (plist-put copy :color
                 (bv-themes-compile--color-token
                  (plist-get copy :color)
                  tokens))))
   (t value)))

(defun bv-themes-compile--resolve-box (value tokens)
  "Resolve box VALUE with TOKENS."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((symbolp value)
    (list :line-width 1 :color (bv-themes-compile--color-token value tokens)))
   ((and (consp value) (plist-get value :color))
    (let ((copy (copy-sequence value)))
      (plist-put copy :color
                 (bv-themes-compile--color-token
                  (plist-get copy :color)
                  tokens))))
   (t value)))

(defun bv-themes-compile--put-attr (attrs key value)
  "Put KEY VALUE into ATTRS unless VALUE is nil."
  (if (null value)
      attrs
    (plist-put attrs key value)))

(defun bv-themes-compile--face-attrs (role plist tokens)
  "Return face attributes for ROLE from PLIST using TOKENS."
  (let ((attrs nil)
        (fg (plist-get plist :fg))
        (bg (plist-get plist :bg))
        (family-var (plist-get plist :family-var))
        (height-var (plist-get plist :height-var)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :inherit (plist-get plist :inherit)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :foreground
           (and fg (bv-themes-compile--color-token fg tokens))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :background
           (and bg (bv-themes-compile--color-token bg tokens))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :family
           (or (bv-themes-compile--variable-value family-var)
               (plist-get plist :family))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :height
           (or (bv-themes-compile--variable-value height-var)
               (plist-get plist :height))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :weight
           (bv-themes-compile--construct-weight
            role
            (plist-get plist :weight))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :slant
           (bv-themes-compile--construct-slant
            role
            (plist-get plist :slant))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :width (plist-get plist :width)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :extend (plist-get plist :extend)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :inverse-video (plist-get plist :inverse-video)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :strike-through (plist-get plist :strike-through)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :stipple (plist-get plist :stipple)))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :distant-foreground
           (and (plist-get plist :distant-foreground)
                (bv-themes-compile--color-token
                 (plist-get plist :distant-foreground)
                 tokens))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :overline
           (and (plist-get plist :overline)
                (bv-themes-compile--color-token
                 (plist-get plist :overline)
                 tokens))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :underline
           (or (and (plist-get plist :wave)
                    (list :style 'wave
                          :color (bv-themes-compile--color-token
                                  (plist-get plist :wave)
                                  tokens)))
               (bv-themes-compile--resolve-underline
                (plist-get plist :underline)
                tokens))))
    (setq attrs
          (bv-themes-compile--put-attr
           attrs :box
           (bv-themes-compile--resolve-box
            (plist-get plist :box)
            tokens)))
    attrs))

(defun bv-themes-compile--adapter-face (entry tokens roles)
  "Compile adapter ENTRY using TOKENS and ROLES."
  (let* ((face (car entry))
         (role (cadr entry))
         (role-plist (if role (bv-themes-roles-spec role roles) nil))
         (overrides (cddr entry))
         (plist (apply #'bv-themes-compile--plist-merge
                       (delq nil (list role-plist overrides))))
         (attrs (bv-themes-compile--face-attrs role plist tokens)))
    (list face (list (list t attrs)))))

(defun bv-themes-compile--duplicate-faces (entries)
  "Return duplicate face names in ENTRIES."
  (let ((seen nil)
        (dups nil))
    (dolist (entry entries)
      (let ((face (car entry)))
        (if (memq face seen)
            (cl-pushnew face dups)
          (push face seen))))
    (nreverse dups)))

(defun bv-themes-compile--face-adapter-specs
    (&optional additions replacements)
  "Return adapter specs with ADDITIONS and REPLACEMENTS applied."
  (let ((specs (copy-sequence bv-themes-adapter-specs)))
    (dolist (replacement replacements)
      (setf (alist-get (car replacement) specs nil nil #'eq)
            (cdr replacement)))
    (append specs additions)))

(defun bv-themes-compile-face-specs
    (tokens &optional roles additions replacements)
  "Compile face specs for TOKENS using ROLES and adapter changes."
  (let* ((adapters
          (bv-themes-compile--face-adapter-specs additions replacements))
         (duplicates (bv-themes-compile--duplicate-faces adapters)))
    (when duplicates
      (error "Duplicate BV theme adapter faces: %S" duplicates))
    (mapcar (lambda (entry)
              (bv-themes-compile--adapter-face entry tokens roles))
            adapters)))

(defun bv-themes-compile--ansi-vector (tokens &optional bright)
  "Return an ANSI color vector from TOKENS.
When BRIGHT is non-nil, use bright terminal tokens."
  (vconcat
   (mapcar (lambda (name)
             (bv-themes-tokens-get name tokens))
           (if bright
               '(term-bright-black term-bright-red term-bright-green
                 term-bright-yellow term-bright-blue term-bright-magenta
                 term-bright-cyan term-bright-white)
             '(term-black term-red term-green term-yellow term-blue
               term-magenta term-cyan term-white)))))

(defun bv-themes-compile--custom-value (value)
  "Return a Custom theme expression for VALUE."
  (if (consp value)
      `',value
    value))

(defun bv-themes-compile-variable-specs (tokens)
  "Return theme variable specs for TOKENS."
  (let* ((ansi (bv-themes-compile--ansi-vector tokens))
         (bright (bv-themes-compile--ansi-vector tokens t))
         (term16 (vconcat (append (append ansi nil) (append bright nil)))))
    `((ansi-color-names-vector ,(bv-themes-compile--custom-value ansi))
      (ansi-term-color-vector
       ,(bv-themes-compile--custom-value (vconcat (vector 'unspecified) ansi)))
      (xterm-color-names ,(bv-themes-compile--custom-value (append term16 nil)))
      (xterm-color-names-bright
       ,(bv-themes-compile--custom-value (append bright nil)))
      (vterm-color-default-foreground
       ,(bv-themes-compile--custom-value
         (bv-themes-tokens-get 'fg-main tokens)))
      (vterm-color-default-background
       ,(bv-themes-compile--custom-value
         (bv-themes-tokens-get 'bg-main tokens)))
      (vterm-color-palette ,(bv-themes-compile--custom-value term16))
      (pdf-view-midnight-colors
       ,(bv-themes-compile--custom-value
         (cons (bv-themes-tokens-get 'fg-main tokens)
               (bv-themes-tokens-get 'bg-main tokens)))))))

(defun bv-themes-compile-options ()
  "Return compiler options from user customizations."
  (list :intensity bv-themes-intensity))

(defun bv-themes-compile (theme)
  "Compile THEME and return a plist artifact."
  (let* ((profile (bv-themes-tokens-profile theme))
         (tokens (bv-themes-tokens-build theme (bv-themes-compile-options)))
         (roles (bv-themes-roles-build
                 (plist-get profile :role-overrides)))
         (faces (bv-themes-compile-face-specs
                 tokens
                 roles
                 (plist-get profile :face-adapters)
                 (plist-get profile :face-adapter-replacements)))
         (variables (bv-themes-compile-variable-specs tokens)))
    (list :theme theme
          :display-name (bv-themes-tokens-display-name theme)
          :variant (bv-themes-tokens-variant theme)
          :policy (plist-get profile :policy)
          :tokens tokens
          :roles roles
          :faces faces
          :variables variables
          :contrast-pairs bv-themes-compile-contrast-pairs)))

(provide 'bv-themes-compile)
;;; bv-themes-compile.el ends here
