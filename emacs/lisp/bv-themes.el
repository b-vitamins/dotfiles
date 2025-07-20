;;; bv-themes.el --- Unified theme engine -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Theme engine supporting multiple variants through a data-driven architecture.
;; Inspired by the Modus themes' design principles.

;;; Code:

(require 'cl-lib)

(defgroup bv-themes ()
  "Unified theme system with semantic color mapping."
  :group 'faces)

;;; User options

(defcustom bv-themes-bold-constructs nil
  "When non-nil, use bold for function names and similar constructs."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-italic-constructs nil
  "When non-nil, use italic for comments and docstrings."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-variable-pitch-ui nil
  "When non-nil, use variable pitch for UI elements."
  :type 'boolean
  :group 'bv-themes)

(defcustom bv-themes-font-family-monospaced "Fira Code"
  "Monospaced font family."
  :type 'string
  :group 'bv-themes)

(defcustom bv-themes-font-family-proportional "Inter"
  "Proportional font family."
  :type 'string
  :group 'bv-themes)

(defcustom bv-themes-font-size 120
  "Font size in units of 1/10 pt."
  :type 'integer
  :group 'bv-themes)

;;; Semantic faces (these are the building blocks)

(defface bv-themes-default nil
  "Base face for default text."
  :group 'bv-themes)

(defface bv-themes-strong nil
  "Face for structurally important elements."
  :group 'bv-themes)

(defface bv-themes-emphasis nil
  "Face for emphasized text."
  :group 'bv-themes)

(defface bv-themes-faded nil
  "Face for de-emphasized elements."
  :group 'bv-themes)

(defface bv-themes-subtle nil
  "Face for subtle backgrounds."
  :group 'bv-themes)

(defface bv-themes-salient nil
  "Face for elements that should stand out."
  :group 'bv-themes)

(defface bv-themes-popout nil
  "Face for attention-grabbing elements."
  :group 'bv-themes)

(defface bv-themes-critical nil
  "Face for critical information."
  :group 'bv-themes)

;;; Header line faces (for modeline)

(defface bv-themes-header-default nil
  "Default face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-strong nil
  "Strong face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-salient nil
  "Salient face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-popout nil
  "Popout face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-faded nil
  "Faded face for the header line."
  :group 'bv-themes)

(defface bv-themes-header-critical nil
  "Critical face for the header line."
  :group 'bv-themes)

;;; Utility faces

(defface fallback '((t :family "Fira Code"))
  "Fallback face for glyphs missing in primary font.")

;;; Calendar faces

(defface bv-calendar-header '((t :inherit bv-themes-strong))
  "Face for calendar headers.")

(defface bv-calendar-today '((t :inherit bv-themes-salient :weight bold))
  "Face for today.")

(defface bv-calendar-current '((t :inherit bv-themes-default :underline t))
  "Face for current selected date.")

(defface bv-calendar-weekend '((t :inherit bv-themes-faded))
  "Face for weekends.")

(defface bv-calendar-holiday '((t :inherit bv-themes-popout))
  "Face for holidays.")

;;; Face specifications

(defconst bv-themes-faces
  '(
    ;; Core semantic faces
    `(bv-themes-default ((,c :foreground ,foreground :background ,background)))
    `(bv-themes-strong ((,c :foreground ,strong :weight ,(if bold-constructs 'medium 'normal))))
    `(bv-themes-emphasis ((,c :foreground ,foreground :slant ,(if italic-constructs 'italic 'normal))))
    `(bv-themes-faded ((,c :foreground ,faded)))
    `(bv-themes-subtle ((,c :background ,subtle)))
    `(bv-themes-salient ((,c :foreground ,salient)))
    `(bv-themes-popout ((,c :foreground ,popout)))
    `(bv-themes-critical ((,c :foreground ,background :background ,critical)))

    ;; Basic faces
    `(default ((,c :inherit bv-themes-default
                   :family ,font-mono
                   :height ,font-size
                   :weight light)))
    `(cursor ((,c :background ,foreground)))
    `(region ((,c :inherit bv-themes-subtle :extend t)))
    `(highlight ((,c :inherit bv-themes-subtle)))
    `(hl-line ((,c :background ,highlight :extend t)))
    `(fringe ((,c :foreground ,subtle :background ,background)))
    `(vertical-border ((,c :foreground ,subtle)))
    `(window-divider ((,c :foreground ,background)))
    `(window-divider-first-pixel ((,c :foreground ,background)))
    `(window-divider-last-pixel ((,c :foreground ,background)))

    ;; Font lock faces
    `(font-lock-comment-face ((,c :inherit bv-themes-faded
                                  :slant ,(if italic-constructs 'italic 'normal))))
    `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-doc-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-string-face ((,c :inherit bv-themes-popout)))
    `(font-lock-keyword-face ((,c :inherit bv-themes-salient)))
    `(font-lock-builtin-face ((,c :inherit bv-themes-salient)))
    `(font-lock-type-face ((,c :inherit bv-themes-salient)))
    `(font-lock-constant-face ((,c :inherit bv-themes-salient)))
    `(font-lock-function-name-face ((,c :inherit bv-themes-strong)))
    `(font-lock-variable-name-face ((,c :inherit bv-themes-strong)))
    `(font-lock-warning-face ((,c :inherit bv-themes-popout :weight bold)))

    ;; UI elements
    `(mode-line ((,c :foreground ,(if (display-graphic-p) background foreground)
                     :background ,background
                     :height 0.1
                     :box nil :underline nil :overline nil)))
    `(mode-line-inactive ((,c :inherit mode-line)))
    `(mode-line-emphasis ((,c :inherit bv-themes-strong)))
    `(mode-line-highlight ((,c :inherit bv-themes-subtle)))

    `(header-line ((,c :foreground ,foreground
                       :background ,background
                       :height 1.0
                       :box (:line-width 1 :color ,background :style nil))))

    `(minibuffer-prompt ((,c :inherit bv-themes-strong)))
    `(completions-common-part ((,c :inherit bv-themes-faded)))
    `(completions-first-difference ((,c :inherit default)))

    ;; Links and buttons
    `(link ((,c :inherit bv-themes-salient :underline t)))
    `(link-visited ((,c :inherit link :foreground ,faded)))
    `(button ((,c :inherit link)))

    ;; Status indicators
    `(success ((,c :inherit bv-themes-salient)))
    `(warning ((,c :inherit bv-themes-popout)))
    `(error ((,c :inherit bv-themes-critical)))

    ;; Search and matching
    `(match ((,c :inherit bv-themes-popout :weight bold)))
    `(isearch ((,c :inherit bv-themes-strong :background ,highlight)))
    `(isearch-fail ((,c :inherit bv-themes-faded)))
    `(lazy-highlight ((,c :inherit bv-themes-subtle)))
    `(query-replace ((,c :inherit isearch)))

    ;; Selection
    `(secondary-selection ((,c :inherit bv-themes-subtle)))

    ;; Parentheses
    `(show-paren-match ((,c :inherit bv-themes-popout :weight bold)))
    `(show-paren-mismatch ((,c :inherit error)))

    ;; Whitespace
    `(trailing-whitespace ((,c :inherit bv-themes-subtle)))
    `(whitespace-trailing ((,c :inherit trailing-whitespace)))

    ;; Line numbers
    `(line-number ((,c :inherit bv-themes-faded)))
    `(line-number-current-line ((,c :inherit default)))

    ;; Variable pitch
    `(variable-pitch ((,c :family ,font-prop)))
    `(fixed-pitch ((,c :family ,font-mono)))

    ;; Info faces
    `(info-title-1 ((,c :inherit bv-themes-strong :height 1.3)))
    `(info-title-2 ((,c :inherit bv-themes-strong :height 1.2)))
    `(info-title-3 ((,c :inherit bv-themes-strong :height 1.1)))
    `(info-menu-star ((,c :inherit bv-themes-salient)))

    ;; Org mode faces
    `(org-level-1 ((,c :inherit bv-themes-strong :height 1.3)))
    `(org-level-2 ((,c :inherit bv-themes-strong :height 1.2)))
    `(org-level-3 ((,c :inherit bv-themes-strong :height 1.1)))
    `(org-level-4 ((,c :inherit bv-themes-strong)))
    `(org-level-5 ((,c :inherit bv-themes-strong)))
    `(org-level-6 ((,c :inherit bv-themes-strong)))
    `(org-level-7 ((,c :inherit bv-themes-strong)))
    `(org-level-8 ((,c :inherit bv-themes-strong)))

    `(org-todo ((,c :inherit bv-themes-salient :weight bold)))
    `(org-done ((,c :inherit bv-themes-faded)))
    `(org-headline-done ((,c :inherit bv-themes-faded)))

    `(org-tag ((,c :inherit bv-themes-popout)))
    `(org-date ((,c :inherit bv-themes-faded)))
    `(org-scheduled ((,c :inherit bv-themes-faded)))
    `(org-scheduled-today ((,c :inherit bv-themes-salient :weight bold)))
    `(org-scheduled-previously ((,c :inherit bv-themes-popout)))
    `(org-upcoming-deadline ((,c :inherit default)))
    `(org-warning ((,c :inherit bv-themes-popout :weight bold)))

    `(org-table ((,c :inherit bv-themes-faded)))
    `(org-formula ((,c :inherit bv-themes-faded)))

    `(org-code ((,c :inherit bv-themes-faded)))
    `(org-verbatim ((,c :inherit bv-themes-popout)))
    `(org-block ((,c :background ,subtle :extend t)))
    `(org-block-begin-line ((,c :inherit bv-themes-faded :extend t)))
    `(org-block-end-line ((,c :inherit bv-themes-faded :extend t)))

    `(org-drawer ((,c :inherit bv-themes-faded)))
    `(org-property-value ((,c :inherit bv-themes-faded)))
    `(org-special-keyword ((,c :inherit bv-themes-faded)))
    `(org-meta-line ((,c :inherit bv-themes-faded)))

    `(org-checkbox ((,c :inherit bv-themes-faded)))
    `(org-checkbox-statistics-todo ((,c :inherit bv-themes-faded)))
    `(org-checkbox-statistics-done ((,c :inherit bv-themes-faded)))

    `(org-link ((,c :inherit link)))
    `(org-footnote ((,c :inherit link)))
    `(org-ellipsis ((,c :inherit bv-themes-faded)))
    `(org-archived ((,c :inherit bv-themes-faded)))

    ;; Version control
    `(diff-added ((,c :inherit bv-themes-salient)))
    `(diff-removed ((,c :inherit bv-themes-popout)))
    `(diff-changed ((,c :inherit bv-themes-popout)))
    `(diff-header ((,c :inherit bv-themes-strong)))
    `(diff-file-header ((,c :inherit bv-themes-strong :weight bold)))
    `(diff-hunk-header ((,c :inherit bv-themes-faded)))

    ;; Magit faces
    `(magit-section-heading ((,c :inherit bv-themes-strong)))
    `(magit-branch-current ((,c :inherit bv-themes-salient :weight bold)))
    `(magit-branch-local ((,c :inherit bv-themes-salient)))
    `(magit-branch-remote ((,c :inherit bv-themes-popout)))
    `(magit-hash ((,c :inherit bv-themes-faded)))
    `(magit-diff-added ((,c :inherit diff-added)))
    `(magit-diff-removed ((,c :inherit diff-removed)))
    `(magit-diff-file-heading ((,c :inherit bv-themes-strong)))
    `(magit-diff-hunk-heading ((,c :inherit bv-themes-faded)))

    ;; Header line faces (for modeline)
    `(bv-themes-header-default ((,c :foreground ,foreground
                                    :background ,subtle
                                    :box (:line-width 1 :color ,background :style nil))))
    `(bv-themes-header-strong ((,c :foreground ,strong
                                   :background ,subtle
                                   :inherit bv-themes-strong
                                   :box (:line-width 1 :color ,background :style nil))))
    `(bv-themes-header-salient ((,c :foreground ,background
                                    :background ,salient
                                    :box (:line-width 1 :color ,background :style nil))))
    `(bv-themes-header-popout ((,c :foreground ,background
                                   :background ,popout
                                   :box (:line-width 1 :color ,background :style nil))))
    `(bv-themes-header-faded ((,c :foreground ,background
                                  :background ,faded
                                  :box (:line-width 1 :color ,background :style nil))))
    `(bv-themes-header-critical ((,c :foreground ,background
                                     :background ,critical
                                     :box (:line-width 1 :color ,background :style nil))))

    ;; Package-specific faces
    ;; Corfu
    `(corfu-current ((,c :inherit bv-themes-subtle :extend t)))
    `(corfu-default ((,c :inherit bv-themes-default :background ,subtle)))
    `(corfu-border ((,c :inherit bv-themes-faded)))
    `(corfu-annotations ((,c :inherit bv-themes-faded)))
    `(corfu-bar ((,c :background ,salient)))

    ;; Which-key
    `(which-key-key-face ((,c :inherit bv-themes-strong)))
    `(which-key-separator-face ((,c :inherit bv-themes-faded)))
    `(which-key-note-face ((,c :inherit bv-themes-faded)))
    `(which-key-command-description-face ((,c :inherit bv-themes-default)))
    `(which-key-local-map-description-face ((,c :inherit bv-themes-salient)))
    `(which-key-group-description-face ((,c :inherit bv-themes-strong)))

    ;; Flymake
    `(flymake-error ((,c :underline (:style wave :color ,critical))))
    `(flymake-warning ((,c :underline (:style wave :color ,popout))))
    `(flymake-note ((,c :underline (:style wave :color ,salient))))

    ;; Flyspell
    `(flyspell-incorrect ((,c :underline (:style wave :color ,critical))))
    `(flyspell-duplicate ((,c :underline (:style wave :color ,popout))))

    ;; Dashboard
    `(dashboard-banner-logo-title ((,c :inherit bv-themes-default :height 0.9)))
    `(dashboard-heading ((,c :inherit bv-themes-strong :height 1.0)))
    `(dashboard-items-face ((,c :inherit bv-themes-default)))
    `(dashboard-no-items-face ((,c :inherit bv-themes-faded)))
    `(dashboard-text-banner ((,c :inherit bv-themes-faded :height 0.9)))

    ;; Calendar
    `(calendar-today ((,c :inherit bv-themes-salient :weight bold :underline t)))
    `(calendar-weekday-header ((,c :inherit bv-themes-faded)))
    `(calendar-weekend-header ((,c :inherit bv-themes-subtle)))
    `(calendar-month-header ((,c :inherit bv-themes-strong :height 1.1)))

    ;; Ace-window
    `(aw-leading-char-face ((,c :inherit bv-themes-salient :height 2.0 :weight bold)))
    `(aw-mode-line-face ((,c :inherit bv-themes-strong)))

    ;; Display-time
    `(display-time-date-and-time ((,c :inherit bv-themes-faded)))

    ;; Keycast
    `(keycast-key ((,c :inherit bv-themes-strong :box nil :height 1.0)))
    `(keycast-command ((,c :inherit bv-themes-salient :weight normal)))

    ;; Perspective
    `(persp-selected-face ((,c :inherit bv-themes-strong :weight bold)))

    ;; Org-agenda specific
    `(org-agenda-structure ((,c :inherit bv-themes-strong :weight light :height 1.2)))
    `(org-agenda-date ((,c :inherit bv-themes-strong :weight regular :underline nil)))
    `(org-agenda-date-today ((,c :inherit bv-themes-salient :weight regular :underline t)))
    `(org-agenda-date-weekend ((,c :inherit bv-themes-faded :weight regular)))
    `(org-agenda-current-time ((,c :inherit bv-themes-popout)))
    `(org-agenda-done ((,c :inherit bv-themes-subtle :strike-through nil)))

    ;; Elfeed
    `(elfeed-search-title-face ((,c :inherit default)))
    `(elfeed-search-unread-title-face ((,c :inherit bold)))
    `(elfeed-search-date-face ((,c :inherit font-lock-comment-face)))
    `(elfeed-search-feed-face ((,c :inherit font-lock-keyword-face)))
    `(elfeed-search-tag-face ((,c :inherit font-lock-type-face)))

    ;; All-the-icons
    `(all-the-icons-red ((,c :foreground ,critical)))
    `(all-the-icons-lred ((,c :foreground ,critical)))
    `(all-the-icons-dred ((,c :foreground ,critical)))
    `(all-the-icons-green ((,c :foreground ,salient)))
    `(all-the-icons-lgreen ((,c :foreground ,salient)))
    `(all-the-icons-dgreen ((,c :foreground ,salient)))
    `(all-the-icons-blue ((,c :foreground ,salient)))
    `(all-the-icons-lblue ((,c :foreground ,salient)))
    `(all-the-icons-dblue ((,c :foreground ,faded)))
    `(all-the-icons-yellow ((,c :foreground ,popout)))
    `(all-the-icons-orange ((,c :foreground ,popout)))
    `(all-the-icons-dorange ((,c :foreground ,popout)))
    `(all-the-icons-purple ((,c :foreground ,faded)))
    `(all-the-icons-maroon ((,c :foreground ,critical)))
    ))

;;; Palette utilities

(defun bv-themes--palette-value (theme &optional overrides)
  "Get palette for THEME with optional OVERRIDES."
  (let ((base-palette (intern (format "%s-palette" theme))))
    (if overrides
        (append overrides (symbol-value base-palette))
      (symbol-value base-palette))))

(defun bv-themes--retrieve-palette-value (color palette)
  "Recursively retrieve COLOR from PALETTE."
  (let ((value (cdr (assq color palette))))
    (cond
     ((stringp value) value)
     ((symbolp value)
      (bv-themes--retrieve-palette-value value palette))
     (t 'unspecified))))

;;; Theme generation macro

(defmacro bv-themes-theme (name palette)
  "Generate theme NAME using PALETTE."
  (let ((palette-sym (make-symbol "palette")))
    `(let ((,palette-sym (symbol-value ',palette)))
       (deftheme ,name
         ,(format "BV theme: %s variant" name))

       (let* ((c '((class color) (min-colors 256)))
              ;; User options
              (bold-constructs bv-themes-bold-constructs)
              (italic-constructs bv-themes-italic-constructs)
              (font-mono bv-themes-font-family-monospaced)
              (font-prop bv-themes-font-family-proportional)
              (font-size bv-themes-font-size)
              ;; Extract colors from palette
              ,@(cl-loop for (key . _) in (symbol-value palette)
                         collect `(,key (bv-themes--retrieve-palette-value
                                        ',key ,palette-sym))))

         (custom-theme-set-faces ',name ,@bv-themes-faces))

       (provide-theme ',name))))

;;; Interactive commands

(defvar bv-themes-variants '(bv-light bv-dark)
  "List of available theme variants.")

(defun bv-themes-load-theme (theme)
  "Load BV theme variant THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun bv-themes-toggle ()
  "Toggle between light and dark BV themes."
  (interactive)
  (let* ((current (cl-find-if (lambda (theme)
                                (memq theme custom-enabled-themes))
                              bv-themes-variants))
         (next (if (eq current 'bv-light) 'bv-dark 'bv-light)))
    (bv-themes-load-theme next)))

(defun bv-themes-current ()
  "Return the currently active BV theme variant, or nil if none active."
  (cl-find-if (lambda (theme)
                (memq theme custom-enabled-themes))
              bv-themes-variants))

(defun bv-themes-variant ()
  "Return the variant name of the current theme ('light' or 'dark')."
  (let ((current (bv-themes-current)))
    (cond
     ((eq current 'bv-light) "light")
     ((eq current 'bv-dark) "dark")
     (t nil))))

(provide 'bv-themes)
;;; bv-themes.el ends here