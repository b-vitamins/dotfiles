;;; bv-layout.el --- Layout configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Professional frame, window, and buffer layout policy for the BV Emacs
;; configuration.  The emphasis is quiet chrome, explicit buffer roles, readable
;; prose, dense code, and predictable side windows.

;;; Code:

(require 'cl-lib)
(require 'disp-table)
(require 'bv-layout-core)

;; Declare external variables to silence elint warnings.
(defvar display-buffer-alist)
(defvar fringes-outside-margins)
(defvar temp-buffer-max-height)
(defvar window-divider-default-bottom-width)
(defvar window-divider-default-places)
(defvar window-divider-default-right-width)
(defvar window-min-height)

;;; Frame policy

(defcustom bv-layout-fringe-width 8
  "Default fringe width in pixels."
  :type 'integer
  :group 'bv-layout)

(defcustom bv-layout-window-divider-width 1
  "Right-side window divider width in pixels."
  :type 'integer
  :group 'bv-layout)

(defcustom bv-layout-resize-frames-to-profile t
  "When non-nil, display profile rules may resize graphical frames."
  :type 'boolean
  :group 'bv-layout)

(defcustom bv-layout-frame-profile-rules
  '((:description "Large presentation display"
     :name "LG TV"
     :parameters ((width . 100)
                  (height . 45)
                  (internal-border-width . 32)
                  (left-fringe . 10)
                  (right-fringe . 10)))
    (:description "Large high-resolution desktop"
     :min-pixel-width 3000
     :parameters ((width . 96)
                  (height . 48)
                  (internal-border-width . 28)
                  (left-fringe . 10)
                  (right-fringe . 10)))
    (:description "Compact laptop panel"
     :max-pixel-width 1600
     :parameters ((width . 81)
                  (height . 43)
                  (internal-border-width . 20)
                  (left-fringe . 8)
                  (right-fringe . 8))))
  "Display-aware frame profile rules.

Rules are plists.  Supported match keys are `:name' as a regular expression,
`:min-pixel-width', `:max-pixel-width', `:min-pixel-height', and
`:max-pixel-height'.  The `:parameters' value is an alist of frame parameters
applied on top of `bv-layout-default-frame-parameters'."
  :type '(repeat plist)
  :group 'bv-layout)

(defun bv-layout--frame-monitor-attributes (frame)
  "Return monitor attributes for FRAME or nil."
  (when (and (fboundp 'frame-monitor-attributes) (frame-live-p frame))
    (frame-monitor-attributes frame)))

(defun bv-layout--frame-monitor-name (frame)
  "Return monitor name for FRAME, or an empty string."
  (let ((attrs (bv-layout--frame-monitor-attributes frame)))
    (or (and attrs (alist-get 'name attrs)) "")))

(defun bv-layout--dimension-value (value index)
  "Return dimension component INDEX from VALUE."
  (cond
   ((vectorp value) (aref value index))
   ((and (consp value) (numberp (cdr value)))
    (if (= index 0) (car value) (cdr value)))
   ((listp value) (nth index value))
   (t nil)))

(defun bv-layout--monitor-pixel-width (attrs)
  "Return monitor pixel width from ATTRS."
  (bv-layout--dimension-value (alist-get 'geometry attrs) 2))

(defun bv-layout--monitor-pixel-height (attrs)
  "Return monitor pixel height from ATTRS."
  (bv-layout--dimension-value (alist-get 'geometry attrs) 3))

(defun bv-layout--rule-number-match-p (rule key value predicate)
  "Return non-nil if RULE KEY matches VALUE using PREDICATE."
  (let ((threshold (plist-get rule key)))
    (or (not threshold)
        (and (numberp value)
             (funcall predicate value threshold)))))

(defun bv-layout--frame-rule-match-p (rule frame)
  "Return non-nil when frame profile RULE applies to FRAME."
  (let* ((attrs (bv-layout--frame-monitor-attributes frame))
         (name (bv-layout--frame-monitor-name frame))
         (pixel-width (and attrs (bv-layout--monitor-pixel-width attrs)))
         (pixel-height (and attrs (bv-layout--monitor-pixel-height attrs)))
         (name-regexp (plist-get rule :name)))
    (and (or (not name-regexp)
             (and (stringp name)
                  (string-match-p name-regexp name)))
         (bv-layout--rule-number-match-p
          rule :min-pixel-width pixel-width #'>=)
         (bv-layout--rule-number-match-p
          rule :max-pixel-width pixel-width #'<=)
         (bv-layout--rule-number-match-p
          rule :min-pixel-height pixel-height #'>=)
         (bv-layout--rule-number-match-p
          rule :max-pixel-height pixel-height #'<=))))

(defun bv-layout--matching-frame-rule (&optional frame)
  "Return the first display profile rule matching FRAME."
  (let ((frame (or frame (selected-frame))))
    (cl-find-if (lambda (rule)
                  (bv-layout--frame-rule-match-p rule frame))
                bv-layout-frame-profile-rules)))

(defun bv-layout--merged-frame-parameters (&optional frame)
  "Return frame parameters for FRAME after applying profile rules."
  (let* ((rule (bv-layout--matching-frame-rule frame))
         (base (bv-layout-default-frame-alist))
         (profile (copy-tree (plist-get rule :parameters))))
    (dolist (parameter profile)
      (setf (alist-get (car parameter) base) (cdr parameter)))
    base))

(defun bv-layout--apply-frame-parameters (&optional frame)
  "Apply display-aware frame parameters to FRAME."
  (let* ((frame (or frame (selected-frame)))
         (parameters (bv-layout--merged-frame-parameters frame))
         (width (alist-get 'width parameters))
         (height (alist-get 'height parameters)))
    (when (display-graphic-p frame)
      (dolist (parameter parameters)
        (unless (memq (car parameter) '(width height))
          (set-frame-parameter frame (car parameter) (cdr parameter))))
      (when (and bv-layout-resize-frames-to-profile
                 (numberp width)
                 (numberp height))
        (set-frame-size frame width height)))))

(defun bv-layout--apply-frame-layout ()
  "Apply frame-level layout defaults."
  (bv-layout-apply-default-frame-alist)
  (when (display-graphic-p)
    (set-fringe-mode bv-layout-fringe-width)
    (bv-layout--apply-frame-parameters))
  (setq fringes-outside-margins t)
  (when (fboundp 'window-divider-mode)
    (setq window-divider-default-right-width bv-layout-window-divider-width
          window-divider-default-bottom-width 0
          window-divider-default-places 'right-only)
    (window-divider-mode 1)))

(add-hook 'after-make-frame-functions #'bv-layout--apply-frame-parameters)

;;; Display glyphs

(defface bv-layout-fallback '((t :inherit fixed-pitch))
  "Fallback face for layout glyphs missing in the primary font."
  :group 'bv-layout)

(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'bv-layout-fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'bv-layout-fallback))

;;; Buffer roles

(defcustom bv-layout-buffer-roles
  '((org
     :modes (org-mode)
     :margins (2 . 2)
     :visual-line t
     :truncate-lines nil
     :line-numbers nil)
    (prose
     :modes (text-mode markdown-mode latex-mode LaTeX-mode rst-mode adoc-mode)
     :margins (2 . 2)
     :visual-line t
     :truncate-lines nil
     :line-numbers nil)
    (docs
     :modes (help-mode Info-mode woman-mode Man-mode)
     :margins (2 . 2)
     :visual-line t
     :truncate-lines nil
     :line-numbers nil)
    (reading
     :modes (eww-mode elfeed-show-mode mu4e-view-mode message-mode mail-mode
             nov-mode)
     :margins (2 . 2)
     :visual-line t
     :truncate-lines nil
     :line-numbers nil)
    (code
     :modes (prog-mode conf-mode)
     :margins (0 . 0)
     :visual-line nil
     :truncate-lines t
     :line-numbers t)
    (navigation
     :modes (dired-mode ibuffer-mode tabulated-list-mode magit-status-mode)
     :margins (0 . 0)
     :visual-line nil
     :truncate-lines t
     :line-numbers nil)
    (terminal
     :modes (term-mode vterm-mode eat-mode eshell-mode shell-mode)
     :margins (0 . 0)
     :visual-line nil
     :truncate-lines t
     :line-numbers nil)
    (default
     :margins (0 . 0)
     :visual-line nil
     :truncate-lines nil
     :line-numbers nil))
  "Spatial roles for buffers.

Each role controls margins, wrapping, truncation, and line-number policy.  The
first role whose `:modes' match `major-mode' wins; the `default' role is used
when no mode-specific role matches."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-layout)

(defun bv-layout--mode-matches-p (modes)
  "Return non-nil when current buffer derives from one of MODES."
  (and modes (apply #'derived-mode-p modes)))

(defun bv-layout-buffer-role ()
  "Return the current buffer's layout role symbol."
  (or (cl-loop for (role . spec) in bv-layout-buffer-roles
               for modes = (plist-get spec :modes)
               when (and modes (bv-layout--mode-matches-p modes))
               return role)
      'default))

(defun bv-layout--role-spec (&optional role)
  "Return layout plist for ROLE."
  (cdr (assq (or role (bv-layout-buffer-role)) bv-layout-buffer-roles)))

(defun bv-layout--role-margins (role)
  "Return cons of left and right margins for ROLE."
  (or (plist-get (bv-layout--role-spec role) :margins) '(0 . 0)))

(defun bv-layout--refresh-buffer-windows (&optional buffer)
  "Refresh windows showing BUFFER so margin changes are visible."
  (let ((buffer (or buffer (current-buffer))))
    (walk-windows
     (lambda (window)
       (when (eq (window-buffer window) buffer)
         (set-window-buffer window buffer)))
     nil
     t)))

(defun bv-layout-apply-buffer-role ()
  "Apply the current buffer's layout role."
  (let* ((role (bv-layout-buffer-role))
         (spec (bv-layout--role-spec role))
         (margins (bv-layout--role-margins role)))
    (setq-local left-margin-width (car margins)
                right-margin-width (cdr margins))
    (when (fboundp 'visual-line-mode)
      (visual-line-mode (if (plist-get spec :visual-line) 1 -1)))
    (setq-local truncate-lines (plist-get spec :truncate-lines))
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode
       (if (plist-get spec :line-numbers) 1 -1)))
    (bv-layout--refresh-buffer-windows)))

(add-hook 'after-change-major-mode-hook #'bv-layout-apply-buffer-role)

;;; Popup and temporary window policy

(defcustom bv-layout-side-window-width 0.36
  "Width used for right-side utility windows."
  :type 'number
  :group 'bv-layout)

(defcustom bv-layout-bottom-window-height 0.25
  "Height used for bottom utility windows."
  :type 'number
  :group 'bv-layout)

(defcustom bv-layout-temp-buffer-max-height 10
  "Maximum height for automatically resized temporary buffers."
  :type 'integer
  :group 'bv-layout)

(defcustom bv-layout-display-buffer-rules
  '(("\\*\\(?:Help\\|Apropos\\|info\\|Occur\\|xref\\).*\\*"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . bv-layout-side-window-width)
     (preserve-size . (nil . t)))
    ("\\*Embark Collect.*\\*"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (slot . 2)
     (window-width . bv-layout-side-window-width)
     (preserve-size . (nil . t)))
    ("\\*\\(?:Warnings\\|Compile-Log\\|Backtrace\\|Flymake diagnostics.*\\).*\\*"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . bottom)
     (slot . 1)
     (window-height . bv-layout-bottom-window-height)
     (preserve-size . (t . nil)))
    ("\\*\\(?:Messages\\|Async Shell Command\\|Shell Command Output\\)\\*"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)
     (window-height . bv-layout-bottom-window-height)
     (preserve-size . (t . nil))))
  "Display-buffer rules owned by the BV layout system.

The values of `window-width' and `window-height' may name layout variables; they
are resolved when rules are installed."
  :type '(repeat sexp)
  :group 'bv-layout)

(defvar bv-layout--installed-display-buffer-rules nil
  "Resolved display-buffer rules currently installed by `bv-layout'.")

(defun bv-layout--resolve-display-buffer-rule (rule)
  "Resolve variable-valued layout RULE entries."
  (pcase-let ((`(,matcher . ,body) rule))
    (cons matcher
          (mapcar (lambda (entry)
                    (cond
                     ((and (consp entry)
                           (memq (car entry) '(window-width window-height))
                           (symbolp (cdr entry))
                           (boundp (cdr entry)))
                      (cons (car entry) (symbol-value (cdr entry))))
                     (t entry)))
                  body))))

(defun bv-layout-install-display-buffer-rules ()
  "Install BV side-window and temporary-buffer display rules."
  (setq display-buffer-alist
        (cl-set-difference display-buffer-alist
                           bv-layout--installed-display-buffer-rules
                           :test #'equal))
  (setq bv-layout--installed-display-buffer-rules
        (mapcar #'bv-layout--resolve-display-buffer-rule
                bv-layout-display-buffer-rules))
  (dolist (rule (reverse bv-layout--installed-display-buffer-rules))
    (add-to-list 'display-buffer-alist rule)))

(defun bv-layout--apply-popup-policy ()
  "Apply popup and temporary window policy."
  (when (fboundp 'temp-buffer-resize-mode)
    (temp-buffer-resize-mode 1))
  (setq temp-buffer-max-height bv-layout-temp-buffer-max-height
        window-min-height 1)
  (bv-layout-install-display-buffer-rules))

;;; Diagnostics

(defun bv-layout-report ()
  "Show a diagnostic report for the active BV layout system."
  (interactive)
  (let ((buffer (get-buffer-create "*BV Layout Report*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (frame (selected-frame)))
        (erase-buffer)
        (insert "#+title: BV Layout Report\n\n")
        (insert "* Frame\n")
        (insert (format "- monitor: %s\n"
                        (or (bv-layout--frame-monitor-name frame) "n/a")))
        (insert (format "- profile: %s\n"
                        (or (plist-get
                             (bv-layout--matching-frame-rule frame)
                             :description)
                            "default")))
        (insert (format "- parameters: %S\n\n"
                        (bv-layout--merged-frame-parameters frame)))
        (insert "* Current Buffer\n")
        (insert (format "- role: %s\n" (bv-layout-buffer-role)))
        (insert (format "- margins: %S\n"
                        (bv-layout--role-margins
                         (bv-layout-buffer-role))))
        (insert (format "- visual-line-mode: %s\n"
                        (if (bound-and-true-p visual-line-mode) "on" "off")))
        (insert (format "- truncate-lines: %s\n" truncate-lines))
        (insert (format "- display-line-numbers-mode: %s\n\n"
                        (if (bound-and-true-p display-line-numbers-mode)
                            "on"
                          "off")))
        (insert "* Popup Policy\n")
        (insert (format "- side width: %s\n" bv-layout-side-window-width))
        (insert (format "- bottom height: %s\n"
                        bv-layout-bottom-window-height))
        (insert (format "- temp max height: %s\n"
                        bv-layout-temp-buffer-max-height))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

;;; Initialization

(defun bv-layout-init ()
  "Initialize the BV layout system."
  (bv-layout--apply-frame-layout)
  (bv-layout--apply-popup-policy)
  (bv-layout-apply-buffer-role))

(bv-layout-init)

(provide 'bv-layout)
;;; bv-layout.el ends here
