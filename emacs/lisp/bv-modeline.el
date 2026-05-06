;;; bv-modeline.el --- BV header-line status system -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; A compact, responsive header-line status surface.  The visible status line is
;; intentionally at the top of each window; the traditional bottom mode line is
;; kept only as a quiet structural separator when multiple windows need it.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup bv-modeline nil
  "BV header-line status surface."
  :group 'bv)

(defcustom bv-modeline-detail-level 'auto
  "Detail policy for the BV header line.
`minimal' shows only durable navigation essentials.  `auto' starts with the
full model and lets the renderer drop low-priority details as space tightens.
`full' asks the renderer to keep optional details for as long as possible."
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Auto" auto)
                 (const :tag "Full" full))
  :group 'bv-modeline)

(defcustom bv-modeline-right-max-ratio 0.46
  "Maximum fraction of header width normally given to right-side metadata."
  :type 'number
  :group 'bv-modeline)

(defcustom bv-modeline-min-title-width 12
  "Preferred minimum width reserved for the primary buffer title."
  :type 'integer
  :group 'bv-modeline)

(defcustom bv-modeline-org-title-scan-lines 80
  "Maximum number of leading Org lines scanned for `#+TITLE'."
  :type 'integer
  :group 'bv-modeline)

(defcustom bv-modeline-project-name-overrides nil
  "Canonical display names for projects in the BV header line.
Each entry is (MATCH . NAME).  MATCH may be a project root path or the raw
project name returned by `project-name'."
  :type '(alist :key-type string :value-type string)
  :group 'bv-modeline)

(defcustom bv-modeline-generic-project-names
  '("code" "project" "projects" "repo" "repos" "src" "source" "work")
  "Directory names that are too generic to be useful as project names."
  :type '(repeat string)
  :group 'bv-modeline)

(defcustom bv-modeline-show-time t
  "Whether the BV header line should reserve space for wall-clock time."
  :type 'boolean
  :group 'bv-modeline)

(defconst bv-modeline--edge-pad-rise 0.15
  "Display raise used to keep the BV header band optically padded.")

(defconst bv-modeline--edge-pad-drop -0.20
  "Display drop used to keep the BV header band optically padded.")

(defconst bv-modeline--status-gutter " "
  "Neutral gutter between the status accent and primary title.")

(defconst bv-modeline--left-separator " · "
  "Separator between left identity segments.")

(defconst bv-modeline--right-separator " · "
  "Separator between right metadata segments.")

(defconst bv-modeline--side-gap-min-width 2
  "Minimum neutral gap between left identity and right metadata.")

;; External variables
(defvar battery-mode-line-string)
(defvar bv-keycast-mode)
(defvar bv-format-on-save-mode)
(defvar compilation-in-progress)
(defvar defining-kbd-macro)
(defvar display-time-string)
(defvar display-time-mode)
(defvar emms-mode-line-string)
(defvar emms-player-playing-p)
(defvar envrc--status)
(defvar eshell-status-in-modeline)
(defvar executing-kbd-macro)
(defvar flymake-mode)
(defvar Info-current-node)
(defvar Info-use-header-line)
(defvar keycast--this-command)
(defvar keycast--this-command-keys)
(defvar no-mode-line)
(defvar org-capture-mode)
(defvar org-mode-line-string)
(defvar shell-file-name)
(defvar vc-mode)

;; External functions
(declare-function bound-and-true-p "subr" (var))
(declare-function derived-mode-p "subr" (&rest modes))
(declare-function eglot-managed-p "eglot" ())
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-running-backends "flymake" ())
(declare-function mode-line-window-selected-p "bindings")
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-name "project" (project))
(declare-function project-root "project" (project))

;; Functions defined in with-eval-after-load blocks
(declare-function calendar-setup-header "bv-modeline")
(declare-function org-capture-turn-off-header-line "bv-modeline")

(defvar-local bv-modeline--cached-org-title nil
  "Cached Org title for the current buffer.")

(defvar-local bv-modeline--cached-org-title-tick nil
  "Buffer modification tick for `bv-modeline--cached-org-title'.")

(defvar-local bv-modeline--cached-project-directory nil
  "Directory used for `bv-modeline--cached-project-info'.")

(defvar-local bv-modeline--cached-project-info nil
  "Cached project metadata for the current buffer.")

(defvar bv-modeline-renderers
  '((bv-modeline-message-mode-p . bv-modeline-message-mode)
    (bv-modeline-dired-mode-p . bv-modeline-dired-mode)
    (bv-modeline-info-mode-p . bv-modeline-info-mode)
    (bv-modeline-help-mode-p . bv-modeline-help-mode)
    (bv-modeline-calendar-mode-p . bv-modeline-calendar-mode)
    (bv-modeline-org-capture-mode-p . bv-modeline-org-capture-mode)
    (bv-modeline-org-agenda-mode-p . bv-modeline-org-agenda-mode)
    (bv-modeline-term-mode-p . bv-modeline-term-mode)
    (bv-modeline-vterm-mode-p . bv-modeline-term-mode)
    (bv-modeline-pdf-mode-p . bv-modeline-pdf-mode)
    (bv-modeline-completion-list-mode-p . bv-modeline-completion-list-mode))
  "Predicate/renderer pairs for special BV header-line presentations.")

(defun bv-modeline--clean (value)
  "Return VALUE as plain, trimmed text."
  (string-trim
   (substring-no-properties
    (cond ((null value) "")
          ((stringp value) value)
          (t (format "%s" value))))))

(defun bv-modeline--blank-p (value)
  "Return non-nil when VALUE is nil or visually empty."
  (string-empty-p (bv-modeline--clean value)))

(defun bv-modeline--right-slice (text width)
  "Return the rightmost WIDTH display columns of TEXT."
  (let* ((text (bv-modeline--clean text))
         (full-width (string-width text))
         (start (max 0 (- full-width width))))
    (truncate-string-to-width text (+ start width) start)))

(defun bv-modeline--truncate-string (text width &optional style)
  "Return TEXT fitted to WIDTH using STYLE.
STYLE may be `left', `middle', or `right'."
  (let ((text (bv-modeline--clean text)))
    (cond
     ((<= width 0) "")
     ((<= (string-width text) width) text)
     ((= width 1) "…")
     ((eq style 'left)
      (concat "…" (bv-modeline--right-slice text (1- width))))
     ((eq style 'middle)
      (let* ((body-width (1- width))
             (front-width (max 1 (/ body-width 2)))
             (back-width (max 1 (- body-width front-width))))
        (concat (truncate-string-to-width text front-width)
                "…"
                (bv-modeline--right-slice text back-width))))
     (t
      (concat (truncate-string-to-width text (1- width)) "…")))))

(defun bv-modeline--segment (key text face &rest properties)
  "Return a modeline segment named KEY with TEXT and FACE.
PROPERTIES is a plist overriding rendering metadata."
  (let ((text (bv-modeline--clean text))
        (segment (list :key key
                       :text ""
                       :face face
                       :priority 50
                       :required nil
                       :min-width 0
                       :truncate 'right)))
    (unless (string-empty-p text)
      (setq segment (plist-put segment :text text))
      (while properties
        (setq segment (plist-put segment (pop properties) (pop properties))))
      segment)))

(defun bv-modeline--segment-width (segment)
  "Return natural display width for SEGMENT."
  (or (plist-get segment :fixed-width)
      (string-width (plist-get segment :text))))

(defun bv-modeline--pad-string (text width align)
  "Pad TEXT to WIDTH columns using ALIGN."
  (let ((pad (max 0 (- width (string-width text)))))
    (pcase align
      ('right (concat (make-string pad ?\s) text))
      ('center (let ((left (/ pad 2)))
                 (concat (make-string left ?\s)
                         text
                         (make-string (- pad left) ?\s))))
      (_ (concat text (make-string pad ?\s))))))

(defun bv-modeline--segment-render (segment)
  "Render SEGMENT."
  (let* ((width (or (plist-get segment :render-width)
                    (bv-modeline--segment-width segment)))
         (text (bv-modeline--truncate-string
                (plist-get segment :text)
                width
                (plist-get segment :truncate)))
         (text (bv-modeline--pad-string
                text width (plist-get segment :align)))
         (face (plist-get segment :face))
         (help (plist-get segment :help-echo))
         (mouse-face (plist-get segment :mouse-face))
         (properties (append (list 'face face)
                             (when help (list 'help-echo help))
                             (when mouse-face
                               (list 'mouse-face mouse-face)))))
    (apply #'propertize text properties)))

(defun bv-modeline--segments-natural-width (segments separator)
  "Return natural display width for SEGMENTS joined by SEPARATOR."
  (if (null segments)
      0
    (+ (cl-loop for segment in segments
                sum (bv-modeline--segment-width segment))
       (* (string-width separator) (1- (length segments))))))

(defun bv-modeline--segments-required-min-width (segments separator)
  "Return minimum width needed by required SEGMENTS."
  (let ((segments (cl-remove-if-not
                   (lambda (segment)
                     (plist-get segment :required))
                   segments)))
    (if (null segments)
        0
      (+ (cl-loop for segment in segments
                  sum (max 1
                           (min (bv-modeline--segment-width segment)
                                (or (plist-get segment :min-width) 1))))
         (* (string-width separator) (1- (length segments)))))))

(defun bv-modeline--segment-removable-p (segment)
  "Return non-nil when SEGMENT can be dropped under width pressure."
  (not (plist-get segment :required)))

(defun bv-modeline--drop-lowest-priority (segments)
  "Drop the lowest priority optional segment from SEGMENTS."
  (let* ((optional (cl-remove-if-not #'bv-modeline--segment-removable-p
                                     segments))
         (drop (car (sort (copy-sequence optional)
                          (lambda (a b)
                            (< (or (plist-get a :priority) 0)
                               (or (plist-get b :priority) 0)))))))
    (delq drop segments)))

(defun bv-modeline--shrink-segments (segments width separator)
  "Set render widths on SEGMENTS so they fit WIDTH with SEPARATOR."
  (let ((remaining width)
        (separator-width (string-width separator))
        rendered)
    (dolist (segment segments (nreverse rendered))
      (let* ((segment (copy-sequence segment))
             (separator-cost (if rendered separator-width 0))
             (space (- remaining separator-cost)))
        (when (> space 0)
          (let* ((natural (bv-modeline--segment-width segment))
                 (render-width (min natural space))
                 (min-width (or (plist-get segment :min-width) 0)))
            (when (or (plist-get segment :required)
                      (>= render-width min-width))
              (setq segment (plist-put segment :render-width render-width))
              (push segment rendered)
              (setq remaining (- remaining separator-cost render-width)))))))))

(defun bv-modeline--fit-segments (segments width separator)
  "Return a copy of SEGMENTS fitted to WIDTH with SEPARATOR."
  (let ((segments (mapcar #'copy-sequence
                          (cl-remove-if #'null segments))))
    (while (and segments
                (> (bv-modeline--segments-natural-width segments separator)
                   width)
                (cl-some #'bv-modeline--segment-removable-p segments))
      (setq segments (bv-modeline--drop-lowest-priority segments)))
    (cond
     ((<= width 0) nil)
     ((<= (bv-modeline--segments-natural-width segments separator) width)
      segments)
     (t
      (bv-modeline--shrink-segments segments width separator)))))

(defconst bv-modeline--inactive-face-map
  '((bv-ui-header-default . bv-ui-header-inactive-default)
    (bv-ui-header-muted . bv-ui-header-inactive-muted)
    (bv-ui-header-strong . bv-ui-header-inactive-strong)
    (bv-ui-header-salient . bv-ui-header-inactive-salient)
    (bv-ui-header-info . bv-ui-header-inactive-info)
    (bv-ui-header-warning . bv-ui-header-inactive-warning)
    (bv-ui-header-error . bv-ui-header-inactive-error)
    (bv-ui-header-popout . bv-ui-header-inactive-popout)
    (bv-ui-header-critical . bv-ui-header-inactive-critical))
  "BV header faces used when a window is inactive.")

(defun bv-modeline--window-face (face selected)
  "Return FACE adjusted for SELECTED window state."
  (if selected
      face
    (or (cdr (assq face bv-modeline--inactive-face-map))
        face)))

(defun bv-modeline--window-segments (segments selected)
  "Return SEGMENTS with faces adjusted for SELECTED window state."
  (mapcar
   (lambda (segment)
     (let ((segment (copy-sequence segment)))
       (plist-put segment :face
                  (bv-modeline--window-face
                   (plist-get segment :face)
                   selected))))
   segments))

(defun bv-modeline--join-segments (segments separator &optional face)
  "Render SEGMENTS joined with SEPARATOR.
When FACE is non-nil, use it for the separator."
  (mapconcat #'bv-modeline--segment-render
             segments
             (propertize separator 'face (or face 'bv-ui-header-muted))))

(defun bv-modeline--selected-window-p ()
  "Return non-nil when rendering the selected window."
  (if (fboundp 'mode-line-window-selected-p)
      (mode-line-window-selected-p)
    t))

(defun bv-modeline--remote-info ()
  "Return remote metadata for `default-directory', or nil."
  (when-let ((remote (and default-directory
                          (ignore-errors (file-remote-p default-directory)))))
    (list :name remote
          :method (ignore-errors (file-remote-p default-directory 'method))
          :user (ignore-errors (file-remote-p default-directory 'user))
          :host (ignore-errors (file-remote-p default-directory 'host))
          :localname (ignore-errors
                       (file-remote-p default-directory 'localname)))))

(defun bv-modeline--elevated-p (&optional remote)
  "Return non-nil when REMOTE or `default-directory' uses elevated access."
  (let* ((remote (or remote (bv-modeline--remote-info)))
         (method (plist-get remote :method))
         (user (plist-get remote :user)))
    (or (member method '("sudo" "doas" "su"))
        (string= user "root"))))

(defun bv-modeline--remote-label (&optional remote)
  "Return a compact remote label for REMOTE or `default-directory'."
  (when-let* ((remote (or remote (bv-modeline--remote-info)))
              (method (or (plist-get remote :method) "tramp")))
    (if (bv-modeline--elevated-p remote)
        "ROOT"
      (upcase method))))

(defun bv-modeline--remote-help (&optional remote)
  "Return hover help for REMOTE or `default-directory'."
  (when-let ((remote (or remote (bv-modeline--remote-info))))
    (string-join
     (delq nil
           (list
            (format "Remote: %s" (plist-get remote :name))
            (when-let ((method (plist-get remote :method)))
              (format "Method: %s" method))
            (when-let ((user (plist-get remote :user)))
              (format "User: %s" user))
            (when-let ((host (plist-get remote :host)))
              (format "Host: %s" host))
            (when-let ((local (plist-get remote :localname)))
              (format "Path: %s" local))))
     "\n")))

(defun bv-modeline-status ()
  "Return buffer/window status: read-only, modified, or read-write."
  (cond ((window-dedicated-p) "DD")
        ((and buffer-file-name (buffer-modified-p)) "**")
        (buffer-read-only "RO")
        ((bv-modeline--elevated-p) "#@")
        ((bv-modeline--remote-info) "R@")
        (t "RW")))

(defun bv-modeline--status-face (status &optional selected)
  "Return the header face used for STATUS."
  (bv-modeline--window-face
   (cond ((string= status "**") 'bv-ui-header-critical)
         ((member status '("RO" "DD" ">_" "#@" "R@"))
          'bv-ui-header-popout)
         (t 'bv-ui-header-muted))
   selected))

(defun bv-modeline--status-help (status)
  "Return hover help for STATUS."
  (string-join
   (delq nil
         (list
          (pcase status
            ("**" "Status: modified, unsaved")
            ("RO" "Status: read-only")
            ("DD" "Status: dedicated window")
            ("#@" "Status: elevated remote access")
            ("R@" "Status: remote buffer")
            (">_" "Status: terminal")
            (_ "Status: read-write"))
          (bv-modeline--remote-help)))
   "\n"))

(defun bv-modeline--status-block (status &optional selected)
  "Render STATUS as the left status block."
  (let* ((status (bv-modeline--clean status))
         (status (if (> (string-width status) 2)
                     (bv-modeline--truncate-string status 2)
                   status))
         (block (propertize (format " %-2s " status)
                            'face (bv-modeline--status-face status selected)
                            'help-echo (bv-modeline--status-help status))))
    (add-text-properties 0 1
                         `(display (raise ,bv-modeline--edge-pad-rise))
                         block)
    block))

(defun bv-modeline--status-gutter (&optional selected)
  "Return the neutral gutter after the status accent block."
  (propertize bv-modeline--status-gutter
              'face (bv-modeline--window-face
                     'bv-ui-header-default
                     selected)))

(defun bv-modeline--edge-pad (&optional drop selected)
  "Return a one-column edge pad that contributes vertical header metrics.
When DROP is non-nil, lower the pad instead of raising it.  The visible text
size stays unchanged; the paired pads restore the header band's previous
breathing room after the renderer rewrite."
  (propertize " " 'face (bv-modeline--window-face
                         'bv-ui-header-default
                         selected)
              'display `(raise ,(if drop
                                     bv-modeline--edge-pad-drop
                                   bv-modeline--edge-pad-rise))))

(defun bv-modeline--mode-name ()
  "Return the current major mode name as plain text."
  (bv-modeline--clean
   (cond ((stringp mode-name) mode-name)
         ((listp mode-name) (format-mode-line mode-name))
         (t mode-name))))

(defconst bv-modeline--role-policies
  '((code :project t :branch t :diagnostics all :decorative t
          :position t :mode t)
    (note :project t :branch nil :diagnostics bad :decorative nil
          :position t :mode t)
    (writing :project t :branch nil :diagnostics bad :decorative nil
             :position t :mode t)
    (dired :project t :branch nil :diagnostics nil :decorative nil
           :position t :mode t)
    (magit :project t :branch nil :diagnostics nil :decorative nil
           :position nil :mode t)
    (terminal :project nil :branch nil :diagnostics nil :decorative nil
              :position nil :mode nil)
    (completion :project nil :branch nil :diagnostics nil :decorative nil
                :position nil :mode nil)
    (help :project nil :branch nil :diagnostics nil :decorative nil
          :position t :mode t)
    (pdf :project t :branch nil :diagnostics nil :decorative nil
         :position t :mode t)
    (agenda :project nil :branch nil :diagnostics nil :decorative nil
            :position nil :mode nil)
    (general :project t :branch t :diagnostics all :decorative t
             :position t :mode t))
  "Per-role segment policies for the BV header line.")

(defun bv-modeline--buffer-role ()
  "Return the display role of the current buffer."
  (cond
   ((derived-mode-p 'org-agenda-mode) 'agenda)
   ((derived-mode-p 'completion-list-mode) 'completion)
   ((derived-mode-p 'term-mode 'vterm-mode 'eshell-mode 'shell-mode) 'terminal)
   ((derived-mode-p 'dired-mode) 'dired)
   ((derived-mode-p 'magit-mode 'magit-status-mode) 'magit)
   ((derived-mode-p 'help-mode 'helpful-mode 'Info-mode 'Custom-mode) 'help)
   ((derived-mode-p 'pdf-view-mode 'doc-view-mode) 'pdf)
   ((derived-mode-p 'org-mode) 'note)
   ((derived-mode-p 'markdown-mode 'gfm-mode 'text-mode) 'writing)
   ((derived-mode-p 'prog-mode 'conf-mode) 'code)
   (t 'general)))

(defun bv-modeline--role-policy (role key &optional fallback)
  "Return policy KEY for ROLE, or FALLBACK."
  (let ((policy (cdr (or (assq role bv-modeline--role-policies)
                         (assq 'general bv-modeline--role-policies)))))
    (if (plist-member policy key)
        (plist-get policy key)
      fallback)))

(defun bv-modeline--vc-branch-info ()
  "Return cheap VC branch metadata for file-backed buffers."
  (when (and buffer-file-name
             (boundp 'vc-mode)
             vc-mode)
    (let* ((raw (bv-modeline--clean vc-mode))
           (backend (when (string-match
                           "\\`[[:space:]]*\\([^:]+\\):" raw)
                      (match-string 1 raw)))
           (branch (replace-regexp-in-string
                    "\\`[[:space:]]*[^:]+:" "" raw)))
      (unless (string-empty-p branch)
        (list :branch branch :backend backend :raw raw)))))

(defun bv-modeline--vc-branch ()
  "Return the current VC branch as plain text."
  (plist-get (bv-modeline--vc-branch-info) :branch))

(defun bv-modeline--project-context-p ()
  "Return non-nil when project context belongs in the header line."
  (or buffer-file-name
      (derived-mode-p 'dired-mode 'vc-dir-mode
                      'magit-mode 'magit-status-mode)))

(defun bv-modeline--project-alias (name root)
  "Return configured alias for project NAME at ROOT, or nil."
  (or (cdr (assoc-string root bv-modeline-project-name-overrides t))
      (cdr (assoc-string name bv-modeline-project-name-overrides t))))

(defun bv-modeline--canonical-project-name (project root)
  "Return a short canonical name for PROJECT at ROOT."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (base (file-name-nondirectory (directory-file-name root)))
         (parent (file-name-nondirectory
                  (directory-file-name
                   (file-name-directory (directory-file-name root)))))
         (raw (bv-modeline--clean
               (or (and (fboundp 'project-name)
                        (ignore-errors (project-name project)))
                   base)))
         (name (if (member (downcase raw) bv-modeline-generic-project-names)
                   (format "%s/%s" parent base)
                 raw)))
    (or (bv-modeline--project-alias name root) name)))

(defun bv-modeline--project-info ()
  "Return current project metadata when it is cheap and useful."
  (let ((directory default-directory))
    (unless (or (not (bv-modeline--project-context-p))
                (file-remote-p directory)
                (not (fboundp 'project-current)))
      (unless (equal directory bv-modeline--cached-project-directory)
        (setq bv-modeline--cached-project-directory directory
              bv-modeline--cached-project-info
              (when-let ((project (ignore-errors (project-current nil))))
                (let ((root (project-root project)))
                  (list :name (bv-modeline--canonical-project-name
                               project root)
                        :root root)))))
      bv-modeline--cached-project-info)))

(defun bv-modeline--project-name ()
  "Return current project name when it is cheap and useful."
  (plist-get (bv-modeline--project-info) :name))

(defun bv-modeline--project-root ()
  "Return current project root when it is cheap and useful."
  (plist-get (bv-modeline--project-info) :root))

(defun bv-modeline--right-essential-segment-p (segment)
  "Return non-nil when SEGMENT should survive normal width pressure."
  (plist-get segment :required))

(defun bv-modeline--comfort-segment-p (segment threshold)
  "Return non-nil when SEGMENT has priority at least THRESHOLD."
  (or (plist-get segment :required)
      (>= (or (plist-get segment :priority) 0) threshold)))

(defun bv-modeline--segments-comfort-width
    (segments separator threshold)
  "Return natural width for SEGMENTS with priority at least THRESHOLD."
  (bv-modeline--segments-natural-width
   (cl-remove-if-not
    (lambda (segment)
      (bv-modeline--comfort-segment-p segment threshold))
    segments)
   separator))

(defun bv-modeline--essential-right-width (segments separator)
  "Return width of essential right-side SEGMENTS joined by SEPARATOR."
  (bv-modeline--segments-natural-width
   (cl-remove-if-not #'bv-modeline--right-essential-segment-p segments)
   separator))

(defun bv-modeline--right-render-budget
    (content-width left-segments right-segments separator)
  "Return width budget for RIGHT-SEGMENTS.
The default policy preserves left identity first.  Optional right-side data
appears when the whole line can breathe, or when detail level is `full'."
  (let* ((right-natural (bv-modeline--segments-natural-width
                         right-segments separator))
         (right-essential (bv-modeline--essential-right-width
                           right-segments separator))
         (left-comfort (bv-modeline--segments-comfort-width
                        left-segments bv-modeline--left-separator 60))
         (right-comfort (bv-modeline--segments-comfort-width
                         right-segments separator 90))
         (left-min (bv-modeline--segments-required-min-width
                    left-segments bv-modeline--left-separator))
         (gap (if (> right-natural 0) bv-modeline--side-gap-min-width 0))
         (right-room (max 0 (- content-width left-min gap))))
    (cond
     ((<= (+ left-comfort gap right-natural) content-width)
      right-natural)
     ((<= (+ left-comfort gap right-comfort) content-width)
      (min right-comfort right-room))
     ((eq bv-modeline-detail-level 'full)
      (min right-natural
           right-room
           (max right-essential
                (bv-modeline--right-budget content-width))))
     (t
      (min right-essential right-room)))))

(defun bv-modeline--scan-org-title ()
  "Return `#+TITLE' from the current Org buffer, or nil."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t)
              (limit (save-excursion
                       (dotimes (_ bv-modeline-org-title-scan-lines)
                         (forward-line 1))
                       (point))))
          (when (re-search-forward
                 "^[ \t]*#\\+title:[ \t]*\\(.+?\\)[ \t]*$"
                 limit t)
            (bv-modeline--clean (match-string 1))))))))

(defun bv-modeline--org-title ()
  "Return cached Org title for the current buffer, or nil."
  (when (derived-mode-p 'org-mode)
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick bv-modeline--cached-org-title-tick)
        (setq bv-modeline--cached-org-title (bv-modeline--scan-org-title)
              bv-modeline--cached-org-title-tick tick))
      bv-modeline--cached-org-title)))

(defun bv-modeline--title ()
  "Return the primary title for the current buffer."
  (or (bv-modeline--org-title)
      (bv-modeline--clean (buffer-name))))

(defun bv-modeline--title-help ()
  "Return hover help for the current buffer title."
  (string-join
   (delq nil
         (list
          (when-let ((org-title (bv-modeline--org-title)))
            (format "Title: %s" org-title))
          (format "Buffer: %s" (buffer-name))
          (when buffer-file-name
            (format "File: %s" (abbreviate-file-name buffer-file-name)))))
   "\n"))

(defun bv-modeline--title-truncation ()
  "Return preferred truncation style for the current buffer title."
  (if (bv-modeline--org-title) 'right 'middle))

(defun bv-modeline--same-label-p (left right)
  "Return non-nil when LEFT and RIGHT are the same non-empty label."
  (and (not (bv-modeline--blank-p left))
       (not (bv-modeline--blank-p right))
       (string= (downcase (bv-modeline--clean left))
                (downcase (bv-modeline--clean right)))))

(defun bv-modeline--context-segments (&optional role)
  "Return left-side context segments for ROLE in the current buffer."
  (let* ((role (or role (bv-modeline--buffer-role)))
         (mode (and (bv-modeline--role-policy role :mode t)
                    (bv-modeline--mode-name)))
         (project-info (and (bv-modeline--role-policy role :project t)
                            (bv-modeline--project-info)))
         (project (plist-get project-info :name))
         (project-root (plist-get project-info :root))
         (branch-info (and (bv-modeline--role-policy role :branch t)
                           (bv-modeline--vc-branch-info)))
         (branch (plist-get branch-info :branch))
         (branch (unless (bv-modeline--same-label-p project branch) branch)))
    (delq nil
          (list
           (bv-modeline--segment 'project project 'bv-ui-header-salient
                                 :priority 60
                                 :min-width 4
                                 :truncate 'middle
                                 :help-echo (when project-root
                                              (format "Project: %s\nRoot: %s"
                                                      project project-root)))
           (bv-modeline--segment 'branch branch 'bv-ui-header-muted
                                 :priority 45
                                 :min-width 4
                                 :truncate 'middle
                                 :help-echo
                                 (when branch
                                   (format "Branch: %s%s"
                                           branch
                                           (if-let ((backend
                                                     (plist-get branch-info
                                                                :backend)))
                                               (format "\nBackend: %s" backend)
                                             ""))))
           (bv-modeline--segment 'mode mode 'bv-ui-header-muted
                                 :priority 80
                                 :min-width 3)))))

(defun bv-modeline--plain-bound-string (symbol)
  "Return SYMBOL's string value when it is bound and non-empty."
  (when (and (boundp symbol)
             (stringp (symbol-value symbol)))
    (let ((value (bv-modeline--clean (symbol-value symbol))))
      (unless (string-empty-p value)
        value))))

(defun bv-modeline--position ()
  "Return point position as line and zero-based column."
  (format "%d:%d" (line-number-at-pos) (current-column)))

(defun bv-modeline--time-text ()
  "Return wall-clock text for the header line."
  (when bv-modeline-show-time
    (or (bv-modeline--plain-bound-string 'display-time-string)
        (format-time-string "%H:%M"))))

(defun bv-modeline--keycast-text ()
  "Return current keycast text when BV keycast mode is active."
  (when (and (bound-and-true-p bv-keycast-mode)
             (boundp 'keycast--this-command-keys)
             (boundp 'keycast--this-command)
             keycast--this-command-keys
             keycast--this-command)
    (format "%s %s"
            (key-description keycast--this-command-keys)
            (symbol-name keycast--this-command))))

(defun bv-modeline--eglot-label ()
  "Return a compact Eglot label when the current buffer is managed."
  (when (and (fboundp 'eglot-managed-p)
             (ignore-errors (eglot-managed-p)))
    "Eglot"))

(defun bv-modeline--flymake-summary ()
  "Return Flymake diagnostic summary for the current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (let ((errors 0)
          (warnings 0)
          (infos 0)
          (checking (and (fboundp 'flymake-running-backends)
                         (ignore-errors (flymake-running-backends))))
          (sample nil))
      (dolist (diag (ignore-errors
                      (flymake-diagnostics (point-min) (point-max))))
        (pcase (and (fboundp 'flymake-diagnostic-type)
                    (flymake-diagnostic-type diag))
          (:error (cl-incf errors))
          (:warning (cl-incf warnings))
          (_ (cl-incf infos)))
        (unless sample
          (setq sample (and (fboundp 'flymake-diagnostic-text)
                            (ignore-errors
                              (flymake-diagnostic-text diag))))))
      (list :errors errors
            :warnings warnings
            :infos infos
            :checking checking
            :eglot (bv-modeline--eglot-label)
            :sample sample))))

(defun bv-modeline--diagnostics-text (summary)
  "Return compact diagnostics text for SUMMARY."
  (let ((parts nil))
    (when (> (plist-get summary :errors) 0)
      (push (format "E%d" (plist-get summary :errors)) parts))
    (when (> (plist-get summary :warnings) 0)
      (push (format "W%d" (plist-get summary :warnings)) parts))
    (when (> (plist-get summary :infos) 0)
      (push (format "I%d" (plist-get summary :infos)) parts))
    (when (and (null parts) (plist-get summary :checking))
      (push "CHK" parts))
    (string-join (nreverse parts) " ")))

(defun bv-modeline--diagnostics-help (summary)
  "Return hover help for diagnostic SUMMARY."
  (string-join
   (delq nil
         (list
          (format "Diagnostics: %d error, %d warning, %d info"
                  (plist-get summary :errors)
                  (plist-get summary :warnings)
                  (plist-get summary :infos))
          (when (plist-get summary :checking)
            "Flymake is checking")
          (when-let ((eglot (plist-get summary :eglot)))
            (format "LSP: %s" eglot))
          (when-let ((sample (plist-get summary :sample)))
            (format "First diagnostic: %s" sample))))
   "\n"))

(defun bv-modeline--diagnostics-face (summary)
  "Return face for diagnostic SUMMARY."
  (cond ((> (plist-get summary :errors) 0) 'bv-ui-header-error)
        ((> (plist-get summary :warnings) 0) 'bv-ui-header-warning)
        ((> (plist-get summary :infos) 0) 'bv-ui-header-info)
        (t 'bv-ui-header-muted)))

(defun bv-modeline--diagnostics-segment (&optional role)
  "Return diagnostics segment for ROLE, when policy allows it."
  (let* ((role (or role (bv-modeline--buffer-role)))
         (policy (bv-modeline--role-policy role :diagnostics 'all))
         (summary (and policy (bv-modeline--flymake-summary)))
         (errors (and summary (plist-get summary :errors)))
         (warnings (and summary (plist-get summary :warnings)))
         (infos (and summary (plist-get summary :infos)))
         (checking (and summary (plist-get summary :checking)))
         (bad (or (and errors (> errors 0))
                  (and warnings (> warnings 0))))
         (visible (and summary
                       (not (eq policy nil))
                       (or bad
                           (and (not (eq policy 'bad))
                                (or (and infos (> infos 0))
                                    checking))))))
    (when visible
      (bv-modeline--segment
       'diagnostics (bv-modeline--diagnostics-text summary)
       (bv-modeline--diagnostics-face summary)
       :priority (if bad 98 86)
       :required bad
       :min-width 3
       :fixed-width 4
       :align 'right
       :truncate 'right
       :help-echo (bv-modeline--diagnostics-help summary)))))

(defun bv-modeline--region-state ()
  "Return active region metadata, or nil."
  (when (and (bv-modeline--selected-window-p)
             (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (chars (- end beg))
           (lines (max 1 (count-lines beg end))))
      (list :label "SEL"
            :help (format "Active region: %d chars, %d lines"
                          chars lines)))))

(defun bv-modeline--envrc-state ()
  "Return envrc state metadata, or nil."
  (when (and (boundp 'envrc--status)
             (not (eq envrc--status 'none)))
    (pcase envrc--status
      ('on (list :label "ENV" :face 'bv-ui-header-info
                 :help "envrc: loaded"))
      (_ (list :label "ENV!" :face 'bv-ui-header-error
               :help (format "envrc: %s" envrc--status))))))

(defun bv-modeline--task-state-items ()
  "Return active task/state items for the current buffer."
  (let ((remote (bv-modeline--remote-info))
        (items nil))
    (when defining-kbd-macro
      (push (list :label "REC" :face 'bv-ui-header-error
                  :help "Recording keyboard macro")
            items))
    (when executing-kbd-macro
      (push (list :label "MAC" :face 'bv-ui-header-warning
                  :help "Executing keyboard macro")
            items))
    (when (> (recursion-depth) 1)
      (push (list :label (format "R%d" (recursion-depth))
                  :face 'bv-ui-header-warning
                  :help "Recursive edit is active")
            items))
    (when (buffer-narrowed-p)
      (push (list :label "NAR" :face 'bv-ui-header-warning
                  :help "Buffer is narrowed")
            items))
    (when-let ((region (bv-modeline--region-state)))
      (push (plist-put region :face 'bv-ui-header-info) items))
    (when remote
      (push (list :label (or (bv-modeline--remote-label remote) "TRAMP")
                  :face (if (bv-modeline--elevated-p remote)
                            'bv-ui-header-error
                          'bv-ui-header-warning)
                  :help (bv-modeline--remote-help remote))
            items))
    (when (and (bound-and-true-p bv-format-on-save-mode)
               (buffer-modified-p))
      (push (list :label "FMT" :face 'bv-ui-header-info
                  :help "Format on save is pending")
            items))
    (when (and (boundp 'compilation-in-progress)
               compilation-in-progress)
      (push (list :label "COMP" :face 'bv-ui-header-info
                  :help "Compilation is running")
            items))
    (when-let ((envrc (bv-modeline--envrc-state)))
      (push envrc items))
    (nreverse items)))

(defun bv-modeline--task-state-face (items)
  "Return the highest-signal face for task ITEMS."
  (cond ((cl-some (lambda (item)
                    (eq (plist-get item :face) 'bv-ui-header-error))
                  items)
         'bv-ui-header-error)
        ((cl-some (lambda (item)
                    (eq (plist-get item :face) 'bv-ui-header-warning))
                  items)
         'bv-ui-header-warning)
        (t 'bv-ui-header-info)))

(defun bv-modeline--task-state-segment ()
  "Return compact active task/state segment."
  (when-let ((items (bv-modeline--task-state-items)))
    (bv-modeline--segment
     'state
     (string-join (mapcar (lambda (item)
                            (plist-get item :label))
                          items)
                  " ")
     (bv-modeline--task-state-face items)
     :priority 96
     :min-width 3
     :truncate 'right
     :help-echo (string-join
                 (delq nil
                       (mapcar (lambda (item)
                                 (plist-get item :help))
                               items))
                 "\n"))))

(defun bv-modeline--right-segments (&optional extra role)
  "Return right-side segments for ROLE, optionally including EXTRA."
  (let* ((role (or role (bv-modeline--buffer-role)))
         (minimal (eq bv-modeline-detail-level 'minimal))
         (decorative (and (not minimal)
                          (bv-modeline--role-policy role :decorative t)))
         (show-position (bv-modeline--role-policy role :position t))
        (clock (bv-modeline--plain-bound-string 'org-mode-line-string))
        (keycast (bv-modeline--keycast-text))
        (emms (when (bound-and-true-p emms-player-playing-p)
                (bv-modeline--plain-bound-string 'emms-mode-line-string)))
        (position (bv-modeline--position))
        (battery (bv-modeline--plain-bound-string 'battery-mode-line-string))
        (time (bv-modeline--time-text)))
    (delq nil
          (append
           (list (bv-modeline--task-state-segment)
                 (bv-modeline--diagnostics-segment role))
           extra
           (when decorative
             (list
              (bv-modeline--segment 'keycast keycast 'bv-ui-header-salient
                                    :priority 85
                                    :min-width 8
                                    :truncate 'middle
                                    :help-echo keycast)
              (bv-modeline--segment 'clock clock 'bv-ui-header-salient
                                    :priority 90
                                    :min-width 5
                                    :truncate 'middle
                                    :help-echo "Org clock")
              (bv-modeline--segment 'emms emms 'bv-ui-header-muted
                                    :priority 25
                                    :min-width 8
                                    :truncate 'middle
                                    :help-echo emms)))
           (when show-position
             (list
              (bv-modeline--segment 'position position 'bv-ui-header-default
                                    :priority 100
                                    :required t
                                    :min-width 3
                                    :fixed-width 7
                                    :align 'right
                                    :truncate 'left
                                    :help-echo "Line:column")))
           (list
            (bv-modeline--segment 'time time 'bv-ui-header-default
                                  :priority 99
                                  :required t
                                  :min-width 5
                                  :fixed-width 5
                                  :truncate 'right
                                  :help-echo "Wall-clock time"))
           (when decorative
             (list
              (bv-modeline--segment 'battery battery 'bv-ui-header-muted
                                    :priority 45
                                    :min-width 5
                                    :truncate 'right
                                    :help-echo battery)))))))

(defun bv-modeline--right-budget (content-width)
  "Return the preferred right-side budget within CONTENT-WIDTH."
  (cond
   ((<= content-width 0) 0)
   ((eq bv-modeline-detail-level 'full) content-width)
   (t
    (let* ((ratio-budget (floor (* (window-total-width)
                                   bv-modeline-right-max-ratio)))
           (title-reserve (min bv-modeline-min-title-width
                               (max 0 (- content-width 1)))))
      (max 0 (min content-width
                  (max ratio-budget
                       (- content-width title-reserve))))))))

(defun bv-modeline-compose (status title context-segments right-segments
                                   &optional title-truncate title-help)
  "Compose a responsive BV header line.
STATUS is rendered as the left accent block.  TITLE is the primary buffer
identity.  CONTEXT-SEGMENTS and RIGHT-SEGMENTS are lists made with
`bv-modeline--segment'.  TITLE-TRUNCATE controls title shortening, and
TITLE-HELP is exposed via hover help."
  (let* ((width (max 1 (window-total-width)))
         (selected (bv-modeline--selected-window-p))
         (edge-right (bv-modeline--edge-pad t selected))
         (edge-width (string-width edge-right))
         (status-block (bv-modeline--status-block status selected))
         (status-gutter (bv-modeline--status-gutter selected))
         (status-width (string-width status-block))
         (status-gutter-width (string-width status-gutter))
         (content-width (max 0 (- width edge-width
                                  status-width status-gutter-width)))
         (title-face (if selected
                         'bv-ui-header-strong
                       'bv-ui-header-muted))
         (title-segment (bv-modeline--segment
                         'title title title-face
                         :priority 100
                         :required t
                         :min-width 4
                         :truncate (or title-truncate 'middle)
                         :help-echo title-help))
         (left-segments (bv-modeline--window-segments
                         (delq nil (cons title-segment context-segments))
                         selected))
         (right-segments (bv-modeline--window-segments
                          (delq nil right-segments)
                          selected))
         (right-separator bv-modeline--right-separator)
         (left-separator bv-modeline--left-separator)
         (separator-face (bv-modeline--window-face
                          'bv-ui-header-muted selected))
         (gap-face (bv-modeline--window-face
                    'bv-ui-header-default selected))
         (right-budget (bv-modeline--right-render-budget
                        content-width left-segments
                        right-segments right-separator))
         (right-fitted (bv-modeline--fit-segments
                        right-segments right-budget right-separator))
         (right (bv-modeline--join-segments
                 right-fitted right-separator separator-face))
         (right-width (string-width right))
         (initial-gap-reserve (if (> right-width 0)
                          bv-modeline--side-gap-min-width
                        0))
         (left-budget (max 0 (- content-width right-width
                                 initial-gap-reserve)))
         (left-fitted (bv-modeline--fit-segments
                       left-segments left-budget left-separator))
         (left (bv-modeline--join-segments
                left-fitted left-separator separator-face))
         (left-width (string-width left))
         (gap-width (cond ((and (> left-width 0) (> right-width 0))
                           (max 0 (- content-width left-width right-width)))
                          ((> right-width 0)
                           (max 0 (- content-width right-width)))
                          (t
                           (max 0 (- content-width left-width)))))
         (gap (propertize (make-string gap-width ?\s)
                          'face gap-face)))
    (concat status-block status-gutter left gap right edge-right)))

(defun bv-modeline-default-mode ()
  "Default header line for ordinary editing buffers."
  (let ((role (bv-modeline--buffer-role)))
    (bv-modeline-compose (bv-modeline-status)
                         (bv-modeline--title)
                         (bv-modeline--context-segments role)
                         (bv-modeline--right-segments nil role)
                         (bv-modeline--title-truncation)
                         (bv-modeline--title-help))))

(defun bv-modeline-org-agenda-mode-p ()
  "Return non-nil if current buffer is in `org-agenda-mode'."
  (derived-mode-p 'org-agenda-mode))

(defun bv-modeline-org-agenda-mode ()
  "Header line for `org-agenda-mode'."
  (bv-modeline-compose
   (bv-modeline-status)
   "Agenda"
   nil
   (bv-modeline--right-segments
    (list (bv-modeline--segment
           'date (format-time-string "%A %-e %B %Y") 'bv-ui-header-muted
           :priority 90
           :min-width 10
           :truncate 'right
           :help-echo "Agenda date"))
    'agenda)))

(defun bv-modeline-org-capture-mode-p ()
  "Return non-nil if current buffer is in `org-capture-mode'."
  (bound-and-true-p org-capture-mode))

(defun bv-modeline-org-capture-mode ()
  "Header line for `org-capture-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Capture"
                       (list (bv-modeline--segment 'mode "Org" 'bv-ui-header-muted
                                                   :priority 80))
                       (bv-modeline--right-segments nil 'note)))

(defun bv-modeline-term-mode-p ()
  "Return non-nil if current buffer is in `term-mode'."
  (derived-mode-p 'term-mode))

(defun bv-modeline-vterm-mode-p ()
  "Return non-nil if current buffer is in `vterm-mode'."
  (derived-mode-p 'vterm-mode))

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of directory name DIR."
  (let ((dir (bv-modeline--clean (abbreviate-file-name dir))))
    (bv-modeline--truncate-string dir max-length 'left)))

(defun bv-modeline-term-mode ()
  "Header line for terminal modes (`term-mode' and `vterm-mode')."
  (bv-modeline-compose
   ">_"
   "Terminal"
   (list (bv-modeline--segment 'shell shell-file-name 'bv-ui-header-muted
                               :priority 80
                               :min-width 4
                               :truncate 'middle
                               :help-echo shell-file-name))
   (bv-modeline--right-segments
    (list (bv-modeline--segment
           'directory (shorten-directory default-directory 40)
           'bv-ui-header-muted
           :priority 85
           :min-width 8
           :truncate 'left
           :help-echo (abbreviate-file-name default-directory)))
    'terminal)))

(defun bv-modeline-message-mode-p ()
  "Return non-nil if current buffer is in `message-mode'."
  (derived-mode-p 'message-mode))

(defun bv-modeline-message-mode ()
  "Header line for `message-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Message"
                       (list (bv-modeline--segment 'state "Draft"
                                                   'bv-ui-header-muted
                                                   :priority 80))
                       (bv-modeline--right-segments nil 'writing)))

(defun bv-modeline-info-mode-p ()
  "Return non-nil if current buffer is in `Info-mode'."
  (derived-mode-p 'Info-mode))

(defun bv-modeline-info-mode ()
  "Header line for `Info-mode'."
  (bv-modeline-compose
   (bv-modeline-status)
   "Info"
   (list (bv-modeline--segment 'node Info-current-node
                               'bv-ui-header-muted
                               :priority 80
                               :min-width 4
                               :truncate 'middle
                               :help-echo Info-current-node))
   (bv-modeline--right-segments nil 'help)))

(defun bv-modeline-help-mode-p ()
  "Return non-nil if current buffer is a help buffer."
  (derived-mode-p 'help-mode 'helpful-mode))

(defun bv-modeline-help-mode ()
  "Header line for help buffers."
  (bv-modeline-compose
   (bv-modeline-status)
   (bv-modeline--clean (buffer-name))
   (list (bv-modeline--segment 'mode (bv-modeline--mode-name)
                               'bv-ui-header-muted
                               :priority 80
                               :min-width 4))
   (bv-modeline--right-segments nil 'help)
   'middle
   (bv-modeline--title-help)))

(defun bv-modeline-dired-mode-p ()
  "Return non-nil if current buffer is in `dired-mode'."
  (derived-mode-p 'dired-mode))

(defun bv-modeline-dired-mode ()
  "Header line for `dired-mode'."
  (bv-modeline-compose
   (bv-modeline-status)
   (file-name-nondirectory
    (directory-file-name (or default-directory "")))
   (bv-modeline--context-segments 'dired)
   (bv-modeline--right-segments
    (list (bv-modeline--segment 'directory
                                (abbreviate-file-name default-directory)
                                'bv-ui-header-muted
                                :priority 90
                                :min-width 8
                                :truncate 'left
                                :help-echo default-directory))
    'dired)
   'middle
   (format "Directory: %s" default-directory)))

(defun bv-modeline-pdf-mode-p ()
  "Return non-nil if current buffer displays a PDF or document page."
  (derived-mode-p 'pdf-view-mode 'doc-view-mode))

(defun bv-modeline-pdf-mode ()
  "Header line for PDF/document buffers."
  (bv-modeline-compose
   (bv-modeline-status)
   (bv-modeline--title)
   (bv-modeline--context-segments 'pdf)
   (bv-modeline--right-segments nil 'pdf)
   (bv-modeline--title-truncation)
   (bv-modeline--title-help)))

(defun bv-modeline-calendar-mode-p ()
  "Return non-nil if current buffer is in `calendar-mode'."
  (derived-mode-p 'calendar-mode))

(defun bv-modeline-calendar-mode ()
  "Empty header line for calendar."
  "")

(defun bv-modeline-completion-list-mode-p ()
  "Return non-nil if current buffer is in `completion-list-mode'."
  (derived-mode-p 'completion-list-mode))

(defun bv-modeline-completion-list-mode ()
  "Header line for `completion-list-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       (bv-modeline--clean (buffer-name))
                       nil
                       (bv-modeline--right-segments nil 'completion)
                       'middle
                       (format "Buffer: %s" (buffer-name))))

(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            (lambda ()
              (setq org-mode-line-string nil)
              (force-mode-line-update t))))

(defun bv-modeline--render ()
  "Render the current buffer with the first matching BV renderer."
  (let ((renderer
         (cl-loop for (predicate . candidate) in bv-modeline-renderers
                  when (funcall predicate)
                  return candidate)))
    (funcall (or renderer #'bv-modeline-default-mode))))

(defun bv-modeline ()
  "Install the BV header line."
  (interactive)
  (setq-default header-line-format '((:eval (bv-modeline--render)))))

(defun bv-modeline-update-windows ()
  "Modify the mode line depending on window configuration."
  (dolist (window (window-list))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (when (or (not (boundp 'no-mode-line)) (not no-mode-line))
          (set-window-parameter window 'mode-line-format
                                (cond ((not mode-line-format) 'none)
                                      ((one-window-p t 'visible) (list ""))
                                      ((eq (window-in-direction 'below)
                                           (minibuffer-window))
                                       (list ""))
                                      ((not (window-in-direction 'below))
                                       (list ""))
                                      (t 'none))))))))

(defun bv-modeline-cycle-detail ()
  "Cycle `bv-modeline-detail-level'."
  (interactive)
  (setq bv-modeline-detail-level
        (pcase bv-modeline-detail-level
          ('auto 'full)
          ('full 'minimal)
          (_ 'auto)))
  (force-mode-line-update t)
  (message "BV modeline detail: %s" bv-modeline-detail-level))

(add-hook 'window-configuration-change-hook 'bv-modeline-update-windows)

(setq eshell-status-in-modeline nil)
(setq-default mode-line-format nil)

(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line '(:inherit bv-ui-header-default :height 0.6)))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

(setq Info-use-header-line nil)

(provide 'bv-modeline)
;;; bv-modeline.el ends here
