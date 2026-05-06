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
(defvar display-time-string)
(defvar emms-mode-line-string)
(defvar emms-player-playing-p)
(defvar eshell-status-in-modeline)
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
  "Directory used for `bv-modeline--cached-project-name'.")

(defvar-local bv-modeline--cached-project-name nil
  "Cached project name for the current buffer.")

(defvar bv-modeline-renderers
  '((bv-modeline-message-mode-p . bv-modeline-message-mode)
    (bv-modeline-info-mode-p . bv-modeline-info-mode)
    (bv-modeline-calendar-mode-p . bv-modeline-calendar-mode)
    (bv-modeline-org-capture-mode-p . bv-modeline-org-capture-mode)
    (bv-modeline-org-agenda-mode-p . bv-modeline-org-agenda-mode)
    (bv-modeline-term-mode-p . bv-modeline-term-mode)
    (bv-modeline-vterm-mode-p . bv-modeline-term-mode)
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
  (string-width (plist-get segment :text)))

(defun bv-modeline--segment-render (segment)
  "Render SEGMENT."
  (let* ((width (or (plist-get segment :render-width)
                    (bv-modeline--segment-width segment)))
         (text (bv-modeline--truncate-string
                (plist-get segment :text)
                width
                (plist-get segment :truncate))))
    (propertize text 'face (plist-get segment :face))))

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

(defun bv-modeline--join-segments (segments separator)
  "Render SEGMENTS joined with SEPARATOR."
  (mapconcat #'bv-modeline--segment-render
             segments
             (propertize separator 'face 'bv-ui-header-muted)))

(defun bv-modeline--selected-window-p ()
  "Return non-nil when rendering the selected window."
  (if (fboundp 'mode-line-window-selected-p)
      (mode-line-window-selected-p)
    t))

(defun bv-modeline-status ()
  "Return buffer/window status: read-only, modified, or read-write."
  (cond ((window-dedicated-p) "--")
        ((and buffer-file-name (buffer-modified-p)) "**")
        (buffer-read-only "RO")
        (t "RW")))

(defun bv-modeline--status-face (status)
  "Return the header face used for STATUS."
  (cond ((string= status "**") 'bv-ui-header-critical)
        ((string= status "RO") 'bv-ui-header-popout)
        ((string= status "--") 'bv-ui-header-popout)
        ((string= status ">_") 'bv-ui-header-popout)
        (t 'bv-ui-header-muted)))

(defun bv-modeline--status-block (status)
  "Render STATUS as the left status block."
  (let* ((status (bv-modeline--clean status))
         (status (if (> (string-width status) 2)
                     (bv-modeline--truncate-string status 2)
                   status))
         (block (propertize (format " %-2s " status)
                            'face (bv-modeline--status-face status))))
    (add-text-properties 0 1
                         `(display (raise ,bv-modeline--edge-pad-rise))
                         block)
    block))

(defun bv-modeline--status-gutter ()
  "Return the neutral gutter after the status accent block."
  (propertize bv-modeline--status-gutter 'face 'bv-ui-header-default))

(defun bv-modeline--edge-pad (&optional drop)
  "Return a one-column edge pad that contributes vertical header metrics.
When DROP is non-nil, lower the pad instead of raising it.  The visible text
size stays unchanged; the paired pads restore the header band's previous
breathing room after the renderer rewrite."
  (propertize " " 'face 'bv-ui-header-default
              'display `(raise ,(if drop
                                     bv-modeline--edge-pad-drop
                                   bv-modeline--edge-pad-rise))))

(defun bv-modeline--mode-name ()
  "Return the current major mode name as plain text."
  (bv-modeline--clean
   (cond ((stringp mode-name) mode-name)
         ((listp mode-name) (format-mode-line mode-name))
         (t mode-name))))

(defun bv-modeline--vc-branch ()
  "Return the current VC branch as plain text."
  (when (and buffer-file-name
             (boundp 'vc-mode)
             vc-mode)
    (let ((branch (replace-regexp-in-string
                   "\\`[[:space:]]*[^:]+:" ""
                   (bv-modeline--clean vc-mode))))
      (unless (string-empty-p branch)
        branch))))

(defun bv-modeline--project-context-p ()
  "Return non-nil when project context belongs in the header line."
  (or buffer-file-name
      (derived-mode-p 'dired-mode 'vc-dir-mode)))

(defun bv-modeline--project-name ()
  "Return current project name when it is cheap and useful."
  (let ((directory default-directory))
    (unless (or (not (bv-modeline--project-context-p))
                (file-remote-p directory)
                (not (fboundp 'project-current)))
      (unless (equal directory bv-modeline--cached-project-directory)
        (setq bv-modeline--cached-project-directory directory
              bv-modeline--cached-project-name
              (when-let ((project (ignore-errors (project-current nil))))
                (bv-modeline--clean
                 (if (fboundp 'project-name)
                     (project-name project)
                   (file-name-nondirectory
                    (directory-file-name (project-root project))))))))
      bv-modeline--cached-project-name)))

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

(defun bv-modeline--title-truncation ()
  "Return preferred truncation style for the current buffer title."
  (if (bv-modeline--org-title) 'right 'middle))

(defun bv-modeline--same-label-p (left right)
  "Return non-nil when LEFT and RIGHT are the same non-empty label."
  (and (not (bv-modeline--blank-p left))
       (not (bv-modeline--blank-p right))
       (string= (downcase (bv-modeline--clean left))
                (downcase (bv-modeline--clean right)))))

(defun bv-modeline--context-segments ()
  "Return left-side context segments for the current buffer."
  (let* ((mode (bv-modeline--mode-name))
         (project (bv-modeline--project-name))
         (branch (bv-modeline--vc-branch))
         (branch (unless (bv-modeline--same-label-p project branch) branch)))
    (delq nil
          (list
           (bv-modeline--segment 'project project 'bv-ui-header-salient
                                 :priority 60
                                 :min-width 4
                                 :truncate 'middle)
           (bv-modeline--segment 'branch branch 'bv-ui-header-muted
                                 :priority 45
                                 :min-width 4
                                 :truncate 'middle)
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

(defun bv-modeline--right-segments (&optional extra)
  "Return right-side segments, optionally preceded by EXTRA."
  (let ((minimal (eq bv-modeline-detail-level 'minimal))
        (clock (bv-modeline--plain-bound-string 'org-mode-line-string))
        (keycast (bv-modeline--keycast-text))
        (emms (when (bound-and-true-p emms-player-playing-p)
                (bv-modeline--plain-bound-string 'emms-mode-line-string)))
        (position (bv-modeline--position))
        (battery (bv-modeline--plain-bound-string 'battery-mode-line-string))
        (time (when (and (boundp 'display-time-mode)
                         display-time-mode)
                (bv-modeline--plain-bound-string 'display-time-string))))
    (delq nil
          (append
           extra
           (unless minimal
             (list
              (bv-modeline--segment 'keycast keycast 'bv-ui-header-salient
                                    :priority 85
                                    :min-width 8
                                    :truncate 'middle)
              (bv-modeline--segment 'clock clock 'bv-ui-header-salient
                                    :priority 90
                                    :min-width 5
                                    :truncate 'middle)
              (bv-modeline--segment 'emms emms 'bv-ui-header-muted
                                    :priority 25
                                    :min-width 8
                                    :truncate 'middle)))
           (list
            (bv-modeline--segment 'position position 'bv-ui-header-default
                                  :priority 100
                                  :required t
                                  :min-width 3
                                  :truncate 'left))
           (unless minimal
             (list
              (bv-modeline--segment 'battery battery 'bv-ui-header-muted
                                    :priority 45
                                    :min-width 5
                                    :truncate 'right)
              (bv-modeline--segment 'time time 'bv-ui-header-default
                                    :priority 92
                                    :min-width 5
                                    :truncate 'right)))))))

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
                                   &optional title-truncate)
  "Compose a responsive BV header line.
STATUS is rendered as the left accent block.  TITLE is the primary buffer
identity.  CONTEXT-SEGMENTS and RIGHT-SEGMENTS are lists made with
`bv-modeline--segment'.  TITLE-TRUNCATE controls title shortening."
  (let* ((width (max 1 (window-total-width)))
         (edge-right (bv-modeline--edge-pad t))
         (edge-width (string-width edge-right))
         (status-block (bv-modeline--status-block status))
         (status-gutter (bv-modeline--status-gutter))
         (status-width (string-width status-block))
         (status-gutter-width (string-width status-gutter))
         (content-width (max 0 (- width edge-width
                                  status-width status-gutter-width)))
         (selected (bv-modeline--selected-window-p))
         (title-face (if selected
                         'bv-ui-header-strong
                       'bv-ui-header-muted))
         (title-segment (bv-modeline--segment
                         'title title title-face
                         :priority 100
                         :required t
                         :min-width 4
                         :truncate (or title-truncate 'middle)))
         (left-segments (delq nil (cons title-segment context-segments)))
         (right-separator bv-modeline--right-separator)
         (left-separator bv-modeline--left-separator)
         (right-budget (bv-modeline--right-render-budget
                        content-width left-segments
                        right-segments right-separator))
         (right-fitted (bv-modeline--fit-segments
                        right-segments right-budget right-separator))
         (right (bv-modeline--join-segments right-fitted right-separator))
         (right-width (string-width right))
         (initial-gap-reserve (if (> right-width 0)
                          bv-modeline--side-gap-min-width
                        0))
         (left-budget (max 0 (- content-width right-width
                                 initial-gap-reserve)))
         (left-fitted (bv-modeline--fit-segments
                       left-segments left-budget left-separator))
         (left (bv-modeline--join-segments left-fitted left-separator))
         (left-width (string-width left))
         (gap-width (cond ((and (> left-width 0) (> right-width 0))
                           (max 0 (- content-width left-width right-width)))
                          ((> right-width 0)
                           (max 0 (- content-width right-width)))
                          (t
                           (max 0 (- content-width left-width)))))
         (gap (propertize (make-string gap-width ?\s)
                          'face 'bv-ui-header-default)))
    (concat status-block status-gutter left gap right edge-right)))

(defun bv-modeline-default-mode ()
  "Default header line for ordinary editing buffers."
  (bv-modeline-compose (bv-modeline-status)
                       (bv-modeline--title)
                       (bv-modeline--context-segments)
                       (bv-modeline--right-segments)
                       (bv-modeline--title-truncation)))

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
           :truncate 'right)))))

(defun bv-modeline-org-capture-mode-p ()
  "Return non-nil if current buffer is in `org-capture-mode'."
  (bound-and-true-p org-capture-mode))

(defun bv-modeline-org-capture-mode ()
  "Header line for `org-capture-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Capture"
                       (list (bv-modeline--segment 'mode "Org" 'bv-ui-header-muted
                                                   :priority 80))
                       (bv-modeline--right-segments)))

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
                               :truncate 'middle))
   (bv-modeline--right-segments
    (list (bv-modeline--segment
           'directory (shorten-directory default-directory 40)
           'bv-ui-header-muted
           :priority 85
           :min-width 8
           :truncate 'left)))))

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
                       (bv-modeline--right-segments)))

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
                               :truncate 'middle))
   (bv-modeline--right-segments)))

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
                       (bv-modeline--right-segments)))

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
