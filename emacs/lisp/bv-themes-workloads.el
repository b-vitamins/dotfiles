;;; bv-themes-workloads.el --- Workflow face probes for BV themes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; This library opens safe, deterministic Emacs buffers that resemble the
;; workflows covered by the theme adapters.  It records the faces made visible
;; by those buffers and checks them against the live theme inventory.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-themes-adapters)
(require 'bv-themes-compile)
(require 'bv-themes-inventory)

(defgroup bv-themes-workloads nil
  "Workflow probes for BV theme face coverage."
  :group 'bv-themes
  :prefix "bv-themes-workloads-")

(defcustom bv-themes-workloads-optional-features
  '(ansi-color apropos avy calendar cape citar comint company consult corfu
    cus-edit dape diff-mode dired ediff eldoc elfeed elfeed-search eglot embark
    eshell flycheck flymake flyspell help-mode helpful hi-lock ibuffer info
    magit markdown-mode marginalia message nerd-icons nerd-icons-completion
    nerd-icons-dired nerd-icons-ibuffer nov orderless org org-agenda org-modern
    package pulse rainbow-delimiters shr simple smartparens term tex-mode
    transient vertico vundo which-key whitespace xref)
  "Features loaded during workflow probes when available."
  :type '(repeat symbol)
  :group 'bv-themes-workloads)

(defconst bv-themes-workloads--text-properties
  '(face font-lock-face mouse-face)
  "Text and overlay properties whose values can contain faces.")

(defconst bv-themes-workloads--package-samples
  '((completion
     completions-common-part completions-first-difference
     completions-annotations completions-highlight completion-preview
     completion-preview-common completion-preview-exact)
    (search
     isearch lazy-highlight isearch-fail query-replace match
     isearch-group-1 isearch-group-2 anzu-mode-line anzu-match-1
     anzu-match-2 anzu-match-3 anzu-replace-to anzu-replace-highlight)
    (diagnostics
     flymake-error flymake-warning flymake-note
     flymake-end-of-line-diagnostics-face flycheck-error flycheck-warning
     flycheck-info flycheck-fringe-error flycheck-fringe-warning
     flycheck-fringe-info flyspell-duplicate flyspell-incorrect)
    (completion-ui
     vertico-current vertico-group-title vertico-group-separator
     vertico-multiline marginalia-documentation marginalia-key
     marginalia-file-name marginalia-date marginalia-size
     orderless-match-face-0 orderless-match-face-1 orderless-match-face-2
     orderless-match-face-3 corfu-current corfu-default corfu-bar
     corfu-border corfu-annotations company-tooltip
     company-tooltip-selection company-tooltip-common company-scrollbar-bg
     company-scrollbar-fg company-preview company-preview-common)
    (transient
     transient-heading transient-key transient-argument transient-value
     transient-inactive-argument transient-enabled-suffix
     transient-disabled-suffix transient-unreachable transient-mismatched-key)
    (version-control
     magit-section-heading magit-section-highlight magit-branch-local
     magit-branch-remote magit-branch-current magit-hash magit-tag
     magit-diff-file-heading magit-diff-hunk-heading magit-diff-added
     magit-diff-added-highlight magit-diff-removed
     magit-diff-removed-highlight magit-diff-context
     magit-diff-context-highlight magit-diff-lines-heading
     diff-hl-insert diff-hl-delete diff-hl-change git-gutter:added
     git-gutter:deleted git-gutter:modified)
    (org
     org-level-1 org-level-2 org-level-3 org-level-4 org-todo org-done
     org-tag org-date org-special-keyword org-block org-block-begin-line
     org-block-end-line org-code org-verbatim org-link org-table
     org-document-title org-agenda-date org-agenda-date-today
     org-agenda-structure org-scheduled org-upcoming-deadline
     org-warning org-modern-label org-modern-tag org-modern-todo)
    (dired
     dired-directory dired-header dired-ignored dired-flagged dired-mark
     dired-marked dired-perm-write dired-symlink dired-warning
     diredfl-dir-name diredfl-file-name diredfl-file-suffix
     diredfl-compressed-file-name diredfl-symlink)
    (terminal
     ansi-color-black ansi-color-red ansi-color-green ansi-color-yellow
     ansi-color-blue ansi-color-magenta ansi-color-cyan ansi-color-white
     ansi-color-bright-black ansi-color-bright-red
     ansi-color-bright-green ansi-color-bright-yellow
     ansi-color-bright-blue ansi-color-bright-magenta
     ansi-color-bright-cyan ansi-color-bright-white term-color-black
     term-color-red term-color-green term-color-yellow term-color-blue
     term-color-magenta term-color-cyan term-color-white)
    (help
     help-key-binding helpful-heading apropos-keybinding apropos-symbol
     Info-title-1 Info-title-2 Info-title-3 Info-title-4 info-header-node
     info-menu-header info-xref info-xref-visited shr-link shr-code)
    (mail-feed
     elfeed-search-title-face elfeed-search-unread-title-face
     elfeed-search-feed-face elfeed-search-date-face
     elfeed-search-tag-face elfeed-search-filter-face
     bv-elfeed-author-face bv-elfeed-star-face bv-elfeed-important-face
     bv-elfeed-high-score-face bv-elfeed-arxiv-face message-header-name
     message-header-subject message-header-to message-header-cc
     message-header-other message-cited-text-1 message-separator)
    (icons
     bv-icon-default bv-icon-muted bv-icon-file bv-icon-directory bv-icon-note
     bv-icon-code bv-icon-science bv-icon-idea bv-icon-proof bv-icon-review
     bv-icon-index bv-icon-system bv-icon-warning bv-icon-success
     bv-icon-special bv-icon-info bv-icon-salient nerd-icons-blue
     nerd-icons-cyan nerd-icons-green nerd-icons-red nerd-icons-yellow
     nerd-icons-purple nerd-icons-silver nerd-icons-completion-dir-face
     nerd-icons-dired-dir-face nerd-icons-ibuffer-icon-face)
    (debug
     dape-breakpoint-face dape-expression-face dape-log-face
     dape-repl-prompt-face breakpoint-enabled breakpoint-disabled)
    (navigation
     avy-lead-face avy-lead-face-0 avy-lead-face-1 avy-lead-face-2
     avy-background-face pulse-highlight-start-face pulse-highlight-face
     pulse-highlight-region-face highlight-symbol-face)
    (editing
     rainbow-delimiters-depth-1-face rainbow-delimiters-depth-2-face
     rainbow-delimiters-depth-3-face rainbow-delimiters-depth-4-face
     rainbow-delimiters-unmatched-face smartparens-global-mode
     sp-show-pair-match-face sp-show-pair-mismatch-face
     whitespace-space whitespace-tab whitespace-trailing whitespace-line)
    (misc
     which-key-key-face which-key-command-description-face
     which-key-group-description-face which-key-local-map-description-face
     tempel-field tempel-form vundo-node vundo-stem olivetti-fringe
     keycast-key keycast-command))
  "Representative package faces sampled when those faces are defined.")

(defun bv-themes-workloads--face< (left right)
  "Return non-nil when face LEFT sorts before face RIGHT."
  (string< (symbol-name left) (symbol-name right)))

(defun bv-themes-workloads--sort-faces (faces)
  "Return FACES sorted by symbol name."
  (sort (delete-dups (copy-sequence faces)) #'bv-themes-workloads--face<))

(defun bv-themes-workloads--normalize-face-value (value)
  "Return face symbols found in VALUE."
  (cond
   ((null value) nil)
   ((and (symbolp value) (facep value)) (list value))
   ((symbolp value) nil)
   ((and (consp value) (keywordp (car value))) nil)
   ((consp value)
    (cl-mapcan #'bv-themes-workloads--normalize-face-value value))
   (t nil)))

(defun bv-themes-workloads--faces-at (position)
  "Return face symbols at POSITION in the current buffer."
  (let (faces)
    (dolist (property bv-themes-workloads--text-properties)
      (setq faces
            (append (bv-themes-workloads--normalize-face-value
                     (get-text-property position property))
                    faces)))
    faces))

(defun bv-themes-workloads--overlay-faces ()
  "Return face symbols present in overlays in the current buffer."
  (let (faces)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (dolist (property bv-themes-workloads--text-properties)
        (setq faces
              (append (bv-themes-workloads--normalize-face-value
                       (overlay-get overlay property))
                      faces))))
    faces))

(defun bv-themes-workloads--observe-buffer ()
  "Return sorted face symbols visible in the current buffer."
  (ignore-errors
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure (point-min) (point-max))))
  (let ((position (point-min))
        faces)
    (while (< position (point-max))
      (setq faces (append (bv-themes-workloads--faces-at position) faces)
            position (or (next-property-change position nil (point-max))
                         (point-max))))
    (bv-themes-workloads--sort-faces
     (append faces (bv-themes-workloads--overlay-faces)))))

(defun bv-themes-workloads--require (feature)
  "Require FEATURE without signaling optional package failures."
  (condition-case err
      (if (require feature nil t)
          (list :feature feature :status 'loaded)
        (list :feature feature :status 'missing))
    (error
     (list :feature feature
           :status 'failed
           :message (error-message-string err)))))

(defun bv-themes-workloads--call-mode (mode)
  "Call MODE with mode hooks delayed when available."
  (when (fboundp mode)
    (delay-mode-hooks
      (funcall mode))))

(defun bv-themes-workloads--buffer-probe (name feature mode contents)
  "Return a workflow probe named NAME for FEATURE, MODE, and CONTENTS."
  (let ((load (and feature (bv-themes-workloads--require feature))))
    (if (and load (not (eq (plist-get load :status) 'loaded)))
        (list :name name :status 'skipped :load load :faces nil)
      (with-current-buffer (generate-new-buffer
                            (format " *bv-theme-workload-%s*" name))
        (unwind-protect
            (progn
              (insert contents)
              (goto-char (point-min))
              (bv-themes-workloads--call-mode mode)
              (list :name name
                    :status 'ok
                    :mode major-mode
                    :load load
                    :faces (bv-themes-workloads--observe-buffer)))
          (kill-buffer (current-buffer)))))))

(defun bv-themes-workloads--sample-probe ()
  "Return a probe that materializes defined package sample faces."
  (with-current-buffer (generate-new-buffer " *bv-theme-workload-samples*")
    (unwind-protect
        (let (faces)
          (dolist (group bv-themes-workloads--package-samples)
            (insert (format "%s\n" (car group)))
            (dolist (face (cdr group))
              (when (facep face)
                (push face faces)
                (insert (propertize (format "  %S\n" face) 'face face)))))
          (special-mode)
          (list :name 'package-samples
                :status 'ok
                :mode major-mode
                :faces (bv-themes-workloads--sort-faces faces)))
      (kill-buffer (current-buffer)))))

(defun bv-themes-workloads--dired-probe ()
  "Return a probe for Dired faces."
  (let ((load (bv-themes-workloads--require 'dired)))
    (if (not (eq (plist-get load :status) 'loaded))
        (list :name 'dired :status 'skipped :load load :faces nil)
      (let ((buffer (dired-noselect default-directory)))
        (unwind-protect
            (with-current-buffer buffer
              (list :name 'dired
                    :status 'ok
                    :mode major-mode
                    :load load
                    :faces (bv-themes-workloads--observe-buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(defun bv-themes-workloads--widget-probe ()
  "Return a probe for widget and custom faces."
  (let ((load (bv-themes-workloads--require 'wid-edit)))
    (if (not (eq (plist-get load :status) 'loaded))
        (list :name 'widgets :status 'skipped :load load :faces nil)
      (with-current-buffer (generate-new-buffer " *bv-theme-workload-widgets*")
        (unwind-protect
            (progn
              (widget-insert "Options\n")
              (widget-create 'checkbox t)
              (widget-insert " Enable generated variant\n")
              (widget-create 'push-button :notify #'ignore "Apply")
              (widget-setup)
              (list :name 'widgets
                    :status 'ok
                    :mode major-mode
                    :load load
                    :faces (bv-themes-workloads--observe-buffer)))
          (kill-buffer (current-buffer)))))))

(defun bv-themes-workloads--completion-probe ()
  "Return a probe for completion-list faces."
  (with-current-buffer (generate-new-buffer " *bv-theme-workload-completions*")
    (unwind-protect
        (progn
          (insert (propertize "alpha" 'face 'completions-common-part)
                  "  "
                  (propertize "alphabet" 'face 'completions-first-difference)
                  "\n"
                  (propertize "annotation" 'face 'completions-annotations)
                  "\n")
          (completion-list-mode)
          (list :name 'completion-list
                :status 'ok
                :mode major-mode
                :faces (bv-themes-workloads--observe-buffer)))
      (kill-buffer (current-buffer)))))

(defun bv-themes-workloads--optional-feature-loads ()
  "Load optional workflow features and return load results."
  (mapcar #'bv-themes-workloads--require
          bv-themes-workloads-optional-features))

(defun bv-themes-workloads--mode-probes ()
  "Return deterministic major-mode workflow probes."
  (list
   (bv-themes-workloads--buffer-probe
    'emacs-lisp 'elisp-mode 'emacs-lisp-mode
    ";;; sample.el\n\n(defcustom sample-count 3\n  \"Example option.\"\n  :type 'integer)\n\n(defun sample-run (items)\n  \"Visit ITEMS and report a status.\"\n  (let ((total 0))\n    (dolist (item items)\n      (cl-incf total item))\n    (message \"total=%s\" total)))\n")
   (bv-themes-workloads--buffer-probe
    'org 'org 'org-mode
    "#+title: Theme Workload\n\n* TODO Calibrate visual priority :work:\nSCHEDULED: <2026-05-06 Wed>\n:PROPERTIES:\n:Owner: BV\n:END:\n\nParagraph with *strong*, /emphasis/, ~code~, =verbatim=, and [[https://example.invalid][link]].\n\n#+begin_src emacs-lisp\n(message \"inside a block\")\n#+end_src\n\n#+begin_quote\nA quoted passage with calmer contrast.\n#+end_quote\n\n| state | count |\n|-------+-------|\n| open  |     3 |\n")
   (bv-themes-workloads--buffer-probe
    'org-agenda 'org-agenda 'org-agenda-mode
    "Day-agenda (W18):\n  work:       Scheduled:  TODO Calibrate visual priority\n  notes:      Deadline:   NEXT Review artifacts\n")
   (bv-themes-workloads--buffer-probe
    'diff 'diff-mode 'diff-mode
    "diff --git a/sample.el b/sample.el\nindex 1111111..2222222 100644\n--- a/sample.el\n+++ b/sample.el\n@@ -1,4 +1,4 @@\n-(message \"old\")\n+(message \"new\")\n context line\n")
   (bv-themes-workloads--buffer-probe
    'compilation 'compile 'compilation-mode
    "sample.el:12:3: warning: unused binding\nsample.el:18:1: error: missing close paren\nCompilation exited abnormally with code 1\n")
   (bv-themes-workloads--buffer-probe
    'shell 'shell 'shell-mode
    "$ printf '%s\\n' sample\nsample\n$ exit\n")
   (bv-themes-workloads--buffer-probe
    'eshell 'esh-mode 'eshell-mode
    "~ $ git status --short\n M emacs/lisp/bv-themes.el\n")
   (bv-themes-workloads--buffer-probe
    'term 'term 'term-mode
    "\033[31mred\033[0m \033[32mgreen\033[0m \033[34mblue\033[0m\n")
   (bv-themes-workloads--buffer-probe
   'help 'help-mode 'help-mode
    "Help buffer\n\nKey binding: C-c t\n\nSee also `bv-themes-load-theme'.\n")
   (bv-themes-workloads--buffer-probe
    'info 'info 'Info-mode
    "File: sample.info,  Node: Top,  Next: Usage,  Up: (dir)\n\n* Menu:\n* Usage::\n\nThis is an Info node with a cross reference.\n")
   (bv-themes-workloads--buffer-probe
    'message 'message 'message-mode
    "From: theme@example.invalid\nTo: user@example.invalid\nSubject: Sample\n\n> quoted text\nplain text\n")
   (bv-themes-workloads--buffer-probe
    'latex 'tex-mode 'latex-mode
    "\\section{Sample}\n\\textbf{Important} $x^2 + y^2 = z^2$\n\\begin{verbatim}\ncode\n\\end{verbatim}\n")
   (bv-themes-workloads--buffer-probe
    'markdown 'markdown-mode 'markdown-mode
    "# Heading\n\n- item with `code` and **strong** text\n\n```elisp\n(message \"sample\")\n```\n")
   (bv-themes-workloads--buffer-probe
    'c 'cc-mode 'c-mode
    "#include <stdio.h>\n\nint main(void) {\n  printf(\"sample\\n\");\n  return 0;\n}\n")
   (bv-themes-workloads--buffer-probe
    'python 'python 'python-mode
    "class Sample:\n    def run(self, value: int) -> str:\n        return f'value={value}'\n")
   (bv-themes-workloads--buffer-probe
    'javascript 'js 'js-mode
    "export function sample(value) {\n  return `${value}`;\n}\n")))

(defun bv-themes-workloads-run ()
  "Run deterministic workflow probes and return a report plist."
  (interactive)
  (let* ((faces-before (face-list))
         (exercise
          (let ((inhibit-message t)
                (message-log-max nil))
            (list :loads (bv-themes-workloads--optional-feature-loads)
                  :probes (append
                           (bv-themes-workloads--mode-probes)
                           (list (bv-themes-workloads--dired-probe)
                                 (bv-themes-workloads--widget-probe)
                                 (bv-themes-workloads--completion-probe)
                                 (bv-themes-workloads--sample-probe))))))
         (loads (plist-get exercise :loads))
         (probes (plist-get exercise :probes))
         (faces-after (face-list))
         (new-faces (bv-themes-workloads--sort-faces
                     (cl-set-difference faces-after faces-before :test #'eq)))
         (observed (bv-themes-workloads--sort-faces
                    (cl-mapcan (lambda (probe)
                                 (copy-sequence (plist-get probe :faces)))
                               probes)))
         (loaded (cl-count-if
                  (lambda (entry) (eq (plist-get entry :status) 'loaded))
                  loads))
         (missing (cl-count-if
                   (lambda (entry) (eq (plist-get entry :status) 'missing))
                   loads))
         (failed (cl-remove-if-not
                  (lambda (entry) (eq (plist-get entry :status) 'failed))
                  loads))
         (report (list :loads loads
                       :loaded loaded
                       :missing missing
                       :failed failed
                       :probes probes
                       :ok-probes (cl-count-if
                                   (lambda (entry)
                                     (eq (plist-get entry :status) 'ok))
                                   probes)
                       :skipped-probes (cl-count-if
                                        (lambda (entry)
                                          (eq (plist-get entry :status)
                                              'skipped))
                                        probes)
                       :faces-before (length faces-before)
                       :faces-after (length faces-after)
                       :new-faces new-faces
                       :observed-faces observed)))
    (when (called-interactively-p 'interactive)
      (bv-themes-workloads-report report))
    report))

(defun bv-themes-workloads-inventory (&optional theme report)
  "Return inventory for observed workflow faces for THEME and REPORT."
  (let* ((theme (or theme
                    (caar bv-themes-token-profiles)
                    (error "No BV theme profile is registered")))
         (report (or report (bv-themes-workloads-run)))
         (artifact (bv-themes-compile theme))
         (observed (plist-get report :observed-faces)))
    (bv-themes-inventory-scan artifact observed)))

(defun bv-themes-workloads-assert (&optional theme report)
  "Signal an error if workflow REPORT exposes uncovered faces for THEME."
  (let* ((report (or report (bv-themes-workloads-run)))
         (failed (plist-get report :failed))
         (inventory (bv-themes-workloads-inventory theme report)))
    (when failed
      (error "BV theme workflow probes failed to load %d features"
             (length failed)))
    (bv-themes-inventory-assert inventory)
    (list :workloads report :inventory inventory)))

(defun bv-themes-workloads--insert-list (title items formatter)
  "Insert TITLE and ITEMS into the current buffer using FORMATTER."
  (insert (format "* %s (%d)\n" title (length items)))
  (if items
      (dolist (item items)
        (insert (format "- %s\n" (funcall formatter item))))
    (insert "- none\n"))
  (insert "\n"))

(defun bv-themes-workloads-report (&optional report)
  "Display workflow probe REPORT."
  (interactive)
  (let ((report (or report (bv-themes-workloads-run)))
        (buffer (get-buffer-create "*BV Theme Workloads*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# BV Theme Workloads\n\n")
        (insert (format "- optional features loaded: %d\n"
                        (plist-get report :loaded)))
        (insert (format "- optional features missing: %d\n"
                        (plist-get report :missing)))
        (insert (format "- optional feature failures: %d\n"
                        (length (plist-get report :failed))))
        (insert (format "- probes ok: %d\n"
                        (plist-get report :ok-probes)))
        (insert (format "- probes skipped: %d\n"
                        (plist-get report :skipped-probes)))
        (insert (format "- faces before: %d\n"
                        (plist-get report :faces-before)))
        (insert (format "- faces after: %d\n"
                        (plist-get report :faces-after)))
        (insert (format "- new faces: %d\n"
                        (length (plist-get report :new-faces))))
        (insert (format "- observed faces: %d\n\n"
                        (length (plist-get report :observed-faces))))
        (bv-themes-workloads--insert-list
         "Probe Results"
         (plist-get report :probes)
         (lambda (probe)
           (format "%S status=%S mode=%S faces=%d"
                   (plist-get probe :name)
                   (plist-get probe :status)
                   (plist-get probe :mode)
                   (length (plist-get probe :faces)))))
        (bv-themes-workloads--insert-list
         "Feature Failures"
         (plist-get report :failed)
         (lambda (entry)
           (format "%S %s"
                   (plist-get entry :feature)
                   (plist-get entry :message))))
        (bv-themes-workloads--insert-list
         "New Faces"
         (plist-get report :new-faces)
         (lambda (face) (format "%S" face)))
        (bv-themes-workloads--insert-list
         "Observed Faces"
         (plist-get report :observed-faces)
         (lambda (face) (format "%S" face)))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(provide 'bv-themes-workloads)
;;; bv-themes-workloads.el ends here
