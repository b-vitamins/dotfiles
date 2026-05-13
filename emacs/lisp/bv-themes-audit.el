;;; bv-themes-audit.el --- Audit harness for BV themes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; The audit harness validates compiled themes without trusting visual taste.
;; It checks structural integrity, contrast contracts, face-level contrast,
;; terminal colors, and semantic color separation under common simulations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-themes-color)
(require 'bv-themes-compile)
(require 'bv-themes-inventory)

(declare-function bv-themes-current "bv-themes")
(declare-function bv-themes-ensure-theme "bv-themes" (theme))
(declare-function bv-themes-known-themes "bv-themes" ())

(defconst bv-themes-audit-cvd-modes
  '(grayscale protanopia deuteranopia tritanopia)
  "Color-vision simulations used by BV theme audits.")

(defconst bv-themes-audit-semantic-colors
  '(error warning success info accent-0 accent-1 accent-2 accent-3)
  "Semantic token colors checked for distinguishability.")

(defconst bv-themes-audit-terminal-colors
  '(term-black term-red term-green term-yellow term-blue term-magenta
    term-cyan term-white term-bright-black term-bright-red
    term-bright-green term-bright-yellow term-bright-blue
    term-bright-magenta term-bright-cyan term-bright-white)
  "Terminal 16-color tokens checked by BV theme audits.")

(defconst bv-themes-audit-diagnostic-distances
  '((error-strong warning-strong 0.080)
    (error-strong info-strong 0.080)
    (warning-strong info-strong 0.080)
    (fg-salient error-strong 0.060)
    (fg-salient warning-strong 0.060)
    (fg-salient info-strong 0.060))
  "Diagnostic severity token separations required by BV theme audits.")

(defconst bv-themes-audit-diff-distances
  '((fg-added fg-removed 0.090)
    (fg-added fg-changed 0.090)
    (fg-removed fg-changed 0.090)
    (bg-added bg-removed 0.018)
    (bg-added bg-changed 0.018)
    (bg-removed bg-changed 0.018)
    (bg-added-refine bg-removed-refine 0.025)
    (bg-added-refine bg-changed-refine 0.025)
    (bg-removed-refine bg-changed-refine 0.025))
  "Diff state token separations required by BV theme audits.")

(defconst bv-themes-audit-heading-distances
  '((prose-heading-1 prose-heading-2 0.060)
    (prose-heading-2 prose-heading-3 0.060)
    (prose-heading-3 prose-heading-4 0.060)
    (prose-heading-4 prose-heading-5 0.060)
    (prose-heading-5 prose-heading-6 0.060)
    (prose-heading-6 prose-heading-7 0.060)
    (prose-heading-7 prose-heading-8 0.060))
  "Prose heading token separations required by BV theme audits.")

(defconst bv-themes-audit-required-bv-faces
  '(bv-face-default bv-face-muted bv-face-dim bv-face-faint
    bv-face-salient bv-face-strong bv-face-critical bv-face-success
    bv-face-warning bv-face-error bv-face-info bv-face-link
    bv-face-prompt bv-face-keybind bv-face-match bv-face-highlight
    bv-face-secondary bv-face-selection bv-icon-default bv-icon-muted
    bv-icon-file bv-icon-directory bv-icon-note bv-icon-code
    bv-icon-science bv-icon-idea bv-icon-proof bv-icon-review
    bv-icon-index bv-icon-system bv-icon-warning bv-icon-success
    bv-icon-special bv-icon-info bv-icon-salient bv-diagnostic-error
    bv-diagnostic-warning bv-diagnostic-info bv-diagnostic-note
    bv-diff-added bv-diff-removed bv-diff-changed bv-diff-added-refine
    bv-diff-removed-refine bv-diff-changed-refine bv-ui-header-default
    bv-ui-header-muted bv-ui-header-strong bv-ui-header-salient
    bv-ui-header-info bv-ui-header-warning bv-ui-header-error
    bv-ui-header-popout bv-ui-header-critical
    bv-ui-header-inactive-default bv-ui-header-inactive-muted
    bv-ui-header-inactive-strong bv-ui-header-inactive-salient
    bv-ui-header-inactive-info bv-ui-header-inactive-warning
    bv-ui-header-inactive-error
    bv-ui-header-inactive-popout bv-ui-header-inactive-critical)
  "Local BV semantic faces that package code may safely target.")

(defconst bv-themes-audit-apca-pairs
  '((default fg-main bg-main 60)
    (muted fg-muted bg-main 45)
    (header fg-header bg-header 60)
    (region fg-main bg-region 60)
    (completion fg-main bg-completion 60)
    (completion-current fg-main bg-completion-current 60)
    (search fg-search bg-search 60)
    (search-current fg-search-current bg-search-current 60)
    (prose-block fg-main bg-prose-block-contents 60)
    (modeline-active modeline-fg-active modeline-bg-active 60)
    (modeline-inactive modeline-fg-inactive modeline-bg-inactive 45)
    (critical critical-fg critical-bg 60))
  "APCA-style contrast pairs checked by BV theme audits.")

(defun bv-themes-audit--face-attrs (face-spec)
  "Return the attribute plist from FACE-SPEC."
  (cadr (car (cadr face-spec))))

(defun bv-themes-audit--duplicates (items)
  "Return duplicate ITEMS."
  (let ((seen nil)
        (dups nil))
    (dolist (item items)
      (if (memq item seen)
          (cl-pushnew item dups)
        (push item seen)))
    (nreverse dups)))

(defun bv-themes-audit--policy-target (artifact)
  "Return the contrast policy target declared by ARTIFACT."
  (or (plist-get (plist-get artifact :policy) :contrast-target)
      'balanced))

(defun bv-themes-audit--strict-visual-policy-p (target)
  "Return non-nil when TARGET asks visual metrics to be hard gates."
  (memq target '(strict high-contrast compliance regulated)))

(defun bv-themes-audit--structural-invariants (invariants)
  "Return non-visual structural INVARIANTS."
  (cl-remove-if-not
   (lambda (item)
     (memq (car item) '(raw-adapter-color missing-bv-face)))
   invariants))

(defun bv-themes-audit--inherit-symbols (inherit)
  "Return all face symbols in INHERIT."
  (cond
   ((null inherit) nil)
   ((symbolp inherit) (list inherit))
   ((listp inherit) (cl-remove-if-not #'symbolp inherit))
   (t nil)))

(defun bv-themes-audit--inheritance (faces)
  "Return undefined inheritance references in FACES."
  (let ((generated (mapcar #'car faces))
        missing)
    (dolist (spec faces)
      (let* ((face (car spec))
             (attrs (bv-themes-audit--face-attrs spec))
             (inherit (plist-get attrs :inherit)))
        (dolist (parent (bv-themes-audit--inherit-symbols inherit))
          (unless (or (memq parent generated)
                      (facep parent))
            (push (list face parent) missing)))))
    (nreverse missing)))

(defun bv-themes-audit--contrast-pairs (artifact)
  "Return failed contrast contracts for ARTIFACT."
  (let ((tokens (plist-get artifact :tokens))
        failures)
    (dolist (pair (plist-get artifact :contrast-pairs))
      (cl-destructuring-bind (label fg-token bg-token minimum) pair
        (let* ((fg (bv-themes-tokens-get fg-token tokens))
               (bg (bv-themes-tokens-get bg-token tokens))
               (ratio (bv-themes-color-contrast-ratio fg bg)))
          (when (< ratio minimum)
            (push (list label fg-token bg-token ratio minimum)
                  failures)))))
    (nreverse failures)))

(defun bv-themes-audit--face-contrast (faces &optional minimum)
  "Return face contrast failures from FACES below MINIMUM."
  (let ((minimum (or minimum 3.0))
        failures)
    (dolist (spec faces)
      (let* ((face (car spec))
             (attrs (bv-themes-audit--face-attrs spec))
             (fg (plist-get attrs :foreground))
             (bg (plist-get attrs :background)))
        (when (and (stringp fg) (stringp bg))
          (let ((ratio (bv-themes-color-contrast-ratio fg bg)))
            (when (< ratio minimum)
              (push (list face ratio minimum fg bg) failures))))))
    (nreverse failures)))

(defun bv-themes-audit--apca-y (color)
  "Return APCA-adjusted luminance for COLOR."
  (let ((y (bv-themes-color-relative-luminance color))
        (threshold 0.022))
    (if (< y threshold)
        (+ y (expt (- threshold y) 1.414))
      y)))

(defun bv-themes-audit-apca-contrast (foreground background)
  "Return an APCA-style signed contrast score for FOREGROUND on BACKGROUND."
  (let* ((txt (bv-themes-audit--apca-y foreground))
         (bg (bv-themes-audit--apca-y background))
         (scale 1.14)
         (lo-clip 0.1)
         (delta (if (> bg txt)
                    (* scale (- (expt bg 0.56) (expt txt 0.57)))
                  (* scale (- (expt bg 0.65) (expt txt 0.62)))))
         (score (if (< (abs delta) lo-clip)
                    0.0
                  (if (> delta 0)
                      (- delta 0.027)
                    (+ delta 0.027)))))
    (* 100 score)))

(defun bv-themes-audit--apca (tokens)
  "Return APCA-style contrast failures for TOKENS."
  (let (failures)
    (dolist (pair bv-themes-audit-apca-pairs)
      (cl-destructuring-bind (label fg-token bg-token minimum) pair
        (let* ((fg (bv-themes-tokens-get fg-token tokens))
               (bg (bv-themes-tokens-get bg-token tokens))
               (score (abs (bv-themes-audit-apca-contrast fg bg))))
          (when (< score minimum)
            (push (list label fg-token bg-token score minimum)
                  failures)))))
    (nreverse failures)))

(defun bv-themes-audit--terminal (tokens)
  "Return terminal contrast failures for TOKENS."
  (let ((bg (bv-themes-tokens-get 'bg-main tokens))
        failures)
    (dolist (token bv-themes-audit-terminal-colors)
      (let* ((minimum 3.0)
             (fg (bv-themes-tokens-get token tokens))
             (ratio (bv-themes-color-contrast-ratio fg bg)))
        (when (< ratio minimum)
          (push (list token ratio minimum) failures))))
    (nreverse failures)))

(defun bv-themes-audit--cvd (tokens &optional minimum)
  "Return CVD distinguishability failures in TOKENS below MINIMUM."
  (let ((minimum (or minimum 0.010))
        failures)
    (dolist (mode bv-themes-audit-cvd-modes)
      (cl-loop for colors on bv-themes-audit-semantic-colors
               do (dolist (right (cdr colors))
                    (let* ((left (car colors))
                           (left-color (bv-themes-color-simulate
                                        (bv-themes-tokens-get left tokens)
                                        mode))
                           (right-color (bv-themes-color-simulate
                                         (bv-themes-tokens-get right tokens)
                                         mode))
                           (distance (bv-themes-color-oklab-distance
                                      left-color right-color)))
                      (when (< distance minimum)
                        (push (list mode left right distance minimum)
                              failures))))))
    (nreverse failures)))

(defun bv-themes-audit--adapter-metadata ()
  "Return adapter metadata audit data."
  (let ((records (bv-themes-adapters-records))
        missing-package
        missing-purpose
        missing-role
        unknown-role)
    (dolist (record records)
      (unless (plist-get record :package)
        (push (plist-get record :face) missing-package))
      (unless (plist-get record :purpose)
        (push (plist-get record :face) missing-purpose))
      (let ((role (plist-get record :role)))
        (unless role
          (push (plist-get record :face) missing-role))
        (unless (or (null role)
                    (assq role bv-themes-roles))
          (push (list (plist-get record :face) role) unknown-role))))
    (list :records (length records)
          :packages (bv-themes-adapters-package-counts)
          :critical (cl-count-if (lambda (record)
                                   (plist-get record :critical))
                                 records)
          :missing-package (nreverse missing-package)
          :missing-purpose (nreverse missing-purpose)
          :missing-role (nreverse missing-role)
          :unknown-role (nreverse unknown-role))))

(defun bv-themes-audit--raw-color-string-p (value)
  "Return non-nil when VALUE is a raw hex color string."
  (and (stringp value)
       (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" value)))

(defun bv-themes-audit--contains-raw-color-p (value)
  "Return non-nil when VALUE contains a raw hex color string."
  (cond
   ((bv-themes-audit--raw-color-string-p value) t)
   ((consp value)
    (or (bv-themes-audit--contains-raw-color-p (car value))
        (bv-themes-audit--contains-raw-color-p (cdr value))))
   ((vectorp value)
    (cl-some #'bv-themes-audit--contains-raw-color-p value))
   (t nil)))

(defun bv-themes-audit--raw-adapter-colors ()
  "Return adapter specs that contain raw color strings."
  (let (failures)
    (dolist (entry bv-themes-adapter-specs)
      (when (bv-themes-audit--contains-raw-color-p (cddr entry))
        (push (car entry) failures)))
    (nreverse failures)))

(defun bv-themes-audit--token-distance-failures (tokens kind pairs)
  "Return KIND failures for token distance PAIRS in TOKENS."
  (let (failures)
    (dolist (pair pairs)
      (cl-destructuring-bind (left right minimum) pair
        (let* ((left-color (bv-themes-tokens-get left tokens))
               (right-color (bv-themes-tokens-get right tokens))
               (distance (bv-themes-color-oklab-distance
                          left-color right-color)))
          (when (< distance minimum)
            (push (list kind left right distance minimum) failures)))))
    (nreverse failures)))

(defun bv-themes-audit--terminal-separation (tokens &optional minimum)
  "Return terminal color separation failures in TOKENS below MINIMUM."
  (let ((minimum (or minimum 0.030))
        failures)
    (cl-loop for colors on bv-themes-audit-terminal-colors
             do (dolist (right (cdr colors))
                  (let* ((left (car colors))
                         (left-color (bv-themes-tokens-get left tokens))
                         (right-color (bv-themes-tokens-get right tokens))
                         (distance (bv-themes-color-oklab-distance
                                    left-color right-color)))
                    (when (< distance minimum)
                      (push (list 'terminal-separation
                                  left right distance minimum)
                            failures)))))
    (nreverse failures)))

(defun bv-themes-audit--heading-hierarchy ()
  "Return heading role hierarchy failures."
  (let ((roles '(heading-1 heading-2 heading-3 heading-4
                 heading-5 heading-6 heading-7 heading-8))
        failures)
    (cl-loop for left on roles
             when (cdr left)
             do (let* ((role (car left))
                       (next (cadr left))
                       (height (or (plist-get
                                    (bv-themes-roles-spec role)
                                    :height)
                                   1.0))
                       (next-height (or (plist-get
                                         (bv-themes-roles-spec next)
                                         :height)
                                        1.0)))
                  (when (< height next-height)
                    (push (list 'heading-hierarchy role next
                                height next-height)
                          failures))))
    (nreverse failures)))

(defun bv-themes-audit--missing-adapter-faces (faces)
  "Return FACES that are not present in adapter specs."
  (let ((available (mapcar #'car bv-themes-adapter-specs)))
    (cl-remove-if (lambda (face)
                    (memq face available))
                  faces)))

(defun bv-themes-audit--role-attribute-failures ()
  "Return role attribute invariant failures."
  (let (failures)
    (dolist (entry '((diagnostic-error :fg error-strong)
                     (diagnostic-error :wave error)
                     (diagnostic-warning :fg warning-strong)
                     (diagnostic-warning :wave warning)
                     (diagnostic-info :fg info-strong)
                     (diagnostic-info :wave info)
                     (diagnostic-note :fg fg-salient)
                     (diagnostic-note :wave blue)
                     (completion-current :extend t)
                     (completion-border :bg bg-completion)
                     (completion-scrollbar-track :bg bg-completion)
                     (completion-scrollbar-thumb :bg border)
                     (completion-match-0 :fg fg-salient)
                     (completion-match-1 :fg fg-special)
                     (completion-match-2 :fg info-strong)
                     (completion-match-3 :fg warning-strong)))
      (cl-destructuring-bind (role key expected) entry
        (let ((actual (plist-get (bv-themes-roles-spec role) key)))
          (unless (equal actual expected)
            (push (list 'role-attribute role key actual expected)
                  failures)))))
    (unless (memq (plist-get (bv-themes-roles-spec 'completion-current)
                             :weight)
                  '(semibold bold))
      (push (list 'role-attribute 'completion-current :weight
                  (plist-get (bv-themes-roles-spec 'completion-current)
                             :weight)
                  '(semibold bold))
            failures))
    (nreverse failures)))

(defun bv-themes-audit--invariants (artifact)
  "Return behavioral invariant failures for ARTIFACT."
  (let ((tokens (plist-get artifact :tokens))
        failures)
    (dolist (failure (bv-themes-audit--terminal-separation tokens))
      (push failure failures))
    (dolist (failure (bv-themes-audit--heading-hierarchy))
      (push failure failures))
    (dolist (failure (bv-themes-audit--role-attribute-failures))
      (push failure failures))
    (dolist (failure (bv-themes-audit--raw-adapter-colors))
      (push (list 'raw-adapter-color failure) failures))
    (dolist (failure (bv-themes-audit--missing-adapter-faces
                      bv-themes-audit-required-bv-faces))
      (push (list 'missing-bv-face failure) failures))
    (dolist (failure (bv-themes-audit--token-distance-failures
                      tokens 'diagnostic-severity
                      bv-themes-audit-diagnostic-distances))
      (push failure failures))
    (dolist (failure (bv-themes-audit--token-distance-failures
                      tokens 'diff-state
                      bv-themes-audit-diff-distances))
      (push failure failures))
    (dolist (failure (bv-themes-audit--token-distance-failures
                      tokens 'completion-state
                      '((bg-completion bg-completion-current 0.045))))
      (push failure failures))
    (dolist (failure (bv-themes-audit--token-distance-failures
                      tokens 'heading-distinguishability
                      bv-themes-audit-heading-distances))
      (push failure failures))
    (nreverse failures)))

(defun bv-themes-audit (theme &optional strict)
  "Audit compiled THEME.
When STRICT is non-nil, signal an error on hard failures."
  (interactive
   (list (intern (completing-read "BV theme: "
                                  (mapcar
                                   #'symbol-name
                                   (if (fboundp 'bv-themes-known-themes)
                                       (bv-themes-known-themes)
                                     (mapcar #'car
                                             bv-themes-token-profiles)))
                                  nil t))
         current-prefix-arg))
  (when (fboundp 'bv-themes-ensure-theme)
    (bv-themes-ensure-theme theme))
  (let* ((artifact (bv-themes-compile theme))
         (faces (plist-get artifact :faces))
         (tokens (plist-get artifact :tokens))
         (face-names (mapcar #'car faces))
         (policy-target (bv-themes-audit--policy-target artifact))
         (visual-hard (bv-themes-audit--strict-visual-policy-p
                       policy-target))
         (duplicates (bv-themes-audit--duplicates face-names))
         (inheritance (bv-themes-audit--inheritance faces))
         (contrast (bv-themes-audit--contrast-pairs artifact))
         (apca (bv-themes-audit--apca tokens))
         (face-contrast (bv-themes-audit--face-contrast faces 3.0))
         (terminal (bv-themes-audit--terminal tokens))
         (cvd (bv-themes-audit--cvd tokens))
         (cvd-hard (cl-remove-if (lambda (item)
                                   (eq (car item) 'grayscale))
                                 cvd))
         (metadata (bv-themes-audit--adapter-metadata))
         (inventory (bv-themes-inventory-scan artifact))
         (invariants (bv-themes-audit--invariants artifact))
         (report (list :theme theme
                       :variant (plist-get artifact :variant)
                       :policy (plist-get artifact :policy)
                       :visual-hard visual-hard
                       :face-count (length faces)
                       :token-count (length tokens)
                       :duplicates duplicates
                       :inheritance inheritance
                       :contrast contrast
                       :apca apca
                       :face-contrast face-contrast
                       :terminal terminal
                       :cvd cvd
                       :cvd-hard cvd-hard
                       :metadata metadata
                       :live-inventory inventory
                       :invariants invariants)))
    (when strict
      (let ((hard-failures
             (append duplicates
                     inheritance
                     (if visual-hard contrast nil)
                     (if visual-hard apca nil)
                     (if visual-hard terminal nil)
                     (if visual-hard cvd-hard nil)
                     (if visual-hard
                         invariants
                       (bv-themes-audit--structural-invariants
                        invariants))
                     (plist-get inventory :unthemed)
                     (plist-get metadata :missing-package)
                     (plist-get metadata :missing-purpose)
                     (plist-get metadata :missing-role)
                     (plist-get metadata :unknown-role))))
        (when hard-failures
          (error "BV theme audit failed for %S: duplicate=%d inherit=%d contrast=%d apca=%d terminal=%d cvd=%d live=%d invariants=%d metadata=%d"
                 theme
                 (length duplicates)
                 (length inheritance)
                 (length contrast)
                 (length apca)
                 (length terminal)
                 (length cvd-hard)
                 (length (plist-get inventory :unthemed))
                 (length invariants)
                 (+ (length (plist-get metadata :missing-package))
                    (length (plist-get metadata :missing-purpose))
                    (length (plist-get metadata :missing-role))
                    (length (plist-get metadata :unknown-role)))))))
    (when (called-interactively-p 'interactive)
      (bv-themes-audit-report report))
    report))

(defun bv-themes-audit-current ()
  "Audit the current BV theme."
  (interactive)
  (unless (fboundp 'bv-themes-current)
    (error "bv-themes is not loaded"))
  (let ((theme (bv-themes-current)))
    (unless theme
      (error "No BV theme is active"))
    (bv-themes-audit theme current-prefix-arg)))

(defun bv-themes-audit--insert-list (title items &optional formatter)
  "Insert TITLE and ITEMS into current buffer."
  (insert (format "* %s (%d)\n" title (length items)))
  (if items
      (dolist (item items)
        (insert (format "- %s\n"
                        (if formatter
                            (funcall formatter item)
                          item))))
    (insert "- none\n"))
  (insert "\n"))

(defun bv-themes-audit--format-invariant (item)
  "Return a display string for invariant ITEM."
  (pcase item
    (`(role-attribute ,role ,key ,actual ,expected)
     (format "role %S %S is %S, expected %S"
             role key actual expected))
    (`(raw-adapter-color ,face)
     (format "%S contains a raw color literal" face))
    (`(missing-bv-face ,face)
     (format "%S is not covered by adapters" face))
    (`(,kind ,left ,right ,distance ,minimum)
     (if (and (numberp distance) (numberp minimum))
         (format "%S %S/%S %.4f < %.4f"
                 kind left right distance minimum)
       (format "%S" item)))
    (_ (format "%S" item))))

(defun bv-themes-audit-report (report)
  "Display audit REPORT."
  (let ((buffer (get-buffer-create "*BV Theme Audit*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# BV Theme Audit: %S\n\n" (plist-get report :theme)))
        (insert (format "- variant: %S\n" (plist-get report :variant)))
        (insert (format "- faces: %d\n" (plist-get report :face-count)))
        (insert (format "- tokens: %d\n\n" (plist-get report :token-count)))
        (bv-themes-audit--insert-list
         "Duplicate Faces" (plist-get report :duplicates))
        (bv-themes-audit--insert-list
         "Undefined Inheritance" (plist-get report :inheritance)
         (lambda (item)
           (format "%S inherits %S" (car item) (cadr item))))
        (bv-themes-audit--insert-list
         "Contrast Contract Failures" (plist-get report :contrast)
         (lambda (item)
           (format "%S %S/%S %.2f < %.2f"
                   (nth 0 item) (nth 1 item) (nth 2 item)
                   (nth 3 item) (nth 4 item))))
        (bv-themes-audit--insert-list
         "APCA Contrast Failures" (plist-get report :apca)
         (lambda (item)
           (format "%S %S/%S %.1f < %.1f"
                   (nth 0 item) (nth 1 item) (nth 2 item)
                   (nth 3 item) (nth 4 item))))
        (bv-themes-audit--insert-list
         "Face Contrast Advisories" (plist-get report :face-contrast)
         (lambda (item)
           (format "%S %.2f < %.2f %s on %s"
                   (nth 0 item) (nth 1 item) (nth 2 item)
                   (nth 3 item) (nth 4 item))))
        (bv-themes-audit--insert-list
         "Terminal Failures" (plist-get report :terminal)
         (lambda (item)
           (format "%S %.2f < %.2f"
                   (nth 0 item) (nth 1 item) (nth 2 item))))
        (bv-themes-audit--insert-list
         "CVD Separation Failures" (plist-get report :cvd)
         (lambda (item)
           (format "%S %S/%S %.4f < %.4f"
                   (nth 0 item) (nth 1 item) (nth 2 item)
                   (nth 3 item) (nth 4 item))))
        (let ((metadata (plist-get report :metadata)))
          (insert "* Adapter Metadata\n")
          (insert (format "- records: %d\n" (plist-get metadata :records)))
          (insert (format "- packages: %d\n"
                          (length (plist-get metadata :packages))))
          (insert (format "- critical records: %d\n\n"
                          (plist-get metadata :critical)))
          (bv-themes-audit--insert-list
           "Adapters Without Package" (plist-get metadata :missing-package))
          (bv-themes-audit--insert-list
           "Adapters Without Purpose" (plist-get metadata :missing-purpose))
          (bv-themes-audit--insert-list
           "Adapters Without Role" (plist-get metadata :missing-role))
          (bv-themes-audit--insert-list
           "Adapters With Unknown Role" (plist-get metadata :unknown-role)
           (lambda (item)
             (format "%S uses %S" (car item) (cadr item)))))
        (let ((inventory (plist-get report :live-inventory)))
          (insert "* Live Face Inventory\n")
          (insert (format "- live faces: %d\n"
                          (plist-get inventory :live-count)))
          (insert (format "- adapter specs: %d\n"
                          (plist-get inventory :adapter-count)))
          (insert (format "- relevant live faces: %d\n"
                          (plist-get inventory :relevant-count)))
          (insert (format "- covered relevant faces: %d\n"
                          (plist-get inventory :covered-live-count)))
          (insert (format "- unthemed relevant faces: %d\n\n"
                          (length (plist-get inventory :unthemed))))
          (bv-themes-audit--insert-list
           "Unthemed Relevant Live Faces"
           (plist-get inventory :unthemed)
           (lambda (entry)
             (format "%S (%S)"
                     (plist-get entry :face)
                     (plist-get entry :package)))))
        (bv-themes-audit--insert-list
         "Invariant Failures" (plist-get report :invariants)
         #'bv-themes-audit--format-invariant)
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(provide 'bv-themes-audit)
;;; bv-themes-audit.el ends here
