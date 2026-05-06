;;; bv-themes-regression.el --- Visual artifacts for BV themes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; Deterministic SVG artifacts for theme review and regression checks.  The
;; fixture is intentionally self-contained: it renders static Emacs workflow
;; states from compiled BV tokens and does not reference external images,
;; fonts, scripts, stylesheets, or network resources.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-themes-compile)
(require 'bv-themes-audit)

(declare-function bv-themes-ensure-theme "bv-themes" (theme))
(declare-function bv-themes-known-themes "bv-themes" ())
(declare-function bv-themes-load-gallery "bv-themes" ())

(defvar bv-themes-regression-themes nil
  "Themes included in regression artifact generation.
When nil, use all registered BV theme profiles.")

(defconst bv-themes-regression-width 1480
  "Width of each deterministic SVG artifact.")

(defconst bv-themes-regression-height 1500
  "Height of each deterministic SVG artifact.")

(defconst bv-themes-regression-required-sections
  '((modeline-header . "Modeline/header")
    (org-prose . "Org/prose")
    (code-syntax . "Code syntax")
    (diff-vc . "Diff and VCS states")
    (completion . "Completion UI")
    (diagnostics . "Diagnostics")
    (dired . "Dired/file listing")
    (terminal . "Terminal ANSI palette")
    (calendar . "Calendar")
    (feed . "Feed rows"))
  "Workflow sections that every visual artifact must include.")

(defconst bv-themes-regression-required-tokens
  '(bg-main bg-dim bg-alt bg-alt-2 bg-active bg-hover bg-hl-line
    bg-region bg-region-subtle bg-header bg-search bg-search-current
    bg-completion bg-completion-current bg-prose-block
    bg-prose-block-contents bg-markup-code bg-added bg-added-refine
    bg-removed bg-removed-refine bg-changed bg-changed-refine fg-main
    fg-muted fg-dim fg-faint fg-salient fg-link fg-link-visited
    fg-added fg-removed fg-changed error error-strong error-subtle
    warning warning-strong warning-subtle success success-strong
    success-subtle info info-strong info-subtle critical-fg critical-bg
    accent-0 accent-1 accent-2 accent-3 syntax-comment syntax-doc
    syntax-string syntax-keyword syntax-builtin syntax-function
    syntax-variable syntax-constant syntax-number syntax-type
    syntax-operator syntax-property prose-heading-1 prose-heading-2
    prose-heading-3 prose-link prose-link-visited prose-code
    prose-verbatim prose-metadata prose-metadata-value prose-table
    prose-todo prose-done modeline-bg-active modeline-fg-active
    modeline-bg-inactive modeline-fg-inactive modeline-bg-accent
    modeline-fg-accent bg-tab-current bg-tab-other fg-header
    fg-header-muted blue-strong red-strong term-black term-red
    term-green term-yellow term-blue term-magenta term-cyan term-white
    term-bright-black term-bright-red term-bright-green
    term-bright-yellow term-bright-blue term-bright-magenta
    term-bright-cyan term-bright-white border-subtle border-strong)
  "Tokens that must be available before rendering workflow artifacts.")

(defconst bv-themes-regression-min-bytes 22000
  "Minimum SVG byte size accepted by artifact validation.")

(defconst bv-themes-regression-min-rects 55
  "Minimum number of rectangle elements accepted by artifact validation.")

(defconst bv-themes-regression-min-texts 90
  "Minimum number of text elements accepted by artifact validation.")

(defun bv-themes-regression--xml-escape (value)
  "Return VALUE escaped for XML."
  (let ((text (format "%s" value)))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (setq text (replace-regexp-in-string ">" "&gt;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    text))

(defun bv-themes-regression--token (tokens name)
  "Return token NAME from TOKENS."
  (bv-themes-tokens-get name tokens))

(defun bv-themes-regression--color (tokens value)
  "Resolve VALUE as a token name or literal color using TOKENS."
  (cond
   ((symbolp value) (bv-themes-regression--token tokens value))
   ((stringp value) value)
   (t (error "Expected color token or string: %S" value))))

(defun bv-themes-regression--text
    (x y text color &optional size weight family)
  "Return SVG TEXT at X Y with COLOR."
  (format (concat "<text x='%d' y='%d' fill='%s' font-family='%s' "
                  "font-size='%d' font-weight='%s' "
                  "xml:space='preserve'>%s</text>\n")
          x y color (or family "Iosevka, JetBrains Mono, monospace")
          (or size 15) (or weight "400")
          (bv-themes-regression--xml-escape text)))

(defun bv-themes-regression--text-token
    (tokens x y text token &optional size weight family)
  "Return SVG TEXT at X Y using TOKEN from TOKENS."
  (bv-themes-regression--text
   x y text (bv-themes-regression--token tokens token)
   size weight family))

(defun bv-themes-regression--text-runs
    (tokens x y runs &optional size family)
  "Return SVG text RUNS starting at X Y.
Each run has the form (TEXT TOKEN :weight WEIGHT)."
  (let ((cursor x)
        (font-size (or size 15))
        (svg ""))
    (dolist (run runs)
      (let* ((text (nth 0 run))
             (token (nth 1 run))
             (weight (or (plist-get (cddr run) :weight) "400"))
             (color (bv-themes-regression--color tokens token)))
        (setq svg
              (concat svg
                      (bv-themes-regression--text
                       cursor y text color font-size weight family)))
        (setq cursor
              (+ cursor (ceiling (* (length text) font-size 0.61))))))
    svg))

(defun bv-themes-regression--rect
    (x y width height color &optional stroke radius)
  "Return SVG rect at X Y with WIDTH, HEIGHT, COLOR, STROKE, and RADIUS."
  (format (concat "<rect x='%d' y='%d' width='%d' height='%d' "
                  "fill='%s'%s%s/>\n")
          x y width height color
          (if stroke
              (format " stroke='%s' stroke-width='1'" stroke)
            "")
          (if radius
              (format " rx='%d' ry='%d'" radius radius)
            "")))

(defun bv-themes-regression--rect-token
    (tokens x y width height token &optional stroke-token radius)
  "Return SVG rect using color TOKEN and optional STROKE-TOKEN."
  (bv-themes-regression--rect
   x y width height
   (bv-themes-regression--token tokens token)
   (and stroke-token
        (bv-themes-regression--token tokens stroke-token))
   radius))

(defun bv-themes-regression--line
    (x1 y1 x2 y2 color &optional width dasharray)
  "Return SVG line from X1 Y1 to X2 Y2."
  (format (concat "<line x1='%d' y1='%d' x2='%d' y2='%d' "
                  "stroke='%s' stroke-width='%d'%s/>\n")
          x1 y1 x2 y2 color (or width 1)
          (if dasharray
              (format " stroke-dasharray='%s'" dasharray)
            "")))

(defun bv-themes-regression--wave (x y width color)
  "Return a deterministic SVG wave underline at X Y."
  (let ((cursor x)
        (target (+ x width))
        (high nil)
        (path (format "M %d %d" x y)))
    (while (< cursor target)
      (setq cursor (min target (+ cursor 6)))
      (setq path
            (concat path
                    (format " L %d %d"
                            cursor
                            (if high (- y 3) (+ y 3)))))
      (setq high (not high)))
    (format (concat "<path d='%s' fill='none' stroke='%s' "
                    "stroke-width='2' stroke-linecap='round'/>\n")
            path color)))

(defun bv-themes-regression--section
    (tokens id title x y width height body)
  "Return a section group with ID, TITLE, bounds, and BODY."
  (let ((fg (bv-themes-regression--token tokens 'fg-main))
        (border (bv-themes-regression--token tokens 'border-subtle)))
    (concat
     (format "<g id='bv-section-%s' data-section='%s'>\n"
             id id)
     (bv-themes-regression--rect-token
      tokens x y width height 'bg-alt 'border-subtle 6)
     (bv-themes-regression--text
      (+ x 18) (+ y 28) title fg 17 "700")
     (bv-themes-regression--line
      (+ x 18) (+ y 42) (- (+ x width) 18) (+ y 42)
      border 1)
     body
     "</g>\n")))

(defun bv-themes-regression--validate-tokens (tokens)
  "Signal if TOKENS does not satisfy the regression contract."
  (dolist (name bv-themes-regression-required-tokens)
    (let ((value (bv-themes-regression--token tokens name)))
      (unless (stringp value)
        (error "Theme token %S did not resolve to a color: %S"
               name value)))))

(defun bv-themes-regression--modeline-header-section
    (theme artifact tokens)
  "Return the modeline/header SVG section for THEME and ARTIFACT."
  (let ((fg-header (bv-themes-regression--token tokens 'fg-header))
        (fg-muted (bv-themes-regression--token tokens 'fg-header-muted))
        (fg-modeline (bv-themes-regression--token tokens 'modeline-fg-active))
        (accent-fg (bv-themes-regression--token tokens 'modeline-fg-accent))
        (border (bv-themes-regression--token tokens 'border-subtle)))
    (concat
     "<g id='bv-section-modeline-header' data-section='modeline-header'>\n"
     (bv-themes-regression--rect-token
      tokens 0 0 bv-themes-regression-width 128 'bg-main)
     (bv-themes-regression--rect-token
      tokens 0 0 bv-themes-regression-width 56 'bg-header)
     (bv-themes-regression--text
      40 35
      (format "BV theme regression: %S" theme)
      fg-header 21 "700")
     (bv-themes-regression--text
      420 35
      (format "variant=%S  tokens=%d  sections=%d"
              (plist-get artifact :variant)
              (length (plist-get artifact :tokens))
              (length bv-themes-regression-required-sections))
      fg-muted 14)
     (bv-themes-regression--rect-token
      tokens 40 66 170 32 'bg-tab-current 'border-subtle 4)
     (bv-themes-regression--text-token
      tokens 56 87 "notes.org" 'fg-main 14 "700")
     (bv-themes-regression--rect-token
      tokens 214 66 180 32 'bg-tab-other 'border-subtle 4)
     (bv-themes-regression--text-token
      tokens 230 87 "bv-themes.el" 'fg-dim 14)
     (bv-themes-regression--rect-token
      tokens 398 66 170 32 'bg-tab-other 'border-subtle 4)
     (bv-themes-regression--text-token
      tokens 414 87 "*Messages*" 'fg-dim 14)
     (bv-themes-regression--rect-token
      tokens 40 102 980 24 'modeline-bg-active)
     (bv-themes-regression--text
      54 119 "  BV  main  Org  L42  Theme:active  UTF-8"
      fg-modeline 13 "700")
     (bv-themes-regression--rect-token
      tokens 1020 102 420 24 'modeline-bg-inactive)
     (bv-themes-regression--text-token
      tokens 1034 119 "  background worker  batch artifact"
      'modeline-fg-inactive 13)
     (cl-loop for token in '(accent-0 accent-1 accent-2 accent-3)
              for x from 1264 by 38
              concat (bv-themes-regression--rect
                      x 16 26 20
                      (bv-themes-regression--token tokens token)
                      border 3))
     (bv-themes-regression--rect-token
      tokens 1168 66 142 24 'modeline-bg-accent nil 4)
     (bv-themes-regression--text
      1182 83 "checked"
      accent-fg 13 "700")
     "</g>\n")))

(defun bv-themes-regression--org-section (tokens)
  "Return the Org/prose SVG section for TOKENS."
  (let ((x 40)
        (y 156)
        (width 650)
        (height 340))
    (bv-themes-regression--section
     tokens 'org-prose "Org/prose" x y width height
     (concat
      (bv-themes-regression--text-token
       tokens (+ x 24) (+ y 68)
       "#+title: Visual regression fixture" 'prose-metadata 14)
      (bv-themes-regression--text-token
       tokens (+ x 24) (+ y 104)
       "* Research queue" 'prose-heading-1 22 "700"
       "IBM Plex Sans, sans-serif")
      (bv-themes-regression--text-token
       tokens (+ x 42) (+ y 136)
       "** Reading notes and tasks" 'prose-heading-2 18 "700"
       "IBM Plex Sans, sans-serif")
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 168)
       '(("TODO" prose-todo :weight "700")
         ("  Verify captured states  " fg-main)
         (":theme:golden:" fg-dim))
       15)
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 198)
       '(("A prose line with a " fg-main)
         ("link" prose-link :weight "700")
         (", " fg-main)
         ("visited link" prose-link-visited)
         (", " fg-main)
         ("~inline-code~" prose-code)
         (", and " fg-main)
         ("=verbatim=" prose-verbatim)
         ("." fg-main))
       15 "IBM Plex Sans, sans-serif")
      (bv-themes-regression--rect-token
       tokens (+ x 42) (+ y 218) 560 24 'bg-prose-block
       'border-subtle 3)
      (bv-themes-regression--text-token
       tokens (+ x 54) (+ y 235)
       "#+begin_src emacs-lisp" 'prose-metadata 13)
      (bv-themes-regression--rect-token
       tokens (+ x 42) (+ y 242) 560 64 'bg-prose-block-contents
       'border-subtle 3)
      (bv-themes-regression--text-runs
       tokens (+ x 54) (+ y 264)
       '(("(setq " syntax-keyword :weight "700")
         ("bv-artifact" syntax-variable)
         (" '" syntax-delimiter)
         ("deterministic" syntax-constant :weight "700")
         (")" syntax-delimiter))
       14)
      (bv-themes-regression--text-runs
       tokens (+ x 54) (+ y 286)
       '(("(message " syntax-keyword :weight "700")
         ("\"No external assets\"" syntax-string)
         (")" syntax-delimiter))
       14)
      (bv-themes-regression--text-token
       tokens (+ x 42) (+ y 326)
       "| state | count | owner |" 'prose-table 14)))))

(defun bv-themes-regression--code-line
    (tokens x y width number runs &optional current)
  "Return a code line at X Y with NUMBER and syntax RUNS."
  (concat
   (when current
     (bv-themes-regression--rect-token
      tokens (- x 8) (- y 18) width 25 'bg-hl-line))
   (bv-themes-regression--text-token
    tokens x y (format "%2d" number) 'fg-faint 14)
   (bv-themes-regression--text-runs
    tokens (+ x 44) y runs 15)))

(defun bv-themes-regression--code-section (tokens)
  "Return the code syntax SVG section for TOKENS."
  (let ((x 40)
        (y 520)
        (width 650)
        (height 300))
    (bv-themes-regression--section
     tokens 'code-syntax "Code syntax" x y width height
     (concat
      (bv-themes-regression--rect-token
       tokens (+ x 24) (+ y 58) 602 28 'bg-header 'border-subtle 4)
      (bv-themes-regression--text-token
       tokens (+ x 38) (+ y 77)
       "emacs/lisp/bv-themes-regression.el" 'fg-header 13 "700")
      (bv-themes-regression--rect-token
       tokens (+ x 24) (+ y 92) 602 176 'bg-main 'border-subtle 3)
      (bv-themes-regression--code-line
       tokens (+ x 42) (+ y 120) 566 1
       '(("(" syntax-delimiter)
         ("defun" syntax-keyword :weight "700")
         (" bv-demo-render" syntax-function :weight "700")
         (" (theme tokens)" syntax-variable)
         (")" syntax-delimiter)))
      (bv-themes-regression--code-line
       tokens (+ x 42) (+ y 148) 566 2
       '(("  " fg-main)
         ("\"Render deterministic UI states.\"" syntax-doc)))
      (bv-themes-regression--code-line
       tokens (+ x 42) (+ y 176) 566 3
       '(("  (" syntax-delimiter)
         ("let*" syntax-keyword :weight "700")
         (" ((" syntax-delimiter)
         ("status" syntax-variable)
         (" :" syntax-delimiter)
         ("ready" syntax-constant :weight "700")
         (") (" syntax-delimiter)
         ("count" syntax-variable)
         (" " fg-main)
         ("10" syntax-number)
         ("))" syntax-delimiter))
       t)
      (bv-themes-regression--code-line
       tokens (+ x 42) (+ y 204) 566 4
       '(("    (" syntax-delimiter)
         ("when" syntax-keyword :weight "700")
         (" (" syntax-delimiter)
         (">" syntax-operator)
         (" count " fg-main)
         ("0" syntax-number)
         (")" syntax-delimiter)))
      (bv-themes-regression--code-line
       tokens (+ x 42) (+ y 232) 566 5
       '(("      (" syntax-delimiter)
         ("plist-put" syntax-builtin :weight "700")
         (" artifact " syntax-variable)
         (":status" syntax-property)
         (" status))))" syntax-delimiter)))
      (bv-themes-regression--text-token
       tokens (+ x 42) (+ y 258)
       "; Comments stay calm while keywords, types, constants, and strings pop."
       'syntax-comment 13)))))

(defun bv-themes-regression--diff-row
    (tokens x y width bg-token runs)
  "Return a diff row at X Y with WIDTH, BG-TOKEN, and text RUNS."
  (concat
   (bv-themes-regression--rect-token
    tokens x y width 28 bg-token)
   (bv-themes-regression--text-runs
    tokens (+ x 12) (+ y 19) runs 14)))

(defun bv-themes-regression--diff-section (tokens)
  "Return the diff and VCS state SVG section for TOKENS."
  (let ((x 40)
        (y 844)
        (width 650)
        (height 300))
    (bv-themes-regression--section
     tokens 'diff-vc "Diff and VCS states" x y width height
     (concat
      (bv-themes-regression--diff-row
       tokens (+ x 24) (+ y 58) 602 'bg-alt-2
       '(("modified  " fg-changed :weight "700")
         ("emacs/lisp/bv-themes-regression.el" fg-salient :weight "700")))
      (bv-themes-regression--diff-row
       tokens (+ x 24) (+ y 88) 602 'bg-alt
       '(("@@ -184,7 +214,12 @@ visual fixtures" fg-muted :weight "700")))
      (bv-themes-regression--diff-row
       tokens (+ x 24) (+ y 118) 602 'bg-removed
       '(("- simple swatch-only sample rows" fg-removed :weight "700")))
      (bv-themes-regression--rect-token
       tokens (+ x 64) (+ y 122) 150 20 'bg-removed-refine)
      (bv-themes-regression--diff-row
       tokens (+ x 24) (+ y 148) 602 'bg-added
       '(("+ workflow section: completion, diagnostics, terminal" fg-added
          :weight "700")))
      (bv-themes-regression--rect-token
       tokens (+ x 232) (+ y 152) 236 20 'bg-added-refine)
      (bv-themes-regression--diff-row
       tokens (+ x 24) (+ y 178) 602 'bg-changed
       '(("~ manifest includes hash, bytes, and section checks" fg-changed
          :weight "700")))
      (bv-themes-regression--rect-token
       tokens (+ x 218) (+ y 182) 210 20 'bg-changed-refine)
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 238)
       '(("staged " fg-added :weight "700")
         ("+2  " fg-main)
         ("modified " fg-changed :weight "700")
         ("1  " fg-main)
         ("deleted " fg-removed :weight "700")
         ("0  " fg-main)
         ("branch " fg-muted)
         ("master" fg-salient :weight "700"))
       14)
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 268)
       '(("process " fg-muted)
         ("ok" success-strong :weight "700")
         ("   review " fg-muted)
         ("pending" warning-strong :weight "700")
         ("   conflict " fg-muted)
         ("none" info-strong :weight "700"))
       14)))))

(defun bv-themes-regression--completion-row
    (tokens x y width bg-token runs &optional annotation)
  "Return a completion candidate row."
  (concat
   (bv-themes-regression--rect-token
    tokens x y width 34 bg-token)
   (bv-themes-regression--text-runs
    tokens (+ x 14) (+ y 23) runs 14)
   (when annotation
     (bv-themes-regression--text-token
      tokens (- (+ x width) 176) (+ y 23)
      annotation 'fg-dim 13))))

(defun bv-themes-regression--completion-section (tokens)
  "Return the completion UI SVG section for TOKENS."
  (let ((x 700)
        (y 156)
        (width 740)
        (height 260))
    (bv-themes-regression--section
     tokens 'completion "Completion UI" x y width height
     (concat
      (bv-themes-regression--rect-token
       tokens (+ x 24) (+ y 60) 692 38 'bg-main 'border-subtle 4)
      (bv-themes-regression--text-runs
       tokens (+ x 40) (+ y 84)
       '(("M-x " prompt :weight "700")
         ("bv-theme" fg-main)
         ("-" fg-muted)
         ("reg" fg-salient :weight "700"))
       15)
      (bv-themes-regression--completion-row
       tokens (+ x 24) (+ y 110) 692 'bg-completion-current
       '(("bv-themes-" fg-main)
         ("regression-run" fg-salient :weight "700"))
       "command")
      (bv-themes-regression--completion-row
       tokens (+ x 24) (+ y 146) 692 'bg-completion
       '(("bv-themes-" fg-main)
         ("regression-svg" fg-salient :weight "700"))
       "function")
      (bv-themes-regression--completion-row
       tokens (+ x 24) (+ y 182) 692 'bg-completion
       '(("bv-themes-" fg-main)
         ("regression-write" fg-salient :weight "700"))
       "function")
      (bv-themes-regression--text-runs
       tokens (+ x 40) (+ y 232)
       '(("annotation" fg-dim)
         ("   matched text" fg-salient :weight "700")
         ("   selected row" fg-main))
       13)))))

(defun bv-themes-regression--diagnostic-row
    (tokens x y width severity fg-token bg-token message location)
  "Return a diagnostic row."
  (let ((fg (bv-themes-regression--token tokens fg-token)))
    (concat
     (bv-themes-regression--rect-token
      tokens x y width 44 bg-token 'border-subtle 3)
     (bv-themes-regression--rect x y 5 44 fg)
     (bv-themes-regression--text
      (+ x 16) (+ y 27) severity fg 14 "700")
     (bv-themes-regression--text-token
      tokens (+ x 112) (+ y 27) message 'fg-main 14)
     (bv-themes-regression--text-token
      tokens (- (+ x width) 142) (+ y 27) location 'fg-dim 13))))

(defun bv-themes-regression--diagnostics-section (tokens)
  "Return the diagnostics SVG section for TOKENS."
  (let ((x 700)
        (y 440)
        (width 740)
        (height 240))
    (bv-themes-regression--section
     tokens 'diagnostics "Diagnostics" x y width height
     (concat
      (bv-themes-regression--diagnostic-row
       tokens (+ x 24) (+ y 58) 692
       "error" 'error-strong 'error-subtle
       "Undefined sample variable in render path" "L214:C18")
      (bv-themes-regression--diagnostic-row
       tokens (+ x 24) (+ y 106) 692
       "warning" 'warning-strong 'warning-subtle
       "Token fallback should be explicit" "L228:C9")
      (bv-themes-regression--diagnostic-row
       tokens (+ x 24) (+ y 154) 692
       "info" 'info-strong 'info-subtle
       "Artifact includes all required sections" "L260:C1")
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 220)
       '(("(render undefined-sample tokens)" fg-main))
       14)
      (bv-themes-regression--wave
       (+ x 105) (+ y 225) 130
       (bv-themes-regression--token tokens 'error))))))

(defun bv-themes-regression--dired-row
    (tokens x y width bg-token runs &optional marked)
  "Return a Dired-like file listing row."
  (concat
   (bv-themes-regression--rect-token
    tokens x y width 32 bg-token)
   (when marked
     (bv-themes-regression--rect-token
      tokens x y 5 32 'accent-0))
   (bv-themes-regression--text-runs
    tokens (+ x 12) (+ y 22) runs 14)))

(defun bv-themes-regression--dired-section (tokens)
  "Return the Dired/file listing SVG section for TOKENS."
  (let ((x 40)
        (y 1168)
        (width 650)
        (height 292))
    (bv-themes-regression--section
     tokens 'dired "Dired/file listing" x y width height
     (concat
      (bv-themes-regression--text-token
       tokens (+ x 24) (+ y 68)
       "  mode   size  modified     name" 'fg-dim 13)
      (bv-themes-regression--dired-row
       tokens (+ x 24) (+ y 84) 602 'bg-region
       '(("* " fg-salient :weight "700")
         ("drwxr-xr-x  4K  May 06  " fg-muted)
         ("lisp/" fg-salient :weight "700"))
       t)
      (bv-themes-regression--dired-row
       tokens (+ x 24) (+ y 118) 602 'bg-main
       '(("  -rw-r--r-- 28K  May 06  " fg-muted)
         ("bv-themes-regression.el" fg-main)))
      (bv-themes-regression--dired-row
       tokens (+ x 24) (+ y 152) 602 'bg-main
       '(("  lrwxrwxrwx  18  May 01  " fg-muted)
         ("current-theme" fg-link :weight "700")
         (" -> " fg-dim)
         ("bv-themes.el" fg-link-visited)))
      (bv-themes-regression--dired-row
       tokens (+ x 24) (+ y 186) 602 'bg-main
       '(("  -rwxr-xr-x 12K  Apr 29  " fg-muted)
         ("scripts/emacs-doctor.sh" success-strong :weight "700")))
      (bv-themes-regression--dired-row
       tokens (+ x 24) (+ y 220) 602 'bg-main
       '(("  -rw-r--r--  1K  Apr 27  " fg-muted)
         (".dir-locals.el" fg-dim)))
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 270)
       '(("marked" fg-salient :weight "700")
         ("  directory" fg-salient)
         ("  symlink" fg-link)
         ("  executable" success-strong)
         ("  dimmed" fg-dim))
       13)))))

(defun bv-themes-regression--terminal-swatch
    (tokens x y label token)
  "Return one terminal color swatch."
  (concat
   (bv-themes-regression--rect
    x y 56 30
    (bv-themes-regression--token tokens token)
    (bv-themes-regression--token tokens 'border-subtle)
    3)
   (bv-themes-regression--text-token
    tokens x (+ y 48) label 'fg-dim 11)))

(defun bv-themes-regression--terminal-section (tokens)
  "Return the terminal ANSI palette SVG section for TOKENS."
  (let ((x 700)
        (y 704)
        (width 740)
        (height 240)
        (normal '(("black" term-black)
                  ("red" term-red)
                  ("green" term-green)
                  ("yellow" term-yellow)
                  ("blue" term-blue)
                  ("magenta" term-magenta)
                  ("cyan" term-cyan)
                  ("white" term-white)))
        (bright '(("br-black" term-bright-black)
                  ("br-red" term-bright-red)
                  ("br-green" term-bright-green)
                  ("br-yellow" term-bright-yellow)
                  ("br-blue" term-bright-blue)
                  ("br-magenta" term-bright-magenta)
                  ("br-cyan" term-bright-cyan)
                  ("br-white" term-bright-white))))
    (bv-themes-regression--section
     tokens 'terminal "Terminal ANSI palette" x y width height
     (concat
      (bv-themes-regression--rect-token
       tokens (+ x 24) (+ y 58) 692 54 'bg-main 'border-subtle 4)
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 82)
       '(("user@host" term-green :weight "700")
         (":" fg-main)
         ("~/dotfiles" term-blue :weight "700")
         ("$ " term-bright-black)
         ("emacs --batch -l bv-themes-regression" term-white))
       14)
      (bv-themes-regression--text-runs
       tokens (+ x 42) (+ y 104)
       '(("ok" term-bright-green :weight "700")
         ("  warning" term-bright-yellow :weight "700")
         ("  error" term-bright-red :weight "700")
         ("  info" term-bright-cyan :weight "700"))
       14)
      (cl-loop for entry in normal
               for index from 0
               concat (bv-themes-regression--terminal-swatch
                       tokens (+ x 30 (* index 84)) (+ y 132)
                       (car entry) (cadr entry)))
      (cl-loop for entry in bright
               for index from 0
               concat (bv-themes-regression--terminal-swatch
                       tokens (+ x 30 (* index 84)) (+ y 184)
                       (car entry) (cadr entry)))))))

(defun bv-themes-regression--calendar-cell
    (tokens x y day column selected)
  "Return one calendar DAY cell at X Y for COLUMN."
  (let* ((empty (string-empty-p day))
         (holiday (member day '("25")))
         (today (string= day "6"))
         (weekend (memq column '(0 6)))
         (bg-token (if selected 'bg-region 'bg-main))
         (fg-token (cond
                    (empty 'fg-faint)
                    (today 'blue-strong)
                    (holiday 'red-strong)
                    (weekend 'fg-dim)
                    (t 'fg-main))))
    (concat
     (bv-themes-regression--rect-token
      tokens x y 38 32 bg-token
      (if today 'blue-strong 'border-subtle)
      3)
     (unless empty
       (bv-themes-regression--text-token
        tokens (+ x 11) (+ y 22) day fg-token 13
        (if (or selected today holiday) "700" "400"))))))

(defun bv-themes-regression--calendar-section (tokens)
  "Return the calendar SVG section for TOKENS."
  (let ((x 700)
        (y 968)
        (width 350)
        (height 492)
        (days '("" "" "" "" "" "1" "2"
                "3" "4" "5" "6" "7" "8" "9"
                "10" "11" "12" "13" "14" "15" "16"
                "17" "18" "19" "20" "21" "22" "23"
                "24" "25" "26" "27" "28" "29" "30"
                "31" "" "" "" "" "" "")))
    (bv-themes-regression--section
     tokens 'calendar "Calendar" x y width height
     (concat
      (bv-themes-regression--text-token
       tokens (+ x 110) (+ y 74) "May 2026"
       'prose-heading-1 18 "700" "IBM Plex Sans, sans-serif")
      (cl-loop for label in '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
               for column from 0
               concat (bv-themes-regression--text-token
                       tokens (+ x 28 (* column 43)) (+ y 110)
                       label
                       (if (memq column '(0 6)) 'fg-dim 'fg-muted)
                       11 "700"))
      (cl-loop for day in days
               for index from 0
               for column = (mod index 7)
               for row = (/ index 7)
               concat (bv-themes-regression--calendar-cell
                       tokens
                       (+ x 25 (* column 43))
                       (+ y 124 (* row 42))
                       day column (string= day "15")))
      (bv-themes-regression--text-runs
       tokens (+ x 28) (+ y 430)
       '(("6" blue-strong :weight "700")
         (" today   " fg-dim)
         ("15" fg-main :weight "700")
         (" selected   " fg-dim)
         ("25" red-strong :weight "700")
         (" holiday" fg-dim))
       12)))))

(defun bv-themes-regression--feed-row
    (tokens x y width bg-token state state-token title source age tags)
  "Return one Elfeed-like feed row."
  (concat
   (bv-themes-regression--rect-token
    tokens x y width 62 bg-token 'border-subtle 4)
   (bv-themes-regression--text-token
    tokens (+ x 12) (+ y 24) state state-token 11 "700")
   (bv-themes-regression--text-token
    tokens (+ x 82) (+ y 24) title 'fg-main 13 "700")
   (bv-themes-regression--text-runs
    tokens (+ x 82) (+ y 48)
    `((,source fg-dim)
      ("  " fg-dim)
      (,age fg-muted)
      ("  " fg-dim)
      (,tags fg-salient :weight "700"))
    11)))

(defun bv-themes-regression--feed-section (tokens)
  "Return the feed rows SVG section for TOKENS."
  (let ((x 1086)
        (y 968)
        (width 354)
        (height 492))
    (bv-themes-regression--section
     tokens 'feed "Feed rows" x y width height
     (concat
      (bv-themes-regression--text-token
       tokens (+ x 24) (+ y 68)
       "Elfeed-like inbox snapshot" 'fg-muted 13)
      (bv-themes-regression--feed-row
       tokens (+ x 18) (+ y 86) 318 'bg-completion-current
       "UNREAD" 'warning-strong
       "Theme compiler notes" "emacs-devel" "09:15" "emacs theme")
      (bv-themes-regression--feed-row
       tokens (+ x 18) (+ y 154) 318 'bg-main
       "UNREAD" 'info-strong
       "Color science digest" "graphics" "08:40" "oklch")
      (bv-themes-regression--feed-row
       tokens (+ x 18) (+ y 222) 318 'bg-main
       "READ" 'fg-dim
       "Completion UI patterns" "ux-notes" "yday" "workflow")
      (bv-themes-regression--feed-row
       tokens (+ x 18) (+ y 290) 318 'bg-main
       "READ" 'fg-dim
       "Terminal palettes" "shell" "Mon" "ansi")
      (bv-themes-regression--text-runs
       tokens (+ x 24) (+ y 404)
       '(("unread" warning-strong :weight "700")
         ("  selected" fg-main)
         ("  read" fg-dim)
         ("  source" fg-muted)
         ("  tag" fg-salient :weight "700"))
       12)))))

(defun bv-themes-regression-svg (theme)
  "Return deterministic SVG artifact for THEME."
  (let* ((artifact (bv-themes-compile theme))
         (tokens (plist-get artifact :tokens))
         (svg nil))
    (bv-themes-regression--validate-tokens tokens)
    (setq svg
          (concat
           (format (concat "<svg xmlns='http://www.w3.org/2000/svg' "
                           "width='%d' height='%d' viewBox='0 0 %d %d'>\n")
                   bv-themes-regression-width
                   bv-themes-regression-height
                   bv-themes-regression-width
                   bv-themes-regression-height)
           (bv-themes-regression--rect-token
            tokens 0 0 bv-themes-regression-width
            bv-themes-regression-height 'bg-main)
           (bv-themes-regression--modeline-header-section
            theme artifact tokens)
           (bv-themes-regression--org-section tokens)
           (bv-themes-regression--code-section tokens)
           (bv-themes-regression--diff-section tokens)
           (bv-themes-regression--dired-section tokens)
           (bv-themes-regression--completion-section tokens)
           (bv-themes-regression--diagnostics-section tokens)
           (bv-themes-regression--terminal-section tokens)
           (bv-themes-regression--calendar-section tokens)
           (bv-themes-regression--feed-section tokens)
           "</svg>\n"))
    (bv-themes-regression-validate-svg svg)
    svg))

(defun bv-themes-regression--count-matches (pattern string)
  "Return number of PATTERN matches in STRING."
  (let ((start 0)
        (count 0))
    (while (string-match pattern string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun bv-themes-regression--section-ids ()
  "Return required regression section identifiers."
  (mapcar #'car bv-themes-regression-required-sections))

(defun bv-themes-regression-validate-svg (svg)
  "Validate SVG and return artifact metadata.
Validation checks required workflow sections, a non-trivial element
shape, and the absence of external image/link dependencies."
  (let* ((sections (bv-themes-regression--section-ids))
         (missing (cl-remove-if
                   (lambda (section)
                     (string-match-p
                      (format "data-section='%s'" section)
                      svg))
                   sections))
         (bytes (string-bytes svg))
         (rects (bv-themes-regression--count-matches "<rect\\_>" svg))
         (texts (bv-themes-regression--count-matches "<text\\_>" svg)))
    (unless (and (string-prefix-p "<svg " svg)
                 (string-suffix-p "</svg>\n" svg))
      (error "Theme regression artifact is not a complete SVG"))
    (when missing
      (error "Theme regression artifact missing sections: %S" missing))
    (when (or (string-match-p "<image\\_>" svg)
              (string-match-p "\\(?:href\\|xlink:href\\)=" svg))
      (error "Theme regression artifact must not reference external assets"))
    (when (< bytes bv-themes-regression-min-bytes)
      (error "Theme regression artifact is unexpectedly small: %d bytes"
             bytes))
    (when (< rects bv-themes-regression-min-rects)
      (error "Theme regression artifact has too few rects: %d" rects))
    (when (< texts bv-themes-regression-min-texts)
      (error "Theme regression artifact has too few text nodes: %d" texts))
    (list :bytes bytes
          :rects rects
          :texts texts
          :sections sections)))

(defun bv-themes-regression-validate-file (file)
  "Validate regression artifact FILE and return artifact metadata."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (bv-themes-regression-validate-svg (buffer-string))))

(defun bv-themes-regression--file-sha256 (file)
  "Return SHA256 hash of FILE contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun bv-themes-regression-write (theme directory)
  "Write regression artifact for THEME into DIRECTORY."
  (make-directory directory t)
  (let* ((file (expand-file-name (format "%s.svg" theme) directory))
         (svg (bv-themes-regression-svg theme)))
    (write-region svg nil file nil 'silent)
    (bv-themes-regression-validate-file file)
    file))

(defun bv-themes-regression-run (&optional directory)
  "Generate and validate deterministic visual artifacts in DIRECTORY."
  (interactive)
  (when (fboundp 'bv-themes-load-gallery)
    (bv-themes-load-gallery))
  (let* ((directory (or directory
                        (expand-file-name
                         "bv-theme-regression"
                         temporary-file-directory)))
         (themes (or bv-themes-regression-themes
                     (and (fboundp 'bv-themes-known-themes)
                          (bv-themes-known-themes))
                     (mapcar #'car bv-themes-token-profiles)))
         (files nil)
         (manifest nil))
    (unless themes
      (error "No BV themes are available for regression artifacts"))
    (dolist (theme themes)
      (when (fboundp 'bv-themes-ensure-theme)
        (bv-themes-ensure-theme theme))
      (let* ((audit (bv-themes-audit theme t))
             (file (bv-themes-regression-write theme directory))
             (metadata (bv-themes-regression-validate-file file))
             (hash (bv-themes-regression--file-sha256 file))
             (entry (append (list :theme theme
                                  :file file
                                  :sha256 hash
                                  :audit-invariants
                                  (length (plist-get audit :invariants)))
                            metadata)))
        (push file files)
        (push entry manifest)))
    (setq manifest (nreverse manifest))
    (write-region
     (concat (prin1-to-string manifest) "\n")
     nil
     (expand-file-name "manifest.el" directory)
     nil
     'silent)
    (when (called-interactively-p 'interactive)
      (message "BV theme regression artifacts: %s" directory))
    (list :directory directory
          :files (nreverse files)
          :manifest manifest)))

(provide 'bv-themes-regression)
;;; bv-themes-regression.el ends here
