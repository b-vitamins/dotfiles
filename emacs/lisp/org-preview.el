;;; org-preview.el --- Fast, async LaTeX previews for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur
;; Copyright (C) 2024  Ayan Das

;; Authors: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;;          Ayan Das <bvits@riseup.net>
;; URL: https://github.com/karthink/org-preview
;; Keywords: tex, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, turn on `org-preview-mode'.

;; TODOs for org-preview.el
;; [X] Basic equation numbering in previews
;; [X] Basic color matching (Match default foreground/background of the theme)
;; [X] Async support for dvisvgm.  (Only supports dvipng right now).
;; [ ] Async support for imagemagick.
;; [-] Code organization, refactor into task-specific functions?
;; [ ] Add image caching.
;; [ ] Async method (process sentinels vs org-async vs custom macro)
;; [ ] Equation ↔ Image mismatch handling
;; [ ] Overlay placement mismatch handling (async problem)
;; [ ] Scale matching
;; [ ] Advanced color matching (Match colors at point)
;; [ ] Formula ↔ Text baseline alignment (for png, svg)
;; [ ] Preamble precompilation
;; [ ] Automatic preview toggle on cursor movement
;; [ ] [Advanced] Consistent equation numbering by section in previews.
;; [ ] Try one TeX run + multiple image-converter processes (more overhead, but threading!)
;; [ ] Fix Org’s code verbosity with macros from Emacs 26+ (when-let, pcase-let, etc)

;;; Code:

(require 'map)
(require 'org)
(require 'org-element)

(defvar org-preview--debug-msg t)
(defvar org-preview--log-buf "*Org Preview Log*")
(defvar org-preview--dvipng-latex-compiler nil)
(defvar org-preview--dvipng-image-converter nil)
(defvar org-preview--dvipng-transparent-image-compiler nil)
(defvar org-preview--dvisvgm-latex-compiler nil)
(defvar org-preview--dvisvgm-image-converter nil)

(defsubst org-preview-report (msg start-time)
  "Log MSG with elapsed time since START-TIME to `org-preview--log-buf'.
Only logs if `org-preview--debug-msg` is non-nil."
  (when org-preview--debug-msg
    (let ((time-elapsed (format-time-string "%H:%M:%S" (time-since start-time) t)))
      (with-current-buffer (get-buffer-create org-preview--log-buf)
        (goto-char (point-max))
        (insert (format "%s: %s\n" time-elapsed msg))))))

(defsubst org-preview--get (&rest keys)
  "Retrieve nested elements from `org-preview-latex-process-alist' using KEYS.
KEYS are a sequence of keys used to access nested values within the alist.
This function navigates through the nested association list
`org-preview-latex-process-alist' by sequentially accessing each key
in KEYS.  It returns the value found at the nested depth, or nil
if any key in the sequence does not match."
  (map-nested-elt org-preview-latex-process-alist keys))

(defun org-preview-process-mathjax (&optional end overlays)
  "Process LaTeX fragments using MathJax until END.

Optional parameter END specifies the end of the region within which
to search for LaTeX fragments.  If nil, the function processes until
the end of the buffer.

OVERLAYS, when non-nil, specifies that images should be displayed on
top of the LaTeX source instead of replacing it."
  (let ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"))
		(while (re-search-forward math-regexp end t)
			(unless (and overlays
									 (eq (get-char-property (point) 'org-overlay-type)
											 'org-latex-overlay))
				(let* ((context (org-element-context))
							 (type (org-element-type context)))
					(when (memq type '(latex-environment latex-fragment))
						(let ((value (org-element-property :value context))
									(beg (org-element-property :begin context))
									(end (save-excursion
												 (goto-char (org-element-property :end context))
												 (skip-chars-backward " \r\t\n")
												 (point))))
							(if (not (string-match "\\`\\$\\$?" value))
									(goto-char end)
								(delete-region beg end)
								(if (string= (match-string 0 value) "$$")
										(insert "\\[" (substring value 2 -2) "\\]")
									(insert "\\(" (substring value 1 -1) "\\)"))))))))))

(defun org-preview-process-html (&optional end overlays)
  "Process LaTeX fragments using HTML until END.

Optional parameter END specifies the end of the region within which
to search for LaTeX fragments.  If nil, the function processes until
the end of the buffer.

OVERLAYS, when non-nil, specifies that images should be displayed on
top of the LaTeX source instead of replacing it."
  (let ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"))
    (while (re-search-forward math-regexp end t)
      (unless (and overlays
                   (eq (get-char-property (point) 'org-overlay-type)
                       'org-latex-overlay))
        (let* ((context (org-element-context))
               (type (org-element-type context)))
          (when (memq type '(latex-environment latex-fragment))
            (let ((value (org-element-property :value context))
									(beg (org-element-property :begin context))
									(end (save-excursion
												 (goto-char (org-element-property :end context))
												 (skip-chars-backward " \r\t\n")
												 (point))))
              (goto-char beg)
							(delete-region beg end)
							(insert (org-format-latex-as-html value)))))))))

(defun org-preview-process-mathml (prefix &optional end overlays dir msg)
  "Process LaTeX fragments using MathML until END.

PREFIX specifies a base directory or filename prefix for any files generated.
Optional parameter END specifies the end of the region within which
to search for LaTeX fragments.  If nil, the function processes until
the end of the buffer.

OVERLAYS, when non-nil, specifies that images should be displayed on
top of the LaTeX source instead of replacing it.

DIR is an optional directory path to store generated files.

MSG, if non-nil, specifies a message to display during processing."
  (let ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
				(cnt 0))
    (while (re-search-forward math-regexp end t)
			(unless (and overlays
									 (eq (get-char-property (point) 'org-overlay-type)
											 'org-latex-overlay))
				(let* ((context (org-element-context))
							 (type (org-element-type context)))
					(when (memq type '(latex-environment latex-fragment))
						(let ((block-type (eq type 'latex-environment))
									(value (org-element-property :value context))
									(beg (org-element-property :begin context))
									(end (save-excursion
												 (goto-char (org-element-property :end context))
												 (skip-chars-backward " \r\t\n")
												 (point))))
							(unless (org-format-latex-mathml-available-p)
								(user-error "LaTeX to MathML converter not configured"))
							(cl-incf cnt)
							(when msg (message msg cnt))
							(goto-char beg)
							(delete-region beg end)
							(insert (org-format-latex-as-mathml
											 value block-type prefix dir)))))))))

(defun org-preview-process-imagemagick ()
  "Raise an error to indicate that ImageMagick based previews are not supported."
  (user-error "Imagemagick based previews are currently not supported.\nPlease customize `org-preview-latex-default-process'"))

(defun org-preview-ensure-directory (prefix dir &optional checkdir-flag)
  "Ensure the directory specified by PREFIX and DIR exists.
PREFIX and DIR are concatenated to form the full path of the directory.
If the directory does not exist and CHECKDIR-FLAG is not set,
it will be created.  CHECKDIR-FLAG is used to prevent redundant
checks and directory creations in recursive calls or loops."
  (let ((absprefix (expand-file-name prefix dir)))
    (unless checkdir-flag ; Prevent redundant directory checks and creations.
	    (setq checkdir-flag t)
	    (let ((todir (file-name-directory absprefix)))
	      (unless (file-directory-p todir)
					(make-directory todir t))))
    absprefix))

(defun determine-color (option face forbuffer)
  "Determine the color based on OPTION, FACE context, and FORBUFFER setting."
	(let ((color (plist-get org-format-latex-options
												  option)))
    (if forbuffer
        (cond
         ((eq color 'auto)
          (face-attribute face option nil 'default))
         ((eq color 'default)
          (face-attribute 'default option nil))
         (t color))
      color)))

(defun prepare-options (fg bg)
  "Prepare options for use in `org-preview-process-generic' using FG and BG."
	(org-combine-plists org-format-latex-options
											`(:foreground ,fg :background ,bg)))

(defun collect-latex-fragments (end overlays &optional forbuffer)
  "Collect LaTeX fragments up to END, considering OVERLAYS for processing.
END specifies the end of the region within which to search for LaTeX fragments.
OVERLAYS, when non-nil, specifies that the LaTeX fragments should consider
any existing overlays.  FORBUFFER, if non-nil, modifies the color determination
process to be suitable for buffer-specific settings."
  (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
         (face (face-at-point))
				 (fg (determine-color :foreground face forbuffer))
         (bg (determine-color :background face forbuffer))
         (math-text nil)
         (math-locations nil)
         (math-hashes nil))
    (save-excursion
      (while (re-search-forward math-regexp end t)
        (unless (and overlays
                     (eq (get-char-property (point) 'org-overlay-type)
                         'org-latex-overlay))
          (let* ((context (org-element-context))
                 (type (org-element-type context)))
            (when (memq type '(latex-environment latex-fragment))
              (let* ((value (org-element-property :value context))
                     (block-beg (org-element-property :begin context))
                     (block-end (save-excursion
                                  (goto-char (org-element-property :end context))
                                  (skip-chars-backward " \r\t\n")
                                  (point)))
                     (hash (sha1 (prin1-to-string
																	(list org-format-latex-header
																				org-latex-default-packages-alist
																				org-latex-packages-alist
																				org-format-latex-options
																				forbuffer value fg bg)))))
                (push value math-text)
                (push (cons block-beg block-end) math-locations)
                (push hash math-hashes)))))))
    (list math-text math-locations math-hashes)))

(defun set-tex-process-sentinels (tex-process start-time)
  "Set sentinels for TEX-PROCESS to handle its completion.
The sentinel logs a success message if TEX-PROCESS completes
without errors, or logs an error and reports it if any issues
occur during processing.

TEX-PROCESS is the process object for the LaTeX processing.
START-TIME should be the time at which TEX-PROCESS was initiated."
  (set-process-sentinel
   tex-process
   (lambda (proc signal)
     (when (not (process-live-p proc))
       (if (string-match-p "finished" signal)
           (message "LaTeX processing completed successfully.")
         (progn
           (message "Error in LaTeX processing.")
           (org-preview-report "LaTeX processing error" start-time)))))))

(defun set-image-process-sentinels (texfilebase image-process absprefix start-time fragments &optional processing-type overlays)
	"Set sentinels for IMAGE-PROCESS to handle its completion.
TEXFILEBASE is the base directory used for storing the image files.
IMAGE-PROCESS is the process object for the image creation.
ABSPREFIX is the absolute path prefix used for image file locations.
START-TIME is the time at which processing started, used for reporting.
FRAGMENTS contains LaTeX fragments' text, their locations, and hashes.
PROCESSING-TYPE is an optional argument how images are processed,
affecting image output types.
OVERLAYS, if non-nil, enables overlay handling rather than replacing text.

When IMAGE-PROCESS completes:
- It checks for image files generated, copies them to new locations,
and replaces or overlays them in the buffer.
- Logs processing details if debugging is enabled.
- Cleans up generated image files."
	(let*
			((processing-info (cdr (assq processing-type org-preview-latex-process-alist)))
       (image-output-type (or (plist-get processing-info :image-output-type) "png"))
       (image-input-type (or (plist-get processing-info :image-input-type) "dvi"))
       (math-locations (cadr fragments))
       (math-hashes (caddr fragments))
			 (num-overlays (length math-locations)))
		(set-process-sentinel
     image-process
     (lambda (proc signal)
       (when org-preview--debug-msg
         (unless (process-live-p proc)
           (org-preview-report "DVI processing" start-time)))
       (when (string= signal "finished\n")
         (let ((images (file-expand-wildcards
                        (concat texfilebase "*." image-output-type)
                        'full)))
           (cl-loop with loc = (point)
                    for hash in (nreverse math-hashes)
                    for (block-beg . block-end) in (nreverse math-locations)
                    for image-file in images
                    for movefile = (format "%s_%s.%s" absprefix hash image-output-type)
                    do (copy-file image-file movefile 'replace)
                    do (if overlays
													 (progn
														 (dolist (o (overlays-in block-beg block-end))
															 (when (eq (overlay-get o 'org-overlay-type)
		        														 'org-latex-overlay)
																 (delete-overlay o)))
														 (org--make-preview-overlay block-beg block-end movefile image-output-type)
														 (goto-char block-end))
												 (delete-region block-beg block-end)
												 (let* ((context (org-element-context))
																(type (org-element-type context))
																(block-type (eq type 'latex-environment))
																(value (org-element-property :value context))
																(sep (and block-type "\n\n"))
																(link (concat sep "[[file:" movefile "]]" sep)))
													 (insert
														(org-add-props link
																(list 'org-latex-src
		        													(replace-regexp-in-string "\"" "" value)
		        													'org-latex-src-embed-type
		        													(if block-type 'paragraph 'character))))))
                    finally do (goto-char loc))))
       (unless (process-live-p proc)
         (mapc #'delete-file (file-expand-wildcards (concat texfilebase "*." image-output-type) 'full))
         (delete-file (concat texfilebase "." image-input-type)))
       (when org-preview--debug-msg
         (org-preview-report "Overlay placement" start-time)
         (with-current-buffer org-preview--log-buf
           (insert (format "Previews: %d, Process: %S\n\n"
                           num-overlays processing-type))))))))

(defun org-preview-process-generic (prefix &optional end dir forbuffer processing-type checkdir-flag overlays)
  "Process LaTeX fragments generically using PROCESSING-TYPE up to END.

PREFIX is used as the base for generating file names and
directories for output.  It specifies where generated files
will be stored, combined with DIR.

END specifies the end point in the buffer for processing
LaTeX fragments.  If nil, processing continues to the end of the buffer.

DIR is the directory relative to which PREFIX will be resolved.
It determines where output files are saved.

FORBUFFER indicates whether processing is adjusted for specific
buffer conditions, affecting color determination among other settings.

PROCESSING-TYPE determines the method used for converting
LaTeX fragments into output formats.

CHECKDIR-FLAG, when non-nil, prevents repeated checks and
creation of the directory where files are stored,
which is useful in recursive or batch operations.

OVERLAYS, if non-nil, specifies that images should overlay the text
instead of replacing it."
  (org-preview-ensure-directory prefix dir checkdir-flag)

  (let* ((face (face-at-point))
         (fg (determine-color :foreground face forbuffer))
         (bg (determine-color :background face forbuffer))
				 (options (prepare-options fg bg))
         (fragments (collect-latex-fragments end overlays))
         (math-text (car fragments))
         (start-time (current-time))
         (absprefix (expand-file-name prefix dir)))

    (pcase-let ((`(,texfilebase ,tex-process ,image-process)
                 (org-preview-create-formula-image
                  (mapconcat #'identity (nreverse math-text) "\n\n")
                  options forbuffer processing-type start-time)))
      (set-tex-process-sentinels tex-process start-time)
      (set-image-process-sentinels texfilebase image-process absprefix start-time fragments processing-type overlays))))

(defun org-preview-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type checkdir-flag)
  "Replace LaTeX fragments with links to an image in the specified region.

PREFIX specifies a base directory or filename prefix for any files generated.

Optional parameters:
  BEG and END specify the region to process.
  OVERLAYS, when non-nil, specifies that images should overlay the
LaTeX source instead of replacing it.
  DIR specifies where any generated files should be stored.
  MSG specifies a message to display during processing.
  FORBUFFER indicates whether the processing is for buffer-specific settings.
  PROCESSING-TYPE specifies the method of LaTeX processing to use.
  CHECKDIR-FLAG, when non-nil, prevents redundant directory checks and creation,
useful in batch operations or when multiple calls are made in succession."
  (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (unless (eq processing-type 'verbatim)
    (goto-char (or beg (point-min)))
    ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
    (when (and overlays (memq processing-type '(dvipng imagemagick)))
			(overlay-recenter (or end (point-max))))
    (cond
     ((eq processing-type 'mathjax)
			(org-preview-process-mathjax end overlays))
     ((eq processing-type 'html)
			(org-preview-process-html end overlays))
     ((eq processing-type 'mathml)
			(org-preview-process-mathml prefix end overlays dir msg))
		 ((eq processing-type 'imagemagick)
			(org-preview-process-imagemagick))
		 ((assq processing-type org-preview-latex-process-alist)
			(org-preview-process-generic prefix end dir forbuffer processing-type checkdir-flag overlays))
     ((assq processing-type org-preview-latex-process-alist)
			(org-preview-process-generic prefix end dir forbuffer processing-type checkdir-flag overlays))
     (t (error "Unknown conversion process %s for LaTeX fragments" processing-type)))))

(defun get-latex-header (processing-info)
  "Use PROCESSING-INFO to retrieve and return the latex header."
	(or (plist-get processing-info :latex-header)
			(org-latex-make-preamble
			 (org-export-get-environment (org-export-get-backend 'latex))
			 org-format-latex-header
			 'snippet)))

(defun get-image-properties (processing-info options buffer)
  "Setup and return image properties based on PROCESSING-INFO, OPTIONS and BUFFER."
  (let* ((image-size-adjust (or (plist-get processing-info :image-size-adjust)
																'(1.0 . 1.0)))
				 (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
									 (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
				 (dpi (* scale (if buffer (org--get-display-dpi) 140.0)))
				 (fg (or (plist-get options (if buffer :foreground :html-foreground))
								 "Black"))
				 (bg (or (plist-get options (if buffer :background :html-background))
								 "Transparent")))
    (list dpi fg bg)))

(defun get-post-clean-items (processing-info)
  "Use PROCESSING-INFO to retrieve and return file extensions.
Files with these extensions must be deleted post preview creation."
	(or (plist-get processing-info :post-clean)
			'(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
				".svg" ".png" ".jpg" ".jpeg" ".out")))

(defun get-image-converter (processing-info background)
  "Use PROCESSING-INFO and BACKGROUND face to return the image converter."
	(or (and (string= background "Transparent")
           (plist-get processing-info :transparent-image-converter))
      (plist-get processing-info :image-converter)))

(defun create-texfile (texfile string latex-header fg bg)
  "Create a LaTeX file named TEXFILE with specified STRING.
Styling based on LATEX-HEADER, FG, and BG is also applied."
  (with-temp-file texfile
    (insert latex-header)
    (insert "\n\\begin{document}\n"
						"\\definecolor{fg}{rgb}{" fg "}%\n"
						(if bg
								(concat "\\definecolor{bg}{rgb}{" bg "}%\n"
												"\n\\pagecolor{bg}%\n")
							"")
						"\n{\\color{fg}\n"
						string
						"\n}\n"
						"\n\\end{document}\n")))

(defun initiate-processing-steps (texfilebase texfile dpi fg bg image-input-type latex-compiler image-converter start-time log-buf processing-type post-clean)
  "Initiate the LaTeX and image conversion processes for the given parameters.
This function creates and manages processes for compiling LaTeX
and converting the output to images.

Arguments:
- TEXFILEBASE: Base name for the temporary LaTeX and image files.
- TEXFILE: Full path to the temporary LaTeX file.
- DPI: Dots per inch setting for image generation.
- FG: Foreground color for the LaTeX document.
- BG: Background color for the LaTeX document.
- IMAGE-INPUT-TYPE: Type of the input file for image conversion.
- LATEX-COMPILER: Command used to compile the LaTeX file.
- IMAGE-CONVERTER: Command used to convert the LaTeX output to images.
- START-TIME: Time when the process was initiated, used for logging.
- LOG-BUF: Buffer used for logging output of the processes.
- PROCESSING-TYPE: Type of processing to be applied, affects
how images are processed.
- POST-CLEAN: List of items to be cleaned up post preview creation."
  (let* ((tex-process)
         (image-process)
         (base-name (file-name-base texfile))
         (out-dir (or (file-name-directory texfile) "./"))
         (spec `((?D . ,(shell-quote-argument (format "%s" dpi)))
								 (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
                 (?b . ,(shell-quote-argument base-name))
                 (?B . ,(shell-quote-argument texfilebase))
								 (?f . ,(shell-quote-argument texfile))
								 (?F . ,(shell-quote-argument (file-truename texfile)))
								 (?o . ,(shell-quote-argument out-dir))
								 (?O . ,(shell-quote-argument (expand-file-name
                                               (concat base-name "." image-input-type) out-dir)))
                 (?c . ,(shell-quote-argument (concat "rgb " (replace-regexp-in-string "," " " fg))))
                 (?g . ,(shell-quote-argument (concat "rgb " (replace-regexp-in-string "," " " bg)))))))
    (when org-preview--debug-msg
      (org-preview-report "Preprocessing" start-time))
    (setq tex-process
          (make-process :name (format "Org-Preview-%s" (file-name-base texfile))
                        :buffer log-buf
                        :command (split-string-shell-command (format-spec (car latex-compiler) spec))
                        :sentinel (lambda (proc _)
                                    (unless (process-live-p proc)
                                      (org-preview-report "Tex process" start-time)
                                      (dolist (e (delete (concat "." image-input-type) post-clean))
                                        (when (file-exists-p (concat texfilebase e))
                                          (delete-file (concat texfilebase e))))
                                      (org-preview-report "Tex cleanup" start-time)))))
    (process-send-string tex-process
                         (format-spec
"\\PassOptionsToPackage{noconfig,active,tightpage,auctex}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined\\RequirePackage[displaymath,floats,graphics,textmath,sections,footnotes]{preview}[2004/11/05]\\fi}\\input\\detokenize{%f}\n"
                          spec))
    (when (equal processing-type 'dvisvgm)
      (while (process-live-p tex-process)
        (accept-process-output tex-process)))
    (setq image-process
          (make-process :name (format "Org-Convert-%s-%s"
                                      (file-name-base texfile)
                                      (symbol-name processing-type))
                        :buffer (format "*Org Convert %s %s*"
                                        (file-name-base texfile)
                                        (symbol-name processing-type))
                        :command (split-string-shell-command (format-spec (car image-converter) spec))))
		(list texfilebase tex-process image-process)))

(defun org-preview-create-formula-image (string options buffer &optional processing-type start-time)
  "Create image from STRING according to specified OPTIONS and BUFFER settings.

This function generates an image from LaTeX code by:

1. Preparing LaTeX and image conversion configurations based on
PROCESSING-TYPE, which defaults to org-preview-latex-default-process.

2. Compiling LaTeX into a document using prepared settings and
converting the output to an image format.

Arguments:
- STRING: LaTeX string to be converted into an image.
- OPTIONS: A plist containing options for foreground, background,
scale, and other LaTeX settings.
- BUFFER: Boolean indicating whether the settings should consider
buffer-specific properties (like DPI).
- PROCESSING-TYPE: Type of processing to be applied, affects
how images are processed.
- START-TIME: Time when the process was initiated, used for logging."
  (let* ((processing-type (or processing-type org-preview-latex-default-process))
         (processing-info (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (post-clean (get-post-clean-items processing-info))
         (latex-header (get-latex-header processing-info))
         (latex-compiler (plist-get processing-info :latex-compiler))
				 (tmpdir (file-name-as-directory temporary-file-directory))
         (texfilebase (make-temp-name (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (config (get-image-properties processing-info options buffer))
         (dpi (car config))
         (fg (nth 1 config))
         (bg (nth 2 config))
         (image-converter (get-image-converter processing-info bg))
         (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
         (resize-mini-windows nil))

    (dolist (program programs)
      (org-check-external-command program error-message))

    (if (eq fg 'default)
        (setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))

    (setq bg (cond
              ((eq bg 'default) (org-latex-color :background))
              ((string= bg "Transparent") nil)
              (t (org-latex-color-format bg))))

		(if (and (not (string-empty-p string))
             (string-suffix-p string "\n"))
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))

    (create-texfile texfile string latex-header fg bg)
    (initiate-processing-steps texfilebase texfile dpi fg bg image-input-type latex-compiler image-converter start-time log-buf processing-type post-clean)))

(define-minor-mode org-preview-mode
  "Asynchronous and batched (much, much faster) LaTeX previews for Org-mode."
  :global t
  :version "0.10"
  :lighter nil
  :group 'org
  (if org-preview-mode
      (progn
        (setq org-preview--dvipng-latex-compiler
              (org-preview--get 'dvipng :latex-compiler))
        (setq org-preview--dvipng-image-converter
              (org-preview--get 'dvipng :image-converter))
        (setq org-preview--dvipng-transparent-image-compiler
              (org-preview--get 'dvipng :transparent-image-compiler))
        (setq org-preview--dvisvgm-latex-compiler
              (org-preview--get 'dvisvgm :latex-compiler))
        (setq org-preview--dvisvgm-image-converter
              (org-preview--get 'dvisvgm :image-converter))
        (let ((dvipng-proc (alist-get 'dvipng org-preview-latex-process-alist)))
          (setq
           dvipng-proc
           (plist-put dvipng-proc
                      :latex-compiler
                      '("latex -interaction nonstopmode -output-directory %o"))
           dvipng-proc
           (plist-put dvipng-proc
                      :image-converter
                      '("dvipng --follow -bg %g -fg %c -D %D -T tight -o %B-%%09d.png %O"))
           dvipng-proc
           (plist-put dvipng-proc
                      :transparent-image-converter
                      '("dvipng --follow -D %D -T tight -bg Transparent -fg %c -o %B-%%09d.png %O")))
          )
        (let ((dvisvgm-proc (alist-get 'dvisvgm org-preview-latex-process-alist)))
          (setq
           dvisvgm-proc
           (plist-put dvisvgm-proc
                      :latex-compiler
                      '("latex -interaction nonstopmode -output-directory %o"))
           dvisvgm-proc
           (plist-put dvisvgm-proc
                      :image-converter
                      '("dvisvgm --page=1- -n -b min -c %S -o %B-%%9p.svg %O"))))
        (advice-add 'org-format-latex :override #'org-preview-format-latex))
    (let ((dvipng-proc (alist-get 'dvipng org-preview-latex-process-alist)))
      (setq
       dvipng-proc
       (plist-put dvipng-proc :latex-compiler
                  org-preview--dvipng-latex-compiler)
       dvipng-proc
       (plist-put dvipng-proc :image-converter
                  org-preview--dvipng-image-converter)
       dvipng-proc
       (plist-put dvipng-proc :transparent-image-converter
                  org-preview--dvipng-transparent-image-compiler))
      )
    (let ((dvisvgm-proc (alist-get 'dvisvgm org-preview-latex-process-alist)))
      (setq
       dvisvgm-proc
       (plist-put dvisvgm-proc :latex-compiler
                  org-preview--dvisvgm-latex-compiler)
       dvisvgm-proc
       (plist-put dvisvgm-proc :image-converter
                  org-preview--dvisvgm-image-converter))
      )
    (advice-remove 'org-format-latex #'org-preview-format-latex)))

(provide 'org-preview)
;;; org-preview.el ends here
