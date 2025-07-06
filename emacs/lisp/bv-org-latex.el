;;; bv-org-latex.el --- Org mode LaTeX preview configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; LaTeX fragment preview configuration for Org mode.

;;; Code:

(defgroup bv-org-latex nil
  "LaTeX preview settings for Org mode."
  :group 'bv)

(defcustom bv-org-latex-scale 6.0
  "Scale factor for LaTeX previews."
  :type 'number
  :group 'bv-org-latex)

(defcustom bv-org-latex-auto-preview t
  "Enable automatic LaTeX preview."
  :type 'boolean
  :group 'bv-org-latex)

(defcustom bv-org-latex-preview-method 'dvipng
  "Method for rendering LaTeX previews."
  :type '(choice (const :tag "dvipng" dvipng)
                 (const :tag "dvisvgm" dvisvgm)
                 (const :tag "imagemagick" imagemagick))
  :group 'bv-org-latex)

(defun bv-org-latex-setup ()
  "Configure Org mode LaTeX preview settings."

  ;; Set XDG-compliant cache directory
  (let ((cache-dir (expand-file-name "emacs/org-latex-preview/"
                                     (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
    ;; Ensure the directory exists
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    ;; Configure Org to use this directory - need to set the image converter
    (setq org-latex-preview-ltximg-directory cache-dir)
    ;; For newer versions of Org
    (when (boundp 'org-persist-directory)
      (setq org-persist-directory (expand-file-name "emacs/org-persist/"
                                                    (or (getenv "XDG_CACHE_HOME") "~/.cache")))))

  ;; Highlight LaTeX fragments
  (setq org-highlight-latex-and-related '(latex script entities))

  ;; Fix background color for LaTeX blocks
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  ;; Configure preview options
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :foreground 'default)

  ;; Override the image directory in the format options
  (let ((cache-dir (expand-file-name "emacs/org-latex-preview/"
                                     (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
    (plist-put org-format-latex-options :image-dir cache-dir))

  ;; Set preview method
  (setq org-preview-latex-default-process bv-org-latex-preview-method)

  ;; Configure the LaTeX preamble for better rendering
  (setq org-latex-preview-preamble
        "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usepackage{arev}  % Better math font
\\pagestyle{empty}  % No page numbers
")

  ;; Enable automatic preview if configured
  (when (and bv-org-latex-auto-preview
             (fboundp 'org-latex-preview-auto-mode))
    (org-latex-preview-auto-mode 1)))

(defun bv-org-latex-increase-scale ()
  "Increase LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-scale (* bv-org-latex-scale 1.1))
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (when (derived-mode-p 'org-mode)
    (org-latex-preview '(16)))
  (message "LaTeX preview scale: %.2f" bv-org-latex-scale))

(defun bv-org-latex-decrease-scale ()
  "Decrease LaTeX preview scale."
  (interactive)
  (setq bv-org-latex-scale (/ bv-org-latex-scale 1.1))
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (when (derived-mode-p 'org-mode)
    (org-latex-preview '(16)))
  (message "LaTeX preview scale: %.2f" bv-org-latex-scale))

(defun bv-org-latex-reset-scale ()
  "Reset LaTeX preview scale to default."
  (interactive)
  (setq bv-org-latex-scale 6.0)
  (plist-put org-format-latex-options :scale bv-org-latex-scale)
  (when (derived-mode-p 'org-mode)
    (org-latex-preview '(16)))
  (message "LaTeX preview scale reset to %.2f" bv-org-latex-scale))

(defun bv-org-latex-toggle-preview ()
  "Toggle LaTeX preview at point or in region."
  (interactive)
  (if (region-active-p)
      (org-latex-preview nil)
    (org-latex-preview)))

(defun bv-org-latex-preview-all ()
  "Preview all LaTeX fragments in buffer."
  (interactive)
  (org-latex-preview '(16)))

(defun bv-org-latex-clear-previews ()
  "Clear all LaTeX previews in buffer."
  (interactive)
  (org-latex-preview '(64)))

;; Key bindings for LaTeX preview control
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x C-l") 'bv-org-latex-toggle-preview)
  (define-key org-mode-map (kbd "C-c C-x +") 'bv-org-latex-increase-scale)
  (define-key org-mode-map (kbd "C-c C-x -") 'bv-org-latex-decrease-scale)
  (define-key org-mode-map (kbd "C-c C-x 0") 'bv-org-latex-reset-scale))

;; Run setup immediately when this file loads, not just in org-mode-hook
(bv-org-latex-setup)

;; Also add to org-mode-hook for buffers opened later
(add-hook 'org-mode-hook 'bv-org-latex-setup)

;; Advice to ensure ltximg directories are created in cache
(defun bv-org-latex-preview-advice (orig-fun &rest args)
  "Ensure LaTeX preview images go to cache directory."
  (let* ((cache-dir (expand-file-name "emacs/org-latex-preview/"
                                      (or (getenv "XDG_CACHE_HOME") "~/.cache")))
         (org-preview-latex-image-directory cache-dir)
         (org-latex-preview-ltximg-directory cache-dir))
    (apply orig-fun args)))

(with-eval-after-load 'org
  (advice-add 'org-latex-preview :around #'bv-org-latex-preview-advice)
  (advice-add 'org-format-latex :around #'bv-org-latex-preview-advice))

;; Ensure settings are applied before startup options are processed
(defun bv-org-latex-startup-setup ()
  "Ensure LaTeX settings are applied before startup options."
  (bv-org-latex-setup))

(add-hook 'org-mode-hook 'bv-org-latex-startup-setup -100)  ; Run very early

(provide 'bv-org-latex)
;;; bv-org-latex.el ends here