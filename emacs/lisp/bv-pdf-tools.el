;;; bv-pdf-tools.el --- PDF viewing and tools configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for PDF tools and viewing.

;;; Code:

(require 'seq)

(autoload 'pdf-view-mode "pdf-view")
(autoload 'pdf-view-themed-minor-mode "pdf-view")
(autoload 'pdf-tools-enable-minor-modes "pdf-tools")
(autoload 'bv-modus-themes--dark-theme-p "bv-modus-themes")

(with-eval-after-load 'tex
  (when (boundp 'TeX-view-program-selection)
    (setopt TeX-view-program-selection '((output-pdf "PDF Tools"))))
  (add-hook 'TeX-mode-hook 'TeX-source-correlate-mode))

(defun bv-pdf-tools--list-buffers ()
  "List all currently-opened `pdf-view' mode buffers.
Returns a list of all buffers that are currently in `pdf-view-mode'."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'pdf-view-mode)))
   (buffer-list)))

(defun bv-pdf-tools-update-buffers (&optional _theme)
  "Apply `bv-pdf-tools-mode' to currently open `pdf-view' mode buffers.
Optional argument _THEME is ignored but kept for compatibility with
theme change hooks."
  (dolist (buffer (bv-pdf-tools--list-buffers))
    (with-current-buffer buffer
      (bv-pdf-tools-mode 1))))

(defgroup bv-pdf-tools nil
  "Custom tweaks for PDF Tools."
  :group 'bv)

(define-minor-mode bv-pdf-tools-mode
  "Apply `pdf-tools' settings based on the current theme.
This minor mode automatically enables or disables `pdf-view-themed-minor-mode'
based on whether the current theme is dark or light."
  :group 'bv-pdf-tools
  (if bv-pdf-tools-mode
      (if (bv-modus-themes--dark-theme-p)
          (pdf-view-themed-minor-mode 1)
        (pdf-view-themed-minor-mode -1))
    (pdf-view-themed-minor-mode -1)))

(when (boundp 'pdf-view-mode-hook)
  (add-hook 'pdf-view-mode-hook 'bv-pdf-tools-mode))
(when (boundp 'circadian-after-load-theme-hook)
  (add-hook 'circadian-after-load-theme-hook 'bv-pdf-tools-update-buffers))

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

(when (boundp 'pdf-view-mode-hook)
  (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes))

(with-eval-after-load 'pdf-view
  (when (boundp 'pdf-view-use-scaling)
    (setq pdf-view-use-scaling t))
  (when (boundp 'pdf-view-display-size)
    (setq pdf-view-display-size 'fit-page))
  (when (boundp 'pdf-view-resize-factor)
    (setq pdf-view-resize-factor 1.025)))

(with-eval-after-load 'saveplace
  (require 'saveplace-pdf-view))

(provide 'bv-pdf-tools)
;;; bv-pdf-tools.el ends here