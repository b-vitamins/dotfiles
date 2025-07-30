;;; bv-pdf-tools.el --- PDF viewing configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced PDF viewing with theme integration.

;;; Code:

(autoload 'pdf-view-mode "pdf-view")
(autoload 'pdf-view-themed-minor-mode "pdf-view")
(autoload 'pdf-tools-enable-minor-modes "pdf-tools")
(autoload 'pdf-tools-install "pdf-tools")

;; Auto mode
(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

;; PDF viewing setup
(defun bv-pdf-tools-setup ()
  "Setup PDF tools."
  (pdf-tools-enable-minor-modes)
  ;; Don't use themed mode by default - it inverts colors including figures
  (pdf-view-themed-minor-mode -1))

(defun bv-pdf-tools-install-and-setup ()
  "Install PDF tools and setup the hook."
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook #'bv-pdf-tools-setup))

;; Install PDF tools on first PDF file access
(add-hook 'pdf-view-mode-hook #'bv-pdf-tools-install-and-setup)


;; PDF view settings
(with-eval-after-load 'pdf-view
  (when (boundp 'pdf-view-use-scaling)
    (setq pdf-view-use-scaling t))
  (when (boundp 'pdf-view-display-size)
    (setq pdf-view-display-size 'fit-page))
  (when (boundp 'pdf-view-resize-factor)
    (setq pdf-view-resize-factor 1.1))
  (when (boundp 'pdf-view-continuous)
    (setq pdf-view-continuous t)))

;; TeX integration
(with-eval-after-load 'tex
  (when (boundp 'TeX-view-program-selection)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))))
  (add-hook 'TeX-mode-hook 'TeX-source-correlate-mode))

;; Night mode with enhanced rendering
(defvar-local bv-pdf-night-mode nil
  "Whether night mode is enabled for current PDF buffer.")

(defun bv-pdf-toggle-night-mode ()
  "Toggle night mode with enhanced inversion for crisp text."
  (interactive)
  (setq bv-pdf-night-mode (not bv-pdf-night-mode))
  (if bv-pdf-night-mode
      (progn
        ;; High contrast colors for better text clarity
        (when (boundp 'pdf-view-midnight-colors)
          (setq-local pdf-view-midnight-colors '("#e8e8e8" . "#181818")))
        ;; Enable midnight mode with inversion
        (pdf-view-midnight-minor-mode 1)
        ;; Increase resolution for crisper text
        (when (boundp 'pdf-view-use-scaling)
          (setq-local pdf-view-use-scaling t))
        (when (boundp 'pdf-view-resolution)
          (setq-local pdf-view-resolution 144)) ; Higher DPI
        ;; Force re-render with new settings
        (pdf-view-redisplay))
    (pdf-view-midnight-minor-mode -1)
    ;; Restore default resolution
    (when (boundp 'pdf-view-resolution)
      (setq-local pdf-view-resolution 96))
    (pdf-view-redisplay))
  (message "PDF night mode %s"
           (if bv-pdf-night-mode "enabled" "disabled")))

(with-eval-after-load 'pdf-view
  (when (boundp 'pdf-view-mode-map)
    (define-key pdf-view-mode-map (kbd "n") 'bv-pdf-toggle-night-mode))
  ;; Midnight mode settings for better inversion
  (when (boundp 'pdf-view-midnight-invert)
    (setq pdf-view-midnight-invert t))
  (when (boundp 'pdf-view-midnight-hue)
    (setq pdf-view-midnight-hue nil))
  ;; Image processing settings for better quality
  (when (boundp 'pdf-view-use-imagemagick)
    (setq pdf-view-use-imagemagick t))
  (when (boundp 'pdf-view-image-relief)
    (setq pdf-view-image-relief 0))
  (when (boundp 'pdf-view-use-unicode-ligther)
    (setq pdf-view-use-unicode-ligther nil)))

;; PDF tools will be installed on first use via the hook above

(provide 'bv-pdf-tools)
;;; bv-pdf-tools.el ends here