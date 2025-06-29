;;; bv-info.el --- Info configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for Info mode with enhanced faces and typography.
;; Provides variable-pitch fonts and improved reference item styling.

;;; Code:

(eval-when-compile
  (require 'modus-themes)
  (require 'cl-seq))

(require 'modus-operandi-deuteranopia-theme)

(autoload 'modus-themes--retrieve-palette-value "modus-themes")
(autoload 'modus-themes--current-theme-palette "modus-themes")
(autoload 'modus-themes--current-theme "modus-themes")


(eval-when-compile
  (enable-theme 'modus-operandi-deuteranopia))

(defun bv-info-set-custom-faces ()
  "Apply more pleasant faces to `Info-mode' and `Info+-mode'."
  (interactive)
  (face-remap-add-relative 'default :inherit 'variable-pitch)
  (when (modus-themes--current-theme)
    (modus-themes-with-colors
      (custom-set-faces
       `(info-reference-item ((,c :background unspecified)))
       `(info-function-ref-item ((,c :background unspecified)))
       `(info-command-ref-item ((,c :background unspecified)))
       `(info-macro-ref-item ((,c :background unspecified)))
       `(info-variable-ref-item ((,c :background unspecified)))))))

(when (boundp 'Info-mode-hook)
  (add-hook 'Info-mode-hook 'bv-info-set-custom-faces))

(with-eval-after-load 'info
  (when (boundp 'Info-mode-map)
    (define-key Info-mode-map "q" 'kill-this-buffer))
  (when (boundp 'Info-use-header-line)
    (setq Info-use-header-line nil))
  (require 'info+)
  (when (boundp 'Info-mode-hook)
    (add-hook 'Info-mode-hook 'visual-line-mode))
  (when (boundp 'info-manual+node-buffer-name-mode)
    (setq info-manual+node-buffer-name-mode t))
  (when (boundp 'Info-persist-history-mode)
    (setq Info-persist-history-mode t))
  (when (boundp 'Info-fontify-isolated-quote-flag)
    (setq Info-fontify-isolated-quote-flag nil))
  (when (boundp 'Info-fontify-reference-items-flag)
    (setq Info-fontify-reference-items-flag t))
  (when (boundp 'Info-fontify-quotations)
    (setq Info-fontify-quotations t))
  (when (boundp 'Info-breadcrumbs-in-mode-line-mode)
    (setq Info-breadcrumbs-in-mode-line-mode nil)))

(provide 'bv-info)
;;; bv-info.el ends here