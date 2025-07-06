;;; bv-info.el --- Info reader configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced Info documentation reader.

;;; Code:

(defun bv-info-setup-faces ()
  "Clean faces for Info mode."
  (face-remap-add-relative 'default :inherit 'variable-pitch)
  (face-remap-add-relative 'Info-quoted :inherit 'fixed-pitch))

(add-hook 'Info-mode-hook #'bv-info-setup-faces)
(add-hook 'Info-mode-hook #'visual-line-mode)

(with-eval-after-load 'info
  (when (boundp 'Info-mode-map)
    (define-key Info-mode-map "q" 'quit-window))
  (when (boundp 'Info-use-header-line)
    (setq Info-use-header-line nil))
  (when (boundp 'Info-hide-note-references)
    (setq Info-hide-note-references t))

  ;; Info+ enhancements
  (require 'info+ nil t)
  (when (featurep 'info+)
    (when (boundp 'Info-fontify-isolated-quote-flag)
      (setq Info-fontify-isolated-quote-flag nil))
    (when (boundp 'Info-fontify-reference-items-flag)
      (setq Info-fontify-reference-items-flag t))))

(provide 'bv-info)
;;; bv-info.el ends here