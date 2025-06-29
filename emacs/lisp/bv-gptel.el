;;; bv-gptel.el --- GPTel configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; GPTel configuration for AI assistance with tool support
;; and Embark integration.

;;; Code:

(with-eval-after-load 'gptel
  (when (boundp 'gptel-use-tools)
    (setopt gptel-use-tools t))
  (when (boundp 'gptel-confirm-tool-calls)
    (setopt gptel-confirm-tool-calls t))
  (when (boundp 'gptel-default-mode)
    (setopt gptel-default-mode 'org-mode))
  
  (with-eval-after-load 'embark
    (when (boundp 'embark-general-map)
      (keymap-set embark-general-map "?" 'gptel-quick))))

(provide 'bv-gptel)
;;; bv-gptel.el ends here