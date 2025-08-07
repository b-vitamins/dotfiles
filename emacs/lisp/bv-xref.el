;;; bv-xref.el --- Cross-reference configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Cross-reference navigation with consult integration.

;;; Code:

(declare-function consult-xref "consult-xref" (fetcher &optional alist))

(with-eval-after-load 'xref
  (when (boundp 'xref-auto-jump-to-first-definition)
    (setq xref-auto-jump-to-first-definition 'move))
  (when (boundp 'xref-auto-jump-to-first-xref)
    (setq xref-auto-jump-to-first-xref 'move))
  (when (boundp 'xref-prompt-for-identifier)
    (setq xref-prompt-for-identifier
          '(not xref-find-definitions-other-window
                xref-find-definitions-other-frame)))
  (when (boundp 'xref-show-xrefs-function)
    (setq xref-show-xrefs-function 'consult-xref))
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function 'consult-xref)))

(provide 'bv-xref)
;;; bv-xref.el ends here