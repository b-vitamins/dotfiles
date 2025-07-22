;;; bv-embark.el --- Embark configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for embark - contextual actions on completions.

;;; Code:

(require 'embark)

;; Key bindings
(global-set-key (kbd "s-.") 'embark-act)
(global-set-key (kbd "s->") 'embark-become)

(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "s-g") 'embark-become))

;; Integration with consult
(with-eval-after-load 'consult
  (require 'embark-consult))

(provide 'bv-embark)
;;; bv-embark.el ends here