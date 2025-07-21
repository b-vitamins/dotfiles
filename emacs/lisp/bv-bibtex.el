;;; bv-bibtex.el --- BibTeX configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; BibTeX configuration for bibliography management.

;;; Code:

(with-eval-after-load 'bibtex
  (when (boundp 'bibtex-dialect)
    (setq bibtex-dialect 'biblatex)))

(provide 'bv-bibtex)
;;; bv-bibtex.el ends here