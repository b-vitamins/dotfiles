;;; bv-org-agenda-files-track.el --- Dynamic agenda file tracking  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Automatic org agenda file discovery with org-ql.

;;; Code:

(require 'org-agenda-files-track-ql)
(autoload 'org-ql--normalize-query "org-ql")

(when (boundp 'org-agenda-include-diary)
  (setq org-agenda-include-diary nil))

(provide 'bv-org-agenda-files-track)
;;; bv-org-agenda-files-track.el ends here