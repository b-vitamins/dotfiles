;;; bv-org-agenda-files-track.el --- Dynamic agenda file tracking  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Automatic org agenda file discovery with org-ql.

;;; Code:

(declare-function org-ql-select "org-ql" (buffers-or-files query &rest rest))
(declare-function org-agenda-files "org" (&optional unrestricted archives))
(defvar org-agenda-include-diary)

(when (boundp 'org-agenda-include-diary)
  (setq org-agenda-include-diary nil))

(provide 'bv-org-agenda-files-track)
;;; bv-org-agenda-files-track.el ends here