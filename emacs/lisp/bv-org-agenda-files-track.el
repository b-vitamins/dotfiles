;;; bv-org-agenda-files-track.el --- Dynamic org agenda files tracking  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for dynamic org agenda files tracking.

;;; Code:


(autoload 'org-ql--normalize-query "org-ql")

(condition-case nil
    (require 'org-agenda-files-track-ql)
  (file-missing
   (message "org-agenda-files-track-ql not available")))

(when (boundp 'org-agenda-include-diary)
  (setq org-agenda-include-diary nil))

(provide 'bv-org-agenda-files-track)
;;; bv-org-agenda-files-track.el ends here