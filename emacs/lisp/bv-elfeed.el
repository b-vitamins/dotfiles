;;; bv-elfeed.el --- Elfeed RSS configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module configures Elfeed, an RSS/Atom feed reader for Emacs.
;; It sets up keybindings, feed sources from an Org file, and integrates
;; with Org capture for quick feed management.

;;; Code:


(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (when (fboundp 'elfeed)
      (define-key bv-app-map (kbd "e") 'elfeed))))

(with-eval-after-load 'elfeed-org
  (when (boundp 'rmh-elfeed-org-files)
    (setq rmh-elfeed-org-files '("~/projects/dotfiles/feeds/feeds.org"))))

(with-eval-after-load 'org-capture
  (when (boundp 'org-capture-templates)
    (add-to-list 'org-capture-templates
                 '("e" "Elfeed" entry
                   (file+headline "~/projects/dotfiles/feeds/feeds.org" "Untagged")
                   "*** %:annotation\n"
                   :immediate-finish t))))

(with-eval-after-load 'elfeed
  (when (boundp 'elfeed-db-directory)
    (setq elfeed-db-directory
          (expand-file-name "elfeed" (getenv "XDG_STATE_HOME"))))
  (when (fboundp 'elfeed-org)
    (elfeed-org)))

(provide 'bv-elfeed)
;;; bv-elfeed.el ends here