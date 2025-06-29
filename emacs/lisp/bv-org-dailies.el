;;; bv-org-dailies.el --- Daily journaling configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for org-roam daily journaling.

;;; Code:


(autoload 'org-roam-dailies-map "org-roam-dailies" "" nil 'keymap)

(when (boundp 'mode-specific-map)
  (define-key mode-specific-map (kbd "d") 'org-roam-dailies-map))

(with-eval-after-load 'org-roam-dailies
  (when (boundp 'org-roam-dailies-directory)
    (setq org-roam-dailies-directory "daily/")))

(provide 'bv-org-dailies)
;;; bv-org-dailies.el ends here