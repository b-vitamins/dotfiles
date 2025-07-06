;;; bv-org-dailies.el --- Daily notes configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Daily journaling with org-roam.

;;; Code:

(autoload 'org-roam-dailies-map "org-roam-dailies" "" nil 'keymap)
(autoload 'org-roam-dailies-goto-today "org-roam-dailies")
(autoload 'org-roam-dailies-capture-today "org-roam-dailies")
(autoload 'org-roam-dailies-goto-yesterday "org-roam-dailies")
(autoload 'org-roam-dailies-goto-tomorrow "org-roam-dailies")
(autoload 'org-roam-dailies-goto-date "org-roam-dailies")

(with-eval-after-load 'org-roam-dailies
  (when (boundp 'org-roam-dailies-directory)
    (setq org-roam-dailies-directory "daily/"))

  (when (boundp 'org-roam-dailies-capture-templates)
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %?"
             :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: daily\n\n"))
            ("m" "morning" entry
             "* Morning Review\n\n** What am I grateful for?\n\n** What would make today great?\n\n** Daily affirmation\n\n"
             :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: daily\n\n"))
            ("e" "evening" entry
             "* Evening Review\n\n** What went well today?\n\n** What could be improved?\n\n** What did I learn?\n\n"
             :target (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+filetags: daily\n\n"))))))

(when (boundp 'mode-specific-map)
  (define-key mode-specific-map (kbd "d") 'org-roam-dailies-map))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "j") 'org-roam-dailies-goto-today)))

(provide 'bv-org-dailies)
;;; bv-org-dailies.el ends here