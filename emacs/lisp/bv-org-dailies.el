;;; bv-org-dailies.el --- Daily notes configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Daily journaling with org-roam.
;;
;; This module configures org-roam-dailies for daily note-taking.
;; It sets up capture templates for default, morning, and evening entries,
;; and provides keybinding integration for quick access to daily notes.

;;; Code:

(defvar org-roam-dailies-directory)
(defvar org-roam-dailies-capture-templates)
(defvar mode-specific-map)
(defvar bv-app-map)

(autoload 'org-roam-dailies-map "org-roam-dailies" "Keymap for org-roam dailies commands." nil 'keymap)
(autoload 'org-roam-dailies-goto-today "org-roam-dailies" "Go to today's daily note.")
(autoload 'org-roam-dailies-capture-today "org-roam-dailies" "Capture entry for today's daily note.")
(autoload 'org-roam-dailies-goto-yesterday "org-roam-dailies" "Go to yesterday's daily note.")
(autoload 'org-roam-dailies-goto-tomorrow "org-roam-dailies" "Go to tomorrow's daily note.")
(autoload 'org-roam-dailies-goto-date "org-roam-dailies" "Go to daily note for specific date.")

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