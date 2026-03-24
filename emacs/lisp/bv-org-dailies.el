;;; bv-org-dailies.el --- Daily notes configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Daily journaling with org-slipbox.
;;
;; This module configures org-slipbox dailies for daily note-taking. It keeps
;; the existing daily note templates and keybinding integration unchanged while
;; using the org-slipbox dailies surface directly.

;;; Code:

(defvar org-slipbox-dailies-directory)
(defvar org-slipbox-dailies-capture-templates)
(defvar mode-specific-map)
(defvar bv-app-map)

(autoload 'org-slipbox-dailies-map "org-slipbox-dailies" "Keymap for org-slipbox dailies commands." nil 'keymap)
(autoload 'org-slipbox-dailies-goto-today "org-slipbox-dailies" "Go to today's daily note.")
(autoload 'org-slipbox-dailies-capture-today "org-slipbox-dailies" "Capture entry for today's daily note.")
(autoload 'org-slipbox-dailies-goto-yesterday "org-slipbox-dailies" "Go to yesterday's daily note.")
(autoload 'org-slipbox-dailies-goto-tomorrow "org-slipbox-dailies" "Go to tomorrow's daily note.")
(autoload 'org-slipbox-dailies-goto-date "org-slipbox-dailies" "Go to daily note for specific date.")

(with-eval-after-load 'org-slipbox-dailies
  (when (boundp 'org-slipbox-dailies-directory)
    (setq org-slipbox-dailies-directory "daily/"))

  (when (boundp 'org-slipbox-dailies-capture-templates)
    (setq org-slipbox-dailies-capture-templates
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
  (define-key mode-specific-map (kbd "d") 'org-slipbox-dailies-map))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "j") 'org-slipbox-dailies-goto-today)))

(provide 'bv-org-dailies)
;;; bv-org-dailies.el ends here
