;;; bv-devdocs.el --- Developer documentation  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Offline developer documentation browser.

;;; Code:

(autoload 'devdocs-lookup "devdocs")
(autoload 'devdocs-install "devdocs")
(autoload 'devdocs-peruse "devdocs")
(autoload 'devdocs-search "devdocs")

(with-eval-after-load 'devdocs
  (when (boundp 'devdocs-data-dir)
    (setq devdocs-data-dir
          (expand-file-name "devdocs"
                           (or (getenv "XDG_STATE_HOME") "~/.local/state")))))

;; Keybindings
(global-set-key (kbd "C-h D") 'devdocs-lookup)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "h") 'devdocs-lookup)
    (define-key bv-app-map (kbd "H") 'devdocs-search)))

(provide 'bv-devdocs)
;;; bv-devdocs.el ends here
