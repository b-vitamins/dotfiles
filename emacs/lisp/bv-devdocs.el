;;; bv-devdocs.el --- Devdocs configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for devdocs integration to browse offline documentation.

;;; Code:


(autoload 'devdocs-lookup "devdocs")

(global-set-key (kbd "C-h D") 'devdocs-lookup)

(with-eval-after-load 'devdocs
  (when (boundp 'devdocs-data-dir)
    (setq devdocs-data-dir (concat (getenv "XDG_STATE_HOME") "/devdocs"))))

(provide 'bv-devdocs)
;;; bv-devdocs.el ends here