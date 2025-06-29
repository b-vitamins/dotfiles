;;; bv-startup.el --- Startup warning configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for startup warnings about conflicting directories.

;;; Code:

;;;###autoload
(when (file-exists-p "~/.emacs.d")
  (display-warning 'bv-startup
                   "~/.emacs.d exists, emacs may load the wrong init.el"))

(provide 'bv-startup)
;;; bv-startup.el ends here