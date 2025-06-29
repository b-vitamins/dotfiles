;;; bv-webpaste.el --- Webpaste configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for webpaste online pastebin service.

;;; Code:


(define-prefix-command 'bv-webpaste-map)

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "P") 'bv-webpaste-map)))

(when (boundp 'bv-webpaste-map)
  (let ((map bv-webpaste-map))
    (define-key map "b" 'webpaste-paste-buffer)
    (define-key map "r" 'webpaste-paste-region)
    (define-key map "p" 'webpaste-paste-buffer-or-region)))

(with-eval-after-load 'webpaste
  (when (boundp 'webpaste-provider-priority)
    (setq webpaste-provider-priority
          '("paste.rs" "bpa.st" "ix.io" "paste.mozilla.org")))
  (when (boundp 'webpaste-paste-confirmation)
    (setq webpaste-paste-confirmation t)))

(with-eval-after-load 'request
  (when (boundp 'request-storage-directory)
    (setq request-storage-directory
          (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/request"))))

(provide 'bv-webpaste)
;;; bv-webpaste.el ends here