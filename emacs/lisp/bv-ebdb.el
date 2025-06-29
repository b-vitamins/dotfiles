;;; bv-ebdb.el --- EBDB configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for EBDB contact management system with keybindings
;; and customization for contact handling.

;;; Code:


(define-prefix-command 'bv-ebdb-map)

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "b") 'bv-ebdb-map))
  (when (boundp 'bv-ebdb-map)
    (let ((map bv-ebdb-map))
      (define-key map "a" 'ebdb-display-all-records)
      (define-key map "c" 'ebdb-create-record-extended))))

(with-eval-after-load 'ebdb
  (require 'ebdb-i18n)
  (require 'ebdb-vcard)
  (require 'ebdb-org)
  (require 'ebdb-ispell)
  
  (when (boundp 'ebdb-sources)
    (setq ebdb-sources (list "~/documents/contacts")))
  (when (boundp 'ebdb-default-country)
    (setq ebdb-default-country nil))
  (when (boundp 'ebdb-default-window-size)
    (setq ebdb-default-window-size 0.3))
  (when (boundp 'ebdb-dedicated-window)
    (setq ebdb-dedicated-window 'ebdb))
  (when (boundp 'ebdb-mail-avoid-redundancy)
    (setq ebdb-mail-avoid-redundancy t))
  (when (boundp 'ebdb-complete-mail)
    (setq ebdb-complete-mail 'capf))
  (when (boundp 'ebdb-completion-display-record)
    (setq ebdb-completion-display-record nil))
  (when (boundp 'ebdb-complete-mail-allow-cycling)
    (setq ebdb-complete-mail-allow-cycling nil))
  (when (boundp 'ebdb-save-on-exit)
    (setq ebdb-save-on-exit t))
  
  (when (boundp 'ebdb-mode-map)
    (define-key ebdb-mode-map "q" 'kill-this-buffer)))

(provide 'bv-ebdb)
;;; bv-ebdb.el ends here