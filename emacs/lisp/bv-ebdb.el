;;; bv-ebdb.el --- Contact database configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Contact management with EBDB.

;;; Code:

(declare-function ebdb-display-all-records "ebdb")
(declare-function ebdb-create-record-extended "ebdb")
(declare-function ebdb-search "ebdb")
(declare-function ebdb-mail "ebdb")
(declare-function ebdb-prompt-for-string "ebdb")

(defgroup bv-ebdb nil
  "Contact database settings."
  :group 'bv)

(defcustom bv-ebdb-idle-delay 2.0
  "Idle time before loading EBDB."
  :type 'number
  :group 'bv-ebdb)

(defcustom bv-ebdb-sources (list (expand-file-name "emacs/contacts.db" (or (getenv "XDG_DATA_HOME") "~/.local/share")))
  "List of EBDB database files."
  :type '(repeat string)
  :group 'bv-ebdb)

;; Load EBDB after idle delay
(run-with-idle-timer bv-ebdb-idle-delay t
                     (lambda ()
                       (require 'ebdb nil t)))

(setq ebdb-sources bv-ebdb-sources
      ebdb-default-country nil
      ebdb-default-window-size 0.3
      ebdb-dedicated-window 'ebdb
      ebdb-mail-avoid-redundancy t
      ebdb-complete-mail 'capf
      ebdb-completion-display-record nil
      ebdb-complete-mail-allow-cycling nil
      ebdb-save-on-exit t)

(with-eval-after-load 'ebdb
  (require 'ebdb-i18n)
  (require 'ebdb-vcard)
  (require 'ebdb-org)
  (require 'ebdb-ispell))

(defun bv-ebdb-search-name ()
  "Search contacts by name."
  (interactive)
  (ebdb-search (ebdb-prompt-for-string "Name: ") nil))

(defun bv-ebdb-search-email ()
  "Search contacts by email."
  (interactive)
  (ebdb-search nil (ebdb-prompt-for-string "Email: ")))

(defun bv-ebdb-add-from-mail ()
  "Add sender of current email to contacts."
  (interactive)
  (when (derived-mode-p 'message-mode 'mail-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^From: \\(.*\\)" nil t)
        (let ((from (match-string 1)))
          (ebdb-create-record-extended from))))))

(with-eval-after-load 'ebdb
  (define-key ebdb-mode-map "q" 'quit-window)
  (define-key ebdb-mode-map "/" 'bv-ebdb-search-name)
  (define-key ebdb-mode-map "@" 'bv-ebdb-search-email))


(provide 'bv-ebdb)
;;; bv-ebdb.el ends here