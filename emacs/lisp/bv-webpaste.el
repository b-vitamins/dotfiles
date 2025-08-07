;;; bv-webpaste.el --- Web pasting service configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Share code snippets via web paste services.

;;; Code:


(declare-function webpaste-paste-buffer "webpaste")
(declare-function webpaste-paste-region "webpaste")
(declare-function webpaste-paste-buffer-or-region "webpaste")

(defvar webpaste-provider-priority)
(defvar webpaste-paste-confirmation)
(defvar webpaste-open-in-browser)
(defvar request-storage-directory)

(defgroup bv-webpaste nil
  "Web paste service settings."
  :group 'bv)

(defcustom bv-webpaste-idle-delay 2.0
  "Idle time before loading webpaste."
  :type 'number
  :group 'bv-webpaste)

(defcustom bv-webpaste-providers '("paste.rs" "ix.io" "0x0.st" "paste.mozilla.org")
  "List of paste providers in order of preference."
  :type '(repeat string)
  :group 'bv-webpaste)

;; Load webpaste after idle delay
(run-with-idle-timer bv-webpaste-idle-delay t
                     (lambda ()
                       (require 'webpaste nil t)))

(setq webpaste-provider-priority bv-webpaste-providers
      webpaste-paste-confirmation t
      webpaste-open-in-browser nil)

(with-eval-after-load 'request
  (setq request-storage-directory
        (expand-file-name "emacs/request" (or (getenv "XDG_CACHE_HOME") "~/.cache"))))

(defun bv-webpaste-paste-dwim ()
  "Paste region if active, otherwise paste buffer."
  (interactive)
  (if (use-region-p)
      (webpaste-paste-region)
    (webpaste-paste-buffer)))

(defun bv-webpaste-paste-defun ()
  "Paste current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (webpaste-paste-region)))

(with-eval-after-load 'webpaste
  (add-hook 'webpaste-return-url-hook
            (lambda (url)
              (message "Pasted to: %s" url)
              (kill-new url))))


(provide 'bv-webpaste)
;;; bv-webpaste.el ends here