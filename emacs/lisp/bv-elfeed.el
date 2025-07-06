;;; bv-elfeed.el --- RSS feed reader configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; RSS and Atom feed reading with Elfeed.

;;; Code:

(declare-function set-face "bv-theme" (face style))

(declare-function elfeed "elfeed")
(declare-function elfeed-db-load "elfeed-db")
(declare-function elfeed-org "elfeed-org")
(declare-function elfeed-search-set-filter "elfeed-search")
(declare-function elfeed-search-tag-all-unread "elfeed-search")
(declare-function elfeed-search-untag-all-unread "elfeed-search")
(declare-function elfeed-update "elfeed")

(defgroup bv-elfeed nil
  "RSS feed reader settings."
  :group 'bv)

(defcustom bv-elfeed-idle-delay 1.5
  "Idle time before loading elfeed."
  :type 'number
  :group 'bv-elfeed)

(defcustom bv-elfeed-default-filter "@1-week-ago +unread"
  "Default search filter for elfeed."
  :type 'string
  :group 'bv-elfeed)

;; Load elfeed after idle delay
(run-with-idle-timer bv-elfeed-idle-delay t
                     (lambda ()
                       (require 'elfeed nil t)))

(setq elfeed-db-directory (expand-file-name "emacs/elfeed" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
      elfeed-enclosure-default-dir (expand-file-name "emacs/elfeed/enclosures" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
      elfeed-show-entry-switch 'pop-to-buffer
      elfeed-search-filter bv-elfeed-default-filter
      elfeed-search-title-max-width 80
      elfeed-search-title-min-width 30
      elfeed-search-date-format '("%m-%d %H:%M" 11 :left))

(with-eval-after-load 'elfeed
  (require 'elfeed-org)
  (setq rmh-elfeed-org-files
        (list (expand-file-name "emacs/feeds.org" (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
  (elfeed-org)
  (elfeed-db-load))

(defun bv-elfeed-search-starred ()
  "Show starred entries."
  (interactive)
  (elfeed-search-set-filter "@6-months-ago +starred"))

(defun bv-elfeed-search-all ()
  "Show all entries."
  (interactive)
  (elfeed-search-set-filter "@1-month-ago"))

(defun bv-elfeed-search-today ()
  "Show today's entries."
  (interactive)
  (elfeed-search-set-filter "@1-day-ago"))

(defun bv-elfeed-mark-all-read ()
  "Mark all visible entries as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun bv-elfeed-toggle-star ()
  "Toggle starred status of entry at point."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             if (elfeed-tagged-p 'starred entry)
             do (elfeed-untag entry 'starred)
             else do (elfeed-tag entry 'starred))
    (mapc #'elfeed-search-update-entry entries)))

(defun bv-elfeed-show-eww ()
  "Open current entry in eww."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww link))))

(with-eval-after-load 'elfeed
  (define-key elfeed-search-mode-map "q" 'quit-window)
  (define-key elfeed-search-mode-map "s" 'bv-elfeed-search-starred)
  (define-key elfeed-search-mode-map "a" 'bv-elfeed-search-all)
  (define-key elfeed-search-mode-map "t" 'bv-elfeed-search-today)
  (define-key elfeed-search-mode-map "A" 'bv-elfeed-mark-all-read)
  (define-key elfeed-search-mode-map "m" 'bv-elfeed-toggle-star)
  (define-key elfeed-search-mode-map "u" 'elfeed-search-tag-all-unread)
  (define-key elfeed-search-mode-map "g" 'elfeed-update)
  (define-key elfeed-show-mode-map "q" 'quit-window)
  (define-key elfeed-show-mode-map "w" 'bv-elfeed-show-eww))

(defun bv-elfeed-setup-faces ()
  "Configure elfeed faces."
  (require 'bv-theme nil t)
  (if (fboundp 'set-face)
      (progn
        (when (facep 'elfeed-search-title-face)
          (set-face 'elfeed-search-title-face 'default))
        (when (facep 'elfeed-search-unread-title-face)
          (set-face 'elfeed-search-unread-title-face 'bold)))
    ;; Fallback if set-face is not available
    (when (facep 'elfeed-search-title-face)
      (set-face-attribute 'elfeed-search-title-face nil
                          :inherit 'default))
    (when (facep 'elfeed-search-unread-title-face)
      (set-face-attribute 'elfeed-search-unread-title-face nil
                          :inherit 'bold)))
  (when (facep 'elfeed-search-date-face)
    (set-face-attribute 'elfeed-search-date-face nil
                        :inherit 'font-lock-comment-face))
  (when (facep 'elfeed-search-feed-face)
    (set-face-attribute 'elfeed-search-feed-face nil
                        :inherit 'font-lock-keyword-face))
  (when (facep 'elfeed-search-tag-face)
    (set-face-attribute 'elfeed-search-tag-face nil
                        :inherit 'font-lock-type-face)))

(with-eval-after-load 'elfeed
  (bv-elfeed-setup-faces)
  (add-hook 'elfeed-search-mode-hook 'bv-elfeed-setup-faces))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("w" "Web feed" entry
                 (file+headline "~/org/feeds.org" "Inbox")
                 "* %:annotation\n%U\n%:description"
                 :immediate-finish t)))

(global-set-key (kbd "C-x w") 'elfeed)

(provide 'bv-elfeed)
;;; bv-elfeed.el ends here