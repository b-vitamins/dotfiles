;;; bv-communication.el --- Communication and sharing tools -*- lexical-binding: t -*-

;;; Commentary:
;; Minimal communication tools focusing on code/text sharing and
;; optional messaging integration. Lower priority features that
;; can be enabled as needed.

;;; Code:

(require 'bv-core)

;;;; Custom Variables
(defgroup bv-communication nil
  "Communication and sharing configuration."
  :group 'bv)

;;; Webpaste - Code/Text Sharing
(defcustom bv-communication-webpaste-providers
  '("dpaste.com" "ix.io" "paste.rs")
  "List of paste providers in order of preference.
Recommended providers:
- dpaste.com: Good for code, syntax highlighting
- ix.io: Minimal, fast, command-line friendly
- paste.rs: Rust playground, good for Rust code"
  :type '(repeat string)
  :group 'bv-communication)

;;;; Configuration

;;; Webpaste - Essential for sharing code snippets
(use-package webpaste
  :defer t
  :config
  (setq webpaste-provider-priority bv-communication-webpaste-providers)
  (setq webpaste-paste-confirmation t)
  (setq webpaste-paste-raw-text t)
  (setq webpaste-return-url-type 'url)
  
  (setq webpaste-add-line-numbers nil)
  
  (setq webpaste-providers-alist
        '(("dpaste.com"
           :uri "https://dpaste.com/api/"
           :post-field "content"
           :post-type "application/x-www-form-urlencoded"
           :success-lambda (lambda () (webpaste--return-url
                                       (concat "https://dpaste.com/"
                                               (cdr (assoc 'slug (json-read)))))
           )
           :format-url (lambda (url) url))
          
          ("ix.io"
           :uri "http://ix.io"
           :post-field "f:1"
           :post-type "application/x-www-form-urlencoded"
           :success-lambda (lambda () (webpaste--return-url
                                       (buffer-substring-no-properties
                                        (point-min) (point-max))))
           :format-url (lambda (url) url))
          
          ("paste.rs"
           :uri "https://paste.rs"
           :post-type "text/plain"
           :success-lambda (lambda () (webpaste--return-url
                                       (buffer-substring-no-properties
                                        (point-min) (point-max))))
           :format-url (lambda (url) url))))
  
  :bind (("C-c w p" . webpaste-paste-region-or-buffer)
         ("C-c w r" . webpaste-paste-region)
         ("C-c w b" . webpaste-paste-buffer)))

;;; Email Integration (optional - uncomment if needed)
;; (use-package notmuch
;;   :defer t
;;   :config
;;   (setq notmuch-search-oldest-first nil)
;;   (setq notmuch-show-logo nil)
;;   (setq notmuch-message-headers-visible nil)
;;   :bind (("C-c m" . notmuch)))

;;; Simple IRC (optional - for technical discussions)
;; (use-package erc
;;   :defer t
;;   :config
;;   (setq erc-server "irc.libera.chat"
;;         erc-port 6667
;;         erc-nick "username"
;;         erc-user-full-name "Full Name"
;;         erc-autojoin-channels-alist '(("libera.chat" "#emacs"))
;;         erc-kill-buffer-on-part t
;;         erc-auto-query 'bury)
;;   :bind (("C-c i" . erc)))

;;; Link Sharing Utilities
(defun bv-communication-yank-code-link (start end)
  "Share code region with link to current file location."
  (interactive "r")
  (let* ((file (buffer-file-name))
         (line-start (line-number-at-pos start))
         (line-end (line-number-at-pos end))
         (url (if (and file (vc-git-root file))
                  (let ((remote (shell-command-to-string
                                 "git remote get-url origin")))
                    (when (string-match "github\\|gitlab" remote)
                      (format "%s#L%d-L%d"
                              (string-trim remote)
                              line-start line-end)))
                (format "%s:%d-%d" (or file "buffer") line-start line-end))))
    (kill-new url)
    (message "Link copied: %s" url)))

(defun bv-communication-share-snippet (start end)
  "Share code snippet with context."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (mode (symbol-name major-mode))
         (context (format ";;; %s snippet\n;;; From: %s\n\n"
                          mode
                          (or (buffer-file-name) "scratch"))))
    (with-temp-buffer
      (insert context)
      (insert code)
      (webpaste-paste-buffer))))

;;; Global Keybindings
(with-eval-after-load 'bv-core
  (bv-leader
    "w" '(:ignore t :which-key "webpaste/share")
    "w p" #'webpaste-paste-region-or-buffer
    "w r" #'webpaste-paste-region
    "w b" #'webpaste-paste-buffer
    "w l" #'bv-communication-yank-code-link
    "w s" #'bv-communication-share-snippet))

;;;; Feature Definition
(defun bv-communication-load ()
  "Load communication configuration."
  (add-to-list 'bv-enabled-features 'communication)
  (message "Communication tools loaded"))

(provide 'bv-communication)
;;; bv-communication.el ends here
