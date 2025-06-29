;;; bv-browse-url.el --- Browse URL configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; URL browsing configuration with pattern-based browser selection
;; and external browser integration.

;;; Code:

(eval-when-compile (require 'cl-lib))

(autoload 'bookmark-make-record-default "bookmark")
(autoload 'bookmark-prop-get "bookmark")
(autoload 'browse-url-default-browser "browse-url")
(autoload 'embark-open-externally "embark")


(defgroup bv-browse-url nil
  "Generic utilities to enhance `browse-url'."
  :group 'bv)

(defcustom bv-browse-url-mappings '()
  "URL mapping alist.
It has the form (SERVICE . ALT) where SERVICE is the original hostname
and ALT is the alternative service host to rewrite urls to, and viceversa."
  :type '(alist :key-type string :value-type string)
  :group 'bv-browse-url)

(defun bv-browse-url--transform-url (url &optional alt)
  "Transform URL to its alternative in `bv-browse-url-mappings'.
If ALT is non-nil, URL is assumed to be an alternative."
  (setq alt (if alt t nil)) ; normalize the argument
  (string-match "\\(.+://[^/]*\\).*" url)
  (let* ((service-url (match-string 1 url))
         (mapping (if alt
                      (rassoc service-url bv-browse-url-mappings)
                    (assoc-string service-url bv-browse-url-mappings))))
    (if mapping
        (if alt
            (replace-regexp-in-string service-url (car mapping) url)
          (replace-regexp-in-string service-url (cdr mapping) url))
      url)))

(defun bv-browse-url-bookmark-make-record (url title)
  "Create a bookmark record from a browser buffer with URL and TITLE."
  (let* ((defaults (delq nil (list title url)))
         (bookmark `(,title
                     ,@(bookmark-make-record-default 'no-file)
                     ,(cons 'browser-url url)
                     ,(cons 'filename url)
                     ,(cons 'handler 'bv-browse-url-bookmark-jump)
                     ,(cons 'defaults defaults))))
    bookmark))

(defun bv-browse-url-bookmark-jump (bookmark)
  "Jump to BOOKMARK in the default browser."
  (let ((location (bookmark-prop-get bookmark 'browser-url)))
    (browse-url-default-browser location)))

(defun bv-browse-url-alt-bookmark-jump (bookmark)
  "Jump to BOOKMARK in an alternative browser."
  (cl-letf (((symbol-function 'browse-url-can-use-xdg-open) 'ignore))
    (bv-browse-url-bookmark-jump bookmark)))

(defun bv-browse-url-open-with-cookies (cookies &optional url)
  "Open URL with COOKIES in corresponding external application."
  (interactive "\nsURL: ")
  (let* ((url-request-extra-headers
          `(("Cookie" ,(mapconcat (lambda (cookie-pair)
                                    (format " %s=%s;" (car cookie-pair) (cadr cookie-pair)))
                                  cookies
                                  ""))))
         (filename (concat (if (boundp 'temporary-file-directory)
                              temporary-file-directory
                              "/tmp/")
                           (car (last (split-string url "/"))))))
    (unless (file-exists-p filename)
      (with-current-buffer (url-retrieve-synchronously url t)
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line 1)
        (delete-region (point) (point-min))
        (write-region (point-min) (point-max) filename)))
    (embark-open-externally filename)))

(with-eval-after-load 'embark
  (when (boundp 'embark-bookmark-map)
    (define-key embark-bookmark-map "c" 'bv-browse-url-alt-bookmark-jump)))

(defun bv-browse-url-add-scheme (fun url &rest args)
  "Add https scheme to URL if missing and invoke FUN and ARGS with it."
  (let ((link (if (string-match "^[A-Z]+:" url)
                  url
                (concat "https:" url))))
    (apply fun link args)))

(defun bv-browse-url-trace-url (fun url &rest args)
  "Transform URL to its original form and invoke FUN and ARGS with it."
  (let ((link (bv-browse-url--transform-url url)))
    (apply fun link args)))

(setq bv-browse-url-mappings (append (list) '()))

(advice-add 'browse-url-xdg-open :around 'bv-browse-url-add-scheme)

(with-eval-after-load 'browse-url
  (when (boundp 'browse-url-browser-function)
    (setq browse-url-browser-function 'browse-url-xdg-open)))

(provide 'bv-browse-url)
;;; bv-browse-url.el ends here