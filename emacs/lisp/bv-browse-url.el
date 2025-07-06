;;; bv-browse-url.el --- URL browsing configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Smart URL handling and browser integration.

;;; Code:

(require 'browse-url)

;; Default browser
(when (boundp 'browse-url-browser-function)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; Browser selection
(when (boundp 'browse-url-generic-program)
  (setq browse-url-generic-program "firefox"))

;; Add https scheme if missing
(defun bv-browse-url-add-scheme (fun url &rest args)
  "Add https scheme to URL if missing."
  (let ((link (if (string-match "^[a-zA-Z]+:" url)
                  url
                (concat "https://" url))))
    (apply fun link args)))

(advice-add 'browse-url :around #'bv-browse-url-add-scheme)

;; Quick browser selection
(defun bv-browse-url-firefox (url &optional _)
  "Browse URL with Firefox."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "firefox"))
    (browse-url-generic url)))

(defun bv-browse-url-chromium (url &optional _)
  "Browse URL with Chromium."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "chromium"))
    (browse-url-generic url)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "u") 'browse-url)
    (define-key bv-app-map (kbd "U") 'browse-url-at-point)))

(provide 'bv-browse-url)
;;; bv-browse-url.el ends here