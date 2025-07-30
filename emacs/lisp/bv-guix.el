;;; bv-guix.el --- GNU Guix integration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; GNU Guix system and package management.

;;; Code:


(declare-function guix "guix")
(declare-function global-guix-prettify-mode "guix-prettify")
(declare-function guix-prettify-mode "guix-prettify")
(declare-function guix-devel-mode "guix-devel")
(declare-function guix-devel-build-package-definition "guix-devel")
(declare-function guix-devel-lint-package "guix-devel")
(declare-function info-lookup-add-help "info-look")
(declare-function guix-packages-by-name "guix")
(declare-function guix-installed-packages "guix")
(declare-function guix-installed-user-packages "guix")
(declare-function guix-generations "guix")
(declare-function guix-services-from-system-config "guix")

;; External variables
(defvar guix-directory)
(defvar guix-repl-use-server)

(defgroup bv-guix nil
  "GNU Guix settings."
  :group 'bv)

(defcustom bv-guix-idle-delay 1.0
  "Idle time before loading guix."
  :type 'number
  :group 'bv-guix)

(defcustom bv-guix-directory "~/projects/guix"
  "Path to local Guix checkout."
  :type 'directory
  :group 'bv-guix)

;; Load guix after idle delay
(run-with-idle-timer bv-guix-idle-delay t
                     (lambda ()
                       (require 'guix nil t)))

(setq guix-directory bv-guix-directory
      guix-repl-use-server nil)

(with-eval-after-load 'guix
  (global-guix-prettify-mode 1))

(with-eval-after-load 'scheme-mode
  (add-hook 'scheme-mode-hook 'guix-devel-mode))

(defun bv-guix-format-buffer ()
  "Format Guix Scheme buffer."
  (interactive)
  (when (derived-mode-p 'scheme-mode)
    (shell-command-on-region
     (point-min) (point-max)
     "guix style -f -"
     nil t)))

(defun bv-guix-build-package ()
  "Build package at point."
  (interactive)
  (guix-devel-build-package-definition))

(defun bv-guix-lint-package ()
  "Lint package at point."
  (interactive)
  (guix-devel-lint-package))

(with-eval-after-load 'info-look
  (info-lookup-add-help
   :mode 'scheme-mode
   :regexp "[^()`',\"        \n]+"
   :ignore-case t
   :doc-spec '(("(r5rs)Index" nil "^[ \t]+-+ [^:]+:[ \t]*" "\\b")
               ("(Guile)Procedure Index" nil nil nil)
               ("(Guile)Variable Index" nil nil nil)
               ("(Guix)Programming Index" nil nil nil))))


(provide 'bv-guix)
;;; bv-guix.el ends here