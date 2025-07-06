;;; bv-guix.el --- GNU Guix integration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; GNU Guix system and package management.

;;; Code:


(declare-function guix "guix")
(declare-function global-guix-prettify-mode "guix-prettify")
(declare-function guix-prettify-mode "guix-prettify")
(declare-function guix-devel-mode "guix-devel")

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

(defun bv-guix-transient ()
  "Transient menu for Guix."
  (interactive)
  (transient-define-prefix bv-guix-transient-menu ()
    "GNU Guix"
    ["Packages"
     ("p" "Search packages" guix-packages-by-name)
     ("i" "Installed packages" guix-installed-packages)
     ("u" "Upgradable packages" guix-installed-user-packages)]
    ["Development"
     ("b" "Build package" bv-guix-build-package)
     ("l" "Lint package" bv-guix-lint-package)
     ("f" "Format buffer" bv-guix-format-buffer)]
    ["System"
     ("g" "Generations" guix-generations)
     ("s" "Services" guix-services-from-system-config)])
  (bv-guix-transient-menu))

(global-set-key (kbd "C-c G") 'bv-guix-transient)

(provide 'bv-guix)
;;; bv-guix.el ends here