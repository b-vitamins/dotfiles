;;; bv-layout-core.el --- Shared layout constants -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Minimal frame-layout data shared by early-init and the full layout module.
;; Keep this dependency-free: early-init loads it before package startup.

;;; Code:

(defgroup bv-layout nil
  "Frame and window layout configuration."
  :group 'frames)

(defcustom bv-layout-default-frame-parameters
  '((min-height . 1)
    (height . 45)
    (min-width . 1)
    (width . 81)
    (vertical-scroll-bars . nil)
    (internal-border-width . 24)
    (left-fringe . 8)
    (right-fringe . 8)
    (tool-bar-lines . 0)
    (menu-bar-lines . 0)
    (undecorated . t))
  "Default frame parameters for the BV layout system."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'bv-layout)

(defun bv-layout-default-frame-alist ()
  "Return a fresh copy of `bv-layout-default-frame-parameters'."
  (copy-tree bv-layout-default-frame-parameters))

(defun bv-layout-apply-default-frame-alist ()
  "Install BV default frame parameters into `default-frame-alist'."
  (dolist (parameter (bv-layout-default-frame-alist))
    (setf (alist-get (car parameter) default-frame-alist)
          (cdr parameter)))
  default-frame-alist)

(provide 'bv-layout-core)
;;; bv-layout-core.el ends here
