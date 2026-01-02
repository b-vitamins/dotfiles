;;; bv-avy.el --- Jump-anywhere navigation with Avy  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Fast, low-friction navigation using Avy.
;; Includes an Embark action for “jump + act” workflows.

;;; Code:

(require 'cl-lib)
(require 'ring)

(defvar goto-map)
(defvar avy-ring)
(defvar avy-dispatch-alist)

(declare-function avy-goto-char-timer "avy")
(declare-function avy-goto-char-2 "avy")
(declare-function avy-goto-word-1 "avy")
(declare-function avy-goto-line "avy")
(declare-function embark-act "embark")

(defun bv-avy-action-embark (pt)
  "Run `embark-act' at PT as an Avy dispatch action."
  (when (require 'embark nil t)
    (save-excursion
      (goto-char pt)
      (call-interactively #'embark-act)))
  ;; Return to the original window after acting.
  (when (and (boundp 'avy-ring) (ring-p avy-ring) (> (ring-length avy-ring) 0))
    (select-window (cdr (ring-ref avy-ring 0))))
  t)

(with-eval-after-load 'avy
  ;; Jump anywhere by default.
  (when (boundp 'avy-all-windows)
    (setq avy-all-windows t))
  ;; Avy dispatch: press `e' to Embark-act on the selected target.
  (when (boundp 'avy-dispatch-alist)
    (setf (alist-get ?e avy-dispatch-alist nil nil #'eq) #'bv-avy-action-embark)))

(when (boundp 'goto-map)
  (define-key goto-map (kbd "j") #'avy-goto-char-timer)
  (define-key goto-map (kbd "J") #'avy-goto-char-2)
  (define-key goto-map (kbd "w") #'avy-goto-word-1)
  (define-key goto-map (kbd "l") #'avy-goto-line))

(provide 'bv-avy)
;;; bv-avy.el ends here
