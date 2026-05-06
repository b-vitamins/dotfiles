;;; bv-keycast.el --- Keycast configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Display current command and key binding in header line.

;;; Code:

(defconst bv-keycast--available-p
  (require 'keycast nil t)
  "Non-nil when the `keycast' package is available.")

;; Declare external variables from keycast package
(defvar keycast--this-command-keys)
(defvar keycast--this-command)
(declare-function keycast--minibuffer-exit "keycast")
(declare-function keycast--update "keycast")

(defun bv-keycast-active-window-p ()
  "Return non-nil if current window is active."
  (eq (selected-window) (old-selected-window)))

(when bv-keycast--available-p
  (when (boundp 'keycast-mode-line-window-predicate)
    (setq keycast-mode-line-window-predicate 'bv-keycast-active-window-p))
  (when (boundp 'keycast-mode-line-format)
    (setq keycast-mode-line-format "%k%c%r "))
  (when (boundp 'keycast-mode-line-insert-after)
    (setq keycast-mode-line-insert-after 'mode-line-misc-info))
  (when (boundp 'keycast-mode-line-remove-tail-elements)
    (setq keycast-mode-line-remove-tail-elements nil)))


(defun bv-keycast-header-line-formatter ()
  "Format keycast information for header line display."
  (when (and (boundp 'keycast--this-command-keys)
             (boundp 'keycast--this-command)
             keycast--this-command-keys
             keycast--this-command)
    (concat " "
            (propertize (key-description keycast--this-command-keys)
                        'face 'keycast-key)
            " → "
            (propertize (symbol-name keycast--this-command)
                        'face 'keycast-command)
            " ")))

(defun bv-keycast--enable-updates ()
  "Enable keycast state updates without letting keycast own the header line."
  (when (fboundp 'keycast--update)
    (add-hook 'post-command-hook #'keycast--update t))
  (when (fboundp 'keycast--minibuffer-exit)
    (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t)))

(defun bv-keycast--disable-updates ()
  "Disable keycast state updates installed by `bv-keycast-mode'."
  (when (fboundp 'keycast--update)
    (remove-hook 'post-command-hook #'keycast--update))
  (when (fboundp 'keycast--minibuffer-exit)
    (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)))

(define-minor-mode bv-keycast-mode
  "Show current command and key binding in header line."
  :global t
  :group 'bv
  (cond
   ((not bv-keycast--available-p)
    (setq bv-keycast-mode nil)
    (unless noninteractive
      (message "Keycast package is unavailable; bv-keycast-mode disabled.")))
   (bv-keycast-mode
    (when (fboundp 'keycast-mode-line-mode)
      (keycast-mode-line-mode -1))
    (when (fboundp 'keycast-header-line-mode)
      (keycast-header-line-mode -1))
    (bv-keycast--enable-updates)
    (force-mode-line-update t))
   (t
    (when (fboundp 'keycast-header-line-mode)
      (keycast-header-line-mode -1))
    (bv-keycast--disable-updates)
    (force-mode-line-update t))))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-toggle-map)
    (define-key bv-toggle-map (kbd "k") 'bv-keycast-mode)))

(provide 'bv-keycast)
;;; bv-keycast.el ends here
