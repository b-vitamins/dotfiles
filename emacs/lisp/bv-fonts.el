;;; bv-fonts.el --- Font configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Font configuration with emoji support, fontaine presets, and
;; interactive emoji insertion functionality.

;;; Code:

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))


(defun bv-fonts--build-emojis ()
  "Build a list of emoji characters with their Unicode names."
  (delete nil
          (cl-loop with range = '(126976 . 129535)
                   for i upto (- (cdr range) (car range))
                   collect (when-let* ((codepoint (+ (car range) i))
                                       (name (get-char-code-property codepoint 'name)))
                             (thread-last (replace-regexp-in-string " " "-" (downcase name))
                                          (format ":%s:")
                                          (format "%s %s" (char-to-string (char-from-name name))))))))

(defun bv-fonts-insert-emoji ()
  "Insert an emoji character selected from a completion interface."
  (interactive)
  (thread-first (completing-read "Select emoji: "
                                 (or (and (boundp 'bv-fonts-emoji-list) bv-fonts-emoji-list)
                                     (set (make-local-variable 'bv-fonts-emoji-list) (bv-fonts--build-emojis))))
                (substring 0 1)
                (insert)))

(when (boundp 'search-map)
  (define-key search-map "e" 'bv-fonts-insert-emoji))
(when (boundp 'minibuffer-mode-map)
  (define-key minibuffer-mode-map (kbd "C-c C-e") 'bv-fonts-insert-emoji))

(with-eval-after-load 'fontset
  (set-fontset-font t 'symbol "Noto Emoji" nil 'append)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  (set-fontset-font "fontset-default" nil (font-spec :name "Noto Emoji")))

(when (boundp 'use-default-font-for-symbols)
  (setq use-default-font-for-symbols nil))

(require 'fontaine)
(when (boundp 'fontaine-current-preset)
  (setq fontaine-current-preset t))
(when (boundp 'fontaine-presets)
  (setq fontaine-presets
        '((t :default-family "Iosevka"
             :default-height 105
             :fixed-pitch-family "Iosevka"
             :fixed-pitch-height 1.0
             :variable-pitch-family "Iosevka Etoile"
             :variable-pitch-height 1.0
             :variable-pitch-weight regular)
          (regular)
          (large :default-weight semilight
                 :default-height 145
                 :bold-weight extrabold))))

(require 'xdg)
(when (boundp 'fontaine-latest-state-file)
  (setq fontaine-latest-state-file
        (expand-file-name "emacs/fontaine-latest.state.eld" (xdg-cache-home))))

(defun bv-font--set-default-fonts ()
  "Set the default font configuration using fontaine."
  (fontaine-set-preset t))

(if after-init-time
    (when (display-graphic-p)
      (bv-font--set-default-fonts))
  (when (boundp 'after-init-hook)
    (add-hook 'after-init-hook 'bv-font--set-default-fonts)))

(provide 'bv-fonts)
;;; bv-fonts.el ends here
