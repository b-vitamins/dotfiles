;;; bv-comint.el --- Comint configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Command interpreter mode configuration.

;;; Code:

(require 'comint)
(require 'ansi-color)
(require 'seq)
(declare-function consult--buffer-action "consult" (buffer &optional norecord))
(declare-function consult--buffer-pair "consult" (buffer))
(declare-function consult--buffer-state "consult")

(defun bv-comint--buffer-list ()
  "Return Consult buffer pairs for live Comint buffers."
  (mapcar #'consult--buffer-pair
          (seq-filter
           (lambda (buffer)
             (with-current-buffer buffer
               (derived-mode-p 'comint-mode)))
           (buffer-list))))

(defvar bv-comint-consult-source
  `(:name "Comint"
    :narrow ?c
    :category buffer
    :face consult-buffer
    :history buffer-name-history
    :state ,#'consult--buffer-state
    :action ,#'consult--buffer-action
    :items bv-comint--buffer-list)
  "Consult source for live Comint buffers.")

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'bv-comint-consult-source 'append)))

(when (boundp 'comint-prompt-read-only)
  (setq comint-prompt-read-only t))
(when (boundp 'comint-scroll-to-bottom-on-input)
  (setq comint-scroll-to-bottom-on-input t))
(when (boundp 'comint-scroll-to-bottom-on-output)
  (setq comint-scroll-to-bottom-on-output t))
(when (boundp 'comint-input-ignoredups)
  (setq comint-input-ignoredups t))

(add-hook 'comint-preoutput-filter-functions 'ansi-color-apply)

(with-eval-after-load 'comint
  (when (boundp 'comint-mode-map)
    (define-key comint-mode-map (kbd "M-r") 'consult-history)))

(provide 'bv-comint)
;;; bv-comint.el ends here
