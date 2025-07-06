;;; bv-comint.el --- Comint configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Command interpreter mode configuration.

;;; Code:

(require 'comint)
(require 'ansi-color)
(declare-function consult--buffer-state "consult")

(defun bv-comint--buffer-list ()
  "Return list of comint buffer names."
  (mapcar #'buffer-name
          (seq-filter (lambda (buf)
                        (with-current-buffer buf
                          (derived-mode-p 'comint-mode)))
                      (buffer-list))))

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources
                 `(:name "Comint"
                   :narrow ?c
                   :category buffer
                   :face consult-buffer
                   :history buffer-name-history
                   :state ,#'consult--buffer-state
                   :items bv-comint--buffer-list)
                 'append)))

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