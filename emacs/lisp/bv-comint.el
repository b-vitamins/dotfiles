;;; bv-comint.el --- Comint configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for comint-mode buffers, including integration with consult
;; for buffer switching and ANSI color support.

;;; Code:

(autoload 'bv-completion--mode-buffers "bv-completion")
(autoload 'consult--buffer-state "consult")

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources
                 `(:name "Comint"
                   :narrow ?c
                   :category buffer
                   :state ,'consult--buffer-state
                   :items ,(lambda ()
                             (mapcar #'buffer-name
                                     (when (fboundp 'bv-completion--mode-buffers)
                                       (bv-completion--mode-buffers 'comint-mode)))))
                 'append)))

(when (boundp 'comint-preoutput-filter-functions)
  (add-hook 'comint-preoutput-filter-functions #'ansi-color-apply nil t))

(provide 'bv-comint)
;;; bv-comint.el ends here