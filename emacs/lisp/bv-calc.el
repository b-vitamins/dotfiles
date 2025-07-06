;;; bv-calc.el --- Calculator configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Advanced calculator with units and currency.

;;; Code:

(autoload 'calc "calc" nil t)
(autoload 'calc-dispatch "calc" nil t)
(autoload 'quick-calc "calc" nil t)

;; Basic calc settings
(with-eval-after-load 'calc
  (when (boundp 'calc-display-trail)
    (setq calc-display-trail nil))
  (when (boundp 'calc-window-height)
    (setq calc-window-height 10)))

;; Currency support
(with-eval-after-load 'calc-currency
  (when (boundp 'calc-currency-exchange-rates-file)
    (setq calc-currency-exchange-rates-file
          (expand-file-name "calc-currency-rates.el"
                           (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
  (when (boundp 'calc-currency-base-currency)
    (setq calc-currency-base-currency 'INR))
  (when (boundp 'calc-currency-update-interval)
    (setq calc-currency-update-interval 7)))

;; Quick calc function
(defun bv-calc-eval (expr)
  "Evaluate EXPR using calc."
  (interactive "sCalc expression: ")
  (message "Result: %s" (calc-eval expr)))

;; Keybindings
(global-set-key (kbd "C-x *") 'calc-dispatch)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "*") 'calc)
    (define-key bv-app-map (kbd "=") 'quick-calc)
    (define-key bv-app-map (kbd "+") 'bv-calc-eval)))

(provide 'bv-calc)
;;; bv-calc.el ends here
