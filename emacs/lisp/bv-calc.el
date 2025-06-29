;;; bv-calc.el --- Calculator configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Configuration for Emacs calculator with currency exchange rates.

;;; Code:

(with-eval-after-load 'calc-currency-autoloads
  (add-hook 'calc-start-hook 'calc-currency-load))

(with-eval-after-load 'calc-currency
  (require 'xdg)
  (when (boundp 'calc-currency-exchange-rates-file)
    (setq calc-currency-exchange-rates-file
          (expand-file-name "emacs/calc-currency-rates.el" (xdg-cache-home))))
  (when (boundp 'calc-currency-base-currency)
    (setq calc-currency-base-currency 'INR))
  (when (boundp 'calc-currency-update-interval)
    (setq calc-currency-update-interval 7)))

(provide 'bv-calc)
;;; bv-calc.el ends here
