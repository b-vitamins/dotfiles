;;; bv-battery.el --- Battery configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Battery status display with icon indicators in the mode line.

;;; Code:

(require 'cl-lib)
(require 'battery nil t)
(autoload 'all-the-icons-faicon "all-the-icons")


(defun bv-battery-add-unicode-indicator (data)
  "Add battery icon indicator to mode line based on battery DATA."
  (let* ((bat-str (cdr (assq ?p data)))
         (bat (if (and bat-str (not (string= bat-str "N/A")))
                  (car (read-from-string bat-str))
                nil)))
    (when (numberp bat)
      (let* ((index (cond
                      ((> bat 75) 0)
                      ((> bat 50) 1)
                      ((> bat 25) 2)
                      ((> bat 10) 3)
                      (t 4)))
             (symbol (nth index
                          (list (all-the-icons-faicon "battery-full")
                                (all-the-icons-faicon "battery-three-quarters")
                                (all-the-icons-faicon "battery-half")
                                (all-the-icons-faicon "battery-quarter")
                                (all-the-icons-faicon "battery-empty")))))
        (when (and (boundp 'battery-mode-line-string) battery-mode-line-string)
          (setq battery-mode-line-string
                (format "%s %s" symbol battery-mode-line-string)))))))

(when (fboundp 'battery-status-function)
  (when (boundp 'battery-update-functions)
    (add-hook 'battery-update-functions 'bv-battery-add-unicode-indicator))
  (when (boundp 'battery-mode-line-format)
    (setq battery-mode-line-format "%p%% "))
  (display-battery-mode))

(provide 'bv-battery)
;;; bv-battery.el ends here