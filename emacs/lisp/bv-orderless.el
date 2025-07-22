;;; bv-orderless.el --- Orderless completion style configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for orderless completion style with custom dispatchers.

;;; Code:

(require 'orderless)

;; Component separator configuration
(setq orderless-component-separator 'orderless-escapable-split-on-space)

;; Custom dispatchers for special matching
(defun bv-orderless-literal-dispatcher (pattern _index _total)
  "Match PATTERN literally when it ends with '='."
  (cond ((equal "=" pattern) '(orderless-literal . "="))
        ((string-suffix-p "=" pattern)
         (cons 'orderless-literal (substring pattern 0 -1)))))

(defun bv-orderless-without-literal-dispatcher (pattern _index _total)
  "Exclude matches for PATTERN when it ends with '!'."
  (cond ((equal "!" pattern) '(orderless-literal . "!"))
        ((string-suffix-p "!" pattern)
         (cons 'orderless-without-literal (substring pattern 0 -1)))))

(defun bv-orderless-initialism-dispatcher (pattern _index _total)
  "Match PATTERN as initialism when it ends with ','."
  (cond ((equal "," pattern) '(orderless-literal . ","))
        ((string-suffix-p "," pattern)
         (cons 'orderless-initialism (substring pattern 0 -1)))))

(defun bv-orderless-flex-dispatcher (pattern _index _total)
  "Match PATTERN flexibly when it ends with '~'."
  (cond ((equal "~" pattern) '(orderless-literal . "~"))
        ((string-suffix-p "~" pattern)
         (cons 'orderless-flex (substring pattern 0 -1)))))

(setq orderless-style-dispatchers
      '(bv-orderless-literal-dispatcher
        bv-orderless-without-literal-dispatcher
        bv-orderless-initialism-dispatcher
        bv-orderless-flex-dispatcher))

;; Matching styles
(setq orderless-matching-styles
      '(orderless-literal
        orderless-regexp
        orderless-initialism
        orderless-prefixes
        orderless-flex))

;; Set orderless as the primary completion style
(setq completion-styles '(orderless basic))
(setq completion-category-overrides
      '((file (styles orderless partial-completion basic))
        (project-file (styles orderless partial-completion basic))
        (buffer (styles orderless flex))
        (consult-location (styles orderless))
        (consult-multi (styles orderless))
        (command (styles orderless flex))
        (variable (styles orderless flex))
        (symbol (styles orderless flex))))
(setq completion-category-defaults nil)

(provide 'bv-orderless)
;;; bv-orderless.el ends here