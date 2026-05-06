;;; bv-orderless.el --- Orderless completion style policy -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Orderless matching vocabulary.
;;
;; Built-in Orderless 1.6 already provides affix dispatch for literal,
;; negation, annotation, initialism, prefix, flex, and char-fold matching.
;; This layer keeps that vocabulary and adds only the two local idioms that
;; are not built in: /regexp/ and a trailing $ word-boundary marker.

;;; Code:

(require 'orderless)
(require 'subr-x)

(defvar minibuffer-local-completion-map)
(defvar completion-category-overrides)
(defvar completion-category-defaults)
(defvar completion-styles)

;;; Dispatchers

(defun bv-orderless-regexp-dispatcher (pattern _index _total)
  "Treat /PATTERN/ as an explicit regexp component."
  (when (and (length> pattern 2)
             (string-prefix-p "/" pattern)
             (string-suffix-p "/" pattern))
    `(orderless-regexp . ,(substring pattern 1 -1))))

(defun bv-orderless-word-boundary-dispatcher (pattern _index _total)
  "Treat PATTERN$ as a word-boundary literal component."
  (when (and (length> pattern 1)
             (string-suffix-p "$" pattern))
    `(orderless-regexp . ,(concat "\\_<"
                                  (regexp-quote (substring pattern 0 -1))
                                  "\\_>"))))

;;; Core Settings

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides
      '((file (styles partial-completion orderless basic))
        (project-file (styles partial-completion orderless basic))
        (buffer (styles orderless basic))
        (command (styles orderless basic))
        (variable (styles orderless basic))
        (function (styles orderless basic))
        (symbol (styles orderless basic))
        (consult-location (styles orderless basic))
        (consult-multi (styles orderless basic))
        (unicode-name (styles orderless substring basic))
        (info-menu (styles orderless substring basic))
        (kill-ring (styles orderless basic))
        (eglot (styles orderless basic))
        (bookmark (styles orderless substring basic))))

(setq orderless-component-separator #'orderless-escapable-split-on-space
      orderless-matching-styles '(orderless-literal orderless-regexp)
      orderless-smart-case t
      orderless-style-dispatchers
      '(bv-orderless-regexp-dispatcher
        bv-orderless-word-boundary-dispatcher
        orderless-affix-dispatch))

;;; Interactive Helpers

(defun bv-orderless-test-completion (pattern)
  "Show the compiled Orderless regexps for PATTERN."
  (interactive "sOrderless pattern: ")
  (if (string-empty-p pattern)
      (message "No pattern")
    (condition-case err
        (message "%S" (cdr (orderless-compile pattern)))
      (error (message "Invalid pattern: %s" (error-message-string err))))))

(defun bv-orderless-toggle-smart-case ()
  "Toggle `orderless-smart-case'."
  (interactive)
  (setq orderless-smart-case (not orderless-smart-case))
  (message "Orderless smart case %s" (if orderless-smart-case "enabled" "disabled")))

(defun bv-orderless-match-components-literally ()
  "Make the current minibuffer match components literally."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)
              orderless-style-dispatchers nil)
  (message "Orderless literal matching for this completion session"))

;;; Integration

(with-eval-after-load 'corfu
  ;; Corfu should not ghost-insert the current candidate while Orderless input
  ;; is still being refined component by component.
  (setq corfu-preview-current nil))

(with-eval-after-load 'consult
  (setq consult-async-min-input 2))

(define-key minibuffer-local-completion-map (kbd "C-c l")
            #'bv-orderless-match-components-literally)
(define-key minibuffer-local-completion-map (kbd "C-c t")
            #'bv-orderless-test-completion)
(define-key minibuffer-local-completion-map (kbd "C-c c")
            #'bv-orderless-toggle-smart-case)

(provide 'bv-orderless)
;;; bv-orderless.el ends here
