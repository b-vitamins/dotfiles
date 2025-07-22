;;; bv-orderless.el --- Enhanced Orderless completion style configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced configuration for orderless completion style with advanced dispatchers,
;; performance optimizations, and quality of life improvements.

;;; Code:

(require 'orderless)

;;;; Core Configuration

(setq orderless-component-separator 'orderless-escapable-split-on-space)
(setq orderless-smart-case t)  ; Smart case matching

;;;; Enhanced Faces for Better Visual Feedback

(set-face-attribute 'orderless-match-face-0 nil
                    :weight 'bold :foreground "#ff6c6b" :background "#2d2d2d")
(set-face-attribute 'orderless-match-face-1 nil
                    :weight 'bold :foreground "#98be65" :background "#2d2d2d")
(set-face-attribute 'orderless-match-face-2 nil
                    :weight 'bold :foreground "#51afef" :background "#2d2d2d")
(set-face-attribute 'orderless-match-face-3 nil
                    :weight 'bold :foreground "#c678dd" :background "#2d2d2d")

;;;; Advanced Dispatchers

(defun bv-orderless-literal-dispatcher (pattern _index _total)
  "Match PATTERN literally when it ends with '='."
  (cond ((equal "=" pattern) '(orderless-literal . "="))
        ((string-suffix-p "=" pattern)
         (cons 'orderless-literal (substring pattern 0 -1)))))

(defun bv-orderless-without-literal-dispatcher (pattern _index _total)
  "Exclude matches for PATTERN when it ends with '!'.
Also supports prefix '!' for negation."
  (cond ((equal "!" pattern) '(orderless-literal . "!"))
        ((string-prefix-p "!" pattern)
         (cons 'orderless-without-literal (substring pattern 1)))
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

(defun bv-orderless-prefix-dispatcher (pattern _index _total)
  "Match PATTERN as a prefix when it begins with '^'."
  (cond ((equal "^" pattern) '(orderless-literal . "^"))
        ((string-prefix-p "^" pattern)
         (cons 'orderless-literal-prefix (substring pattern 1)))))

(defun bv-orderless-regexp-dispatcher (pattern _index _total)
  "Force regexp matching when PATTERN is surrounded by slashes."
  (when (and (>= (length pattern) 3)
             (string-prefix-p "/" pattern)
             (string-suffix-p "/" pattern))
    (cons 'orderless-regexp (substring pattern 1 -1))))

(defun bv-orderless-annotation-dispatcher (pattern _index _total)
  "Match annotations when PATTERN begins with '&'."
  (cond ((equal "&" pattern) '(orderless-literal . "&"))
        ((string-prefix-p "&" pattern)
         (cons 'orderless-annotation (substring pattern 1)))))

(defun bv-orderless-char-fold-dispatcher (pattern _index _total)
  "Match ignoring diacritics when PATTERN ends with '%'."
  (cond ((equal "%" pattern) '(orderless-literal . "%"))
        ((string-suffix-p "%" pattern)
         (cons 'char-fold-to-regexp (substring pattern 0 -1)))))

(defun bv-orderless-word-boundary-dispatcher (pattern _index _total)
  "Match at word boundaries when PATTERN ends with '$'."
  (when (and (> (length pattern) 1)
             (string-suffix-p "$" pattern))
    (cons 'orderless-regexp
          (concat "\\b" (regexp-quote (substring pattern 0 -1)) "\\b"))))

;;;; Smart First-Component Dispatcher

(defun bv-orderless-first-component-dispatcher (pattern index _total)
  "Special handling for the first component to improve performance.
Uses prefix matching for better candidate filtering."
  (when (= index 0)
    (cond
     ;; File paths should use prefix matching
     ((and minibuffer-completing-file-name
           (not (string-match-p "[~=/!,%$]$" pattern)))
      'orderless-literal-prefix)
     ;; Commands and variables benefit from prefix matching
     ((memq (completion-metadata-get
             (completion-metadata "" minibuffer-completion-table nil)
             'category)
            '(command variable function))
      'orderless-literal-prefix))))

;;;; Configure Dispatchers

(setq orderless-style-dispatchers
      '(bv-orderless-first-component-dispatcher
        bv-orderless-literal-dispatcher
        bv-orderless-without-literal-dispatcher
        bv-orderless-initialism-dispatcher
        bv-orderless-flex-dispatcher
        bv-orderless-prefix-dispatcher
        bv-orderless-regexp-dispatcher
        bv-orderless-annotation-dispatcher
        bv-orderless-char-fold-dispatcher
        bv-orderless-word-boundary-dispatcher))

;;;; Matching Styles Configuration

(setq orderless-matching-styles
      '(orderless-literal
        orderless-regexp))

;;;; Category-Specific Configurations

(setq completion-styles '(orderless basic))

(setq completion-category-overrides
      '((file (styles orderless partial-completion basic)
              (orderless-matching-styles orderless-literal
                                        orderless-regexp
                                        orderless-literal-prefix))
        (project-file (styles orderless partial-completion basic)
                      (orderless-matching-styles orderless-literal
                                                orderless-regexp
                                                orderless-literal-prefix))
        (buffer (styles orderless flex)
                (orderless-matching-styles orderless-literal
                                          orderless-regexp
                                          orderless-initialism))
        (consult-location (styles orderless))
        (consult-multi (styles orderless))
        (command (styles orderless flex)
                 (orderless-matching-styles orderless-literal
                                           orderless-regexp
                                           orderless-initialism
                                           orderless-prefixes))
        (variable (styles orderless flex)
                  (orderless-matching-styles orderless-literal
                                            orderless-regexp
                                            orderless-prefixes))
        (symbol (styles orderless flex))
        (unicode-name (styles orderless substring))
        (info-menu (styles orderless substring basic))
        (kill-ring (styles orderless))
        (eglot (styles orderless flex))
        (bookmark (styles orderless substring basic))))

(setq completion-category-defaults nil)

;;;; Performance Optimizations

;; Increase GC threshold during completion
(defun bv-orderless-fast-dispatch (orig-fun &rest args)
  "Temporarily increase GC threshold during completion."
  (let ((gc-cons-threshold most-positive-fixnum))
    (apply orig-fun args)))

(advice-add 'orderless-all-completions :around #'bv-orderless-fast-dispatch)
(advice-add 'orderless-try-completion :around #'bv-orderless-fast-dispatch)

;;;; Interactive Commands for Debugging and Configuration

(defun bv-orderless-test-completion (pattern)
  "Test orderless completion with PATTERN interactively."
  (interactive "sPattern: ")
  (let* ((components (orderless-compile pattern))
         (pred (car components))
         (regexps (cdr components)))
    (message "Components: %S\nPredicate: %S\nRegexps: %S"
             (if (functionp orderless-component-separator)
                 (funcall orderless-component-separator pattern)
               (split-string pattern orderless-component-separator t))
             pred
             regexps)))

(defun bv-orderless-toggle-smart-case ()
  "Toggle smart case matching."
  (interactive)
  (setq orderless-smart-case (not orderless-smart-case))
  (message "Orderless smart case %s" (if orderless-smart-case "enabled" "disabled")))

(defun bv-orderless-insert-separator ()
  "Insert the orderless separator character.
Useful when the separator is escaped space."
  (interactive)
  (insert " "))

;;;; Additional Utility Functions

(defun bv-orderless-match-components-literally ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)))

(defun bv-orderless-temporary-literal-matching (fn &rest args)
  "Temporarily use literal matching for FN with ARGS."
  (let ((orderless-matching-styles '(orderless-literal)))
    (apply fn args)))

;;;; Integration with Other Packages

(with-eval-after-load 'marginalia
  ;; Ensure annotation matching works well with marginalia
  (add-to-list 'marginalia-prompt-categories '("\\<completion\\>" . command)))

(with-eval-after-load 'corfu
  ;; Optimize orderless for Corfu
  (setq corfu-preview-current nil)  ; Disable preview for performance
  (defun bv-orderless-corfu-setup ()
    "Setup orderless optimizations for Corfu."
    (setq-local orderless-matching-styles
                '(orderless-literal
                  orderless-regexp
                  orderless-literal-prefix)))
  (add-hook 'corfu-mode-hook #'bv-orderless-corfu-setup))

(with-eval-after-load 'consult
  ;; Better integration with consult
  (setq consult-async-min-input 2)
  (defun bv-orderless-consult-suffix ()
    "Use suffix matching for consult commands."
    (interactive)
    (minibuffer-with-setup-hook
        (lambda () (setq-local orderless-matching-styles '(orderless-regexp)))
      (call-interactively 'consult-line))))

;;;; Keybindings

(define-key minibuffer-local-completion-map (kbd "C-c SPC") #'bv-orderless-insert-separator)
(define-key minibuffer-local-completion-map (kbd "C-c l") #'bv-orderless-match-components-literally)

;;;; Quick Reference Comment

;; Quick reference for dispatchers:
;; - End with = for literal matching (example=)
;; - End with ! or start with ! for exclusion (!pattern or pattern!)
;; - End with , for initialism matching (eli,)
;; - End with ~ for flex matching (flx~)
;; - Start with ^ for prefix matching (^prefix)
;; - Surround with / for regexp (/re.*gex/)
;; - Start with & for annotation matching (&TODO)
;; - End with % for diacritic-ignoring matching (cafe%)
;; - End with $ for word boundary matching (word$)

(provide 'bv-orderless)
;;; bv-orderless.el ends here
