;;; bv-orderless.el --- A polished and robust Orderless configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;;
;; This configuration for the `orderless` completion style is designed to be
;; comprehensive, intuitive, and powerful, leveraging the full capabilities
;; of the package as detailed in its source and documentation.
;;
;; It provides a thoughtful default experience that works seamlessly with
;; Vertico, Consult, Corfu, and Marginalia, while offering a concise and
;; powerful syntax for advanced filtering.  This version has been personalized
;; to include custom dispatchers and settings.
;;
;;----------------------------------------------------------------------------
;;; Comprehensive Usage Guide
;;----------------------------------------------------------------------------
;;
;; 1. Basic Usage: Out-of-Order Substrings
;; ------------------------------------------
;; The core idea of Orderless is simple: type space-separated terms, and it
;; will find candidates that contain all those terms, in any order.
;;
;;   - Input: `orderless el`
;;   - Matches: `bv-orderless.el`
;;
;;
;; 2. The Dispatcher Syntax: Your Power Tools
;; ------------------------------------------
;; To get more specific, you can use special characters to change how a term
;; matches.  This is your "query syntax".
;;
;;   `...=` (Literal Matching):
;;   Forces the term to be matched exactly as a substring.
;;   - Input: `orderless=` -> Matches `bv-orderless.el`
;;
;;   `...~` (Flex Matching):
;;   Characters must appear in order, but can be separated by anything.
;;   - Input: `fbr~` -> Matches `foo-bar-baz`
;;
;;   `...,` (Initialism Matching):
;;   Each character must be the first letter of a word part.
;;   - Input: `ffap,` -> Matches `find-file-at-point`
;;
;;   `^...` (Prefix Matching):
;;   The term must appear at the very beginning of the candidate.
;;   - Input: `^bv-` -> Matches `bv-orderless.el`
;;
;;   `!...` or `...!` (Exclusion/NOT):
;;   The term must NOT appear in the candidate.
;;   - Input: `bv- !org` -> Excludes anything with "org".
;;
;;   `/.../` (Regexp Matching):
;;   Treats the term as a regular expression.
;;   - Input: `/^bv-.+\.el$/` -> Matches files starting with `bv-` and ending with `.el`.
;;
;;   `...$` (Word Boundary Matching):
;;   Matches the term as a whole word.
;;   - Input: `orderless$` -> Matches `bv-orderless.el` but not `bv-orderless-extra.el`.
;;
;;   `&...` (Annotation Matching):
;;   Matches against the annotation provided by packages like Marginalia.
;;   - Input: `&Lisp` -> In `M-x`, matches commands in the "Lisp" category.
;;
;;   `...%` (Character Folding):
;;   Matches ignoring case, accents, and diacritics.
;;   - Input: `cafe%` -> Matches `café`, `Cafe`, `CAFÉ`
;;
;;
;; 3. Context-Aware Completion
;; --------------------------------------------------------
;; This configuration automatically changes its behavior based on what you are
;; doing, with special logic for the first component you type to speed up
;; filtering for files and commands.

;;; Code:

(require 'orderless)

;; External variable declarations for elint compliance
(defvar completion-styles)
(defvar completion-category-defaults)
(defvar completion-category-overrides)
(defvar corfu-preview-current)
(defvar consult-async-min-input)
(defvar minibuffer-local-completion-map)
(defvar marginalia-prompt-categories)

;; 1. Core Settings
;; =============================================================================
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      orderless-component-separator 'orderless-escapable-split-on-space
      orderless-smart-case t)

;; 2. Faces
;; =============================================================================

(setq orderless-match-faces
      [orderless-match-face-0
       orderless-match-face-1
       orderless-match-face-2
       orderless-match-face-3])

;; 3. Personalized Dispatchers
;; =============================================================================
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

;; REMOVED the problematic first-component dispatcher.
(setq orderless-style-dispatchers
      '(bv-orderless-literal-dispatcher
        bv-orderless-without-literal-dispatcher
        bv-orderless-initialism-dispatcher
        bv-orderless-flex-dispatcher
        bv-orderless-prefix-dispatcher
        bv-orderless-regexp-dispatcher
        bv-orderless-annotation-dispatcher
        bv-orderless-char-fold-dispatcher
        bv-orderless-word-boundary-dispatcher))

;; 4. Default Matching Styles
;; =============================================================================
(setq orderless-matching-styles
      '(orderless-literal
        orderless-regexp))

;; 5. Category-Specific Overrides
;; =============================================================================
(setq completion-category-overrides
      '((file (styles orderless partial-completion basic))
        (project-file (styles orderless partial-completion basic))
        (buffer (styles orderless flex)
                (orderless-matching-styles orderless-literal orderless-initialism))
        (command (styles orderless flex)
                 (orderless-matching-styles orderless-literal orderless-initialism orderless-prefixes))
        (variable (styles orderless flex)
                  (orderless-matching-styles orderless-literal orderless-prefixes))
        (consult-location (styles orderless))
        (consult-multi (styles orderless))
        (symbol (styles orderless flex))
        (unicode-name (styles orderless substring))
        (info-menu (styles orderless substring basic))
        (kill-ring (styles orderless))
        (eglot (styles orderless flex))
        (bookmark (styles orderless substring basic))))

;; 6. Performance and Utilities
;; =============================================================================
(defun bv-orderless-gc-optimize (orig-fun &rest args)
  "Temporarily increase GC threshold during completion.
ORIG-FUN is the original function being advised.
ARGS are the arguments passed to ORIG-FUN."
  (let ((gc-cons-threshold most-positive-fixnum))
    (apply orig-fun args)))

(advice-add 'orderless-all-completions :around #'bv-orderless-gc-optimize)
(advice-add 'orderless-try-completion :around #'bv-orderless-gc-optimize)

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

(defun bv-orderless-match-components-literally ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)))

;; 7. Integrations with Other Packages
;; =============================================================================
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-prompt-categories '("\\<completion\\>" . command)))

(with-eval-after-load 'corfu
  (setq corfu-preview-current nil))

(with-eval-after-load 'consult
  (setq consult-async-min-input 2))

;; 8. Keybindings
;; =============================================================================
(define-key minibuffer-local-completion-map (kbd "C-c l") #'bv-orderless-match-components-literally)
(define-key minibuffer-local-completion-map (kbd "C-c t") #'bv-orderless-test-completion)
(define-key minibuffer-local-completion-map (kbd "C-c c") #'bv-orderless-toggle-smart-case)

(provide 'bv-orderless)
;;; bv-orderless.el ends here
