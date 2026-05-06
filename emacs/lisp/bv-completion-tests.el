;;; bv-completion-tests.el --- Tests for BV completion policy -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; ERT checks for the shared minibuffer/completion policy.  These tests cover
;; the inline command annotation rules that keep Marginalia useful without
;; pushing command docs to a visually jarring right edge.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'bv-marginalia)

(defmacro bv-completion-tests--with-width (width &rest body)
  "Evaluate BODY with completion surface WIDTH."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'bv-completion-window-width)
              (lambda (&optional _window) ,width)))
     ,@body))

(defun bv-completion-tests--verbose-command ()
  "Open a deliberately verbose annotation.
This should truncate cleanly when horizontal space is tight."
  (interactive))

(ert-deftest bv-completion-command-annotations-are-inline ()
  "Command annotations should not use Marginalia's right-align marker."
  :tags '(bv-completion)
  (bv-completion-tests--with-width 100
    (let ((annotation (bv-marginalia-annotate-command "find-file")))
      (should annotation)
      (should-not (text-property-any 0 (length annotation)
                                     'marginalia--align t annotation)))))

(ert-deftest bv-completion-command-annotations-keep-real-keys ()
  "Command annotations should prefer useful bindings over low-level events."
  :tags '(bv-completion)
  (bv-completion-tests--with-width 100
    (let ((annotation (bv-marginalia-annotate-command "find-file")))
      (should annotation)
      (should (string-match-p "(C-x C-f)" annotation))
      (should-not (string-match-p "<open>" annotation)))))

(ert-deftest bv-completion-command-docs-truncate-to-row-budget ()
  "Command docs should truncate instead of disappearing by command context."
  :tags '(bv-completion)
  (bv-completion-tests--with-width 74
    (let* ((candidate "bv-completion-tests--verbose-command")
           (budget (bv-marginalia--command-row-budget candidate))
           (annotation (bv-marginalia-annotate-command candidate)))
      (should annotation)
      (should (<= (string-width annotation) (+ budget 2)))
      (should (string-match-p " ->\\'" annotation)))))

(provide 'bv-completion-tests)
;;; bv-completion-tests.el ends here
