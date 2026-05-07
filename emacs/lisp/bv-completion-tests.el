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
(require 'seq)
(require 'bv-comint)
(require 'bv-consult)
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
      (should (string-match-p "  ->\\'" annotation)))))

(ert-deftest bv-completion-default-truncation-marker-has-gap ()
  "Shared completion truncation should use the BV spaced continuation marker."
  :tags '(bv-completion)
  (let ((text (bv-completion-truncate
               "a deliberately overlong completion annotation" 18)))
    (should (string-match-p "  ->\\'" text))))

(ert-deftest bv-completion-padded-fields-truncate-with-gap ()
  "Fixed-width completion fields should not glue continuation markers to text."
  :tags '(bv-completion)
  (let ((text (bv-completion-pad
               "a deliberately overlong fixed-width field" 18)))
    (should (= (string-width text) 18))
    (should (string-match-p "  ->\\'" text))))

(ert-deftest bv-completion-consult-buffer-drops-malformed-sources ()
  "Consult buffer source normalization should ignore unusable entries."
  :tags '(bv-completion)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (sources (bv-consult--safe-buffer-sources
                   '(consult-source-buffer bv-missing-consult-source)))
         (names (mapcar (lambda (source) (plist-get source :name)) sources)))
    (should (member "Buffer" names))
    (should-not (member "bv-missing-consult-source" names))))

(ert-deftest bv-completion-consult-buffer-source-errors-stay-contained ()
  "Broken source item functions should not take down Vertico."
  :tags '(bv-completion)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (broken (list :name "Broken"
                       :category 'buffer
                       :items (lambda () (error "boom"))))
         (source (car (bv-consult--safe-buffer-sources (list broken)))))
    (should source)
    (should-not (funcall (plist-get source :items)))))

(ert-deftest bv-completion-comint-source-uses-buffer-pairs ()
  "Comint source items should keep Consult's native buffer candidate shape."
  :tags '(bv-completion)
  (let ((buffer (get-buffer-create "*bv-comint-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (let ((items (bv-comint--buffer-list)))
            (should (seq-some
                     (lambda (item)
                       (and (consp item)
                            (string= (car item) (buffer-name buffer))
                            (eq (cdr item) buffer)))
                     items))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'bv-completion-tests)
;;; bv-completion-tests.el ends here
