;;; bv-nerd-icons-tests.el --- Tests for BV icon policy -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; ERT checks for the role-based icon stack.  These tests keep icon selection,
;; typography ownership, and completion density policy from regressing into
;; scattered package-local choices.

;;; Code:

(require 'ert)
(require 'bv-completion)
(require 'bv-nerd-icons)

(defconst bv-nerd-icons-tests--required-roles
  '(file directory project note code terminal git pdf help warning success
    navigation-caret)
  "Roles that must exist for the BV icon system to be coherent.")

(ert-deftest bv-nerd-icons-role-registry-has-required-roles ()
  "The icon registry should expose the core UI roles."
  :tags '(bv-nerd-icons)
  (dolist (role bv-nerd-icons-tests--required-roles)
    (should (alist-get role bv-nerd-icons-role-alist))))

(ert-deftest bv-nerd-icons-required-roles-render ()
  "Representative roles should render to nonempty icon strings."
  :tags '(bv-nerd-icons)
  (dolist (role bv-nerd-icons-tests--required-roles)
    (let ((icon (bv-nerd-icons-icon role)))
      (should (stringp icon))
      (should (> (length icon) 0)))))

(ert-deftest bv-nerd-icons-style-policy-controls-role-faces ()
  "Icon style should determine the face used by BV-owned roles."
  :tags '(bv-nerd-icons)
  (let ((bv-nerd-icons-style 'semantic))
    (should (memq 'bv-icon-note
                  (bv-nerd-icons--icon-entry 'note))))
  (let ((bv-nerd-icons-style 'muted))
    (should (memq 'bv-icon-muted
                  (bv-nerd-icons--icon-entry 'note))))
  (let ((bv-nerd-icons-style 'monochrome))
    (should (memq 'bv-icon-default
                  (bv-nerd-icons--icon-entry 'note)))))

(ert-deftest bv-nerd-icons-font-family-follows-font-role ()
  "The icon module should use `bv-fonts-icon-family' unless explicitly overridden."
  :tags '(bv-nerd-icons)
  (let ((bv-nerd-icons-font-family nil)
        (bv-fonts-icon-family "BV Test Icon Font"))
    (should (equal (bv-nerd-icons-effective-font-family)
                   "BV Test Icon Font")))
  (let ((bv-nerd-icons-font-family "Explicit Icon Font")
        (bv-fonts-icon-family "BV Test Icon Font"))
    (should (equal (bv-nerd-icons-effective-font-family)
                   "Explicit Icon Font"))))

(ert-deftest bv-nerd-icons-completion-policy-is-category-aware ()
  "Files and notes keep icons; command-like categories need roomy surfaces."
  :tags '(bv-nerd-icons)
  (should (bv-nerd-icons-completion-category-enabled-p 'file 60))
  (should (bv-nerd-icons-completion-category-enabled-p 'org-slipbox-node 60))
  (should-not (bv-nerd-icons-completion-category-enabled-p 'command 100))
  (should (bv-nerd-icons-completion-category-enabled-p 'command 160)))

(ert-deftest bv-nerd-icons-completion-keeps-virtual-buffers-safe ()
  "Virtual buffer candidates should not let icon rendering break completion."
  :tags '(bv-nerd-icons)
  (let* ((bv-nerd-icons-completion-category-policy
          '((buffer . always) (t . always)))
         (candidate
          "Solutions: Deep Learning Foundation and Concepts | slips/example.org")
         (icon (bv-nerd-icons-completion-get-icon
                (lambda (_cand _category)
                  (buffer-local-value 'major-mode nil))
                candidate
                'buffer)))
    (should (stringp icon))
    (should (> (length icon) 0))))

(ert-deftest bv-nerd-icons-completion-still-delegates-live-buffers ()
  "Non-file buffer candidates should keep using upstream icon logic."
  :tags '(bv-nerd-icons)
  (let ((bv-nerd-icons-completion-category-policy
         '((buffer . always) (t . always)))
        (buffer (get-buffer-create " *bv-nerd-icons-live-buffer-test*"))
        (called nil))
    (unwind-protect
        (should
         (equal
          (bv-nerd-icons-completion-get-icon
           (lambda (cand category)
             (setq called (list cand category))
             "ok")
           buffer
           'buffer)
          "ok"))
      (kill-buffer buffer))
    (should (equal called (list buffer 'buffer)))))

(ert-deftest bv-nerd-icons-completion-file-backed-buffers-use-file-icons ()
  "File-backed buffer candidates should share file completion icon semantics."
  :tags '(bv-nerd-icons)
  (let ((bv-nerd-icons-completion-category-policy
         '((buffer . always) (file . always) (t . always)))
        (buffer (get-buffer-create " *bv-nerd-icons-file-buffer-test*"))
        (file "/tmp/bv-nerd-icons-note.org")
        (called nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-file-name file)
          (should
           (equal
            (bv-nerd-icons-completion-get-icon
             (lambda (cand category)
               (setq called (list cand category))
               "file-icon")
             buffer
             'buffer)
            "file-icon")))
      (kill-buffer buffer))
    (should (equal called (list file 'file)))))

(ert-deftest bv-nerd-icons-private-use-ranges-are-valid ()
  "Configured icon font ranges should be well-formed PUA ranges."
  :tags '(bv-nerd-icons)
  (dolist (range bv-nerd-icons-font-ranges)
    (should (bv-nerd-icons--range-valid-p range))
    (should (>= (car range) #xe000))))

(ert-deftest bv-nerd-icons-font-is-ready-or-displayless ()
  "Graphical sessions should be able to find the configured icon font."
  :tags '(bv-nerd-icons)
  (skip-unless (display-graphic-p))
  (should (bv-nerd-icons-font-ready-p)))

(provide 'bv-nerd-icons-tests)
;;; bv-nerd-icons-tests.el ends here
