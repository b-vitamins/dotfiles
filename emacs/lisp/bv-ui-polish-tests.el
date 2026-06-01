;;; bv-ui-polish-tests.el --- Tests for crowded UI affordances -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; ERT checks for small package-specific renderers that can otherwise regress
;; into crowded markers, glued prefixes, or cramped status/icon columns.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'bv-vertico)
(require 'bv-which-key)
(require 'bv-flymake)
(require 'bv-corfu)
(require 'bv-git)
(require 'bv-keycast)
(require 'bv-nerd-icons)

(ert-deftest bv-ui-polish-vertico-count-compacts-large-totals ()
  "Vertico counts should compact large totals before the prompt."
  :tags '(bv-ui-polish)
  (let ((vertico--index 0)
        (vertico--total 11379)
        (vertico--allow-prompt nil))
    (let ((count (bv-vertico--format-count)))
      (should (string-prefix-p "1/11k" count))
      (should-not (string-match-p "11379" count))
      (should (= (string-width count)
                 (1+ bv-vertico-count-field-width)))
      (should (string-suffix-p " " count)))))

(ert-deftest bv-ui-polish-which-key-spacing-policy ()
  "Which-Key should leave room around prefixes and truncation markers."
  :tags '(bv-ui-polish)
  (should (string-prefix-p " " which-key-ellipsis))
  (should (string-suffix-p " " which-key-prefix-prefix))
  (should (>= which-key-add-column-padding 2))
  (should (floatp which-key-max-description-length)))

(ert-deftest bv-ui-polish-flymake-uses-diagnostic-gutter ()
  "Flymake should reserve a right fringe gutter while active."
  :tags '(bv-ui-polish)
  (let ((flymake-diagnostic-functions nil))
    (with-temp-buffer
      (flymake-mode 1)
      (should (local-variable-p 'right-fringe-width))
      (should (= right-fringe-width bv-flymake-diagnostic-fringe-width))
      (flymake-mode -1)
      (should-not (local-variable-p 'right-fringe-width)))))

(ert-deftest bv-ui-polish-corfu-kind-labels-have-trailing-gap ()
  "Corfu kind labels should reserve a trailing gutter before candidates."
  :tags '(bv-ui-polish)
  (let ((label (bv-corfu--format-kind 'function)))
    (should (= (string-width label) bv-corfu-kind-label-width))
    (should (string-suffix-p " " label))))

(ert-deftest bv-ui-polish-keycast-fallback-separates-fields ()
  "Keycast fallback mode-line format should not glue key and command text."
  :tags '(bv-ui-polish)
  (skip-unless (boundp 'keycast-mode-line-format))
  (should (string-match-p " → " keycast-mode-line-format)))

(ert-deftest bv-ui-polish-ibuffer-status-icon-cluster-has-gutters ()
  "Ibuffer status and icon columns should not run into the buffer name."
  :tags '(bv-ui-polish)
  (let ((format (car nerd-icons-ibuffer-formats)))
    (should (equal (seq-take format 7)
                   '(mark " " modified " " read-only "  "
                          nerd-icons-ibuffer-icon)))
    (should (member "  " format))))

(ert-deftest bv-ui-polish-magit-section-indicators-have-weight ()
  "Magit section indicators should be deliberate, not faint hairlines."
  :tags '(bv-ui-polish)
  (require 'magit-section)
  (should (>= bv-git-magit-section-fringe-width 16))
  (should (equal (car magit-section-visibility-indicators)
                 '(magit-fringe-bitmap-bold> . magit-fringe-bitmap-boldv)))
  (should (string-prefix-p
           " "
           (car (cadr magit-section-visibility-indicators)))))

(ert-deftest bv-ui-polish-git-gutter-bitmaps-have-substance ()
  "Git gutter bitmaps should be visible without becoming blocks."
  :tags '(bv-ui-polish)
  (should (equal bv-git--fringe-bar-bitmap [#b00011000]))
  (should (>= (length bv-git--fringe-deleted-bitmap) 4)))

(provide 'bv-ui-polish-tests)
;;; bv-ui-polish-tests.el ends here
