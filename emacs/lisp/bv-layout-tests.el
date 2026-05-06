;;; bv-layout-tests.el --- Tests for BV layout policy -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; ERT checks for global layout affordances that are easy to regress visually.

;;; Code:

(require 'ert)
(require 'bv-layout)

(ert-deftest bv-layout-truncation-display-marker-has-gap ()
  "Display-table truncation markers should reserve a leading gap."
  :tags '(bv-layout)
  (let ((slot (display-table-slot standard-display-table 'truncation)))
    (should (vectorp slot))
    (should (>= (length slot) 2))
    (should (equal (aref slot 0)
                   (make-glyph-code ?\s 'bv-layout-fallback)))))

(ert-deftest bv-layout-wrap-display-marker-has-gap ()
  "Display-table wrap markers should reserve a leading gap."
  :tags '(bv-layout)
  (let ((slot (display-table-slot standard-display-table 'wrap)))
    (should (vectorp slot))
    (should (>= (length slot) 2))
    (should (equal (aref slot 0)
                   (make-glyph-code ?\s 'bv-layout-fallback)))))

(ert-deftest bv-layout-truncation-fringe-uses-spaced-bitmaps ()
  "Graphical truncation indicators should use BV-spaced fringe bitmaps."
  :tags '(bv-layout)
  (should (equal (alist-get 'truncation fringe-indicator-alist)
                 '(bv-layout-truncation-left bv-layout-truncation-right))))

(provide 'bv-layout-tests)
;;; bv-layout-tests.el ends here
