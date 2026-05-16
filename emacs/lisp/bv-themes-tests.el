;;; bv-themes-tests.el --- Tests for BV themes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; Focused invariants for the generated BV theme payload.

;;; Code:

(require 'ert)
(require 'bv-themes)

(ert-deftest bv-themes-custom-variable-values-quote-conses ()
  "Theme variables must be Custom expressions, not raw cons values."
  (bv-themes-ensure-theme 'bv-light)
  (let* ((artifact (bv-themes-compile 'bv-light))
         (variables (plist-get artifact :variables)))
    (dolist (variable '(pdf-view-midnight-colors
                        xterm-color-names
                        xterm-color-names-bright))
      (let ((expression (cadr (assq variable variables))))
        (should (eq (car-safe expression) 'quote))
        (should (eval expression t))))))

(provide 'bv-themes-tests)
;;; bv-themes-tests.el ends here
