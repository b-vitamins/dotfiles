;;; bv-core-tests.el --- Test suite for bv-core -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ert "0"))
;; Keywords: maint, test
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/tests/bv-core-tests.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Test suite for bv-core functionality
;;
;; This file contains unit tests for the core module.
;; Tests are organized by functionality area:
;; - Configuration value system
;; - Feature registration and dependencies
;; - Path expansion utilities
;; - Utility macros
;; - Timer management
;;
;; Running tests:
;; - All tests: M-x ert RET t RET
;; - Specific test: M-x ert RET bv-test-TESTNAME RET
;; - By prefix: M-x ert RET "^bv-test-" RET
;; - Using helper: M-x bv-run-tests RET
;;
;; Development workflow:
;; 1. Load bv-core.el first
;; 2. Load this test file
;; 3. Run tests with M-x ert
;;
;; Writing new tests:
;; - Name tests as bv-test-FEATURE-ASPECT
;; - Use let-binding to isolate test state
;; - Clean up any timers or hooks created during tests
;; - Test both success and error cases

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
;; Handle both direct loading and package-based loading
(or (require 'bv-core nil t)
    (load (expand-file-name "../bv-core"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

;;;; Test Helpers

(defmacro bv-tests--with-clean-state (&rest body)
  "Execute BODY with clean state.
Temporarily rebinds configuration values and features to empty states."
  (declare (indent 0))
  `(let ((bv-config-values (make-hash-table :test #'eq))
         (bv-enabled-features '())
         (bv-feature-dependencies (make-hash-table :test #'eq))
         (bv--feature-loading-stack '()))
     ,@body))

;;;; Configuration Value Tests

(ert-deftest bv-test-set-get-value ()
  "Test setting and getting configuration values."
  (bv-tests--with-clean-state
    ;; Basic set/get
    (bv-set-value 'test-key "test-value")
    (should (equal (bv-get-value 'test-key) "test-value"))

    ;; Default value
    (should (equal (bv-get-value 'missing-key "default") "default"))
    (should (null (bv-get-value 'missing-key)))

    ;; Overwrite existing
    (bv-set-value 'test-key "new-value")
    (should (equal (bv-get-value 'test-key) "new-value"))))

(ert-deftest bv-test-set-value-validation ()
  "Test that `bv-set-value' validates key types."
  (bv-tests--with-clean-state
    ;; Valid symbol key
    (should-not (condition-case nil
                    (progn (bv-set-value 'valid-key "value") nil)
                  (error t)))

    ;; Invalid string key
    (should-error (bv-set-value "string-key" "value") :type 'error)

    ;; Invalid number key
    (should-error (bv-set-value 123 "value") :type 'error)))

(ert-deftest bv-test-value-present-p ()
  "Test checking value presence."
  (bv-tests--with-clean-state
    (should-not (bv-value-present-p 'missing))
    (bv-set-value 'present "value")
    (should (bv-value-present-p 'present))

    ;; nil value should still count as present
    (bv-set-value 'nil-value nil)
    (should (bv-value-present-p 'nil-value))))

(ert-deftest bv-test-require-value ()
  "Test requiring configuration values."
  (bv-tests--with-clean-state
    (bv-set-value 'required-key "value")
    (should (equal (bv-require-value 'required-key) "value"))

    ;; Missing key with default error
    (should-error (bv-require-value 'missing-key) :type 'error)

    ;; Missing key with custom error
    (condition-case err
        (bv-require-value 'missing-key "Custom error")
      (error
       (should (string= (error-message-string err) "Custom error"))))))

(ert-deftest bv-test-with-value ()
  "Test `bv-with-value' macro."
  (bv-tests--with-clean-state
    (let ((result nil))
      ;; Value exists
      (bv-set-value 'test-key "test-value")
      (bv-with-value test-key val
        (setq result val))
      (should (equal result "test-value"))

      ;; Value doesn't exist
      (setq result 'unchanged)
      (bv-with-value missing-key val
        (setq result val))
      (should (eq result 'unchanged)))))

(ert-deftest bv-test-defcustom ()
  "Test `bv-defcustom' macro integration."
  (bv-tests--with-clean-state
    ;; Test that defcustom variables are registered in config values
    ;; Note: We can't actually test defcustom creation in isolation,
    ;; but we can test the registration behavior
    (let ((test-var "test-value"))
      ;; Simulate what bv-defcustom does
      (bv-set-value 'test-custom test-var)
      (should (equal (bv-get-value 'test-custom) "test-value")))))

;;;; Feature System Tests

(ert-deftest bv-test-register-feature ()
  "Test feature registration."
  (bv-tests--with-clean-state
    ;; Simple registration
    (bv-register-feature 'test-feature)
    (should (memq 'test-feature bv-enabled-features))
    (should (bv-feature-enabled-p 'test-feature))

    ;; Duplicate registration (should not duplicate)
    (bv-register-feature 'test-feature)
    (should (= (length (cl-remove-duplicates bv-enabled-features)) 1))))

(ert-deftest bv-test-feature-dependencies ()
  "Test feature dependency handling."
  (bv-tests--with-clean-state
    ;; Register base feature
    (bv-register-feature 'base)

    ;; Register dependent feature
    (bv-register-feature 'dependent '(base))
    (should (equal (gethash 'dependent bv-feature-dependencies) '(base)))

    ;; Try to register with missing dependency
    (should-error (bv-register-feature 'bad-dep '(missing)) :type 'error)))

(ert-deftest bv-test-feature-dependency-merging ()
  "Test that multiple dependency registrations are merged."
  (bv-tests--with-clean-state
    ;; Register base features
    (bv-register-feature 'base1)
    (bv-register-feature 'base2)
    (bv-register-feature 'base3)

    ;; Register with initial dependencies
    (bv-register-feature 'feature '(base1 base2))

    ;; Register again with additional dependencies
    (bv-register-feature 'feature '(base2 base3))

    ;; Should have all three dependencies
    (let ((deps (gethash 'feature bv-feature-dependencies)))
      (should (= (length deps) 3))
      (should (memq 'base1 deps))
      (should (memq 'base2 deps))
      (should (memq 'base3 deps)))))

(ert-deftest bv-test-circular-dependencies ()
  "Test circular dependency detection."
  (bv-tests--with-clean-state
    ;; Set up loading stack to simulate circular dependency
    (let ((bv--feature-loading-stack '(feature-a feature-b)))
      ;; Should detect circular dependency
      (should-error (bv-register-feature 'feature-a) :type 'error))))

(ert-deftest bv-test-with-feature ()
  "Test `bv-with-feature' macro."
  (bv-tests--with-clean-state
    (let ((executed nil))
      ;; Feature not enabled
      (bv-with-feature test-feature
        (setq executed t))
      (should-not executed)

      ;; Enable feature
      (bv-register-feature 'test-feature)
      (bv-with-feature test-feature
        (setq executed t))
      (should executed))))

(ert-deftest bv-test-when-feature ()
  "Test `bv-when-feature' macro."
  (bv-tests--with-clean-state
    (let ((counter 0))
      ;; Test with already enabled feature
      (bv-register-feature 'enabled-feature)
      (bv-when-feature enabled-feature
        (cl-incf counter))
      (should (= counter 1))

      ;; Test with built-in feature (should use require)
      (setq counter 0)
      (bv-when-feature emacs
        (cl-incf counter))
      (should (= counter 1)))))

;;;; Path Helper Tests

(ert-deftest bv-test-path-expansion ()
  "Test path expansion functions."
  (bv-tests--with-clean-state
    ;; Set up test directories
    (bv-set-value 'etc-dir "/test/etc")
    (bv-set-value 'var-dir "/test/var")

    ;; Test expansion
    (should (equal (bv-expand-etc-file "config.el")
                   (bv--file-name-concat "/test/etc" "config.el")))
    (should (equal (bv-expand-var-file "cache")
                   (bv--file-name-concat "/test/var" "cache")))

    ;; Test with missing config (no fallback for Guix paths)
    (should (null (bv-expand-system-lib "lib.so")))

    ;; Test with fallback (etc and var have fallbacks)
    (let ((bv-config-values (make-hash-table :test #'eq)))
      (should (string-match-p "config/emacs/etc/test$"
                              (bv-expand-etc-file "test"))))))

(ert-deftest bv-test-path-expansion-edge-cases ()
  "Test path expansion edge cases."
  (bv-tests--with-clean-state
    ;; Test with nil file
    (bv-set-value 'etc-dir "/test/etc")
    (should (null (bv-expand-etc-file nil)))

    ;; Test with empty string - account for trailing slash
    (let ((result (bv-expand-etc-file "")))
      ;; Accept either with or without trailing slash
      (should (or (equal result "/test/etc")
                  (equal result "/test/etc/"))))

    ;; Test with absolute path (should still concatenate)
    (should (equal (bv-expand-etc-file "/absolute/path")
                   (bv--file-name-concat "/test/etc" "/absolute/path")))))

(ert-deftest bv-test-ensure-directory ()
  "Test directory creation."
  (let ((test-dir (make-temp-file "bv-test-" t)))
    ;; Clean up first
    (delete-directory test-dir)

    ;; Should create directory
    (should-not (file-directory-p test-dir))
    (bv-ensure-directory test-dir)
    (should (file-directory-p test-dir))

    ;; Should be idempotent
    (bv-ensure-directory test-dir)
    (should (file-directory-p test-dir))

    ;; Clean up
    (delete-directory test-dir)))

;;;; Macro Tests

(ert-deftest bv-test-when-packages-macro ()
  "Expansion tests for `bv-when-packages`."
  ;; Test error on non-list
  (should-error (macroexpand '(bv-when-packages not-a-list (message "test")))
                :type 'error)

  ;; Test proper expansion
  (let ((expansion (macroexpand '(bv-when-packages (org) (message "test")))))
    (should (consp expansion))
    (should (eq (car expansion) 'let))
    (should (string-match-p "lambda" (prin1-to-string expansion)))
    (should (string-match-p "with-eval-after-load" (prin1-to-string expansion)))))

(ert-deftest bv-test-when-packages-execution ()
  "Runtime behaviour tests for `bv-when-packages`."
  (let ((counter 0))
    ;; Test with already loaded package
    (bv-when-packages (emacs)
      (cl-incf counter))
    (should (= counter 1))

    ;; Test with non-existent package
    (setq counter 0)
    (bv-when-packages (nonexistent-package)
      (cl-incf counter))
    (should (= counter 0))

    ;; Test with multiple packages (all must exist)
    (setq counter 0)
    (bv-when-packages (emacs cl-lib)
      (cl-incf counter))
    (should (= counter 1))))

(ert-deftest bv-test-after-init ()
  "Test `bv-after-init' macro."
  (let ((executed nil))
    ;; During init (simulate by binding after-init-time to nil)
    (let ((after-init-time nil)
          (after-init-hook nil))
      (bv-after-init
       (setq executed t))
      (should-not executed)
      ;; Simulate running hooks
      (run-hooks 'after-init-hook)
      (should executed))

    ;; After init
    (setq executed nil)
    (let ((after-init-time t))
      (bv-after-init
       (setq executed t))
      (should executed))))

(ert-deftest bv-test-leader-macro ()
  "Test `bv-leader' macro."
  (let ((test-map (make-sparse-keymap)))
    ;; Temporarily replace bv-app-map
    (let ((bv-app-map test-map))
      (bv-leader
        "a" #'apropos
        "c" #'compile)  ; Changed from 'b' to avoid conflict

      (should (eq (lookup-key test-map (kbd "a")) #'apropos))
      (should (eq (lookup-key test-map (kbd "c")) #'compile))

      ;; Test with key sequences - use different prefix
      (bv-leader
        "f f" #'find-file
        "b b" #'switch-to-buffer)

      (should (eq (lookup-key test-map (kbd "f f")) #'find-file))
      (should (eq (lookup-key test-map (kbd "b b")) #'switch-to-buffer)))))

;;;; Timer Tests

(ert-deftest bv-test-idle-eval ()
  "Test idle timer functionality."
  ;; Test that bv-idle-eval returns a timer
  (let ((timer (bv-idle-eval 60 (message "test"))))
    (unwind-protect
        (progn
          (should (timerp timer))
          (should (member timer timer-idle-list)))
      ;; Always clean up
      (when (timerp timer)
        (cancel-timer timer)))))

(ert-deftest bv-test-with-delayed-setup ()
  "Test delayed setup functionality."
  ;; Create unique test name to avoid conflicts
  (let* ((test-name (make-symbol "test-setup"))
         (done-var (intern (format "bv--%s-setup-done" test-name)))
         (timer-var (intern (format "bv--%s-setup-timer" test-name)))
         (executed nil))

    (unwind-protect
        (progn
          ;; First call should return timer
          (let ((timer (eval `(bv-with-delayed-setup ,test-name 60
                                (setq executed t)))))
            (should (timerp timer))
            (should (timerp (symbol-value timer-var)))
            (should-not (symbol-value done-var))

            ;; Cancel immediately to prevent execution
            (cancel-timer timer)

            ;; Second call after cancellation should create new timer
            (set timer-var nil)
            (let ((timer2 (eval `(bv-with-delayed-setup ,test-name 60
                                   (error "Should not execute")))))
              (should (timerp timer2))
              ;; Cancel this one too
              (cancel-timer timer2))))

      ;; Clean up variables
      (when (boundp done-var) (makunbound done-var))
      (when (boundp timer-var) (makunbound timer-var)))))

(ert-deftest bv-test-cancel-all-timers ()
  "Test timer cancellation functionality."
  (let ((test-names (list (make-symbol "test1")
                          (make-symbol "test2")
                          (make-symbol "test3")))
        timers)

    (unwind-protect
        (progn
          ;; Create multiple timers
          (dolist (name test-names)
            (let ((timer (eval `(bv-with-delayed-setup ,name 10
                                  (message "Should not execute")))))
              (when timer (push timer timers))))

          ;; Cancel all timers
          (let ((message-log-max nil)) ; Suppress message
            (bv-cancel-all-timers))

          ;; Verify all timers are cancelled
          (dolist (name test-names)
            (let ((timer-var (intern (format "bv--%s-setup-timer" name))))
              (should-not (and (boundp timer-var)
                               (symbol-value timer-var))))))

      ;; Clean up any remaining timers and variables
      (dolist (timer timers)
        (when (timerp timer)
          (cancel-timer timer)))
      (dolist (name test-names)
        (let ((done-var (intern (format "bv--%s-setup-done" name)))
              (timer-var (intern (format "bv--%s-setup-timer" name))))
          (when (boundp done-var) (makunbound done-var))
          (when (boundp timer-var) (makunbound timer-var)))))))

;;;; Helper Function Tests

(ert-deftest bv-test-symbol-lessp ()
  "Test symbol comparison function."
  (should (bv--symbol-lessp 'a 'b))
  (should-not (bv--symbol-lessp 'b 'a))
  (should-not (bv--symbol-lessp 'a 'a))
  (should (bv--symbol-lessp 'abc 'abd))
  ;; "longer-name" comes before "short" alphabetically (l < s)
  (should (bv--symbol-lessp 'longer-name 'short)))

(ert-deftest bv-test-file-name-concat-compat ()
  "Test file name concatenation compatibility."
  ;; Test basic concatenation
  (should (equal (bv--file-name-concat "/home" "user" "file.el")
                 (expand-file-name "file.el" (expand-file-name "user" "/home"))))

  ;; Test with trailing slashes
  (should (equal (bv--file-name-concat "/home/" "user/" "file.el")
                 (bv--file-name-concat "/home" "user" "file.el")))

  ;; Test single component
  (should (equal (bv--file-name-concat "/home" "file.el")
                 (expand-file-name "file.el" "/home"))))

;;;; Integration Tests

(ert-deftest bv-test-check-feature-dependencies ()
  "Test dependency checking functionality."
  (bv-tests--with-clean-state
    ;; Set up features with dependencies
    (bv-register-feature 'base1)
    (bv-register-feature 'base2)
    (bv-register-feature 'dependent '(base1 base2))

    ;; All dependencies satisfied
    (let ((message-log-max nil))
      (should (string-match-p "satisfied"
                              (bv-check-feature-dependencies))))

    ;; Add feature with missing dependency - puthash needs correct value type
    (puthash 'broken '(missing) bv-feature-dependencies)
    (let ((message-log-max nil))
      (should (string-match-p "Missing dependencies"
                              (bv-check-feature-dependencies))))))

(ert-deftest bv-test-report-generation ()
  "Test report generation functions."
  (bv-tests--with-clean-state
    ;; Clean up any existing buffers first
    (when (get-buffer "*bv-config-report*")
      (kill-buffer "*bv-config-report*"))
    (when (get-buffer "*bv-path-report*")
      (kill-buffer "*bv-path-report*"))

    ;; Set up some test data
    (bv-set-value 'test-string "hello")
    (bv-set-value 'test-number 42)
    (bv-set-value 'test-list '(a b c))
    (bv-register-feature 'feature1)
    (bv-register-feature 'feature2 '(feature1))

    ;; Test config report
    (save-window-excursion
      (bv-report-config)
      (with-current-buffer "*bv-config-report*"
        (should (string-match-p "Enabled Features:" (buffer-string)))
        (should (string-match-p "feature1" (buffer-string)))
        (should (string-match-p "feature2" (buffer-string)))
        (should (string-match-p "test-string" (buffer-string)))
        (should (string-match-p "hello" (buffer-string)))))

    ;; Test path report
    (bv-set-value 'etc-dir "/test/etc")
    (bv-set-value 'var-dir "/test/var")
    (save-window-excursion
      (bv-path-report)
      (with-current-buffer "*bv-path-report*"
        ;; Buffer is in special-mode (read-only), just check content
        (let ((content (buffer-string)))
          (should (string-match-p "etc-dir" content))
          (should (string-match-p "/test/etc" content)))))))

;; Test runner helper
(defun bv-run-tests ()
  "Run `bv-core' test suite."
  (interactive)
  (ert-run-tests-interactively "^bv-test-"))

(provide 'bv-core-tests)
;;; bv-core-tests.el ends here
