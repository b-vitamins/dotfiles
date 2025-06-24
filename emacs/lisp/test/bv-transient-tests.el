;;; bv-transient-tests.el --- Test suite for bv-transient -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ert "0"))
;; Keywords: maint, test
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/test/bv-transient-tests.el

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
;; Test suite for bv-transient functionality
;;
;; This file contains unit tests for the transient menu interface.
;; Tests cover:
;; - Interactive command wrappers
;; - Value manipulation functions
;; - Feature management commands
;; - Path expansion helpers
;; - Developer tools
;;
;; Running tests:
;; - All tests: M-x ert RET t RET
;; - Specific test: M-x ert RET bv-transient-test-TESTNAME RET
;; - By prefix: M-x ert RET "^bv-transient-test-" RET
;; - Using helper: M-x bv-transient-run-tests RET
;;
;; Note: Some tests use mocking to avoid actual file system operations
;; and interactive prompts.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the modules under test
(or (require 'bv-core nil t)
    (load (expand-file-name "../bv-core"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

(or (require 'bv-transient nil t)
    (load (expand-file-name "../bv-transient"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

;;;; Test Helpers

(defmacro bv-transient-tests--with-clean-state (&rest body)
  "Execute BODY with clean state.
Temporarily rebinds configuration values and features to empty states."
  (declare (indent 0))
  `(let ((bv-config-values (make-hash-table :test #'eq))
         (bv-enabled-features '())
         (bv-feature-dependencies (make-hash-table :test #'eq))
         (bv--feature-loading-stack '())
         (kill-ring nil))
     ,@body))

(defmacro bv-transient-tests--with-mocked-input (input &rest body)
  "Execute BODY with mocked interactive input.
INPUT should be a string that will be returned by `read-string' and
similar functions."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'read-string)
              (lambda (&rest _) ,input))
             ((symbol-function 'completing-read)
              (lambda (&rest _) ,input))
             ((symbol-function 'y-or-n-p)
              (lambda (&rest _) t))
             ;; Prevent transient menus from flashing during tests
             ((symbol-function 'transient--show)
              #'ignore))
     ,@body))

;;;; Value Manipulation Tests

(ert-deftest bv-transient-test-menu-set-value ()
  "Test `bv-menu-set-value' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test setting a new value
    (ert-info ("Setting a new value")
      (bv-transient-tests--with-mocked-input "test-key"
        (cl-letf (((symbol-function 'read-string)
                   (let ((call-count 0))
                     (lambda (&rest _)
                       (cl-incf call-count)
                       (if (= call-count 1) "test-key" "test-value")))))
          (bv-menu-set-value 'test-key "test-value")
          (should (equal (bv-get-value 'test-key) "test-value")))))

    ;; Test setting with existing value
    (ert-info ("Updating an existing value")
      (bv-set-value 'existing-key "old-value")
      (bv-transient-tests--with-mocked-input "new-value"
        (bv-menu-set-value 'existing-key "new-value")
        (should (equal (bv-get-value 'existing-key) "new-value"))))

    ;; Test malformed Lisp input
    (ert-info ("Handling malformed Lisp input")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args))))
                  ((symbol-function 'read-string)
                   (let ((call-count 0))
                     (lambda (&rest _)
                       (cl-incf call-count)
                       (if (= call-count 1) "bad-key" "(unmatched paren")))))
          (bv-menu-set-value 'bad-key "(unmatched paren")
          (should (equal (bv-get-value 'bad-key) "(unmatched paren"))
          (should (string-match-p "Set 'bad-key' to \"(unmatched paren\""
                                  message-called)))))))

(ert-deftest bv-transient-test-menu-get-value ()
  "Test `bv-menu-get-value' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test getting existing value
    (ert-info ("Getting an existing value")
      (bv-set-value 'test-key "test-value")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-get-value 'test-key)
          (should (string-match-p "'test-key' = \"test-value\"" message-called)))))

    ;; Test getting non-existent value
    (ert-info ("Getting a non-existent value")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-get-value 'missing-key)
          (should (string-match-p "Key 'missing-key' not found" message-called)))))

    ;; Test interactive completion path
    (ert-info ("Testing interactive completion")
      (bv-set-value 'alpha-key "alpha")
      (bv-set-value 'beta-key "beta")
      (let ((completion-result nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt choices &rest _)
                     (setq completion-result choices)
                     "alpha-key")))
          (call-interactively #'bv-menu-get-value)
          (should (member "alpha-key" completion-result))
          (should (member "beta-key" completion-result)))))))

(ert-deftest bv-transient-test-menu-require-value ()
  "Test `bv-menu-require-value' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test requiring existing value
    (ert-info ("Requiring an existing value")
      (bv-set-value 'required-key "required-value")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-require-value 'required-key)
          (should (string-match-p
                   "'required-key' = \"required-value\" (required)"
                   message-called)))))

    ;; Test requiring missing value
    (ert-info ("Requiring a missing value triggers error")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-require-value 'missing-key)
          (should (string-match-p "Error:" message-called)))))))

(ert-deftest bv-transient-test-menu-check-value-presence ()
  "Test `bv-menu-check-value-presence' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test present value
    (ert-info ("Checking a present value")
      (bv-set-value 'present-key "present-value")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-check-value-presence 'present-key)
          (should (string-match-p "Key 'present-key' is present" message-called)))))

    ;; Test absent value
    (ert-info ("Checking an absent value")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-check-value-presence 'absent-key)
          (should (string-match-p "Key 'absent-key' is NOT present" message-called)))))))

;;;; Feature Management Tests

(ert-deftest bv-transient-test-menu-register-feature ()
  "Test `bv-menu-register-feature' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test registering feature without dependencies
    (ert-info ("Registering feature without dependencies")
      (cl-letf (((symbol-function 'read-string)
                 (let ((call-count 0))
                   (lambda (&rest _)
                     (cl-incf call-count)
                     (if (= call-count 1) "test-feature" "")))))
        (let ((message-called nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-called (apply #'format fmt args)))))
            (bv-menu-register-feature 'test-feature nil)
            (should (bv-feature-enabled-p 'test-feature))
            (should (string-match-p "Registered feature 'test-feature'"
                                    message-called))))))

    ;; Test registering feature with dependencies
    (ert-info ("Registering feature with dependencies")
      (bv-register-feature 'base-feature)
      (cl-letf (((symbol-function 'read-string)
                 (let ((call-count 0))
                   (lambda (&rest _)
                     (cl-incf call-count)
                     (if (= call-count 1) "dependent-feature" "base-feature")))))
        (bv-menu-register-feature 'dependent-feature '(base-feature))
        (should (bv-feature-enabled-p 'dependent-feature))
        (should (equal (gethash 'dependent-feature bv-feature-dependencies)
                       '(base-feature)))))

    ;; Test empty feature name error
    (ert-info ("Handling empty feature name")
      (let ((error-signaled nil))
        (cl-letf (((symbol-function 'read-string)
                   (let ((call-count 0))
                     (lambda (&rest _)
                       (cl-incf call-count)
                       (if (= call-count 1) "" "")))))
          (condition-case err
              (call-interactively #'bv-menu-register-feature)
            (error
             (setq error-signaled (error-message-string err))))
          ;; Should see an error about empty feature name
          (should error-signaled)
          (should (string-match-p "Feature name cannot be empty or blank"
                                  error-signaled)))))

    ;; Test whitespace-only feature name error
    (ert-info ("Handling whitespace-only feature name")
      (let ((error-signaled nil))
        (cl-letf (((symbol-function 'read-string)
                   (let ((call-count 0))
                     (lambda (&rest _)
                       (cl-incf call-count)
                       (if (= call-count 1) "   " "")))))
          (condition-case err
              (call-interactively #'bv-menu-register-feature)
            (error
             (setq error-signaled (error-message-string err))))
          ;; Should see an error about blank feature name
          (should error-signaled)
          (should (string-match-p "Feature name cannot be empty or blank"
                                  error-signaled)))))

    ;; Test circular dependency error
    (ert-info ("Handling circular dependency error")
      ;; Set up a circular dependency scenario
      ;; First register both features
      (bv-register-feature 'feat-a)
      (bv-register-feature 'feat-b)

      ;; Add dependency from feat-a to feat-b
      (bv-register-feature 'feat-a '(feat-b))

      ;; Now try to make feat-b depend on feat-a through the menu command
      ;; This should trigger a circular dependency error
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args))))
                  ((symbol-function 'read-string)
                   (let ((call-count 0))
                     (lambda (&rest _)
                       (cl-incf call-count)
                       (if (= call-count 1) "feat-b" "feat-a")))))
          ;; Call the menu function which should catch and report the error
          (bv-menu-register-feature 'feat-b '(feat-a))
          ;; Verify we got an error message
          (should message-called)
          (should (string-match-p "Error:" message-called))
          ;; The message should contain information about circular dependency
          ;; It will be "Error: Circular dependency detected:
          ;; feat-b -> feat-a -> feat-b"
          (should (string-match-p "[Cc]ircular dependency" message-called)))))))

(ert-deftest bv-transient-test-menu-check-feature ()
  "Test `bv-menu-check-feature' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test checking enabled feature
    (ert-info ("Checking an enabled feature")
      (bv-register-feature 'enabled-feature)
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-check-feature 'enabled-feature)
          (should (string-match-p "Feature 'enabled-feature' is enabled" message-called)))))

    ;; Test checking disabled feature
    (ert-info ("Checking a disabled feature")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-check-feature 'disabled-feature)
          (should (string-match-p "Feature 'disabled-feature' is NOT enabled"
                                  message-called)))))))

(ert-deftest bv-transient-test-menu-list-features ()
  "Test `bv-menu-list-features' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with no features
    (ert-info ("Listing when no features are registered")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-list-features)
          (should (string-match-p "No features registered" message-called)))))

    ;; Test with multiple features
    (ert-info ("Listing multiple features in alphabetical order")
      (bv-register-feature 'feature-b)
      (bv-register-feature 'feature-a)
      (bv-register-feature 'feature-c)
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-list-features)
          (should (string-match-p
                   "Enabled features: feature-a, feature-b, feature-c"
                   message-called)))))))

;;;; Path Operation Tests

(ert-deftest bv-transient-test-menu-expand-etc ()
  "Test `bv-menu-expand-etc' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with configured etc-dir
    (ert-info ("Expanding path with configured etc-dir")
      (bv-set-value 'etc-dir "/test/etc")
      (bv-transient-tests--with-mocked-input "config.el"
        (bv-menu-expand-etc "config.el")
        (should (equal (car kill-ring)
                       (bv--file-name-concat "/test/etc" "config.el")))))

    ;; Test without configured etc-dir
    (ert-info ("Handling missing etc-dir configuration")
      (let ((bv-config-values (make-hash-table :test #'eq))
            (message-called nil)
            (kill-ring-before kill-ring))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-expand-etc "config.el")
          ;; Should not add nil to kill-ring
          (should (equal kill-ring kill-ring-before))
          (should (string-match-p "etc-dir not configured" message-called)))))))

(ert-deftest bv-transient-test-menu-expand-var ()
  "Test `bv-menu-expand-var' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with configured var-dir
    (ert-info ("Expanding path with configured var-dir")
      (bv-set-value 'var-dir "/test/var")
      (bv-transient-tests--with-mocked-input "cache"
        (bv-menu-expand-var "cache")
        (should (equal (car kill-ring)
                       (bv--file-name-concat "/test/var" "cache")))))

    ;; Test without configured var-dir
    (ert-info ("Handling missing var-dir configuration")
      (let ((bv-config-values (make-hash-table :test #'eq))
            (message-called nil)
            (kill-ring-before kill-ring))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-expand-var "cache")
          ;; Should not add nil to kill-ring
          (should (equal kill-ring kill-ring-before))
          (should (string-match-p "var-dir not configured" message-called)))))))

(ert-deftest bv-transient-test-menu-ensure-directory ()
  "Test `bv-menu-ensure-directory' functionality."
  (bv-transient-tests--with-clean-state
    (ert-info ("Creating a new directory")
      (let ((test-dir (make-temp-name "/tmp/bv-test-"))
            (message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args))))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) test-dir)))
          (unwind-protect
              (progn
                (bv-menu-ensure-directory test-dir)
                (should (file-directory-p test-dir))
                (should (string-match-p "Directory exists:" message-called)))
            ;; Clean up
            (when (file-directory-p test-dir)
              (delete-directory test-dir))))))))

(ert-deftest bv-transient-test-menu-expand-system ()
  "Test `bv-menu-expand-system' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with configured system directory
    (ert-info ("Expanding path with configured system directory")
      (bv-set-value 'system-lib-dir "/usr/lib")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "lib"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "test.so")))
        (bv-menu-expand-system "lib")
        (should (equal (car kill-ring) "/usr/lib/test.so"))))

    ;; Test with unconfigured directory
    (ert-info ("Handling unconfigured system directory")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "bin")))
          (bv-menu-expand-system "bin")
          (should (string-match-p "System bin directory not configured"
                                  message-called)))))))

(ert-deftest bv-transient-test-menu-expand-home ()
  "Test `bv-menu-expand-home' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with configured home directory
    (ert-info ("Expanding path with configured home directory")
      (bv-set-value 'home-share-dir "~/.local/share")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "share"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "icons")))
        (bv-menu-expand-home "share")
        (should (equal (car kill-ring)
                       (file-name-concat "~/.local/share" "icons")))))))

;;;; Developer Tools Tests

(ert-deftest bv-transient-test-menu-reload-config ()
  "Test `bv-menu-reload-config' functionality."
  (bv-transient-tests--with-clean-state
    (ert-info ("Reloading configuration after confirmation")
      (let ((load-called nil)
            (timers-cancelled nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'bv-cancel-all-timers)
                   (lambda () (setq timers-cancelled t)))
                  ((symbol-function 'load-library)
                   (lambda (lib) (setq load-called lib))))
          (bv-menu-reload-config)
          (should timers-cancelled)
          (should (equal load-called "bv-core")))))))

(ert-deftest bv-transient-test-menu-run-tests ()
  "Test `bv-menu-run-tests' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with ERT available
    (ert-info ("Running tests when ERT is available")
      (let ((ert-called nil))
        (cl-letf (((symbol-function 'ert)
                   (lambda (pattern) (setq ert-called pattern))))
          (bv-menu-run-tests)
          (should (equal ert-called "^bv-test-")))))

    ;; Test without ERT (mock failure)
    (ert-info ("Handling missing ERT")
      (let ((message-called nil))
        (cl-letf (((symbol-function 'require)
                   (lambda (&rest _) nil))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-run-tests)
          (should (string-match-p "ERT not available" message-called)))))))

(ert-deftest bv-transient-test-menu-byte-compile ()
  "Test `bv-menu-byte-compile' functionality."
  (bv-transient-tests--with-clean-state
    ;; Test with configured lisp-dir
    (ert-info ("Compiling files in configured lisp-dir")
      (bv-set-value 'lisp-dir "/test/lisp")
      (let ((compiled-files '())
            (message-called nil))
        (cl-letf (((symbol-function 'directory-files)
                   (lambda (dir &rest _)
                     (when (equal dir "/test/lisp")
                       '("/test/lisp/bv-core.el"
                         "/test/lisp/bv-ui.el"))))
                  ((symbol-function 'byte-compile-file)
                   (lambda (file) (push file compiled-files)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args))))
                  ((symbol-function 'get-buffer)
                   (lambda (_) nil))
                  ((symbol-function 'get-buffer-create)
                   (lambda (name) (get-buffer-create name))))
          (bv-menu-byte-compile)
          (should (= (length compiled-files) 2))
          (should (string-match-p "Compiled 2 files" message-called)))))

    ;; Test with file error during compilation
    (ert-info ("Handling file errors during compilation")
      (bv-set-value 'lisp-dir "/test/lisp")
      (let ((compiled-count 0)
            (failed-count 0)
            (message-calls '())
            (compile-log-shown nil))
        (cl-letf (((symbol-function 'directory-files)
                   (lambda (dir &rest _)
                     (when (equal dir "/test/lisp")
                       '("/test/lisp/bv-core.el"
                         "/test/lisp/bv-broken.el"))))
                  ((symbol-function 'byte-compile-file)
                   (lambda (file)
                     (if (string-match-p "broken" file)
                         (signal 'file-error '("Syntax error"))
                       (cl-incf compiled-count))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) message-calls)))
                  ((symbol-function 'get-buffer)
                   (lambda (_) t))
                  ((symbol-function 'get-buffer-create)
                   (lambda (name) (generate-new-buffer name)))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (setq compile-log-shown t))))
          (bv-menu-byte-compile)
          (should (= compiled-count 1))
          (should (cl-some (lambda (msg)
                             (string-match-p "Failed to compile" msg))
                           message-calls))
          (should (cl-some (lambda (msg)
                             (string-match-p "Compiled 1 files (1 failed)" msg))
                           message-calls))
          (should compile-log-shown))))

    ;; Test without lisp-dir
    (ert-info ("Handling missing lisp-dir configuration")
      (let ((bv-config-values (make-hash-table :test #'eq))
            (message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))
          (bv-menu-byte-compile)
          (should (string-match-p "lisp-dir not configured" message-called)))))))

(ert-deftest bv-transient-test-menu-timer-help ()
  "Test `bv-menu-timer-help' functionality."
  (bv-transient-tests--with-clean-state
    (ert-info ("Creating timer help buffer with expected content")
      ;; Clean up any existing buffer first
      (when (get-buffer "*bv-timer-help*")
        (kill-buffer "*bv-timer-help*"))

      ;; Call the function
      (bv-menu-timer-help)

      ;; Check that the buffer was created with expected content
      (should (get-buffer "*bv-timer-help*"))
      (with-current-buffer "*bv-timer-help*"
        (let ((content (buffer-string)))
          ;; Check for key content
          (should (string-match-p "Timer Management" content))
          (should (string-match-p "M-x list-timers" content))
          (should (string-match-p "bv-cancel-all-timers" content))
          (should (string-match-p "bv-idle-eval" content))
          (should (string-match-p "Press 'q' to close" content)))
        ;; Check that help-mode is active
        (should (eq major-mode 'help-mode)))

      ;; Clean up
      (kill-buffer "*bv-timer-help*"))))

;;;; Integration Tests

(ert-deftest bv-transient-test-feature-registration ()
  "Test that bv-transient registers itself as a feature."
  (ert-info ("Verifying bv-transient self-registration")
    ;; Load fresh to test registration
    (let ((bv-enabled-features '())
          (bv-feature-dependencies (make-hash-table :test #'eq)))
      (load "bv-transient" nil t)
      (should (bv-feature-enabled-p 'bv-transient)))))

(ert-deftest bv-transient-test-transient-menu-existence ()
  "Test that transient menus are defined."
  (ert-info ("Checking transient menu definitions")
    ;; Check that the transient prefixes exist
    (should (fboundp 'bv-menu))
    (should (fboundp 'bv-menu-advanced))
    ;; Check autoload cookies
    (should (get 'bv-menu 'function-documentation))
    (should (get 'bv-menu-advanced 'function-documentation))))

;; Test runner helper
;;;###autoload
(defun bv-transient-run-tests ()
  "Run `bv-transient' test suite."
  (interactive)
  (ert-run-tests-interactively "^bv-transient-test-"))

(provide 'bv-transient-tests)
;;; bv-transient-tests.el ends here
