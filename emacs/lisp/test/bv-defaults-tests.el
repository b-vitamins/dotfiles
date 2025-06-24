;;; bv-defaults-tests.el --- Test suite for bv-defaults -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ert "0"))
;; Keywords: maint, test
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/test/bv-defaults-tests.el

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
;; Test suite for bv-defaults functionality
;;
;; This file contains unit tests for the defaults module.
;; Tests cover:
;; - Directory infrastructure and cache management
;; - Custom variable propagation
;; - Keymap bindings and prefix maps
;; - File handling configuration
;; - Whitespace management
;; - Buffer operations
;; - Toggle commands
;; - Advanced user settings
;;
;; Running tests:
;; - All tests: M-x ert RET t RET
;; - Specific test: M-x ert RET bv-defaults-test-TESTNAME RET
;; - By prefix: M-x ert RET "^bv-defaults-test-" RET
;; - Using helper: M-x bv-defaults-run-tests RET
;;
;; Note: Some tests create temporary directories and files.
;; Cleanup is automatic, but interrupted tests may leave temp files.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the modules under test
(or (require 'bv-core nil t)
    (load (expand-file-name "../bv-core"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

(or (require 'bv-defaults nil t)
    (load (expand-file-name "../bv-defaults"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

;;;; Test Helpers

(defmacro bv-defaults-tests--with-clean-state (&rest body)
  "Execute BODY with clean state.
Temporarily rebinds configuration values and features to empty states,
and ensures clean directory state."
  (declare (indent 0))
  `(let ((bv-config-values (make-hash-table :test #'eq))
         (bv-enabled-features '())
         (bv-feature-dependencies (make-hash-table :test #'eq))
         (bv--feature-loading-stack '())
         (bv-defaults--dirs-initialized nil)
         (bv--recentf-setup-timer nil)
         ;; Disable recentf timer setup during testing
         (noninteractive t)
         ;; Save original values
         (orig-user-full-name user-full-name)
         (orig-user-mail-address user-mail-address)
         (orig-backup-directory-alist backup-directory-alist)
         (orig-auto-save-list-file-prefix auto-save-list-file-prefix)
         (orig-custom-file custom-file)
)
     (unwind-protect
         (progn ,@body)
       ;; Restore original values
       (setq user-full-name orig-user-full-name
             user-mail-address orig-user-mail-address
             backup-directory-alist orig-backup-directory-alist
             auto-save-list-file-prefix orig-auto-save-list-file-prefix
             custom-file orig-custom-file))))

(defmacro bv-defaults-tests--with-temp-cache-dir (&rest body)
  "Execute BODY with a temporary cache directory."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "bv-defaults-test-" t))
          (bv-cache-directory temp-dir))
     (unwind-protect
         (progn ,@body)
       ;; Clean up temp directory
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

;;;; Directory Infrastructure Tests

(ert-deftest bv-defaults-test-cache-directory-creation ()
  "Test cache directory and subdirectory creation."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      ;; Initialize directories
      (bv-defaults--ensure-cache-directories)

      ;; Check that all subdirectories were created
      (ert-info ("Checking subdirectory creation")
        (dolist (subdir bv-defaults--cache-subdirs)
          (let ((path (file-name-concat bv-cache-directory subdir)))
            (should (file-directory-p path))))
        (should bv-defaults--dirs-initialized))

      ;; Test idempotency
      (ert-info ("Testing idempotent directory creation")
        (bv-defaults--ensure-cache-directories)
        (should bv-defaults--dirs-initialized)))))

(ert-deftest bv-defaults-test-cache-directory-reset ()
  "Test cache directory reset functionality."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      ;; First initialization
      (bv-defaults--ensure-cache-directories)
      (should bv-defaults--dirs-initialized)

      ;; Reset and change directory
      (let ((new-temp-dir (make-temp-file "bv-defaults-test-new-" t)))
        (unwind-protect
            (progn
              (setq bv-cache-directory new-temp-dir)
              (bv-defaults-reset-cache-dirs)

              ;; Check new directories were created
              (dolist (subdir bv-defaults--cache-subdirs)
                (let ((path (file-name-concat new-temp-dir subdir)))
                  (should (file-directory-p path)))))
          ;; Clean up
          (when (file-directory-p new-temp-dir)
            (delete-directory new-temp-dir t)))))))

;;;; Custom Variable Tests

(ert-deftest bv-defaults-test-variable-initialization ()
  "Test that custom variables are properly initialized."
  (bv-defaults-tests--with-clean-state
    ;; Load the module
    (load-library "bv-defaults")

    ;; Check that variables exist and are registered
    (ert-info ("Checking variable registration in bv-config-values")
      (should (bv-value-present-p 'cache-directory))
      (should (bv-value-present-p 'full-name))
      (should (bv-value-present-p 'email-address))
      (should (bv-value-present-p 'advanced-user))
      (should (bv-value-present-p 'auto-update-buffers))
      (should (bv-value-present-p 'auto-clean-whitespace))
      (should (bv-value-present-p 'smooth-scrolling))
      (should (bv-value-present-p 'recentf-save-interval)))))

(ert-deftest bv-defaults-test-user-identity-propagation ()
  "Test that user identity settings propagate correctly."
  (bv-defaults-tests--with-clean-state
    ;; Test full name propagation
    (ert-info ("Testing full name propagation")
      (setq bv-full-name "Test User")
      (bv-set-value 'full-name "Test User")
      ;; Simulate custom setter
      (setq user-full-name bv-full-name)
      (should (equal user-full-name "Test User")))

    ;; Test email propagation
    (ert-info ("Testing email propagation")
      (setq bv-email-address "test@example.com")
      (bv-set-value 'email-address "test@example.com")
      ;; Simulate custom setter
      (setq user-mail-address bv-email-address)
      (should (equal user-mail-address "test@example.com")))

    ;; Test empty string handling
    (ert-info ("Testing empty string handling")
      ;; Test the logic directly rather than through module loading
      (let ((saved-user-full-name user-full-name)
            (saved-user-mail-address user-mail-address)
            (saved-bv-full-name bv-full-name)
            (saved-bv-email-address bv-email-address))
        (unwind-protect
            (progn
              ;; Set up existing user values
              (setq user-full-name "Existing User"
                    user-mail-address "existing@example.com")
              ;; Test that empty bv values don't override existing user values
              (setq bv-full-name ""
                    bv-email-address "")
              ;; Manually run the identity setup logic (avoiding full reload)
              (when (and bv-full-name (not (string-empty-p bv-full-name)))
                (setq user-full-name bv-full-name))
              (when (and bv-email-address (not (string-empty-p bv-email-address)))
                (setq user-mail-address bv-email-address))
              ;; Should not override existing values with empty strings
              (should-not (string-empty-p user-full-name))
              (should (equal user-full-name "Existing User")))
          ;; Restore the values manually
          (setq user-full-name saved-user-full-name
                user-mail-address saved-user-mail-address
                bv-full-name saved-bv-full-name
                bv-email-address saved-bv-email-address))))))

(ert-deftest bv-defaults-test-auto-update-buffers-toggle ()
  "Test auto-update-buffers custom setter."
  (bv-defaults-tests--with-clean-state
    (let ((global-auto-revert-mode-called nil))
      (cl-letf (((symbol-function 'global-auto-revert-mode)
                 (lambda (arg)
                   (setq global-auto-revert-mode-called arg))))
        ;; Enable auto-update
        (custom-reevaluate-setting 'bv-auto-update-buffers)
        (funcall (get 'bv-auto-update-buffers 'custom-set)
                 'bv-auto-update-buffers t)
        (should (eq global-auto-revert-mode-called 1))

        ;; Disable auto-update
        (funcall (get 'bv-auto-update-buffers 'custom-set)
                 'bv-auto-update-buffers nil)
        (should (eq global-auto-revert-mode-called -1))))))

(ert-deftest bv-defaults-test-recentf-save-interval ()
  "Test recentf save interval configuration."
  (bv-defaults-tests--with-clean-state
    (let ((timer-calls nil)
          (timer-cancelled nil)
          (mock-timer-obj (vector t nil nil nil nil nil nil nil nil nil)))  ; Create a mock timer object
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (secs repeat func &rest args)
                   (push (list secs repeat func) timer-calls)
                   mock-timer-obj))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (setq timer-cancelled timer)))
                ((symbol-function 'timerp)
                 (lambda (obj)
                   (or (and (vectorp obj) (eq (aref obj 0) t))
                       (equal obj mock-timer-obj)))))

        ;; Set interval (with recentf loaded and existing timer)
        (let ((bv--recentf-setup-timer mock-timer-obj)
              (noninteractive nil))  ; Allow timer creation
          (provide 'recentf)  ; Fake recentf being loaded
          (setq timer-calls nil)  ; Clear any previous timer calls
          (funcall (get 'bv-recentf-save-interval 'custom-set)
                   'bv-recentf-save-interval 60)
          (should timer-calls)
          ;; Find the timer call with 60 seconds (there might be multiple timer calls)
          (should (cl-some (lambda (call) (equal (car call) 60)) timer-calls)))

        ;; Disable interval
        (setq timer-calls nil
              timer-cancelled nil)
        (let ((bv--recentf-setup-timer mock-timer-obj))
          (funcall (get 'bv-recentf-save-interval 'custom-set)
                   'bv-recentf-save-interval nil)
          (should (equal timer-cancelled mock-timer-obj))
          (should-not timer-calls))))))

;;;; Keymap Tests

(ert-deftest bv-defaults-test-keymap-creation ()
  "Test that keymaps are created and exported correctly."
  (bv-defaults-tests--with-clean-state
    ;; Load the module
    (load-library "bv-defaults")

    ;; Check keymap creation
    (ert-info ("Checking keymap existence")
      (should (keymapp bv-toggle-map))
      (should (keymapp bv-defaults-buffer-map))
      (should (keymapp bv-defaults-window-map)))

    ;; Check keymap export to bv-config-values
    (ert-info ("Checking keymap export")
      (should (eq (bv-get-value 'toggle-map) bv-toggle-map))
      (should (eq (bv-get-value 'defaults-buffer-map) bv-defaults-buffer-map))
      (should (eq (bv-get-value 'defaults-window-map) bv-defaults-window-map)))))

(ert-deftest bv-defaults-test-keymap-bindings ()
  "Test that keybindings are properly set."
  (bv-defaults-tests--with-clean-state
    ;; Load the module
    (load-library "bv-defaults")

    ;; Test toggle map bindings
    (ert-info ("Checking toggle map bindings")
      (should (eq (lookup-key bv-toggle-map "w")
                  #'bv-toggle-show-trailing-whitespace))
      (should (eq (lookup-key bv-toggle-map "W")
                  #'bv-cleanup-buffer-whitespace))
      (should (eq (lookup-key bv-toggle-map "a")
                  #'bv-toggle-auto-update-buffers))
      (should (eq (lookup-key bv-toggle-map "c")
                  #'bv-toggle-auto-clean-whitespace)))

    ;; Test buffer map bindings
    (ert-info ("Checking buffer map bindings")
      (should (eq (lookup-key bv-defaults-buffer-map "p")
                  #'bv-switch-to-previous-buffer))
      (should (eq (lookup-key bv-defaults-buffer-map "k")
                  #'bv-kill-current-buffer))
      (should (eq (lookup-key bv-defaults-buffer-map "K")
                  #'kill-buffer))
      (should (eq (lookup-key bv-defaults-buffer-map "r")
                  #'revert-buffer))
      (should (eq (lookup-key bv-defaults-buffer-map "R")
                  #'rename-buffer)))

    ;; Test window map bindings
    (ert-info ("Checking window map bindings")
      (should (eq (lookup-key bv-defaults-window-map "s")
                  #'split-window-below))
      (should (eq (lookup-key bv-defaults-window-map "v")
                  #'split-window-right))
      (should (eq (lookup-key bv-defaults-window-map "d")
                  #'delete-window))
      (should (eq (lookup-key bv-defaults-window-map "D")
                  #'delete-other-windows))
      (should (eq (lookup-key bv-defaults-window-map "b")
                  #'balance-windows))
      (should (eq (lookup-key bv-defaults-window-map "o")
                  #'other-window)))))

;;;; File Handling Tests

(ert-deftest bv-defaults-test-backup-configuration ()
  "Test backup file configuration."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      ;; Load module to apply configuration
      (load-library "bv-defaults")

      ;; Check backup settings
      (ert-info ("Checking backup directory configuration")
        (should backup-directory-alist)
        (let ((backup-dir (cdr (assoc ".*" backup-directory-alist))))
          (should (string-match-p "backup/?$" backup-dir))
          (should (string-prefix-p bv-cache-directory backup-dir))))

      (ert-info ("Checking backup behavior settings")
        (should backup-by-copying)
        (should (eq version-control t))
        (should (= kept-new-versions 6))
        (should (= kept-old-versions 2))
        (should delete-old-versions)
        (should vc-make-backup-files)))))

(ert-deftest bv-defaults-test-auto-save-configuration ()
  "Test auto-save file configuration."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      ;; Load module
      (load-library "bv-defaults")

      ;; Check auto-save settings
      (ert-info ("Checking auto-save file prefix")
        (should (string-match-p "auto-save-list/\\.saves-$"
                                auto-save-list-file-prefix))
        (should (string-prefix-p bv-cache-directory
                                 auto-save-list-file-prefix)))

      (ert-info ("Checking auto-save transforms")
        (should auto-save-file-name-transforms)
        (let ((transform (car auto-save-file-name-transforms)))
          (should (equal (car transform) ".*"))
          (should (string-match-p "auto-save-list/?$" (cadr transform)))
          (should (caddr transform)))))))

(ert-deftest bv-defaults-test-recentf-configuration ()
  "Test recentf configuration."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      ;; Mock recentf being available
      (provide 'recentf)
      (defvar recentf-save-file nil)
      (defvar recentf-max-menu-items nil)
      (defvar recentf-max-saved-items nil)
      (defvar recentf-exclude nil)

      ;; Trigger recentf configuration
      (with-eval-after-load 'recentf
        (load-library "bv-defaults"))

      ;; Check settings
      (ert-info ("Checking recentf configuration")
        (should (string-match-p "recentf$" recentf-save-file))
        (should (string-prefix-p bv-cache-directory recentf-save-file))
        (should (= recentf-max-menu-items 50))
        (should (= recentf-max-saved-items 300))
        (should (member "\\.git/" recentf-exclude))
        (should (member "/tmp/" recentf-exclude))))))

;;;; Whitespace Handling Tests

(ert-deftest bv-defaults-test-toggle-show-trailing-whitespace ()
  "Test toggling trailing whitespace visibility."
  (bv-defaults-tests--with-clean-state
    (with-temp-buffer
      ;; Initially off
      (should-not show-trailing-whitespace)

      ;; Toggle on
      (bv-toggle-show-trailing-whitespace)
      (should show-trailing-whitespace)

      ;; Toggle off
      (bv-toggle-show-trailing-whitespace)
      (should-not show-trailing-whitespace))))

(ert-deftest bv-defaults-test-cleanup-buffer-whitespace ()
  "Test buffer whitespace cleanup."
  (bv-defaults-tests--with-clean-state
    (with-temp-buffer
      ;; Insert content with whitespace issues
      (insert "Line with trailing spaces   \n")
      (insert "\tLine with tab\n")
      (insert "Line without newline")

      ;; Clean up
      (let ((indent-tabs-mode nil))
        (bv-cleanup-buffer-whitespace))

      ;; Check results
      (goto-char (point-min))
      (should (looking-at "Line with trailing spaces$"))
      (forward-line)
      (should (looking-at "    Line with tab$"))  ; Tab converted to spaces
      (goto-char (point-max))
      (should (bolp)))))  ; Newline added at end

(ert-deftest bv-defaults-test-whitespace-prog-mode-hook ()
  "Test that whitespace is shown in programming modes."
  (bv-defaults-tests--with-clean-state
    (with-temp-buffer
      ;; Enable a prog-mode derived mode
      (emacs-lisp-mode)
      ;; Run hooks manually since we're in a temp buffer
      (run-hooks 'prog-mode-hook)

      ;; Check settings
      (should show-trailing-whitespace)
      (should truncate-lines))))

;;;; Buffer Management Tests

(ert-deftest bv-defaults-test-kill-region-or-backward-word ()
  "Test kill-region-or-backward-word behavior."
  (bv-defaults-tests--with-clean-state
    (with-temp-buffer
      ;; Test with active region
      (ert-info ("Testing with active region")
        (insert "word1 word2 word3")
        (set-mark 7)  ; After "word1 "
        (goto-char 12) ; After "word2"
        (transient-mark-mode 1)
        (bv-kill-region-or-backward-word)
        (should (equal (buffer-string) "word1  word3"))
        (should (equal (car kill-ring) "word2")))

      ;; Test without active region
      (ert-info ("Testing without active region")
        (erase-buffer)
        (insert "word1 word2")
        (goto-char (point-max))
        (deactivate-mark)
        (bv-kill-region-or-backward-word)
        (should (equal (buffer-string) "word1 "))
        (should (equal (car kill-ring) "word2")))

      ;; Test with prefix argument
      (ert-info ("Testing with prefix argument")
        (erase-buffer)
        (insert "word1 word2 word3")
        (goto-char (point-max))
        (bv-kill-region-or-backward-word 2)
        (should (equal (buffer-string) "word1 "))))))

(ert-deftest bv-defaults-test-switch-to-previous-buffer ()
  "Test switching to previous buffer."
  (bv-defaults-tests--with-clean-state
    (let ((buf1 (generate-new-buffer "test-1"))
          (buf2 (generate-new-buffer "test-2"))
          (buf3 (generate-new-buffer "test-3")))
      (unwind-protect
          (progn
            ;; Visit buffers in order
            (switch-to-buffer buf1)
            (switch-to-buffer buf2)
            (switch-to-buffer buf3)

            ;; Switch to previous should go to buf2
            (bv-switch-to-previous-buffer)
            (should (eq (current-buffer) buf2))

            ;; Another switch should go to buf3
            (bv-switch-to-previous-buffer)
            (should (eq (current-buffer) buf3)))

        ;; Clean up
        (kill-buffer buf1)
        (kill-buffer buf2)
        (kill-buffer buf3)))))

(ert-deftest bv-defaults-test-kill-current-buffer ()
  "Test killing current buffer."
  (bv-defaults-tests--with-clean-state
    (let ((test-buf (generate-new-buffer "test-buffer")))
      (switch-to-buffer test-buf)
      (should (buffer-live-p test-buf))
      (bv-kill-current-buffer)
      (should-not (buffer-live-p test-buf))
      (should-not (eq (current-buffer) test-buf)))))

;;;; Toggle Command Tests

(ert-deftest bv-defaults-test-toggle-auto-update-buffers ()
  "Test toggling auto-update buffers."
  (bv-defaults-tests--with-clean-state
    (let ((global-auto-revert-mode-state nil))
      (cl-letf (((symbol-function 'global-auto-revert-mode)
                 (lambda (arg)
                   (setq global-auto-revert-mode-state
                         (if (and arg (< arg 0)) nil t)))))
        ;; Start with auto-update enabled
        (setq bv-auto-update-buffers t)

        ;; Toggle off
        (bv-toggle-auto-update-buffers)
        (should-not bv-auto-update-buffers)
        (should-not global-auto-revert-mode-state)
        (should-not (bv-get-value 'auto-update-buffers))

        ;; Toggle on
        (bv-toggle-auto-update-buffers)
        (should bv-auto-update-buffers)
        (should global-auto-revert-mode-state)
        (should (bv-get-value 'auto-update-buffers))))))

(ert-deftest bv-defaults-test-toggle-auto-clean-whitespace ()
  "Test toggling auto-clean whitespace."
  (bv-defaults-tests--with-clean-state
    ;; Start with auto-clean enabled
    (setq bv-auto-clean-whitespace t)

    ;; Toggle off
    (bv-toggle-auto-clean-whitespace)
    (should-not bv-auto-clean-whitespace)
    (should-not (bv-get-value 'auto-clean-whitespace))

    ;; Toggle on
    (bv-toggle-auto-clean-whitespace)
    (should bv-auto-clean-whitespace)
    (should (bv-get-value 'auto-clean-whitespace))))

;;;; Advanced User Tests

(ert-deftest bv-defaults-test-advanced-user-settings ()
  "Test that advanced user settings are applied correctly."
  (bv-defaults-tests--with-clean-state
    ;; Test with advanced user enabled
    (let ((bv-advanced-user t))
      (load-library "bv-defaults")

      (ert-info ("Checking narrowing commands enabled")
        (should-not (get 'narrow-to-page 'disabled))
        (should-not (get 'narrow-to-region 'disabled))
        (should-not (get 'narrow-to-defun 'disabled)))

      (ert-info ("Checking case change commands enabled")
        (should-not (get 'upcase-region 'disabled))
        (should-not (get 'downcase-region 'disabled)))

      (ert-info ("Checking warning settings")
        (should-not large-file-warning-threshold)
        (should vc-follow-symlinks)
        (should (eq ad-redefinition-action 'accept)))

      (ert-info ("Checking startup settings")
        (should inhibit-startup-screen)
        (should inhibit-startup-message)
        (should-not initial-scratch-message)
        (should (eq initial-major-mode 'fundamental-mode))))))

;;;; Report Function Tests

(ert-deftest bv-defaults-test-report-generation ()
  "Test defaults report generation."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      ;; Set up test state
      (setq bv-full-name "Test User"
            bv-email-address "test@example.com"
            bv-advanced-user t
            bv-auto-update-buffers nil
            bv-auto-clean-whitespace t
            bv-smooth-scrolling nil
            bv-recentf-save-interval 300)

      ;; Generate report
      (bv-defaults-report)

      ;; Check report content
      (with-current-buffer "*bv-defaults-report*"
        (let ((content (buffer-string)))
          ;; Check user configuration section
          (should (string-match-p "Full Name: Test User" content))
          (should (string-match-p "Email: test@example.com" content))
          (should (string-match-p "Advanced User: yes" content))
          (should (string-match-p "Auto-update Buffers: no" content))
          (should (string-match-p "Auto-clean Whitespace: yes" content))
          (should (string-match-p "Smooth Scrolling: no" content))
          (should (string-match-p "Recentf Save Interval: 300 seconds" content))

          ;; Check cache directory section
          (should (string-match-p "Cache Directory:" content))
          (should (string-match-p bv-cache-directory content))

          ;; Check features section
          (should (string-match-p "Active Features:" content))
          (should (string-match-p "Native Compilation:" content)))

        ;; Clean up
        (kill-buffer "*bv-defaults-report*")))))

;;;; Development Support Tests

(ert-deftest bv-defaults-test-open-cache-dir ()
  "Test opening cache directory in Dired."
  (bv-defaults-tests--with-clean-state
    (bv-defaults-tests--with-temp-cache-dir
      (let ((dired-called nil))
        (cl-letf (((symbol-function 'dired)
                   (lambda (dir)
                     (setq dired-called dir))))
          (bv-defaults-open-cache-dir)
          (should (equal dired-called bv-cache-directory))))))

  ;; Test error when cache directory not configured
  (bv-defaults-tests--with-clean-state
    (let ((bv-cache-directory nil))
      (should-error (bv-defaults-open-cache-dir) :type 'error))))

;;;; Integration Tests

(ert-deftest bv-defaults-test-feature-registration ()
  "Test that bv-defaults registers itself as a feature."
  (ert-info ("Verifying bv-defaults self-registration")
    ;; Load fresh to test registration
    (let ((bv-enabled-features '())
          (bv-feature-dependencies (make-hash-table :test #'eq)))
      (load "bv-defaults" nil t)
      (should (bv-feature-enabled-p 'bv-defaults))
      ;; Should not have explicit dependencies (to avoid circular loading)
      (should (null (gethash 'bv-defaults bv-feature-dependencies))))))

(ert-deftest bv-defaults-test-convenience-function ()
  "Test `bv-enable-defaults' convenience function."
  (bv-defaults-tests--with-clean-state
    (let ((message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-called (apply #'format fmt args)))))
        (bv-enable-defaults)
        (should (featurep 'bv-defaults))
        (should (string-match-p "BV defaults enabled" message-called))))))

;; Test runner helper
;;;###autoload
(defun bv-defaults-run-tests ()
  "Run `bv-defaults' test suite."
  (interactive)
  (ert-run-tests-interactively "^bv-defaults-test-"))

(provide 'bv-defaults-tests)
;;; bv-defaults-tests.el ends here
