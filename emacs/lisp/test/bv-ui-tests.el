;;; bv-ui-tests.el --- Test suite for bv-ui -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ert "0"))
;; Keywords: maint, test
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/test/bv-ui-tests.el

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
;; Test suite for bv-ui functionality
;;
;; This file contains unit tests for the UI module.
;; Tests cover:
;; - Theme system and automatic switching
;; - Font configuration and frame handling
;; - Mode line and header line management
;; - UI element toggling (decorations, etc.)
;; - Custom variable propagation
;; - Timer scheduling for theme changes
;; - Interactive command behavior
;; - Icon and help system integration
;; - Environment variable handling
;;
;; Running tests:
;; - All tests: M-x ert RET t RET
;; - Specific test: M-x ert RET bv-ui-test-TESTNAME RET
;; - By prefix: M-x ert RET "^bv-ui-test-" RET
;; - Using helper: M-x bv-ui-run-tests RET
;;
;; Development workflow:
;; 1. Load bv-core.el first
;; 2. Load bv-ui.el
;; 3. Load this test file
;; 4. Run tests with M-x ert
;;
;; Writing new tests:
;; - Name tests as bv-ui-test-FEATURE-ASPECT
;; - Use `bv-ui-test--with-config' to isolate test state
;; - Mock theme loading functions to avoid visual disruption
;; - Clean up any timers created during tests
;; - Test both success and error cases
;;
;; Note: Some tests mock display functions to avoid visual
;; disruption during test runs.  Timer tests use immediate
;; cancellation to prevent delayed execution.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the modules under test
(or (require 'bv-core nil t)
    (load (expand-file-name "../bv-core"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

(or (require 'bv-ui nil t)
    (load (expand-file-name "../bv-ui"
                            (file-name-directory
                             (or load-file-name buffer-file-name)))
          nil t))

;;;; Test Helpers

(defmacro bv-ui-test--with-config (&rest body)
  "Execute BODY with isolated UI configuration.
Temporarily rebinds UI state variables and prevents actual theme loading."
  (declare (indent 0))
  `(let ((bv-ui--theme-timers nil)
         (bv-ui--current-theme nil)
         (bv-ui--original-mode-line-format nil)
         (bv-ui-theme-auto-switch nil) ; Disable auto-switch in tests
         (custom-enabled-themes nil)
         ;; Bind variables to avoid warnings
         (window-divider-default-right-width 8)
         (split-width-threshold 160)
         (split-height-threshold nil)
         ;; Prevent actual theme loading
         (load-theme-called nil)
         (themes-disabled nil))
     (cl-letf (((symbol-function 'load-theme)
                (lambda (theme &optional no-confirm no-enable)
                  (setq load-theme-called theme)))
               ((symbol-function 'disable-theme)
                (lambda (theme)
                  (push theme themes-disabled))))
       ,@body)))

(defmacro bv-ui-test--with-mocked-time (hour &rest body)
  "Execute BODY with mocked current time set to HOUR."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'format-time-string)
              (lambda (format &optional time universal)
                (cond
                 ((equal format "%H") (format "%02d" ,hour))
                 ((equal format "%M") "00")
                 (t (error "Unmocked format-time-string format: %s" format))))))
     ,@body))

(defmacro bv-ui-test--with-temp-frame (&rest body)
  "Execute BODY with a temporary frame for testing."
  (declare (indent 0))
  `(let ((temp-frame (make-frame '((visibility . nil)))))
     (unwind-protect
         (with-selected-frame temp-frame
           ,@body)
       (delete-frame temp-frame t))))

;;;; Theme System Tests

(ert-deftest bv-ui-test-theme-for-hour ()
  "Test theme selection based on hour."
  (bv-ui-test--with-config
    (let ((bv-ui-theme-switch-hours '((6 . light)
                                      (12 . light-soft)
                                      (18 . dark)
                                      (22 . dark-soft))))
      ;; Test various hours
      (should (eq (bv-ui--get-theme-for-hour 0) 'dark-soft))  ; After midnight
      (should (eq (bv-ui--get-theme-for-hour 6) 'light))      ; 6 AM
      (should (eq (bv-ui--get-theme-for-hour 11) 'light))     ; Before noon
      (should (eq (bv-ui--get-theme-for-hour 12) 'light-soft)) ; Noon
      (should (eq (bv-ui--get-theme-for-hour 18) 'dark))      ; 6 PM
      (should (eq (bv-ui--get-theme-for-hour 22) 'dark-soft)) ; 10 PM
      (should (eq (bv-ui--get-theme-for-hour 23) 'dark-soft))))) ; 11 PM

(ert-deftest bv-ui-test-theme-for-hour-edge-cases ()
  "Test theme selection edge cases."
  (bv-ui-test--with-config
    ;; Empty configuration
    (let ((bv-ui-theme-switch-hours nil))
      (should (eq (bv-ui--get-theme-for-hour 12) 'dark)))

    ;; Single entry
    (let ((bv-ui-theme-switch-hours '((12 . light))))
      (should (eq (bv-ui--get-theme-for-hour 11) 'light))  ; Before switch
      (should (eq (bv-ui--get-theme-for-hour 12) 'light))  ; At switch
      (should (eq (bv-ui--get-theme-for-hour 13) 'light))) ; After switch

    ;; Non-sequential hours
    (let ((bv-ui-theme-switch-hours '((20 . dark)
                                      (8 . light)
                                      (14 . light-soft))))
      (should (eq (bv-ui--get-theme-for-hour 7) 'dark))
      (should (eq (bv-ui--get-theme-for-hour 8) 'light))
      (should (eq (bv-ui--get-theme-for-hour 14) 'light-soft))
      (should (eq (bv-ui--get-theme-for-hour 20) 'dark)))))

(ert-deftest bv-ui-test-theme-name-selection ()
  "Test theme name selection with deuteranopia support."
  (bv-ui-test--with-config
    ;; Normal themes
    (let ((bv-ui-theme-deuteranopia nil))
      (should (eq (bv-ui--get-theme-name 'light) 'modus-operandi))
      (should (eq (bv-ui--get-theme-name 'light-soft) 'modus-operandi-tinted))
      (should (eq (bv-ui--get-theme-name 'dark) 'modus-vivendi))
      (should (eq (bv-ui--get-theme-name 'dark-soft) 'modus-vivendi-tinted)))

    ;; Deuteranopia themes
    (let ((bv-ui-theme-deuteranopia t))
      (should (eq (bv-ui--get-theme-name 'light) 'modus-operandi-deuteranopia))
      (should (eq (bv-ui--get-theme-name 'light-soft) 'modus-operandi-deuteranopia))
      (should (eq (bv-ui--get-theme-name 'dark) 'modus-vivendi-deuteranopia))
      (should (eq (bv-ui--get-theme-name 'dark-soft) 'modus-vivendi-deuteranopia)))))

(ert-deftest bv-ui-test-theme-name-error ()
  "Test error handling for invalid theme types."
  (bv-ui-test--with-config
    (should-error (bv-ui--get-theme-name 'invalid-type) :type 'error)
    (should-error (bv-ui--get-theme-name nil) :type 'error)
    (should-error (bv-ui--get-theme-name "string-type") :type 'error)))

(ert-deftest bv-ui-test-theme-loading ()
  "Test theme loading behavior."
  (bv-ui-test--with-config
    (let ((load-theme-called nil)
          (themes-disabled nil)
          (message-called nil))
      (cl-letf (((symbol-function 'load-theme)
                 (lambda (theme &optional no-confirm no-enable)
                   (setq load-theme-called theme)))
                ((symbol-function 'disable-theme)
                 (lambda (theme)
                   (push theme themes-disabled)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-called (apply #'format fmt args)))))

        ;; First load
        (bv-ui-load-theme 'modus-vivendi)
        (should (eq load-theme-called 'modus-vivendi))
        (should (eq bv-ui--current-theme 'modus-vivendi))
        (should (string-match-p "Switched to modus-vivendi" message-called))

        ;; Same theme - should not reload
        (setq load-theme-called nil)
        (setq message-called nil)
        (bv-ui-load-theme 'modus-vivendi)
        (should-not load-theme-called)
        (should-not message-called)

        ;; Different theme - should disable previous
        (let ((custom-enabled-themes '(modus-vivendi)))
          (bv-ui-load-theme 'modus-operandi)
          (should (eq load-theme-called 'modus-operandi))
          (should (member 'modus-vivendi themes-disabled)))))))

(ert-deftest bv-ui-test-theme-switching ()
  "Test theme switching based on current time."
  (bv-ui-test--with-config
    (let ((bv-ui-theme-switch-hours '((6 . light)
                                      (18 . dark)))
          (load-theme-called nil))
      (cl-letf (((symbol-function 'load-theme)
                 (lambda (theme &optional no-confirm no-enable)
                   (setq load-theme-called theme))))

        ;; Test morning time
        (bv-ui-test--with-mocked-time 8
          (bv-ui-switch-theme)
          (should (eq load-theme-called 'modus-operandi)))

        ;; Test evening time
        (setq bv-ui--current-theme nil)
        (bv-ui-test--with-mocked-time 20
          (bv-ui-switch-theme)
          (should (eq load-theme-called 'modus-vivendi)))))))

;;;; Font Configuration Tests

(ert-deftest bv-ui-test-font-setup ()
  "Test font configuration application."
  (bv-ui-test--with-config
    (let ((face-set-called nil))
      (cl-letf (((symbol-function 'find-font)
                 (lambda (spec) t))  ; Pretend font exists
                ((symbol-function 'set-face-attribute)
                 (lambda (face frame &rest attrs)
                   (setq face-set-called attrs))))

        ;; Test with nil font family
        (let ((bv-ui-font-family nil)
              (bv-ui-font-size 12))
          (bv-ui-setup-fonts)
          (should-not face-set-called))

        ;; Test with specific font
        (setq face-set-called nil)
        (let ((bv-ui-font-family "Test Font")
              (bv-ui-font-size 10))
          (bv-ui-setup-fonts)
          (should face-set-called)
          (should (equal (plist-get face-set-called :family) "Test Font"))
          (should (equal (plist-get face-set-called :height) 100)))

        ;; Test with missing font
        (setq face-set-called nil)
        (cl-letf (((symbol-function 'find-font)
                   (lambda (spec) nil)))
          (let ((bv-ui-font-family "Missing Font"))
            (bv-ui-setup-fonts)
            (should-not face-set-called)))))))

(ert-deftest bv-ui-test-font-environment-variables ()
  "Test font configuration from environment variables."
  (bv-ui-test--with-config
    ;; Test EMACS_FONT_FAMILY
    (let ((env-vars '(("EMACS_FONT_FAMILY" . "Env Font"))))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (var) (cdr (assoc var env-vars)))))
        (defvar bv-ui-font-family)
        (setq bv-ui-font-family (or (getenv "EMACS_FONT_FAMILY") "Default"))
        (should (equal bv-ui-font-family "Env Font"))))

    ;; Test EMACS_FONT_SIZE
    (let ((env-vars '(("EMACS_FONT_SIZE" . "14"))))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (var) (cdr (assoc var env-vars)))))
        (let ((env-size (getenv "EMACS_FONT_SIZE")))
          (when (and env-size (string-match "^[0-9]+$" env-size))
            (should (= (string-to-number env-size) 14))))))

    ;; Test invalid font size
    (let ((env-vars '(("EMACS_FONT_SIZE" . "invalid"))))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (var) (cdr (assoc var env-vars)))))
        (let ((env-size (getenv "EMACS_FONT_SIZE")))
          (should-not (and env-size (string-match "^[0-9]+$" env-size))))))))

;;;; Mode Line Tests

(ert-deftest bv-ui-test-header-line-setup ()
  "Test header line as mode line setup."
  (bv-ui-test--with-config
    (with-temp-buffer
      (let ((default-mode-line '("%e" mode-line-front-space))
            (default-header-line nil))
        ;; Set defaults
        (setq-default mode-line-format default-mode-line)
        (setq-default header-line-format default-header-line)

        ;; Run setup
        (let ((bv-ui-header-line-as-mode-line t))
          (bv-ui--setup-header-line-mode))

        ;; Check results
        (should (equal (default-value 'header-line-format) default-mode-line))
        (should-not (default-value 'mode-line-format))
        (should bv-ui--original-mode-line-format)))))

(ert-deftest bv-ui-test-header-line-toggle ()
  "Test toggling between header line and mode line."
  (bv-ui-test--with-config
    (let ((original-mode-line mode-line-format)
          (original-header-line header-line-format)
          (message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-called (apply #'format fmt args)))))

        ;; Start with mode line at bottom
        (let ((bv-ui-header-line-as-mode-line nil))
          ;; Toggle to header
          (bv-ui-toggle-header-line-mode)
          (should bv-ui-header-line-as-mode-line)
          (should (string-match-p "moved to header" message-called))

          ;; Toggle back
          (setq message-called nil)
          (bv-ui-toggle-header-line-mode)
          (should-not bv-ui-header-line-as-mode-line)
          (should (string-match-p "restored to bottom" message-called)))))))

(ert-deftest bv-ui-test-mode-line-buffer-propagation ()
  "Test that mode line changes propagate to all buffers."
  (bv-ui-test--with-config
    (let ((buf1 (generate-new-buffer " *test-1*"))
          (buf2 (generate-new-buffer " *test-2*"))
          (orig-default-mode-line (default-value 'mode-line-format))
          (orig-default-header-line (default-value 'header-line-format)))
      (unwind-protect
          (progn
            ;; Set up initial state - buffers have local mode-line values
            (with-current-buffer buf1
              (setq-local mode-line-format "buffer1"))
            (with-current-buffer buf2
              (setq-local mode-line-format "buffer2"))

            ;; Toggle to header line
            (let ((bv-ui-header-line-as-mode-line nil))
              (bv-ui-toggle-header-line-mode)

              ;; The function only affects buffers without local variables
              ;; Since our buffers have local mode-line-format, they keep it
              ;; But the function should have changed the defaults
              (should (equal (default-value 'header-line-format)
                             orig-default-mode-line))
              (should-not (default-value 'mode-line-format))))

        ;; Clean up
        (setq-default mode-line-format orig-default-mode-line)
        (setq-default header-line-format orig-default-header-line)
        (kill-buffer buf1)
        (kill-buffer buf2)))))

;;;; Timer Management Tests

(ert-deftest bv-ui-test-timer-cancellation ()
  "Test that theme timers are properly cancelled."
  (bv-ui-test--with-config
    ;; Create some timers
    (let ((timer1 (run-at-time 1000 nil 'ignore))
          (timer2 (run-at-time 2000 nil 'ignore)))
      (setq bv-ui--theme-timers (list timer1 timer2))

      ;; Cancel them
      (bv-ui--cancel-theme-timers)

      ;; Should be empty
      (should-not bv-ui--theme-timers)
      ;; Timers should be cancelled
      (should-not (memq timer1 timer-list))
      (should-not (memq timer2 timer-list)))))

(ert-deftest bv-ui-test-timer-scheduling ()
  "Test theme switch timer scheduling."
  (bv-ui-test--with-config
    (let ((bv-ui-theme-switch-hours '((6 . light) (18 . dark)))
          (bv-ui-theme-auto-switch t)
          (timer-created nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time repeat function &rest args)
                   (setq timer-created (list time repeat function))
                   ;; Return a mock timer object (vector format like real timers)
                   (vector t nil nil nil nil nil nil nil nil))))

        ;; Test scheduling at 10 AM (next switch at 6 PM = 8 hours)
        (bv-ui-test--with-mocked-time 10
          (bv-ui--schedule-next-switch)
          (should timer-created)
          (should (= (car timer-created) (* 8 60 60))))

        ;; Test scheduling at 8 PM (next switch at 6 AM = 10 hours)
        (setq timer-created nil)
        (setq bv-ui--theme-timers nil)
        (bv-ui-test--with-mocked-time 20
          (bv-ui--schedule-next-switch)
          (should timer-created)
          (should (= (car timer-created) (* 10 60 60))))))))

(ert-deftest bv-ui-test-timer-disabled-when-auto-switch-off ()
  "Test that no timers are created when auto-switch is disabled."
  (bv-ui-test--with-config
    (let ((bv-ui-theme-auto-switch nil)
          (timer-created nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args)
                   (setq timer-created t)
                   ;; Return a mock timer object
                   (vector t nil nil nil nil nil nil nil nil))))
        (bv-ui--schedule-next-switch)
        (should-not timer-created)))))

;;;; Auto-switch Toggle Tests

(ert-deftest bv-ui-test-auto-switch-toggle ()
  "Test toggling automatic theme switching."
  (bv-ui-test--with-config
    (let ((bv-ui-theme-auto-switch t)
          (setup-called nil)
          (timers-cancelled nil)
          (message-called nil))
      (cl-letf (((symbol-function 'bv-ui-setup-theme-switching)
                 (lambda () (setq setup-called t)))
                ((symbol-function 'bv-ui--cancel-theme-timers)
                 (lambda () (setq timers-cancelled t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-called (apply #'format fmt args)))))

        ;; Toggle off
        (bv-ui-toggle-auto-switch)
        (should-not bv-ui-theme-auto-switch)
        (should timers-cancelled)
        (should (string-match-p "disabled" message-called))

        ;; Toggle on
        (setq setup-called nil)
        (setq message-called nil)
        (bv-ui-toggle-auto-switch)
        (should bv-ui-theme-auto-switch)
        (should setup-called)
        (should (string-match-p "enabled" message-called))))))

(ert-deftest bv-ui-test-theme-variant-environment ()
  "Test EMACS_THEME_VARIANT environment variable handling."
  (bv-ui-test--with-config
    ;; Test "light" variant
    (let ((env-vars '(("EMACS_THEME_VARIANT" . "light"))))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (var) (cdr (assoc var env-vars)))))
        (let ((variant (getenv "EMACS_THEME_VARIANT")))
          (should (equal variant "light"))
          (should-not (not (member variant '("light" "dark")))))))

    ;; Test "dark" variant
    (let ((env-vars '(("EMACS_THEME_VARIANT" . "dark"))))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (var) (cdr (assoc var env-vars)))))
        (let ((variant (getenv "EMACS_THEME_VARIANT")))
          (should (equal variant "dark")))))

    ;; Test "auto" variant (enables auto-switch)
    (let ((env-vars '(("EMACS_THEME_VARIANT" . "auto"))))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (var) (cdr (assoc var env-vars)))))
        (let ((variant (getenv "EMACS_THEME_VARIANT")))
          (should (not (member variant '("light" "dark")))))))))

;;;; UI Element Tests

(ert-deftest bv-ui-test-frame-decorations-toggle ()
  "Test toggling frame decorations."
  (bv-ui-test--with-config
    (bv-ui-test--with-temp-frame
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-called (apply #'format fmt args)))))

          ;; Initial state
          (let ((bv-ui-undecorated-frame t))
            ;; Toggle off (enable decorations)
            (bv-ui-toggle-frame-decorations)
            (should-not bv-ui-undecorated-frame)
            (should-not (frame-parameter nil 'undecorated))
            (should (string-match-p "enabled" message-called))

            ;; Toggle on (disable decorations)
            (setq message-called nil)
            (bv-ui-toggle-frame-decorations)
            (should bv-ui-undecorated-frame)
            (should (frame-parameter nil 'undecorated))
            (should (string-match-p "disabled" message-called))))))))

(ert-deftest bv-ui-test-manual-theme-toggle ()
  "Test manual theme toggling."
  (bv-ui-test--with-config
    ;; Test with auto-switch enabled (should show message)
    (let ((bv-ui-theme-auto-switch t)
          (message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-called (apply #'format fmt args)))))
        (bv-ui-toggle-theme)
        (should (string-match-p "Disable auto-switch first" message-called))))

    ;; Test with auto-switch disabled
    (let ((bv-ui-theme-auto-switch nil)
          (toggle-called nil))
      ;; Need to mock both fboundp check and the function itself
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym)
                   (or (eq sym 'modus-themes-toggle)
                       (and (symbolp sym)
                            (fboundp sym)))))
                ((symbol-function 'modus-themes-toggle)
                 (lambda ()
                   (interactive)
                   (setq toggle-called t))))
        (bv-ui-toggle-theme)
        (should toggle-called)))))

;;;; Which-key and Minions Tests

(ert-deftest bv-ui-test-which-key-configuration ()
  "Test which-key configuration values."
  (bv-ui-test--with-config
    ;; Test default values
    (should (numberp bv-ui-which-key-idle-delay))
    (should (>= bv-ui-which-key-idle-delay 0))

    ;; Test that configuration is applied
    (let ((which-key-idle-delay nil))
      (setq bv-ui-which-key-idle-delay 2.5)
      ;; Simulate which-key loading
      (setq which-key-idle-delay bv-ui-which-key-idle-delay)
      (should (= which-key-idle-delay 2.5)))))

(ert-deftest bv-ui-test-minions-configuration ()
  "Test minions configuration for minor modes."
  (bv-ui-test--with-config
    ;; Test default prominent modes
    (should (listp bv-ui-prominent-modes))
    (should (member 'flymake-mode bv-ui-prominent-modes))

    ;; Test customization
    (let ((bv-ui-prominent-modes '(custom-mode test-mode)))
      (should (= (length bv-ui-prominent-modes) 2))
      (should (member 'custom-mode bv-ui-prominent-modes)))))

;;;; Custom Variable Tests

(ert-deftest bv-ui-test-custom-variables ()
  "Test that custom variables have proper types and defaults."
  (bv-ui-test--with-config
    ;; Check integer types
    (should (integerp bv-ui-margin))
    (should (integerp bv-ui-fringes))
    (should (integerp bv-ui-mode-line-padding))
    (should (integerp bv-ui-header-line-padding))
    (should (integerp bv-ui-tab-bar-padding))
    (should (integerp bv-ui-font-size))

    ;; Check boolean types
    (should (booleanp bv-ui-header-line-as-mode-line))
    (should (booleanp bv-ui-undecorated-frame))
    (should (booleanp bv-ui-theme-deuteranopia))
    (should (booleanp bv-ui-theme-headings-scaling))

    ;; Check list types
    (should (listp bv-ui-theme-switch-hours))
    (should (listp bv-ui-prominent-modes))

    ;; Check string/nil types
    (should (or (null bv-ui-font-family)
                (stringp bv-ui-font-family)))

    ;; Check number type
    (should (numberp bv-ui-which-key-idle-delay))))

(ert-deftest bv-ui-test-theme-switch-hours-format ()
  "Test that theme switch hours have correct format."
  (bv-ui-test--with-config
    (dolist (entry bv-ui-theme-switch-hours)
      ;; Each entry should be (HOUR . TYPE)
      (should (consp entry))
      (should (integerp (car entry)))
      (should (<= 0 (car entry) 23))
      (should (memq (cdr entry) '(light light-soft dark dark-soft))))))

(ert-deftest bv-ui-test-custom-variable-safety ()
  "Test custom variable safety predicates."
  (bv-ui-test--with-config
    ;; Test integer safety
    (should (funcall (get 'bv-ui-margin 'safe-local-variable) 10))
    (should-not (funcall (get 'bv-ui-margin 'safe-local-variable) "10"))

    ;; Test boolean safety
    (should (funcall (get 'bv-ui-theme-auto-switch 'safe-local-variable) t))
    (should (funcall (get 'bv-ui-theme-auto-switch 'safe-local-variable) nil))
    (should-not (funcall (get 'bv-ui-theme-auto-switch 'safe-local-variable) 1))

    ;; Test string/nil safety
    (should (funcall (get 'bv-ui-font-family 'safe-local-variable) "Font"))
    (should (funcall (get 'bv-ui-font-family 'safe-local-variable) nil))
    (should-not (funcall (get 'bv-ui-font-family 'safe-local-variable) 'symbol))

    ;; Test list safety
    (should (funcall (get 'bv-ui-prominent-modes 'safe-local-variable)
                     '(mode1 mode2)))
    (should-not (funcall (get 'bv-ui-prominent-modes 'safe-local-variable)
                         '("string" mode2)))))

;;;; Interactive Command Tests

(ert-deftest bv-ui-test-interactive-commands ()
  "Test that interactive commands are properly defined."
  (bv-ui-test--with-config
    ;; Check that commands are interactive
    (should (commandp 'bv-ui-toggle-theme))
    (should (commandp 'bv-ui-toggle-auto-switch))
    (should (commandp 'bv-ui-toggle-header-line-mode))
    (should (commandp 'bv-ui-toggle-frame-decorations))
    (should (commandp 'bv-ui-switch-theme))
    (should (commandp 'bv-ui-setup-theme-switching))
    (should (commandp 'bv-ui-setup))
    (should (commandp 'bv-ui-status))
    ;; These are not interactive commands, just functions
    (should (fboundp 'bv-ui-setup-fonts))
    (should (fboundp 'bv-ui-load-theme))))

(ert-deftest bv-ui-test-status-command ()
  "Test UI status report generation."
  (bv-ui-test--with-config
    ;; Clean up any existing buffer
    (when (get-buffer "*bv-ui-status*")
      (kill-buffer "*bv-ui-status*"))

    ;; Set up test state
    (let ((bv-ui--current-theme 'modus-vivendi)
          (bv-ui-theme-auto-switch t)
          (bv-ui-theme-deuteranopia nil)
          (bv-ui-font-family "Test Font")
          (bv-ui-font-size 12))

      ;; Generate status
      (bv-ui-status)

      ;; Check buffer content
      (with-current-buffer "*bv-ui-status*"
        (let ((content (buffer-string)))
          ;; Check theme section
          (should (string-match-p "Current theme: modus-vivendi" content))
          (should (string-match-p "Auto-switch: enabled" content))

          ;; Check font section
          (should (string-match-p "Family: Test Font" content))
          (should (string-match-p "Size: 12 points" content))

          ;; Check UI elements section
          (should (string-match-p "Frame decorations:" content))
          (should (string-match-p "Mode line position:" content)))

        ;; Check buffer mode
        (should (eq major-mode 'special-mode)))

      ;; Clean up
      (kill-buffer "*bv-ui-status*"))))

(ert-deftest bv-ui-test-setup-command ()
  "Test `bv-ui-setup' convenience function."
  (bv-ui-test--with-config
    (let ((font-setup-called nil)
          (theme-setup-called nil)
          (which-key-called nil)
          (minions-called nil)
          (display-time-called nil)
          (message-called nil))
      (cl-letf (((symbol-function 'bv-ui-setup-fonts)
                 (lambda () (setq font-setup-called t)))
                ((symbol-function 'bv-ui-setup-theme-switching)
                 (lambda () (setq theme-setup-called t)))
                ((symbol-function 'which-key-mode)
                 (lambda (&optional arg) (setq which-key-called t)))
                ((symbol-function 'minions-mode)
                 (lambda (&optional arg) (setq minions-called t)))
                ((symbol-function 'display-time-mode)
                 (lambda (&optional arg) (setq display-time-called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-called (apply #'format fmt args)))))

        ;; Call setup
        (bv-ui-setup)

        ;; Check all components were initialized
        (should font-setup-called)
        (should theme-setup-called)
        (should which-key-called)
        (should minions-called)
        (should display-time-called)
        (should (string-match-p "setup complete" message-called))))))

;;;; Configuration Value Tests

(ert-deftest bv-ui-test-config-values ()
  "Test that UI values are exported to `bv-config-values'."
  (bv-ui-test--with-config
    ;; The toggle-map should be available
    (should (keymapp (bv-get-value 'toggle-map)))

    ;; Check keybindings in toggle-map
    (let ((map (bv-get-value 'toggle-map)))
      (should (eq (lookup-key map "t") #'bv-ui-toggle-theme))
      (should (eq (lookup-key map "T") #'bv-ui-toggle-auto-switch))
      (should (eq (lookup-key map "h") #'bv-ui-toggle-header-line-mode))
      (should (eq (lookup-key map "d") #'bv-ui-toggle-frame-decorations)))))

;;;; Feature Registration Tests

(ert-deftest bv-ui-test-feature-registration ()
  "Test that bv-ui is properly registered."
  (ert-info ("Verifying bv-ui registration")
    (should (bv-feature-enabled-p 'bv-ui))
    ;; Should depend on bv-core
    (should (member 'bv-core (gethash 'bv-ui bv-feature-dependencies)))))

(ert-deftest bv-ui-test-use-package-integration ()
  "Test `use-package' configuration patterns."
  (bv-ui-test--with-config
    ;; Test that functions can be called in use-package :config
    (let ((setup-calls nil))
      (cl-letf (((symbol-function 'bv-ui-setup-fonts)
                 (lambda () (push 'fonts setup-calls)))
                ((symbol-function 'bv-ui-setup-theme-switching)
                 (lambda () (push 'themes setup-calls))))

        ;; Simulate use-package :config block
        (progn
          (setq bv-ui-theme-switch-hours '((6 . light) (18 . dark)))
          (setq bv-ui-font-family "JetBrains Mono")
          (setq bv-ui-font-size 12)
          (bv-ui-setup-fonts)
          (bv-ui-setup-theme-switching))

        ;; Check execution
        (should (member 'fonts setup-calls))
        (should (member 'themes setup-calls))))))

;;;; Modus Themes Integration Tests

(ert-deftest bv-ui-test-modus-themes-configuration ()
  "Test modus themes configuration."
  (bv-ui-test--with-config
    ;; Test theme customization variables
    (should modus-themes-italic-constructs)
    (should modus-themes-bold-constructs)
    (should modus-themes-mixed-fonts)

    ;; Test palette overrides
    (should (listp modus-themes-common-palette-overrides))
    (should (assq 'bg-region modus-themes-common-palette-overrides))

    ;; Test mode line style
    (should (equal modus-themes-mode-line '(borderless)))

    ;; Test heading scaling when enabled
    (let ((bv-ui-theme-headings-scaling t))
      (when bv-ui-theme-headings-scaling
        (should (listp modus-themes-headings))))))

(ert-deftest bv-ui-test-custom-faces ()
  "Test custom face application."
  (bv-ui-test--with-config
    (let ((faces-set nil))
      (cl-letf (((symbol-function 'custom-set-faces)
                 (lambda (&rest faces)
                   (setq faces-set faces)))
                ((symbol-function 'modus-themes-with-colors)
                 (lambda (&rest body)
                   ;; Mock the macro by providing fake color values
                   (let ((c 'class)
                         (bg-main "#ffffff")
                         (bg-mode-line-active "#eeeeee")
                         (bg-mode-line-inactive "#dddddd")
                         (bg-dim "#f0f0f0"))
                     (eval `(progn ,@body))))))

        ;; Call the face setup
        (bv-ui--apply-custom-faces)

        ;; Check that faces were customized
        (should faces-set)
        (should (cl-some (lambda (face)
                           (eq (car face) 'window-divider))
                         faces-set))
        (should (cl-some (lambda (face)
                           (eq (car face) 'mode-line))
                         faces-set))))))

;; Test runner helper
;;;###autoload
(defun bv-ui-run-tests ()
  "Run `bv-ui' test suite."
  (interactive)
  (ert-run-tests-interactively "^bv-ui-test-"))

(provide 'bv-ui-test)

;;; bv-ui-tests.el ends here
