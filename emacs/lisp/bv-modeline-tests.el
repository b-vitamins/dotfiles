;;; bv-modeline-tests.el --- Tests for BV header line -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; ERT coverage for the BV modeline/header-line renderer.  These tests focus on
;; invariants that are easy to regress during visual refinement: width budgets,
;; segment policy, status semantics, title fallback, and active/inactive faces.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'bv-modeline)

(defvar display-time-mode)
(defvar display-time-string)
(defvar vc-mode)

(defmacro bv-modeline-tests--with-width (width selected &rest body)
  "Evaluate BODY with modeline WIDTH and SELECTED window state."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'window-total-width)
              (lambda (&optional _window) ,width))
             ((symbol-function 'bv-modeline--selected-window-p)
              (lambda () ,selected)))
     ,@body))

(ert-deftest bv-modeline-time-is-durable ()
  "Wall-clock time should remain visible outside decorative policy."
  :tags '(bv-modeline)
  (let ((bv-modeline-detail-level 'minimal)
        (display-time-string "03:09"))
    (bv-modeline-tests--with-width 42 t
      (let ((line (bv-modeline-compose
                   "RW" "*scratch*"
                   (list (bv-modeline--segment
                          'mode "Text" 'bv-ui-header-muted
                          :priority 80))
                   (bv-modeline--right-segments nil 'general)
                   'middle
                   "scratch")))
        (should (= (string-width line) 42))
        (should (string-match-p "03:09" line))))))

(ert-deftest bv-modeline-width-invariant ()
  "Rendered lines should exactly fill the window budget."
  :tags '(bv-modeline)
  (let ((bv-modeline-detail-level 'full)
        (display-time-mode t)
        (display-time-string "01:26")
        (buffer-file-name "/tmp/very-long-file-name-for-modeline-tests.el")
        (vc-mode " Git:feature/instrumented-headerline"))
    (dolist (width '(24 40 72 110))
      (bv-modeline-tests--with-width width t
        (let ((line (bv-modeline-compose
                     "**"
                     "Solutions: Deep Learning Foundation and Concepts"
                     (list
                      (bv-modeline--segment
                       'project "myslipbox" 'bv-ui-header-salient
                       :priority 60 :min-width 4)
                      (bv-modeline--segment
                       'branch "feature/instrumented-headerline"
                       'bv-ui-header-muted
                       :priority 45 :min-width 4))
                     (bv-modeline--right-segments nil 'code)
                     'middle
                     "Title help")))
          (should (= (string-width line) width)))))))

(ert-deftest bv-modeline-org-title-wins-over-filename ()
  "Org buffers should prefer #+TITLE over a generated filename."
  :tags '(bv-modeline)
  (skip-unless (require 'org nil t))
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Deep Learning Foundation and Concepts\n\n* TODO")
    (setq buffer-file-name
          "/tmp/2026-01-23-001152-solutions-deep-learning-foundation-and-concepts.org")
    (should (equal (bv-modeline--title)
                   "Deep Learning Foundation and Concepts"))
    (should (eq (bv-modeline--title-truncation) 'right))))

(ert-deftest bv-modeline-scratch-suppresses-project-context ()
  "Non-file scratch buffers should not fabricate project identity."
  :tags '(bv-modeline)
  (with-temp-buffer
    (rename-buffer "*scratch-test*" t)
    (let ((buffer-file-name nil))
      (should-not (bv-modeline--project-context-p))
      (let ((keys (mapcar (lambda (segment)
                            (plist-get segment :key))
                          (bv-modeline--context-segments 'general))))
        (should-not (memq 'project keys))
        (should-not (memq 'branch keys))))))

(ert-deftest bv-modeline-diagnostics-follow-role-policy ()
  "Code sees all diagnostics; writing only interrupts for bad states."
  :tags '(bv-modeline)
  (cl-letf (((symbol-function 'bv-modeline--flymake-summary)
             (lambda ()
               '(:errors 0 :warnings 0 :infos 1
                 :checking nil :eglot "Eglot"
                 :sample "informational note"))))
    (should (bv-modeline--diagnostics-segment 'code))
    (should-not (bv-modeline--diagnostics-segment 'note)))
  (cl-letf (((symbol-function 'bv-modeline--flymake-summary)
             (lambda ()
               '(:errors 1 :warnings 0 :infos 0
                 :checking nil :eglot "Eglot"
                 :sample "hard failure"))))
    (let ((segment (bv-modeline--diagnostics-segment 'note)))
      (should segment)
      (should (plist-get segment :required))
      (should (string= (plist-get segment :text) "E1")))))

(ert-deftest bv-modeline-non-left-signals-do-not-paint-blocks ()
  "Right-side diagnostics and task states should not use block faces."
  :tags '(bv-modeline)
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (narrow-to-region (point-min) (+ (point-min) 5))
    (cl-letf (((symbol-function 'bv-modeline--flymake-summary)
               (lambda ()
                 '(:errors 1 :warnings 0 :infos 0
                   :checking nil :eglot "Eglot"
                   :sample "hard failure"))))
      (bv-modeline-tests--with-width 80 t
        (let ((line (bv-modeline-compose
                     "RW" "buffer"
                     nil
                     (bv-modeline--right-segments nil 'code)
                     'middle
                     "buffer")))
          (should (text-property-any
                   5 (length line) 'face 'bv-ui-header-error line))
          (should (text-property-any
                   5 (length line) 'face 'bv-ui-header-warning line))
          (should-not (text-property-any
                       5 (length line) 'face 'bv-ui-header-critical line))
          (should-not (text-property-any
                       5 (length line) 'face 'bv-ui-header-popout line)))))))

(ert-deftest bv-modeline-position-and-time-have-stable-widths ()
  "Right-side position and time should not jitter as values grow."
  :tags '(bv-modeline)
  (let* ((display-time-string "03:09")
         (segments (bv-modeline--right-segments nil 'code))
         (position (cl-find 'position segments
                            :key (lambda (segment)
                                   (plist-get segment :key))))
         (time (cl-find 'time segments
                        :key (lambda (segment)
                               (plist-get segment :key)))))
    (should (= (plist-get position :fixed-width) 7))
    (should (eq (plist-get position :align) 'right))
    (should (= (plist-get time :fixed-width) 5))))

(ert-deftest bv-modeline-status-semantics ()
  "Left status grammar should distinguish common operational states."
  :tags '(bv-modeline)
  (with-temp-buffer
    (setq buffer-file-name "/tmp/bv-modeline-status.txt")
    (set-buffer-modified-p t)
    (should (equal (bv-modeline-status) "**"))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (should (equal (bv-modeline-status) "RO"))
    (setq buffer-read-only nil)
    (cl-letf (((symbol-function 'bv-modeline--remote-info)
               (lambda ()
                 '(:name "/ssh:host:/tmp/" :method "ssh"
                   :host "host" :localname "/tmp/"))))
      (should (equal (bv-modeline-status) "R@")))
    (cl-letf (((symbol-function 'bv-modeline--remote-info)
               (lambda ()
                 '(:name "/sudo:root@host:/etc/" :method "sudo"
                   :user "root" :host "host" :localname "/etc/"))))
      (should (equal (bv-modeline-status) "#@")))))

(ert-deftest bv-modeline-task-state-segment ()
  "Active modal states should coalesce into one compact task segment."
  :tags '(bv-modeline)
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (narrow-to-region (point-min) (+ (point-min) 5))
    (let ((segment (bv-modeline--task-state-segment)))
      (should segment)
      (should (string-match-p "\\bNAR\\b" (plist-get segment :text)))
      (should (string-match-p "narrowed" (plist-get segment :help-echo))))))

(ert-deftest bv-modeline-ordinary-minibuffer-recursion-is-quiet ()
  "A normal minibuffer recursive edit should not advertise R1."
  :tags '(bv-modeline)
  (cl-letf (((symbol-function 'recursion-depth)
             (lambda () 1)))
    (should-not (bv-modeline--task-state-segment)))
  (cl-letf (((symbol-function 'recursion-depth)
             (lambda () 2)))
    (let ((segment (bv-modeline--task-state-segment)))
      (should segment)
      (should (string-match-p "\\bR2\\b" (plist-get segment :text))))))

(ert-deftest bv-modeline-inactive-face-boundary ()
  "Inactive windows should render through inactive header faces."
  :tags '(bv-modeline)
  (bv-modeline-tests--with-width 60 nil
    (let ((line (bv-modeline-compose
                 "RW" "inactive-buffer"
                 (list (bv-modeline--segment
                        'mode "Text" 'bv-ui-header-muted
                        :priority 80))
                 nil
                 'middle
                 "inactive help")))
      (should (= (string-width line) 60))
      (should (text-property-any
               0 (length line) 'face 'bv-ui-header-inactive-muted line))
      (should (text-property-any
               0 (length line) 'face 'bv-ui-header-inactive-default line))
      (should-not (text-property-any
                   0 (length line) 'face 'bv-ui-header-muted line))
      (should-not (text-property-any
                   0 (length line) 'face 'bv-ui-header-default line)))))

(provide 'bv-modeline-tests)
;;; bv-modeline-tests.el ends here
