;;; bv-doctor.el --- Batch validation helpers -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; "Config doctor" helpers for validating this Emacs configuration:
;; - `check-parens' across config files
;; - Byte-compile all modules in `emacs/lisp/'
;; - Load BV theme specification files through the standard theme loader
;; - Run the BV theme compiler audit
;; - Exercise representative theme workflow probes
;; - Generate deterministic visual regression artifacts
;; - Run layout overflow affordance tests
;; - Run package-local UI polish tests
;; - Run completion surface policy tests
;; - Run modeline/header-line invariant tests
;;
;; Designed to run in non-interactive batch mode and from pre-commit hooks.

;;; Code:

(require 'cl-lib)

(declare-function bv-themes-regression-run "bv-themes-regression" (&optional directory))
(declare-function bv-themes-workloads-assert "bv-themes-workloads" (&optional theme report))
(declare-function ert-run-tests-batch "ert" (selector))
(declare-function ert-stats-completed-unexpected "ert" (stats))

(defconst bv-doctor--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing BV Emacs Lisp modules.")

(defconst bv-doctor--emacs-dir
  (expand-file-name ".." bv-doctor--lisp-dir)
  "Root directory of the BV Emacs configuration.")

(defconst bv-doctor--themes-dir
  (expand-file-name "themes" bv-doctor--emacs-dir)
  "Directory containing BV theme specifications.")

(defun bv-doctor--elisp-files ()
  "Return a list of `.el' files that make up this configuration."
  (let* ((init (expand-file-name "init.el" bv-doctor--emacs-dir))
         (early-init (expand-file-name "early-init.el" bv-doctor--emacs-dir))
         (modules (directory-files bv-doctor--lisp-dir t "\\.el\\'"))
         (themes (and (file-directory-p bv-doctor--themes-dir)
                      (directory-files bv-doctor--themes-dir t
                                       "\\.el\\'"))))
    (cl-remove-if-not #'file-exists-p
                      (append (list early-init init) modules themes))))

(defun bv-doctor-check-parens (&optional files)
  "Run `check-parens' for FILES.

When FILES is nil, validate the full BV Emacs configuration."
  (dolist (file (or files (bv-doctor--elisp-files)))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (condition-case err
          (check-parens)
        (error
         (error "%s: %s" file (error-message-string err)))))))

(defun bv-doctor-byte-compile (&optional files)
  "Byte-compile FILES without writing `.elc' into the repository tree.

When FILES is nil, compiles all `.el' files under `emacs/lisp/'."
  (require 'bytecomp)
  (let* ((temp-dir (make-temp-file "bv-doctor-elc" t))
         (byte-compile-dest-file-function
          (lambda (src)
            (expand-file-name
             (concat (file-name-sans-extension (file-name-nondirectory src)) ".elc")
             temp-dir))))
    (dolist (file (or files (directory-files bv-doctor--lisp-dir t "\\.el\\'")))
      (byte-compile-file file))))

(defun bv-doctor-load-themes ()
  "Load BV themes through the standard Custom theme path."
  (require 'bv-themes)
  (add-to-list 'custom-theme-load-path bv-doctor--themes-dir)
  (bv-themes-load-gallery)
  (dolist (theme (bv-doctor--theme-suite))
    (load-theme theme t t))
  (dolist (theme (bv-doctor--theme-suite))
    (load-theme theme t)))

(defun bv-doctor--theme-suite ()
  "Return the BV themes covered by doctor checks."
  (require 'bv-themes)
  (or bv-themes-toggle-themes
      (bv-themes-known-themes)))

(defun bv-doctor-audit-themes ()
  "Run policy-aware audits for BV themes."
  (require 'bv-themes)
  (bv-themes-load-gallery)
  (dolist (theme (bv-doctor--theme-suite))
    (bv-themes-audit theme t)))

(defun bv-doctor-check-theme-regression ()
  "Generate temporary BV theme visual regression artifacts."
  (require 'bv-themes-regression)
  (bv-themes-regression-run
   (make-temp-file "bv-theme-regression" t)))

(defun bv-doctor-check-theme-workloads (&optional theme)
  "Exercise workflow face probes and assert coverage for THEME."
  (require 'bv-themes)
  (require 'bv-themes-workloads)
  (let ((theme (or theme
                   (and (fboundp 'bv-themes-current)
                        (bv-themes-current))
                   bv-themes-default-theme
                   (car (bv-doctor--theme-suite)))))
    (bv-themes-workloads-assert theme)))

(defun bv-doctor-check-live-theme-inventory (&optional theme)
  "Assert live face coverage for THEME in the current Emacs session."
  (require 'bv-themes)
  (require 'bv-themes-inventory)
  (let* ((theme (or theme
                    (and (fboundp 'bv-themes-current)
                         (bv-themes-current))
                    bv-themes-default-theme
                    (car (bv-doctor--theme-suite))))
         (artifact (bv-themes-compile theme)))
    (bv-themes-inventory-assert
     (bv-themes-inventory-scan artifact))))

(defun bv-doctor-check-modeline-tests ()
  "Run BV modeline/header-line ERT checks."
  (require 'ert)
  (require 'bv-modeline-tests)
  (let ((stats (ert-run-tests-batch '(tag bv-modeline))))
    (unless (zerop (ert-stats-completed-unexpected stats))
      (error "BV modeline tests failed"))))

(defun bv-doctor-check-completion-tests ()
  "Run BV completion/minibuffer ERT checks."
  (require 'ert)
  (require 'bv-completion-tests)
  (let ((stats (ert-run-tests-batch '(tag bv-completion))))
    (unless (zerop (ert-stats-completed-unexpected stats))
      (error "BV completion tests failed"))))

(defun bv-doctor-check-layout-tests ()
  "Run BV layout ERT checks."
  (require 'ert)
  (require 'bv-layout-tests)
  (let ((stats (ert-run-tests-batch '(tag bv-layout))))
    (unless (zerop (ert-stats-completed-unexpected stats))
      (error "BV layout tests failed"))))

(defun bv-doctor-check-ui-polish-tests ()
  "Run BV package-local UI polish ERT checks."
  (require 'ert)
  (require 'bv-ui-polish-tests)
  (let ((stats (ert-run-tests-batch '(tag bv-ui-polish))))
    (unless (zerop (ert-stats-completed-unexpected stats))
      (error "BV UI polish tests failed"))))

;;;###autoload
(defun bv-doctor-run ()
  "Run the BV config doctor interactively."
  (interactive)
  (setq load-prefer-newer t)
  (message "bv-doctor: check-parens…")
  (bv-doctor-check-parens)
  (message "bv-doctor: byte-compiling modules…")
  (bv-doctor-byte-compile)
  (message "bv-doctor: loading themes…")
  (bv-doctor-load-themes)
  (message "bv-doctor: auditing themes…")
  (bv-doctor-audit-themes)
  (message "bv-doctor: exercising theme workflow probes…")
  (bv-doctor-check-theme-workloads)
  (message "bv-doctor: checking live theme inventory…")
  (bv-doctor-check-live-theme-inventory)
  (message "bv-doctor: generating theme regression artifacts…")
  (bv-doctor-check-theme-regression)
  (message "bv-doctor: testing layout invariants…")
  (bv-doctor-check-layout-tests)
  (message "bv-doctor: testing package-local UI polish…")
  (bv-doctor-check-ui-polish-tests)
  (message "bv-doctor: testing completion invariants…")
  (bv-doctor-check-completion-tests)
  (message "bv-doctor: testing modeline invariants…")
  (bv-doctor-check-modeline-tests)
  (message "bv-doctor: OK"))

(defun bv-doctor-run-batch ()
  "Run the BV config doctor in batch mode.

Signals an error on failure (causing a non-zero exit in batch mode)."
  (setq load-prefer-newer t)
  (bv-doctor-check-parens)
  (bv-doctor-byte-compile)
  (bv-doctor-load-themes)
  (bv-doctor-audit-themes)
  (bv-doctor-check-theme-workloads)
  (bv-doctor-check-live-theme-inventory)
  (bv-doctor-check-theme-regression)
  (bv-doctor-check-layout-tests)
  (bv-doctor-check-ui-polish-tests)
  (bv-doctor-check-completion-tests)
  (bv-doctor-check-modeline-tests)
  (message "bv-doctor: OK"))

(defun bv-doctor-run-live-batch ()
  "Run live checks after the full Emacs configuration has loaded."
  (setq load-prefer-newer t)
  (bv-doctor-check-theme-workloads)
  (bv-doctor-check-live-theme-inventory)
  (message "bv-doctor live: OK"))

(provide 'bv-doctor)
;;; bv-doctor.el ends here
