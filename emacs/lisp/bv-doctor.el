;;; bv-doctor.el --- Batch validation helpers -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; "Config doctor" helpers for validating this Emacs configuration:
;; - `check-parens' across config files
;; - Byte-compile all modules in `emacs/lisp/'
;; - Load both BV themes
;;
;; Designed to run in non-interactive batch mode and from pre-commit hooks.

;;; Code:

(require 'cl-lib)

(defconst bv-doctor--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing BV Emacs Lisp modules.")

(defconst bv-doctor--emacs-dir
  (expand-file-name ".." bv-doctor--lisp-dir)
  "Root directory of the BV Emacs configuration.")

(defconst bv-doctor--themes-dir
  (expand-file-name "themes" bv-doctor--emacs-dir)
  "Directory containing BV themes.")

(defun bv-doctor--elisp-files ()
  "Return a list of `.el' files that make up this configuration."
  (let* ((init (expand-file-name "init.el" bv-doctor--emacs-dir))
         (early-init (expand-file-name "early-init.el" bv-doctor--emacs-dir))
         (themes (list (expand-file-name "bv-dark-theme.el" bv-doctor--themes-dir)
                       (expand-file-name "bv-light-theme.el" bv-doctor--themes-dir)))
         (modules (directory-files bv-doctor--lisp-dir t "\\.el\\'")))
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
  "Load `bv-dark' and `bv-light' themes."
  (add-to-list 'custom-theme-load-path bv-doctor--themes-dir)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'bv-dark t)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'bv-light t))

;;;###autoload
(defun bv-doctor-run ()
  "Run the BV config doctor interactively."
  (interactive)
  (message "bv-doctor: check-parens…")
  (bv-doctor-check-parens)
  (message "bv-doctor: byte-compiling modules…")
  (bv-doctor-byte-compile)
  (message "bv-doctor: loading themes…")
  (bv-doctor-load-themes)
  (message "bv-doctor: OK"))

(defun bv-doctor-run-batch ()
  "Run the BV config doctor in batch mode.

Signals an error on failure (causing a non-zero exit in batch mode)."
  (bv-doctor-check-parens)
  (bv-doctor-byte-compile)
  (bv-doctor-load-themes)
  (message "bv-doctor: OK"))

(provide 'bv-doctor)
;;; bv-doctor.el ends here
