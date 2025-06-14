;;; bv-core.el --- Core utilities and infrastructure -*- lexical-binding: t -*-

;;; Commentary:
;; Core infrastructure for Emacs configuration
;; Provides feature system, configuration values, and utilities

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Variables defined in init.el during bootstrap
(defvar bv-emacs-dir)
(defvar bv-lisp-dir)
(defvar bv-var-dir)
(defvar bv-etc-dir)

;; Guix-specific paths (defined in init.el)
(defvar bv-guix-system-profile)
(defvar bv-guix-home-profile)
(defvar bv-guix-user-profile)
(defvar bv-system-lib-dir)
(defvar bv-home-lib-dir)
(defvar bv-system-bin-dir)
(defvar bv-home-bin-dir)
(defvar bv-system-share-dir)
(defvar bv-home-share-dir)

;;;; Feature System

(defvar bv-enabled-features '()
  "List of enabled features.")

(defvar bv-feature-dependencies (make-hash-table :test 'eq)
  "Hash table mapping features to their dependencies.")

(defun bv-feature-enabled-p (feature)
  "Return non-nil if FEATURE is enabled."
  (memq feature bv-enabled-features))

(defun bv-register-feature (feature &optional dependencies)
  "Register FEATURE as enabled with optional DEPENDENCIES."
  (when dependencies
    (puthash feature dependencies bv-feature-dependencies))
  (cl-pushnew feature bv-enabled-features))

(defmacro bv-with-feature (feature &rest body)
  "Execute BODY if FEATURE is enabled."
  (declare (indent 1))
  `(when (bv-feature-enabled-p ',feature)
     ,@body))

(defmacro bv-when-feature (feature &rest body)
  "Execute BODY when FEATURE becomes available."
  (declare (indent 1))
  `(if (bv-feature-enabled-p ',feature)
       (progn ,@body)
     (with-eval-after-load ',feature
       ,@body)))

;;;; Configuration Value System

(defvar bv-config-values (make-hash-table :test 'eq)
  "Central configuration value storage.")

(defun bv-set-value (key value)
  "Set configuration KEY to VALUE."
  (puthash key value bv-config-values))

(defun bv-get-value (key &optional default)
  "Get configuration value for KEY, or DEFAULT if not set."
  (gethash key bv-config-values default))

(defun bv-require-value (key &optional error-message)
  "Get configuration value for KEY, error if not present.
If ERROR-MESSAGE is provided, use it instead of the default error message."
  (let ((value (gethash key bv-config-values 'bv--not-found)))
    (if (eq value 'bv--not-found)
        (error (or error-message
                   (format "Required configuration value '%s' not provided" key)))
      value)))

(defmacro bv-with-value (key var &rest body)
  "Bind VAR to the value of KEY and execute BODY if KEY exists."
  (declare (indent 2))
  (let ((val (gensym "val")))
    `(let ((,val (gethash ',key bv-config-values 'bv--not-found)))
       (unless (eq ,val 'bv--not-found)
         (let ((,var ,val))
           ,@body)))))

;;;; Configuration Helpers

(defmacro bv-defcustom (name default docstring &rest args)
  "Define a customizable variable NAME and register it in `bv-config-values'.
NAME is the variable name, DEFAULT is its default value, and DOCSTRING
is its documentation.  Additional ARGS are passed to `defcustom'."
  (declare (doc-string 3) (indent 2))
  (let* ((name-str (symbol-name name))
         (value-key (if (string-prefix-p "bv-" name-str)
                        (substring name-str 3)
                      name-str)))
    `(progn
       (defcustom ,name ,default ,docstring ,@args)
       (bv-set-value ',(intern value-key) ,name))))

;;;; Utility Macros

(defmacro bv-after-init (&rest body)
  "Execute BODY after Emacs initialization."
  `(if after-init-time
       (progn ,@body)
     (add-hook 'after-init-hook (lambda () ,@body))))

(defmacro bv-idle-eval (delay &rest body)
  "Execute BODY after DELAY seconds of idle time."
  (declare (indent 1))
  `(run-with-idle-timer ,delay nil (lambda () ,@body)))

(defmacro bv-when-packages (&rest packages-and-body)
  "Execute body when all PACKAGES are available.
PACKAGES-AND-BODY can either be a list of packages followed by body forms,
or a single package followed by body forms."
  (let ((packages (if (listp (car packages-and-body))
                      (car packages-and-body)
                    (list (car packages-and-body))))
        (body (if (listp (car packages-and-body))
                  (cdr packages-and-body)
                (cdr packages-and-body))))
    `(when (cl-every #'featurep ',packages)
       ,@body)))

;;;; Keybinding Helpers

(defmacro bv-leader (&rest bindings)
  "Bind key sequences in `bv-app-map`.
BINDINGS is a flat list of key/command pairs."
  (declare (indent 0))
  (let (forms)
    (while bindings
      (let ((key (pop bindings))
            (cmd (pop bindings)))
        (when (and key cmd)
          (push `(define-key bv-app-map (kbd ,key) ,cmd) forms))))
    `(progn ,@(nreverse forms))))

;;;; Path and Directory Utilities

(defun bv-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bv-expand-etc-file (file)
  "Expand FILE relative to etc directory."
  (let ((etc-dir (bv-get-value 'etc-dir)))
    (if etc-dir
        (expand-file-name file etc-dir)
      (expand-file-name file "~/.config/emacs/etc"))))

(defun bv-expand-var-file (file)
  "Expand FILE relative to var directory."
  (let ((var-dir (bv-get-value 'var-dir)))
    (if var-dir
        (expand-file-name file var-dir)
      (expand-file-name file "~/.cache/emacs/var"))))

;; Guix path helpers
(defun bv-expand-system-lib (file)
  "Expand FILE relative to system lib directory."
  (when (bv-get-value 'system-lib-dir)
    (expand-file-name file (bv-get-value 'system-lib-dir))))

(defun bv-expand-home-lib (file)
  "Expand FILE relative to home lib directory."
  (when (bv-get-value 'home-lib-dir)
    (expand-file-name file (bv-get-value 'home-lib-dir))))

(defun bv-expand-system-bin (file)
  "Expand FILE relative to system bin directory."
  (when (bv-get-value 'system-bin-dir)
    (expand-file-name file (bv-get-value 'system-bin-dir))))

(defun bv-expand-home-bin (file)
  "Expand FILE relative to home bin directory."
  (when (bv-get-value 'home-bin-dir)
    (expand-file-name file (bv-get-value 'home-bin-dir))))

(defun bv-expand-system-share (file)
  "Expand FILE relative to system share directory."
  (when (bv-get-value 'system-share-dir)
    (expand-file-name file (bv-get-value 'system-share-dir))))

(defun bv-expand-home-share (file)
  "Expand FILE relative to home share directory."
  (when (bv-get-value 'home-share-dir)
    (expand-file-name file (bv-get-value 'home-share-dir))))

;;;; Development Helpers

(defun bv-report-config ()
  "Report current configuration values and enabled features."
  (interactive)
  (with-current-buffer (get-buffer-create "*BV Configuration*")
    (erase-buffer)
    (insert "BV Configuration Report\n")
    (insert "======================\n\n")

    (insert "Enabled Features:\n")
    (dolist (feature (sort bv-enabled-features #'string<))
      (insert (format "  - %s\n" feature)))

    (insert "\nConfiguration Values:\n")
    (let ((keys '()))
      (maphash (lambda (k _) (push k keys)) bv-config-values)
      (dolist (key (sort keys #'string<))
        (let ((value (gethash key bv-config-values)))
          (insert (format "  %-20s: %S\n" key
                          (if (stringp value)
                              value
                            (prin1-to-string value)))))))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;; Feature Registration

(bv-register-feature 'bv-core)

;; Export core directories (only if bound - for byte-compilation safety)
(when (boundp 'bv-emacs-dir)
  (bv-set-value 'emacs-dir bv-emacs-dir))
(when (boundp 'bv-lisp-dir)
  (bv-set-value 'lisp-dir bv-lisp-dir))
(when (boundp 'bv-var-dir)
  (bv-set-value 'var-dir bv-var-dir))
(when (boundp 'bv-etc-dir)
  (bv-set-value 'etc-dir bv-etc-dir))

;; Export Guix paths (defined in init.el)
(when (boundp 'bv-guix-system-profile)
  (bv-set-value 'guix-system-profile bv-guix-system-profile))
(when (boundp 'bv-guix-home-profile)
  (bv-set-value 'guix-home-profile bv-guix-home-profile))
(when (boundp 'bv-guix-user-profile)
  (bv-set-value 'guix-user-profile bv-guix-user-profile))
(when (boundp 'bv-system-lib-dir)
  (bv-set-value 'system-lib-dir bv-system-lib-dir))
(when (boundp 'bv-home-lib-dir)
  (bv-set-value 'home-lib-dir bv-home-lib-dir))
(when (boundp 'bv-system-bin-dir)
  (bv-set-value 'system-bin-dir bv-system-bin-dir))
(when (boundp 'bv-home-bin-dir)
  (bv-set-value 'home-bin-dir bv-home-bin-dir))
(when (boundp 'bv-system-share-dir)
  (bv-set-value 'system-share-dir bv-system-share-dir))
(when (boundp 'bv-home-share-dir)
  (bv-set-value 'home-share-dir bv-home-share-dir))

(provide 'bv-core)
;;; bv-core.el ends here
