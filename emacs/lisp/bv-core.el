;;; bv-core.el --- Core utilities and infrastructure -*- lexical-binding: t -*-

;;; Commentary:
;; Core infrastructure for Emacs configuration
;; Feature system, configuration values, type validation

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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
  (let ((feature-sym (if (symbolp feature) feature (eval feature))))
    `(if (bv-feature-enabled-p ',feature-sym)
         (progn ,@body)
       (with-eval-after-load ',feature-sym
         ,@body))))

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
  "Get configuration value for KEY, error if not present."
  (let ((value (gethash key bv-config-values 'bv--not-found)))
    (if (eq value 'bv--not-found)
        (error (or error-message
                   (format "Required configuration value '%s' not provided" key)))
      value)))

(defun bv-value-exists-p (key)
  "Return non-nil if configuration KEY exists."
  (not (eq (gethash key bv-config-values 'bv--not-found) 'bv--not-found)))

(defmacro bv-with-value (key var &rest body)
  "Bind VAR to the value of KEY and execute BODY."
  (declare (indent 2))
  `(when-let ((,var (gethash ,key bv-config-values 'bv--not-found)))
     (unless (eq ,var 'bv--not-found)
       ,@body)))

;;;; Type Predicates and Validation

;; Basic predicates
(defun bv-path-p (x)
  "Return non-nil if X is a valid path string."
  (and (stringp x)
       (not (string-empty-p x))))

(defun bv-url-p (x)
  "Return non-nil if X looks like a URL."
  (and (stringp x)
       (string-match-p "\\`\\(https?\\|ftp\\|file\\)://" x)))

(defun bv-package-p (x)
  "Return non-nil if X is a package symbol or '(package . min-version)."
  (or (symbolp x)
      (and (consp x)
           (symbolp (car x))
           (or (null (cdr x))
               (stringp (cdr x))))))

;; Maybe predicates (nil or type)
(defun bv-maybe (predicate)
  "Return a predicate that accepts nil or values satisfying PREDICATE."
  (lambda (x)
    (or (null x)
        (funcall predicate x))))

(defalias 'bv-maybe-string-p (bv-maybe #'stringp))
(defalias 'bv-maybe-integer-p (bv-maybe #'integerp))
(defalias 'bv-maybe-path-p (bv-maybe #'bv-path-p))
(defalias 'bv-maybe-function-p (bv-maybe #'functionp))

;; List predicates
(defun bv-list-of (predicate)
  "Return a predicate that checks if a value is a list of PREDICATE."
  (lambda (x)
    (and (listp x)
         (cl-every predicate x))))

(defalias 'bv-list-of-strings-p (bv-list-of #'stringp))
(defalias 'bv-list-of-symbols-p (bv-list-of #'symbolp))
(defalias 'bv-list-of-packages-p (bv-list-of #'bv-package-p))

;; Validation helpers
(defmacro bv-ensure (predicate value &optional error-message)
  "Ensure VALUE satisfies PREDICATE, signal error otherwise."
  `(unless (funcall ,predicate ,value)
     (error (or ,error-message
                (format "Value '%S' does not satisfy predicate '%s'"
                        ,value
                        ,(if (symbolp predicate)
                             (symbol-name predicate)
                           "custom predicate"))))))

(defun bv-validate-type (value type)
  "Validate that VALUE matches TYPE specification."
  (cond
   ((functionp type) (funcall type value))
   ((eq type 'string) (stringp value))
   ((eq type 'integer) (integerp value))
   ((eq type 'boolean) (booleanp value))
   ((eq type 'function) (functionp value))
   ((eq type 'list) (listp value))
   ((eq type 'alist) (and (listp value) (cl-every #'consp value)))
   ((eq type 'plist) (and (listp value) (zerop (mod (length value) 2))))
   (t (error "Unknown type specification: %s" type))))

;;;; Conditional Loading Helpers

(defmacro bv-when-packages (&rest packages-and-body)
  "Execute body when all PACKAGES are available."
  (let ((packages (if (listp (car packages-and-body))
                      (car packages-and-body)
                    (list (car packages-and-body))))
        (body (if (listp (car packages-and-body))
                  (cdr packages-and-body)
                (cdr packages-and-body))))
    `(when (cl-every #'featurep ',packages)
       ,@body)))

(defmacro bv-after-init (&rest body)
  "Execute BODY after Emacs initialization."
  `(if after-init-time
       (progn ,@body)
     (add-hook 'after-init-hook (lambda () ,@body))))

(defmacro bv-idle-eval (delay &rest body)
  "Execute BODY after DELAY seconds of idle time."
  (declare (indent 1))
  `(run-with-idle-timer ,delay nil (lambda () ,@body)))

;;;; Configuration Helpers

(defun bv-expand-config (config)
  "Expand CONFIG value, evaluating functions with bv-config-values."
  (if (functionp config)
      (funcall config bv-config-values)
    config))

(defmacro bv-defcustom (name default docstring &rest args)
  "Define a customizable variable and register it in bv-config-values."
  (declare (doc-string 3) (indent 2))
  (let* ((name-str (symbol-name name))
         (value-key (if (string-prefix-p "bv-" name-str)
                        (substring name-str 3)
                      name-str)))
    `(progn
       (defcustom ,name ,default ,docstring ,@args)
       (bv-set-value ',(intern value-key) ,name))))

;;;; Utility Functions

(defun bv-keyword-plist-p (plist)
  "Return non-nil if PLIST is a valid keyword property list."
  (and (listp plist)
       (zerop (mod (length plist) 2))
       (cl-loop for (key _) on plist by #'cddr
                always (keywordp key))))

(defun bv-alist-p (list)
  "Return non-nil if LIST is an association list."
  (and (listp list)
       (cl-every (lambda (x) (and (consp x) (atom (car x)))) list)))

(defun bv-ensure-list (x)
  "Ensure X is a list."
  (if (listp x) x (list x)))

;;;; Keybinding Helpers

(defmacro bv-leader (&rest bindings)
  "Bind key sequences in `bv-app-map`.
BINDINGS is a flat list of key/command pairs."
  (declare (indent 1))
  (let (forms)
    (while bindings
      (let ((key (pop bindings))
            (cmd (pop bindings)))
        (when (and cmd (not (keywordp cmd)))
          (push `(define-key bv-app-map (kbd ,key) ,cmd) forms))))
    `(progn ,@(nreverse forms))))

;;;; Package Management Helpers

(defun bv-package-installed-p (package)
  "Return non-nil if PACKAGE is installed."
  (if (consp package)
      (package-installed-p (car package) (cdr package))
    (package-installed-p package)))

(defun bv-ensure-packages (&rest packages)
  "Ensure all PACKAGES are installed."
  (dolist (package packages)
    (unless (bv-package-installed-p package)
      (package-refresh-contents)
      (if (consp package)
          (package-install (car package) (cdr package))
        (package-install package)))))

;;;; Development Helpers

(defun bv-report-config ()
  "Report current configuration values and enabled features."
  (interactive)
  (with-current-buffer (get-buffer-create "*Configuration Report*")
    (erase-buffer)
    (insert "Configuration Report\n")
    (insert "===================\n\n")

    (insert "Enabled Features:\n")
    (dolist (feature bv-enabled-features)
      (insert (format "  - %s\n" feature)))

    (insert "\nConfiguration Values:\n")
    (maphash (lambda (key value)
               (insert (format "  %s: %S\n" key
                               (if (stringp value)
                                   value
                                 (prin1-to-string value)))))
             bv-config-values)

    (display-buffer (current-buffer)))
  )

(provide 'bv-core)
;;; bv-core.el ends here
