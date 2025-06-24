;;; bv-core.el --- Core utilities and infrastructure -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Ayan Das

;; Author: Ayan Das <bvits@riseup.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: convenience, lisp, extensions
;; URL: https://github.com/b-vitamins/dotfiles/emacs/lisp/bv-core.el

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
;; Core infrastructure for Emacs configuration
;; Provides feature system, configuration values, and utilities
;;
;; Main components:
;; - Feature registration with dependency management
;; - Configuration value storage
;; - XDG-compliant directory management
;; - Delayed initialization utilities
;; - Leader keybinding support via `bv-app-map'
;;
;; Optional dependencies:
;; - xdg (Emacs 30+): For XDG Base Directory compliance
;;   Functions work without it using fallback paths.
;;
;; Key APIs:
;; - Feature system: `bv-register-feature', `bv-feature-enabled-p'
;; - Config values: `bv-set-value', `bv-get-value', `bv-value-present-p'
;; - Path utilities: `bv-expand-etc-file', `bv-expand-var-file'
;; - Utility macros: `bv-after-init', `bv-idle-eval', `bv-when-packages'
;; - Leader map: `bv-app-map' for application-specific keybindings
;;
;; Example leader setup:
;;   (bv-leader
;;     "SPC" #'execute-extended-command
;;     "f f" #'find-file
;;     "b b" #'switch-to-buffer)
;;
;; Path utilities use `file-name-concat' when available (Emacs 28+).
;; All path functions are created via partial application.
;;
;; All macros include (declare (indent N)) forms for correct
;; indentation with C-M-q.  Use M-x pp-macroexpand-last-sexp to debug
;; macro expansions during development.
;;
;; Performance notes:
;; - Report functions use single-pass hash table iteration
;; - Symbol sorting is done directly without string conversion
;; - Lists are copied before sorting to avoid modifying originals
;;
;; Timer management:
;; - `bv-idle-eval' and `bv-with-delayed-setup' return timer objects
;; - Timers can be cancelled with `cancel-timer'
;; - See function docstrings for cancellation examples
;;
;; Testing:
;; - Test suite available in bv-core-tests.el
;; - Run all tests with: M-x ert RET t RET
;; - Run specific test with: M-x ert RET bv-test-TESTNAME RET
;;
;; Common type specifications for bv-defcustom:
;;   'boolean          - checkbox widget
;;   'integer          - number input
;;   'string           - text input
;;   'directory        - directory selector
;;   'file             - file selector
;;   'symbol           - symbol input
;;   '(repeat TYPE)    - list of TYPE
;;   '(choice SPECS)   - radio buttons or dropdown
;;   '(const VALUE)    - constant value option
;;   '(alist ...)      - association list
;;   '(plist ...)      - property list
;;
;; Always include :safe predicates for file-local variable safety:
;;   #'booleanp, #'integerp, #'stringp, #'symbolp, etc.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Declare functions that might not be available at compile time
(eval-when-compile
  (declare-function xdg-config-home "xdg" ())
  (declare-function xdg-cache-home "xdg" ())
  (declare-function file-name-concat "files" (directory &rest components)))

;; Soft dependency on xdg.el (available in Emacs 30+)
(defvar bv--have-xdg (ignore-errors (require 'xdg nil t))
  "Non-nil if xdg.el is available.")

;; Compatibility layer for Emacs 30+ changes
(eval-and-compile
  ;; Check if we're on Emacs 30+ with the new sort API
  (defconst bv--emacs30-p (and (boundp 'emacs-version)
                                (string-match "^30\\." emacs-version))
    "Non-nil if running on Emacs 30 or later.")

  ;; Compatibility wrapper for sort function
  (if bv--emacs30-p
      ;; Emacs 30+ with keyword-only API
      (defun bv--sort-list (list predicate)
        "Sort LIST using PREDICATE in a non-destructive way."
        (sort (copy-sequence list) :lessp predicate))
    ;; Emacs < 30 with traditional API
    (defun bv--sort-list (list predicate)
      "Sort LIST using PREDICATE in a non-destructive way."
      (sort (copy-sequence list) predicate)))

  ;; Compatibility wrapper for cl-find-if
  (if bv--emacs30-p
      (defun bv--find-if (predicate list)
        "Find the first item in LIST where PREDICATE is true."
        (catch 'found
          (dolist (item list)
            (when (funcall predicate item)
              (throw 'found item)))
          nil))
    (defalias 'bv--find-if 'cl-find-if))

  ;; Compatibility wrapper for cl-union
  (if bv--emacs30-p
      (defun bv--union (list1 list2)
        "Return the union of LIST1 and LIST2."
        (let ((result (copy-sequence list1)))
          (dolist (item list2)
            (unless (memq item result)
              (push item result)))
          result))
    (defun bv--union (list1 list2)
      "Return the union of LIST1 and LIST2."
      (cl-union list1 list2))))

;;;; Customization Group

(defgroup bv nil
  "Configuration utilities and infrastructure."
  :group 'convenience)

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

;; User leader map for keybindings
;;;###autoload
(defvar bv-app-map (make-sparse-keymap)
  "User leader map for application-specific keybindings.")

;;;; Feature System

(defvar bv-enabled-features '()
  "List of enabled features.")

(defvar bv-feature-dependencies (make-hash-table :test #'eq)
  "Hash table mapping features to their dependencies.")

(defvar bv--feature-loading-stack '()
  "Stack of features currently being loaded, for circular dependency detection.")

;;;###autoload
(defun bv-feature-enabled-p (feature)
  "Return non-nil if FEATURE is enabled."
  (memq feature bv-enabled-features))

(defun bv--check-circular-dependency (feature)
  "Check if loading FEATURE would create a circular dependency."
  (when (memq feature bv--feature-loading-stack)
    (error "Circular dependency detected: %s -> %s"
           (mapconcat #'symbol-name (reverse bv--feature-loading-stack) " -> ")
           feature)))

(defun bv--find-dependency-path (from to &optional visited)
  "Find a dependency path from FROM to TO feature.
Returns the path as a list of features if found, nil otherwise.
VISITED is used internally to track visited nodes."
  (if (eq from to)
      (list from)
    (let ((visited (or visited (make-hash-table :test #'eq)))
          (result nil))
      (unless (gethash from visited)
        (puthash from t visited)
        (dolist (dep (gethash from bv-feature-dependencies))
          (unless result  ; Only continue if we haven't found a path yet
            (when-let ((path (bv--find-dependency-path dep to visited)))
              (setq result (cons from path))))))
      result)))

(defun bv--would-create-cycle-p (feature new-deps)
  "Check if adding NEW-DEPS to FEATURE would create a circular dependency.
Returns non-nil if a cycle would be created."
  (let ((result nil))
    (dolist (dep new-deps)
      (unless result  ; Only continue if we haven't found a cycle
        (when (bv--find-dependency-path dep feature)
          (setq result t))))
    result))

;;;###autoload
(defun bv-register-feature (feature &optional dependencies)
  "Register FEATURE as enabled with optional DEPENDENCIES.
If DEPENDENCIES is provided, it should be a list of features that must
be loaded before this feature.  Circular dependencies are detected and
will signal an error.  If called multiple times, dependencies are merged."
  ;; Check for circular dependencies in loading stack
  (bv--check-circular-dependency feature)

  ;; Push onto loading stack
  (push feature bv--feature-loading-stack)

  ;; Check if the new dependencies would create a cycle
  (when dependencies
    ;; First check that all dependencies exist
    (dolist (dep dependencies)
      (unless (bv-feature-enabled-p dep)
        (error "Feature %s depends on %s which is not enabled" feature dep)))

    ;; Then check for cycles
    (when (bv--would-create-cycle-p feature dependencies)
      ;; Find the specific cycle for a better error message
      (let ((cycle-dep (bv--find-if
                        (lambda (dep)
                          (bv--find-dependency-path dep feature))
                        dependencies)))
        (if cycle-dep
            (let ((path (bv--find-dependency-path cycle-dep feature)))
              (error "Circular dependency detected: %s -> %s -> %s"
                     feature cycle-dep
                     (mapconcat #'symbol-name (cdr path) " -> ")))
          (error "Circular dependency detected involving %s" feature)))))

  ;; Store/merge dependencies
  (when dependencies
    (let ((existing (gethash feature bv-feature-dependencies)))
      (puthash feature
               (bv--union existing dependencies)
               bv-feature-dependencies)))

  ;; Register the feature
  (cl-pushnew feature bv-enabled-features :test #'eq)

  ;; Pop from loading stack
  (pop bv--feature-loading-stack))

;;;###autoload
(defmacro bv-with-feature (feature &rest body)
  "Execute BODY if FEATURE is enabled."
  (declare (indent 1))
  `(when (bv-feature-enabled-p ',feature)
     ,@body))

;;;###autoload
(defmacro bv-when-feature (feature &rest body)
  "Execute BODY when FEATURE becomes available.
FEATURE can be either a symbol (matching both the feature name and
the file to load), or a list (FEATURE-NAME FILE-OR-LIBRARY) where
FEATURE-NAME is the feature to check and FILE-OR-LIBRARY is what
to pass to `with-eval-after-load'.

This macro handles autoloaded features by attempting to load them
first with `require'.

Examples:
  ;; Simple case - feature name matches file name
  (bv-when-feature completion
    (setq completion-styles '(orderless basic)))

  ;; Complex case - feature name differs from file
  (bv-when-feature (my-completion \"completion-config\")
    (setq completion-styles '(orderless basic)))"
  (declare (indent 1))
  (let ((feature-name (if (listp feature) (car feature) feature))
        (load-name (if (listp feature) (cadr feature) feature)))
    `(cond
      ;; Already enabled in bv
      ((bv-feature-enabled-p ',feature-name)
       (progn ,@body))
      ;; Try to load it now (handles autoloaded features)
      ((require ',load-name nil 'noerror)
       (progn ,@body))
      ;; Defer until loaded
      (t
       (with-eval-after-load ',load-name
         ,@body)))))

;;;; Configuration Value System

(defvar bv-config-values (make-hash-table :test #'eq)
  "Central configuration value storage.")

(defconst bv--not-found (make-symbol "bv--not-found")
  "Sentinel value for missing configuration entries.")

;;;###autoload
(defun bv-set-value (key value)
  "Set configuration KEY to VALUE.
KEY must be a symbol.  Signals an error if KEY is not a symbol."
  (unless (symbolp key)
    (error "Configuration key must be a symbol, got %S of type %s"
           key (type-of key)))
  (puthash key value bv-config-values))

;;;###autoload
(defun bv-get-value (key &optional default)
  "Get configuration value for KEY, or DEFAULT if not set."
  (gethash key bv-config-values default))

;;;###autoload
(defun bv-value-present-p (key)
  "Return non-nil if configuration KEY is present."
  (not (eq (gethash key bv-config-values bv--not-found) bv--not-found)))

;;;###autoload
(defun bv-require-value (key &optional error-message)
  "Get configuration value for KEY, error if not present.
If ERROR-MESSAGE is provided, use it instead of the default error message."
  (let ((value (gethash key bv-config-values bv--not-found)))
    (if (eq value bv--not-found)
        (error (or error-message
                   (format "Required configuration value '%s' not provided" key)))
      value)))

;;;###autoload
(defmacro bv-with-value (key var &rest body)
  "Bind VAR to the value of KEY and execute BODY if KEY exists."
  (declare (indent 2))
  `(let ((,var (gethash ',key bv-config-values bv--not-found)))
     (unless (eq ,var bv--not-found)
       ,@body)))

;;;; Configuration Helpers

;;;###autoload
(defmacro bv-defcustom (name default docstring &rest args)
  "Define a customizable variable NAME and register it in `bv-config-values'.
NAME is the variable name, DEFAULT is its default value, and DOCSTRING
is its documentation.  Additional ARGS are passed to `defcustom'.

Example usage:
  (bv-defcustom bv-enable-feature nil
    \"Enable experimental feature.\"
    :type 'boolean
    :safe #'booleanp)

  (bv-defcustom bv-cache-directory \"~/.cache/bv\"
    \"Directory for cache files.\"
    :type 'directory
    :safe #'stringp)"
  (declare (doc-string 3) (indent 2))
  (let* ((name-str (symbol-name name))
         (value-key (if (string-prefix-p "bv-" name-str)
                        (substring name-str 3)
                      name-str)))
    `(progn
       (defcustom ,name ,default ,docstring
         :group 'bv
         ,@args)
       (bv-set-value ',(intern value-key) ,name))))

;;;###autoload
(defmacro bv-with-delayed-setup (name delay &rest body)
  "Execute BODY after DELAY, but only once per Emacs session.
NAME should be a unique symbol identifying this setup block.
This is useful for expensive initialization that should happen
in the background without blocking startup.

Returns the timer object if setup is scheduled, nil if already done.
The timer can be cancelled with `cancel-timer'.

Example:
  (bv-with-delayed-setup 'heavy-mode-setup 5
    (require 'heavy-mode)
    (heavy-mode-global-setup)
    (message \"Heavy mode initialized\"))

  ;; To cancel if needed:
  (let ((timer (get 'bv--heavy-mode-setup-timer 'timer)))
    (when (timerp timer)
      (cancel-timer timer)))"
  (declare (indent 2))
  (let ((done-var (intern (format "bv--%s-setup-done" name)))
        (timer-var (intern (format "bv--%s-setup-timer" name))))
    `(progn
       (defvar ,done-var nil)
       (defvar ,timer-var nil)
       (unless ,done-var
         (setq ,timer-var
               (run-with-idle-timer ,delay nil
                                    (lambda ()
                                      (setq ,done-var t)
                                      (setq ,timer-var nil)
                                      ,@body)))
         ,timer-var))))

;;;; Utility Macros

;;;###autoload
(defmacro bv-after-init (&rest body)
  "Execute BODY after Emacs initialization."
  `(if after-init-time
       (progn ,@body)
     (add-hook 'after-init-hook (lambda () ,@body))))

;;;###autoload
(defmacro bv-idle-eval (delay &rest body)
  "Execute BODY after DELAY seconds of idle time.
WARNING: Idle timers run on the main thread and will block user input
while executing.  Keep BODY lightweight or consider using `run-with-timer'
for long-running tasks.

Returns the timer object, which can be cancelled with `cancel-timer'.
Note: Timers hold strong references and may accumulate memory if not
cancelled.  Use `bv-cancel-all-timers' for cleanup when needed.

Example:
  ;; Simple usage
  (bv-idle-eval 2
    (require 'heavy-package)
    (heavy-package-setup))

  ;; With cancellation
  (defvar my-setup-timer
    (bv-idle-eval 5
      (message \"Delayed setup running...\")
      (my-expensive-setup)))

  ;; Later, if needed:
  (when (timerp my-setup-timer)
    (cancel-timer my-setup-timer))"
  (declare (indent 1))
  `(run-with-idle-timer ,delay nil (lambda () ,@body)))

;;;###autoload
(defmacro bv-when-packages (packages &rest body)
  "Execute BODY when all PACKAGES are available.
PACKAGES should be a list of package symbols.  If all packages are
already loaded, BODY is executed immediately.  Otherwise, execution
is deferred until all packages are available.

The macro sets up `with-eval-after-load' hooks for each package,
and executes BODY only once after all packages have been loaded.

Examples:
  ;; Wait for multiple packages
  (bv-when-packages (company yasnippet)
    (setq company-backends '(company-yasnippet)))

  ;; Wait for a single package (still use a list)
  (bv-when-packages (org)
    (setq org-startup-indented t))

\(fn PACKAGES &rest BODY)"
  (declare (indent 1) (debug ((&rest symbolp) body)))
  (unless (listp packages)
    (error "PACKAGES must be a list, got %S" packages))
  (let ((func-sym (make-symbol "bv-when-packages-func")))
    `(let ((,func-sym (let ((done nil))
                        (lambda ()
                          (unless done
                            (when (cl-every #'featurep ',packages)
                              (setq done t)
                              ,@body))))))
       (if (cl-every #'featurep ',packages)
           (funcall ,func-sym)
         ,@(mapcar (lambda (p)
                     `(with-eval-after-load ',p
                        (funcall ,func-sym)))
                   packages)))))

;;;; Keybinding Helpers

;;;###autoload
(defmacro bv-leader (&rest bindings)
  "Bind key sequences in `bv-app-map`.
BINDINGS is a flat list of key/command pairs."
  (declare (indent 0) (debug (&rest form)))
  (let (forms)
    (while bindings
      (let ((key (pop bindings))
            (cmd (pop bindings)))
        (when (and key cmd)
          (push `(define-key bv-app-map (kbd ,key) ,cmd) forms))))
    `(progn ,@(nreverse forms))))

;;;; Path and Directory Utilities

;; User-configurable fallback directories
(defcustom bv-default-etc-directory nil
  "Default etc directory when not configured.
If nil, uses XDG_CONFIG_HOME/emacs/etc or ~/.config/emacs/etc."
  :type '(choice (const :tag "Auto-detect (XDG)" nil)
                 (directory :tag "Custom directory"))
  :tag "Default etc directory"
  :group 'bv)

(defcustom bv-default-var-directory nil
  "Default var directory when not configured.
If nil, uses XDG_CACHE_HOME/emacs/var or ~/.cache/emacs/var."
  :type '(choice (const :tag "Auto-detect (XDG)" nil)
                 (directory :tag "Custom directory"))
  :tag "Default var directory"
  :group 'bv)

(defun bv--default-etc-directory ()
  "Get default etc directory respecting XDG."
  (or bv-default-etc-directory
      (if bv--have-xdg
          (expand-file-name "emacs/etc" (xdg-config-home))
        "~/.config/emacs/etc")))

(defun bv--default-var-directory ()
  "Get default var directory respecting XDG."
  (or bv-default-var-directory
      (if (and bv--have-xdg (fboundp 'xdg-cache-home))
          (expand-file-name "emacs/var" (xdg-cache-home))
        "~/.cache/emacs/var")))

;; Compatibility shim for older Emacs versions
(defalias 'bv--file-name-concat
  (if (fboundp 'file-name-concat)
      #'file-name-concat
    (lambda (dir &rest components)
      "Join DIR and COMPONENTS using `expand-file-name'.
Compatibility shim for Emacs < 28."
      (let ((result (file-name-as-directory dir)))
        (dolist (comp components result)
          (setq result (expand-file-name comp result))))))
  "Function to concatenate file path components.")

(defun bv--expand-relative (key file &optional fallback)
  "Expand FILE relative to directory stored in config KEY.
If KEY is not set and FALLBACK is provided, use FALLBACK as the base directory.
Uses `file-name-concat' when available (Emacs 28+) for better path handling.
Handles both POSIX and Windows paths correctly."
  (when file
    (let ((file-name-handler-alist nil)) ; Skip TRAMP for performance
      (if-let ((dir (bv-get-value key)))
          (bv--file-name-concat (file-name-as-directory dir) file)
        (when fallback
          (bv--file-name-concat (file-name-as-directory fallback) file))))))

;;;###autoload
(defun bv-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Define path expansion functions using partial application
;;;###autoload
(defalias 'bv-expand-etc-file
  (lambda (file) (bv--expand-relative 'etc-dir file (bv--default-etc-directory)))
  "Expand FILE relative to etc directory.")

;;;###autoload
(defalias 'bv-expand-var-file
  (lambda (file) (bv--expand-relative 'var-dir file (bv--default-var-directory)))
  "Expand FILE relative to var directory.")

;; Guix path helpers (no fallbacks)
;;;###autoload
(defalias 'bv-expand-system-lib
  (apply-partially #'bv--expand-relative 'system-lib-dir)
  "Expand FILE relative to system lib directory.")

;;;###autoload
(defalias 'bv-expand-home-lib
  (apply-partially #'bv--expand-relative 'home-lib-dir)
  "Expand FILE relative to home lib directory.")

;;;###autoload
(defalias 'bv-expand-system-bin
  (apply-partially #'bv--expand-relative 'system-bin-dir)
  "Expand FILE relative to system bin directory.")

;;;###autoload
(defalias 'bv-expand-home-bin
  (apply-partially #'bv--expand-relative 'home-bin-dir)
  "Expand FILE relative to home bin directory.")

;;;###autoload
(defalias 'bv-expand-system-share
  (apply-partially #'bv--expand-relative 'system-share-dir)
  "Expand FILE relative to system share directory.")

;;;###autoload
(defalias 'bv-expand-home-share
  (apply-partially #'bv--expand-relative 'home-share-dir)
  "Expand FILE relative to home share directory.")

;;;###autoload
(defun bv-path-report ()
  "Display a report of all configured paths."
  (interactive)
  (require 'faces nil t) ; Ensure faces are available
  (let ((path-keys '(emacs-dir lisp-dir var-dir etc-dir
                               guix-system-profile guix-home-profile guix-user-profile
                               system-lib-dir home-lib-dir
                               system-bin-dir home-bin-dir
                               system-share-dir home-share-dir))
        (buffer (get-buffer-create "*bv-path-report*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Path Configuration\n")
        (insert "==================\n\n")
        ;; Process in symbol order using dedicated comparison function
        (dolist (key (bv--sort-list path-keys #'bv--symbol-lessp))
          (let ((value (bv-get-value key)))
            (insert (format "%-20s: %s\n" key
                            (if value
                                (propertize value 'face
                                            (if (and (stringp value)
                                                     (file-directory-p value))
                                                'success
                                              'warning))
                              (propertize "not configured" 'face 'shadow))))))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buffer)))

;;;###autoload
(defun bv-cancel-all-timers ()
  "Cancel all pending setup timers.
This is useful for cleanup or when reloading configuration.
Only cancels timers created by `bv-with-delayed-setup'."
  (interactive)
  (let ((cancelled 0))
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (string-match-p "^bv--.*-setup-timer$" (symbol-name sym))
                           (timerp (symbol-value sym)))
                  (cancel-timer (symbol-value sym))
                  (set sym nil)
                  (cl-incf cancelled))))
    (message "Cancelled %d setup timer(s)" cancelled)))

;;;; Development Helpers

(defun bv--symbol-lessp (a b)
  "Compare symbols A and B by their names.
More efficient than using `string<' with `symbol-name' inline."
  (string< (symbol-name a) (symbol-name b)))

;;;###autoload
(defun bv-report-config ()
  "Report current configuration values and enabled features."
  (interactive)
  (with-current-buffer (get-buffer-create "*bv-config-report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Configuration Report\n")
      (insert "====================\n\n")

      (insert "Enabled Features:\n")
      ;; Sort symbols directly
      (dolist (feature (bv--sort-list bv-enabled-features #'bv--symbol-lessp))
        (insert (format "  - %s" feature))
        (when-let ((deps (gethash feature bv-feature-dependencies)))
          (insert (format " (depends on: %s)"
                          (mapconcat #'symbol-name deps ", "))))
        (insert "\n"))

      (insert "\nConfiguration Values:\n")
      ;; Single pass collection with direct symbol sorting
      (let ((entries '()))
        (maphash (lambda (k v) (push (cons k v) entries)) bv-config-values)
        (dolist (entry (bv--sort-list entries (lambda (a b)
                                                 (bv--symbol-lessp (car a) (car b)))))
          (let ((key (car entry))
                (value (cdr entry)))
            (insert (format "  %-20s: %S\n" key
                            (if (stringp value)
                                value
                              (prin1-to-string value)))))))

      (goto-char (point-min)))
    (display-buffer (current-buffer))))

;;;###autoload
(defun bv-check-feature-dependencies ()
  "Check for any missing feature dependencies or circular dependencies."
  (interactive)
  (let ((missing-deps nil)
        (circular-deps nil))
    ;; Check for missing dependencies
    (maphash (lambda (feature deps)
               (dolist (dep deps)
                 (unless (bv-feature-enabled-p dep)
                   (push (cons feature dep) missing-deps))))
             bv-feature-dependencies)
    ;; Check for circular dependencies
    (maphash (lambda (feature deps)
               (dolist (dep deps)
                 (when (bv--find-dependency-path dep feature)
                   (push (cons feature dep) circular-deps))))
             bv-feature-dependencies)
    ;; Report results
    (cond
     (circular-deps
      (error "Circular dependencies detected: %s"
             (mapconcat (lambda (pair)
                          (format "%s -> %s" (car pair) (cdr pair)))
                        circular-deps ", ")))
     (missing-deps
      (message "Missing dependencies: %s"
               (mapconcat (lambda (pair)
                            (format "%s needs %s" (car pair) (cdr pair)))
                          missing-deps ", ")))
     (t
      (message "All feature dependencies satisfied")))))

;;;; Feature Registration

;; Register bv-core itself (no dependencies)
(condition-case err
    (bv-register-feature 'bv-core)
  (error
   ;; If there's a circular dependency error during initialization,
   ;; we still want to proceed but warn the user
   (message "Warning during bv-core registration: %s" (error-message-string err))
   ;; Force registration without dependency check as fallback
   (cl-pushnew 'bv-core bv-enabled-features :test #'eq)))

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

;;;###autoload
(provide 'bv-core)
;;; bv-core.el ends here
