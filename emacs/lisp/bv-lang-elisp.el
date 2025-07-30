;;; bv-lang-elisp.el --- Enhanced Emacs Lisp development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Enhanced Emacs Lisp development environment with integrated linting,
;; formatting, and development tools.

;;; Code:

(require 'elisp-mode)
(require 'flymake)
(require 'eldoc)

;; Optional package loading
(autoload 'lispy-mode "lispy" nil t)
(autoload 'helpful-callable "helpful" nil t)
(autoload 'helpful-variable "helpful" nil t)
(autoload 'helpful-function "helpful" nil t)
(autoload 'helpful-macro "helpful" nil t)
(autoload 'helpful-at-point "helpful" nil t)
(autoload 'eros-mode "eros" nil t)
(autoload 'package-lint-current-buffer "package-lint" nil t)
(autoload 'ielm "ielm" nil t)

;; External functions declarations
(declare-function update-file-autoloads "autoload" (file &optional save-after outfile))
(declare-function ielm-return "ielm" ())

;; External variables
(defvar generated-autoload-file)

;; Enhanced flymake for Elisp
(defun bv-elisp-setup-flymake ()
  "Setup enhanced flymake for Emacs Lisp."
  (flymake-mode 1)
  ;; Additional flymake checkers can be added here
  (setq-local flymake-no-changes-timeout 0.5))

(add-hook 'emacs-lisp-mode-hook #'bv-elisp-setup-flymake)
(add-hook 'lisp-interaction-mode-hook #'bv-elisp-setup-flymake)

;; Enable lispy when available
(defun bv-elisp-maybe-enable-lispy ()
  "Enable lispy-mode if available."
  (when (fboundp 'lispy-mode)
    (lispy-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'bv-elisp-maybe-enable-lispy)
(add-hook 'lisp-interaction-mode-hook #'bv-elisp-maybe-enable-lispy)

;; Enable eros (inline results) when available
(defun bv-elisp-maybe-enable-eros ()
  "Enable eros-mode if available."
  (when (fboundp 'eros-mode)
    (eros-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'bv-elisp-maybe-enable-eros)

;; Eldoc configuration
(setq eldoc-idle-delay 0.1)
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

;; Enhanced evaluation functions
(defun bv-elisp-eval-buffer-and-message ()
  "Evaluate buffer and display message."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated: %s" (buffer-name)))

(defun bv-elisp-eval-defun-and-test ()
  "Evaluate defun and run any associated test."
  (interactive)
  (eval-defun nil)
  (when (string-match-p "test-" (or (thing-at-point 'symbol) ""))
    (ert (thing-at-point 'symbol))))

(defun bv-elisp-eval-and-insert ()
  "Evaluate last sexp and insert result."
  (interactive)
  (let ((value (eval-last-sexp nil)))
    (save-excursion
      (forward-sexp -1)
      (insert (format " ;=> %S" value)))))

;; Formatting functions
(defun bv-elisp-format-buffer ()
  "Format current Elisp buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp 1)
      (when (looking-at "\\s-*\n\\s-*")
        (just-one-space))
      (forward-char 1)))
  (indent-region (point-min) (point-max)))

(defun bv-elisp-align-let-bindings ()
  "Align let bindings in the current form."
  (interactive)
  (save-excursion
    (backward-up-list)
    (when (looking-at "(let\\*?\\s-*(")
      (down-list 2)
      (let ((beg (point)))
        (up-list)
        (align-regexp beg (point) "\\(\\s-*\\)" 1 1 nil)))))

;; Package development helpers
(defun bv-elisp-generate-autoloads ()
  "Generate autoloads for current buffer."
  (interactive)
  (if (fboundp 'update-file-autoloads)
      (let ((generated-autoload-file (concat (file-name-sans-extension buffer-file-name) "-autoloads.el")))
        (update-file-autoloads buffer-file-name t)
        (message "Generated autoloads in %s" generated-autoload-file))
    (message "Autoload generation not available")))

(defun bv-elisp-byte-compile-and-load ()
  "Byte compile and load current file."
  (interactive)
  (byte-compile-file buffer-file-name)
  (load (concat (file-name-sans-extension buffer-file-name) ".elc")))

;; Documentation helpers - use helpful when available
(defun bv-elisp-describe-thing-at-point ()
  "Describe function, variable, or face at point using helpful if available."
  (interactive)
  (if (fboundp 'helpful-at-point)
      (helpful-at-point)
    (let ((symbol (symbol-at-point)))
      (cond
       ((fboundp symbol) (describe-function symbol))
       ((boundp symbol) (describe-variable symbol))
       ((facep symbol) (describe-face symbol))
       (t (message "No documentation found for %s" symbol))))))

(defun bv-elisp-describe-function (symbol)
  "Describe function SYMBOL using helpful if available."
  (interactive (list (intern (completing-read "Function: " obarray 'fboundp t))))
  (if (fboundp 'helpful-function)
      (helpful-function symbol)
    (describe-function symbol)))

(defun bv-elisp-describe-variable (symbol)
  "Describe variable SYMBOL using helpful if available."
  (interactive (list (intern (completing-read "Variable: " obarray 'boundp t))))
  (if (fboundp 'helpful-variable)
      (helpful-variable symbol)
    (describe-variable symbol)))

;; Linting with package-lint and checkdoc
(defun bv-elisp-lint-buffer ()
  "Run various linters on current buffer."
  (interactive)
  (checkdoc)
  (when (fboundp 'package-lint-current-buffer)
    (package-lint-current-buffer)))

;; Testing helpers
(defun bv-elisp-run-tests ()
  "Run ERT test for current buffer."
  (interactive)
  (ert-run-tests-interactively
   (concat "^" (regexp-quote (file-name-base buffer-file-name)) "-")))

(defun bv-elisp-create-test-file ()
  "Create a test file for current buffer."
  (interactive)
  (let* ((base (file-name-base buffer-file-name))
         (dir (file-name-directory buffer-file-name))
         (test-dir (expand-file-name "test" dir))
         (test-file (expand-file-name (concat base "-test.el") test-dir)))
    (unless (file-exists-p test-dir)
      (make-directory test-dir t))
    (find-file test-file)
    (insert (format ";;; %s-test.el --- Tests for %s  -*- lexical-binding: t -*-\n\n"
                    base base))
    (insert "(require 'ert)\n")
    (insert (format "(require '%s)\n\n" base))
    (insert (format "(ert-deftest %s-test-example ()\n  \"Example test.\"\n  (should (= 1 1)))\n\n" base))
    (insert (format ";;; %s-test.el ends here\n" base))))

;; Navigation improvements
(defun bv-elisp-goto-defun ()
  "Go to definition of symbol at point."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (cond
     ((fboundp symbol) (find-function symbol))
     ((boundp symbol) (find-variable symbol))
     ((facep symbol) (find-face-definition symbol))
     (t (message "No definition found for %s" symbol)))))

;; REPL enhancements
(defun bv-elisp-send-to-ielm ()
  "Send region or defun to IELM."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun))))
    (with-current-buffer (get-buffer-create "*ielm*")
      (unless (derived-mode-p 'inferior-emacs-lisp-mode)
        (ielm))
      (goto-char (point-max))
      (insert text)
      (when (fboundp 'ielm-return)
        (ielm-return)))))

;; Keybindings
(with-eval-after-load 'elisp-mode
  (let ((map emacs-lisp-mode-map))
    ;; Evaluation
    (define-key map (kbd "C-c C-b") #'bv-elisp-eval-buffer-and-message)
    (define-key map (kbd "C-c C-c") #'bv-elisp-eval-defun-and-test)
    (define-key map (kbd "C-c C-e") #'bv-elisp-eval-and-insert)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-elisp-format-buffer)
    (define-key map (kbd "C-c C-a") #'bv-elisp-align-let-bindings)
    ;; Development
    (define-key map (kbd "C-c C-l") #'bv-elisp-lint-buffer)
    (define-key map (kbd "C-c C-t") #'bv-elisp-run-tests)
    (define-key map (kbd "C-c C-T") #'bv-elisp-create-test-file)
    (define-key map (kbd "C-c C-k") #'bv-elisp-byte-compile-and-load)
    ;; Navigation
    (define-key map (kbd "M-.") #'bv-elisp-goto-defun)
    (define-key map (kbd "C-c C-d") #'bv-elisp-describe-thing-at-point)
    (define-key map (kbd "C-h f") #'bv-elisp-describe-function)
    (define-key map (kbd "C-h v") #'bv-elisp-describe-variable)
    ;; REPL
    (define-key map (kbd "C-c C-z") #'ielm)
    (define-key map (kbd "C-c C-s") #'bv-elisp-send-to-ielm)))

;; Override global help keys in elisp buffers to use helpful
(defun bv-elisp-setup-helpful-keys ()
  "Setup helpful keybindings for elisp buffers."
  (when (fboundp 'helpful-callable)
    (local-set-key (kbd "C-h o") #'helpful-at-point)))

;; Outline configuration (inherited from bv-elisp.el)
(defun bv-elisp-outline-level ()
  "Custom outline level for Emacs Lisp."
  (- (match-end 0) (match-beginning 0)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local outline-regexp ";;[;*]+\\|\\s-*([^)]")
            (setq-local outline-level 'bv-elisp-outline-level)
            (outline-minor-mode 1)
            (bv-elisp-setup-helpful-keys)))

;; Pretty printing configuration
(with-eval-after-load 'pp
  (autoload 'pp-eval-last-sexp "pp")
  (autoload 'pp-eval-expression "pp")
  (autoload 'pp-macroexpand-last-sexp "pp"))

;; Integration with existing bv-elisp.el
(with-eval-after-load 'bv-elisp
  ;; This ensures compatibility with existing configuration
  t)

(provide 'bv-lang-elisp)
;;; bv-lang-elisp.el ends here