;;; bv-format.el --- Unified formatting layer -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; One formatting story across languages:
;; - `bv-format-buffer' formats the current buffer using the best available
;;   backend for the current major mode.
;; - `bv-format-on-save-mode' is an opt-in, buffer-local minor mode.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eglot nil t)

(autoload 'clang-format-buffer "clang-format" nil t)

(declare-function eglot-managed-p "eglot")
(declare-function eglot-format-buffer "eglot")

(defgroup bv-format nil
  "Unified formatting helpers."
  :group 'tools)

(defcustom bv-format-mode-backends
  '((python-ts-mode . (ruff eglot))
    (python-mode . (ruff eglot))
    (rust-ts-mode . (eglot rustfmt))
    (rust-mode . (eglot rustfmt))
    (c-ts-mode . (clang-format eglot))
    (c-mode . (clang-format eglot))
    (c++-ts-mode . (clang-format eglot))
    (c++-mode . (clang-format eglot))
    (prog-mode . (eglot)))
  "Formatter backends to try per major mode.

Each entry is (MODE . BACKENDS). MODE is matched using `derived-mode-p'
in the current buffer, so you can use parent modes such as `prog-mode'."
  :type '(repeat (cons (symbol :tag "Mode")
                       (repeat (symbol :tag "Backend"))))
  :group 'bv-format)

(defcustom bv-format-on-save-quiet t
  "When non-nil, do not error during save if formatting fails.

Failures are reported via `message' and the save continues."
  :type 'boolean
  :group 'bv-format)

(defun bv-format--buffer-backends ()
  "Return an ordered backend list for the current buffer."
  (or (cl-loop for (mode . backends) in bv-format-mode-backends
               when (derived-mode-p mode)
               return backends)
      nil))

(defun bv-format--replace-buffer-with (text)
  "Replace current buffer contents with TEXT, preserving point when possible."
  (let ((tmp (generate-new-buffer " *bv-format*")))
    (unwind-protect
        (progn
          (with-current-buffer tmp
            (insert text)
            (goto-char (point-min)))
          (let ((inhibit-read-only t))
            (replace-buffer-contents tmp)))
      (kill-buffer tmp))))

(defun bv-format--apply-stdin-formatter (program args)
  "Format current buffer by piping it through PROGRAM with ARGS.

Returns non-nil on success."
  (when-let ((exe (executable-find program)))
    (let ((stderr (generate-new-buffer " *bv-format stderr*"))
          (stdout (generate-new-buffer " *bv-format stdout*")))
      (unwind-protect
          (let ((exit (apply #'call-process-region
                             (point-min) (point-max)
                             exe nil (list stdout stderr) nil
                             args)))
            (if (eq exit 0)
                (let ((output (with-current-buffer stdout (buffer-string))))
                  (bv-format--replace-buffer-with output)
                  t)
              (message "Formatter failed (%s): %s"
                       program
                       (with-current-buffer stderr
                         (string-trim (buffer-string))))
              nil))
        (kill-buffer stdout)
        (kill-buffer stderr)))))

(defun bv-format--eglot ()
  "Format using Eglot when available."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (fboundp 'eglot-format-buffer))
    (condition-case err
        (progn
          (eglot-format-buffer)
          t)
      (error
       (message "Eglot format failed: %s" (error-message-string err))
       nil))))

(defun bv-format--clang-format ()
  "Format using clang-format."
  (cond
   ((fboundp 'clang-format-buffer)
    (clang-format-buffer)
    t)
   ((executable-find "clang-format")
    (let ((file buffer-file-name))
      (bv-format--apply-stdin-formatter
       "clang-format"
       (when file (list "-assume-filename" file)))))
   (t nil)))

(defun bv-format--ruff ()
  "Format using ruff."
  (when (executable-find "ruff")
    (let ((file buffer-file-name))
      (bv-format--apply-stdin-formatter
       "ruff"
       (append
        '("format")
        (when file (list "--stdin-filename" file))
        '("-"))))))

(defun bv-format--rust-edition ()
  "Return the Rust edition for the current project, or nil."
  (when-let ((root (locate-dominating-file default-directory "Cargo.toml")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "Cargo.toml" root))
      (when (re-search-forward "^[[:space:]]*edition[[:space:]]*=[[:space:]]*\"\\([0-9]+\\)\""
                               nil t)
        (match-string 1)))))

(defun bv-format--rustfmt ()
  "Format using rustfmt."
  (when (executable-find "rustfmt")
    (let ((edition (bv-format--rust-edition)))
      (bv-format--apply-stdin-formatter
       "rustfmt"
       (append
        '("--emit" "stdout")
        (when edition (list "--edition" edition)))))))

(defun bv-format--run-backend (backend)
  "Try formatting with BACKEND.

Returns non-nil on success."
  (pcase backend
    ('eglot (bv-format--eglot))
    ('clang-format (bv-format--clang-format))
    ('ruff (bv-format--ruff))
    ('rustfmt (bv-format--rustfmt))
    (_ nil)))

(defun bv-format-buffer (&optional arg)
  "Format the current buffer.

With prefix argument ARG, prompt for a specific backend to use."
  (interactive "P")
  (when buffer-read-only
    (user-error "Buffer is read-only"))
  (let* ((backends (bv-format--buffer-backends))
         (chosen (when arg
                   (intern
                    (completing-read
                     "Formatter: "
                     (mapcar (lambda (s) (symbol-name s)) backends)
                     nil t)))))
    (when chosen
      (setq backends (list chosen)))
    (unless backends
      (user-error "No formatter configured for %s" major-mode))
    (unless (cl-some #'bv-format--run-backend backends)
      (user-error "No formatter available for %s" major-mode))))

(defun bv-format--before-save ()
  "Format buffer before saving (for `bv-format-on-save-mode')."
  (condition-case err
      (bv-format-buffer)
    (error
     (if bv-format-on-save-quiet
         (message "Format-on-save skipped: %s" (error-message-string err))
       (signal (car err) (cdr err))))))

;;;###autoload
(define-minor-mode bv-format-on-save-mode
  "Format buffer on save (buffer-local)."
  :lighter " Fmt"
  (if bv-format-on-save-mode
      (add-hook 'before-save-hook #'bv-format--before-save nil t)
    (remove-hook 'before-save-hook #'bv-format--before-save t)))

(provide 'bv-format)
;;; bv-format.el ends here
