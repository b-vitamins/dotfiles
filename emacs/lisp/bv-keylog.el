;;; bv-keylog.el --- Low-noise command telemetry for keybinding design -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; `bv-keylog-mode' records command-level key usage as JSON Lines.  It is meant
;; for later keybinding audits, not for content capture: self-insert text,
;; minibuffer contents, file names, and buffer contents are never written.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(defvar bv-help-map)
(defvar bv-toggle-map)

(defgroup bv-keylog nil
  "Command-level key usage telemetry."
  :group 'bv
  :prefix "bv-keylog-")

(defcustom bv-keylog-directory
  (expand-file-name "emacs/keylog/"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state/")))
  "Directory where BV key logs are stored."
  :type 'directory
  :group 'bv-keylog)

(defcustom bv-keylog-enabled-by-default nil
  "Whether `bv-keylog-mode' should be enabled during startup."
  :type 'boolean
  :group 'bv-keylog)

(defcustom bv-keylog-skip-commands
  '(self-insert-command
    org-self-insert-command
    quoted-insert
    newline
    newline-and-indent
    electric-newline-and-maybe-indent
    delete-backward-char
    backward-delete-char-untabify
    delete-char
    mwheel-scroll
    mouse-set-point
    ignore
    undefined)
  "Commands omitted from the persistent command log."
  :type '(repeat symbol)
  :group 'bv-keylog)

(defcustom bv-keylog-redacted-modes
  '(epa-passphrase-mode
    password-mode)
  "Major modes where key logging is disabled entirely."
  :type '(repeat symbol)
  :group 'bv-keylog)

(defvar bv-keylog--command-start nil
  "Float timestamp captured before the current command.")

(defun bv-keylog--file ()
  "Return today's JSONL log file."
  (expand-file-name (format-time-string "%Y-%m-%d.jsonl") bv-keylog-directory))

(defun bv-keylog--project-name ()
  "Return a safe project name for the current buffer, or nil."
  (when (fboundp 'project-current)
    (when-let* ((project (ignore-errors (project-current)))
                (root (ignore-errors (project-root project))))
      (file-name-nondirectory (directory-file-name root)))))

(defun bv-keylog--buffer-role ()
  "Return a coarse, non-sensitive buffer role."
  (cond
   ((minibufferp) "minibuffer")
   ((derived-mode-p 'prog-mode) "code")
   ((derived-mode-p 'org-mode) "org")
   ((derived-mode-p 'text-mode) "text")
   ((derived-mode-p 'special-mode) "special")
   (t "buffer")))

(defun bv-keylog--bound-command-prefix (keys command)
  "Return the shortest prefix of KEYS bound to COMMAND.

Commands that read minibuffer input can leave the prompted text inside
`this-command-keys-vector'.  This recovers the actual invocation prefix, such as
`C-x C-f', before any prompted path, query, or dispatch key."
  (catch 'found
    (dotimes (index (length keys))
      (let* ((prefix (seq-subseq keys 0 (1+ index)))
             (binding (ignore-errors (key-binding prefix t))))
        (when (eq binding command)
          (throw 'found (key-description prefix)))))
    nil))

(defun bv-keylog--redacted-invocation (description)
  "Return a safe fallback for prompted key DESCRIPTION."
  (cond
   ((string-match-p "\\`\\(?:M-x\\|ESC x\\)\\(?: \\|\\'\\)" description) "M-x")
   ((string-match-p "\\`M-:\\(?: \\|\\'\\)" description) "M-:")
   ((or (string-match-p " RET\\(?: \\|\\'\\)" description)
        (> (length (split-string description)) 6))
    "<redacted>")
   (t description)))

(defun bv-keylog--safe-key-description-from-vector (keys command)
  "Return a safe description for KEYS invoking COMMAND."
  (unless (zerop (length keys))
    (or (and (commandp command)
             (bv-keylog--bound-command-prefix keys command))
        (bv-keylog--redacted-invocation (key-description keys)))))

(defun bv-keylog--safe-key-description (command)
  "Return the current COMMAND key sequence without textual payload."
  (when-let ((keys (ignore-errors (this-command-keys-vector))))
    (condition-case nil
        (bv-keylog--safe-key-description-from-vector keys command)
      (error "<unavailable>"))))

(defun bv-keylog--skip-p (command)
  "Return non-nil when COMMAND should not be logged."
  (or (null command)
      (memq command bv-keylog-skip-commands)
      (memq major-mode bv-keylog-redacted-modes)
      (and (minibufferp)
           (memq command '(self-insert-command
                           delete-backward-char
                           backward-delete-char-untabify)))))

(defun bv-keylog--event (command)
  "Build a JSON-serializable event for COMMAND."
  (let* ((now (float-time))
         (duration (and bv-keylog--command-start
                        (- now bv-keylog--command-start)))
         (key (bv-keylog--safe-key-description command)))
    `((time . ,(format-time-string "%FT%T%z"))
      (key . ,(or key ""))
      (command . ,(symbol-name command))
      (mode . ,(symbol-name major-mode))
      (role . ,(bv-keylog--buffer-role))
      (project . ,(bv-keylog--project-name))
      (prefix_depth . ,(length (split-string (or key ""))))
      (minibuffer . ,(if (minibufferp) t json-false))
      (duration_ms . ,(and duration (round (* 1000 duration)))))))

(defun bv-keylog--write (event)
  "Append EVENT to the current key log."
  (make-directory bv-keylog-directory t)
  (with-temp-buffer
    (insert (json-encode event) "\n")
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max) (bv-keylog--file) t 'silent))))

(defun bv-keylog--pre-command ()
  "Capture the command start time."
  (setq bv-keylog--command-start (float-time)))

(defun bv-keylog--post-command ()
  "Persist command usage after the command completes."
  (condition-case err
      (let ((command this-command))
        (unless (bv-keylog--skip-p command)
          (bv-keylog--write (bv-keylog--event command))))
    (error
     (message "bv-keylog: %s" (error-message-string err)))))

;;;###autoload
(define-minor-mode bv-keylog-mode
  "Record command-level key usage for later keybinding audits."
  :global t
  :group 'bv-keylog
  :lighter nil
  (if bv-keylog-mode
      (progn
        (add-hook 'pre-command-hook #'bv-keylog--pre-command)
        (add-hook 'post-command-hook #'bv-keylog--post-command))
    (remove-hook 'pre-command-hook #'bv-keylog--pre-command)
    (remove-hook 'post-command-hook #'bv-keylog--post-command)))

(defun bv-keylog-open-current-log ()
  "Open today's command log."
  (interactive)
  (find-file (bv-keylog--file)))

(defun bv-keylog--read-events (&optional limit)
  "Read up to LIMIT events from today's key log."
  (let ((file (bv-keylog--file))
        events)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n" t))
          (push (ignore-errors
                  (json-read-from-string line))
                events))))
    (setq events (delq nil (nreverse events)))
    (if (and limit (> (length events) limit))
        (last events limit)
      events)))

(defun bv-keylog-report (&optional limit)
  "Show a compact usage report for the last LIMIT logged commands."
  (interactive "P")
  (let* ((events (bv-keylog--read-events (and limit (prefix-numeric-value limit))))
         (commands (make-hash-table :test #'equal))
         (keys (make-hash-table :test #'equal))
         (deep (make-hash-table :test #'equal)))
    (dolist (event events)
      (let-alist event
        (cl-incf (gethash .command commands 0))
        (when (and .key (not (string-empty-p .key)))
          (cl-incf (gethash .key keys 0)))
        (when (and .prefix_depth (> .prefix_depth 3))
          (cl-incf (gethash .key deep 0)))))
    (with-current-buffer (get-buffer-create "*BV Keylog Report*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "BV Keylog Report (%d events)\n\n" (length events)))
        (insert "Top commands\n")
        (bv-keylog--insert-top commands)
        (insert "\nTop key sequences\n")
        (bv-keylog--insert-top keys)
        (insert "\nHigh-depth sequences\n")
        (bv-keylog--insert-top deep)
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer (current-buffer)))))

(defun bv-keylog--insert-top (table)
  "Insert top rows from hash TABLE into the current buffer."
  (let (rows)
    (maphash (lambda (key count)
               (push (cons key count) rows))
             table)
    (setq rows (cl-sort rows #'> :key #'cdr))
    (if rows
        (dolist (row (seq-take rows 18))
          (insert (format "  %-40s %5d\n" (car row) (cdr row))))
	      (insert "  none\n"))))

(with-eval-after-load 'bv-bindings
  (define-key bv-help-map (kbd "K") #'bv-keylog-open-current-log)
  (define-key bv-help-map (kbd "L") #'bv-keylog-report)
  (define-key bv-toggle-map (kbd "K") #'bv-keylog-mode))

(when (and bv-keylog-enabled-by-default
           (not noninteractive))
  (bv-keylog-mode 1))

(provide 'bv-keylog)
;;; bv-keylog.el ends here
