;;; bv-tramp.el --- TRAMP configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for TRAMP remote file access.

;;; Code:

(eval-when-compile
  (require 'tramp)
  (require 'cl-lib))


(autoload 'consult--buffer-state "consult")



(define-prefix-command 'bv-tramp-map)

(defun bv-tramp--parse-sconfig-hosts ()
  "Parse SSH configuration file and return a list of host definitions."
  (when-let ((file (expand-file-name "~/.ssh/config")))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (delete nil
              (cl-loop while (not (eobp))
                       when (re-search-forward
                             (rx bol
                                 (* space)
                                 "Host"
                                 space
                                 (group (+ (any "a-z" "A-Z" "0-9" "_.%*" "-"))))
                             (pos-eol)
                             t)
                       collect (match-string 1)
                       unless (> (skip-chars-forward "\t") 0)
                       do (forward-line 1))))))

(cl-defun bv-tramp-run (command &rest args &key thing &allow-other-keys)
  "Execute COMMAND with ARGS in TRAMP session and manipulate remote THING."
  (let* ((host (completing-read "SSH host: " (bv-tramp--parse-sconfig-hosts)))
         (read-thing
          (if thing
              (pcase thing
                (:directory (read-directory-name
                             (format "Directory (%s): " host)
                             (format "/-:%s:" host)))
                (:file (read-file-name
                        (format "File (%s): " host)
                        (format "/-:%s:" host))))
            (format "/-:%s:" host)))
         (default-directory read-thing))
    (cond ((and args thing) (apply command read-thing (cddr args)))
          (args (apply command args))
          (t (funcall command)))))

(autoload 'tramp-list-remote-buffers "tramp-cmds")

(with-eval-after-load 'tramp
  (when (boundp 'bv-tramp-buffer-source)
    (setq bv-tramp-buffer-source
      `(:name "Tramp"
              :narrow ?r
              :category buffer
              :state ,'consult--buffer-state
              :items ,(lambda () (mapcar 'buffer-name (when (fboundp 'tramp-list-remote-buffers)
                                                       (tramp-list-remote-buffers)))))))
  
  (with-eval-after-load 'consult
    (when (and (boundp 'consult-buffer-sources)
               (boundp 'bv-tramp-buffer-source))
      (add-to-list 'consult-buffer-sources bv-tramp-buffer-source 'append))))

(defun bv-tramp-shell (&optional arg)
  "Open a shell buffer inside a TRAMP host with ARG."
  (interactive "P")
  (bv-tramp-run 'shell arg))

(defun bv-tramp-eshell (&optional arg)
  "Open an eshell buffer inside a TRAMP host with ARG."
  (interactive "P")
  (bv-tramp-run 'eshell arg))

(defun bv-tramp-dired ()
  "Open a Dired buffer inside a TRAMP host."
  (interactive)
  (bv-tramp-run 'dired :thing :directory))

(defun bv-tramp-find-file ()
  "Open a file inside a TRAMP host."
  (interactive)
  (bv-tramp-run 'find-file :thing :file))

(with-eval-after-load 'bv-keymaps
  (when (and (boundp 'bv-app-map) (boundp 'bv-tramp-map))
    (define-key bv-app-map (kbd "R") 'bv-tramp-map)))

(when (boundp 'bv-tramp-map)
  (let ((map bv-tramp-map))
    (define-key map "f" 'bv-tramp-find-file)
    (define-key map "d" 'bv-tramp-dired)
    (define-key map "s" 'bv-tramp-shell)
    (define-key map "e" 'bv-tramp-eshell)))

(with-eval-after-load 'tramp
  (when (boundp 'tramp-verbose)
    (setq tramp-verbose 1))
  (when (boundp 'tramp-default-method)
    (setq tramp-default-method "ssh"))
  (when (boundp 'tramp-remote-path)
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
  (when (boundp 'tramp-default-proxies-alist)
    (set-default 'tramp-default-proxies-alist '((".*" "\\`root\\'" "/ssh:%h:")))))

(provide 'bv-tramp)
;;; bv-tramp.el ends here