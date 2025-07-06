;;; bv-tramp.el --- TRAMP configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; TRAMP remote file access with SSH config integration.

;;; Code:

(require 'tramp)
(require 'cl-lib)
(declare-function consult--read "consult")
(declare-function consult--directory-prompt "consult")
(declare-function consult--file-prompt "consult")
(defun bv-tramp--parse-ssh-hosts ()
  "Parse SSH config for host names."
  (when-let ((file (expand-file-name "~/.ssh/config"))
             ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (cl-loop while (re-search-forward "^Host \\([^*\n]+\\)$" nil t)
               collect (match-string 1)))))

(defun bv-tramp--read-host ()
  "Read SSH host with consult if available."
  (let ((hosts (bv-tramp--parse-ssh-hosts)))
    (if (and (fboundp 'consult--read) (boundp 'consult--tofu-char))
        (consult--read hosts
                       :prompt "SSH host: "
                       :require-match t
                       :sort nil
                       :category 'ssh-host)
      (completing-read "SSH host: " hosts nil t))))

(defun bv-tramp-run (command &optional type)
  "Execute COMMAND on remote host with optional TYPE (:file or :directory)."
  (let* ((host (bv-tramp--read-host))
         (remote-path (format "/ssh:%s:" host))
         (default-directory
           (pcase type
             (:directory
              (if (fboundp 'consult--directory-prompt)
                  (consult--directory-prompt remote-path nil)
                (read-directory-name
                 (format "Directory (%s): " host) remote-path)))
             (:file
              (if (fboundp 'consult--file-prompt)
                  (consult--file-prompt remote-path nil)
                (read-file-name
                 (format "File (%s): " host) remote-path)))
             (_ remote-path))))
    (funcall command default-directory)))

(defun bv-tramp-shell ()
  "Open shell on remote host."
  (interactive)
  (bv-tramp-run 'shell))

(defun bv-tramp-eshell ()
  "Open eshell on remote host."
  (interactive)
  (bv-tramp-run 'eshell))

(defun bv-tramp-dired ()
  "Open dired on remote host."
  (interactive)
  (bv-tramp-run 'dired :directory))

(defun bv-tramp-find-file ()
  "Open file on remote host."
  (interactive)
  (bv-tramp-run 'find-file :file))

(when (boundp 'tramp-verbose)
  (setq tramp-verbose 1))
(when (boundp 'tramp-default-method)
  (setq tramp-default-method "ssh"))
(when (boundp 'tramp-remote-path)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(when (boundp 'tramp-use-ssh-controlmaster-options)
  (setq tramp-use-ssh-controlmaster-options t))
(when (boundp 'tramp-persistency-file-name)
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" user-emacs-directory)))
(when (boundp 'remote-file-name-inhibit-cache)
  (setq remote-file-name-inhibit-cache nil))
(when (boundp 'vc-ignore-dir-regexp)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(defvar bv-tramp-consult-source
  `(:name "SSH Hosts"
    :narrow ?h
    :category ssh-host
    :face bv-face-popout
    :history ssh-host-history
    :items ,#'bv-tramp--parse-ssh-hosts
    :action (lambda (host) (find-file (format "/ssh:%s:" host))))
  "Consult source for SSH hosts.")

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'bv-tramp-consult-source 'append)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "R f") 'bv-tramp-find-file)
    (define-key bv-app-map (kbd "R d") 'bv-tramp-dired)
    (define-key bv-app-map (kbd "R s") 'bv-tramp-shell)
    (define-key bv-app-map (kbd "R e") 'bv-tramp-eshell)))

(provide 'bv-tramp)
;;; bv-tramp.el ends here