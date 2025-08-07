;;; bv-tramp.el --- TRAMP configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; TRAMP remote file access with SSH config integration.
;; Enhanced with better UI integration for themes.

;;; Code:

(require 'tramp)
(require 'cl-lib)
(require 'pcase)

;; Declare external variables and functions to avoid warnings
(defvar user-emacs-directory)
(defvar remote-file-name-inhibit-cache)
(defvar vc-ignore-dir-regexp)
(defvar tramp-use-ssh-controlmaster-options)
(defvar embark-keymap-alist)
(declare-function consult--read "consult")
(declare-function consult--directory-prompt "consult")
(declare-function consult--file-prompt "consult")
(declare-function vterm "vterm")

(defgroup bv-tramp nil
  "TRAMP configuration for BV."
  :group 'bv
  :prefix "bv-tramp-")

(defcustom bv-tramp-default-method "ssh"
  "Default TRAMP method."
  :type 'string
  :group 'bv-tramp)

(defcustom bv-tramp-ssh-config-file "~/.ssh/config"
  "Path to SSH configuration file."
  :type 'file
  :group 'bv-tramp)

(defcustom bv-tramp-history-file "~/.emacs.d/tramp-history"
  "File to store TRAMP connection history."
  :type 'file
  :group 'bv-tramp)

(defvar bv-tramp--host-history nil
  "History of recently used SSH hosts.")

;;; SSH Config Parsing

(defun bv-tramp--parse-ssh-config ()
  "Parse SSH config for host entries with metadata."
  (when-let ((file (expand-file-name bv-tramp-ssh-config-file))
             ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((hosts '())
            current-host
            current-props)
        (while (not (eobp))
          (cond
           ;; Host line
           ((looking-at "^Host\\s-+\\([^*\n]+\\)$")
            (when (and current-host current-props)
              (push (cons current-host current-props) hosts))
            (setq current-host (match-string 1)
                  current-props '()))
           ;; HostName line
           ((looking-at "^\\s-*HostName\\s-+\\(.+\\)$")
            (push (cons 'hostname (match-string 1)) current-props))
           ;; User line
           ((looking-at "^\\s-*User\\s-+\\(.+\\)$")
            (push (cons 'user (match-string 1)) current-props))
           ;; Port line
           ((looking-at "^\\s-*Port\\s-+\\(.+\\)$")
            (push (cons 'port (match-string 1)) current-props)))
          (forward-line 1))
        ;; Don't forget the last host
        (when (and current-host current-props)
          (push (cons current-host current-props) hosts))
        (nreverse hosts)))))

(defun bv-tramp--parse-ssh-hosts ()
  "Parse SSH config for host names only."
  (mapcar #'car (bv-tramp--parse-ssh-config)))

(defun bv-tramp--host-metadata (host)
  "Get metadata for HOST from SSH config."
  (cdr (assoc host (bv-tramp--parse-ssh-config))))

;;; Enhanced host selection

(defun bv-tramp--annotate-host (host)
  "Annotate HOST with metadata for display."
  (let* ((props (bv-tramp--host-metadata host))
         (hostname (cdr (assoc 'hostname props)))
         (user (cdr (assoc 'user props)))
         (port (cdr (assoc 'port props)))
         (parts (delq nil
                      (list
                       (when user (format "user=%s" user))
                       (when (and port (not (string= port "22")))
                         (format "port=%s" port))
                       (when (and hostname (not (string= hostname host)))
                         (format "→%s" hostname))))))
    (if parts
        (format "%-30s %s" host
                (propertize (string-join parts " ")
                            'face 'bv-themes-faded))
      host)))

(defun bv-tramp--read-host ()
  "Read SSH host with enhanced completion."
  (let* ((hosts (bv-tramp--parse-ssh-hosts))
         (annotated-hosts (mapcar (lambda (host)
                                    (cons (bv-tramp--annotate-host host) host))
                                  hosts)))
    (if (and (fboundp 'consult--read) (boundp 'consult--tofu-char))
        ;; Use consult if available
        (let ((selected (consult--read annotated-hosts
                                       :prompt "SSH host: "
                                       :require-match t
                                       :sort nil
                                       :history 'bv-tramp--host-history
                                       :category 'ssh-host)))
          (cdr (assoc selected annotated-hosts)))
      ;; Fallback to completing-read
      (let ((selected (completing-read "SSH host: "
                                       annotated-hosts
                                       nil t nil
                                       'bv-tramp--host-history)))
        (cdr (assoc selected annotated-hosts))))))

;;; Connection management

(defun bv-tramp--make-tramp-path (host &optional path)
  "Create TRAMP path for HOST with optional PATH."
  (concat "/" bv-tramp-default-method ":" host ":" (or path "")))

(defun bv-tramp-run (command &optional type)
  "Execute COMMAND on remote host with optional TYPE (:file or :directory)."
  (let* ((host (bv-tramp--read-host))
         (remote-path (bv-tramp--make-tramp-path host))
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

;;; Interactive commands

(defun bv-tramp-shell ()
  "Open shell on remote host."
  (interactive)
  (bv-tramp-run 'shell))

(defun bv-tramp-eshell ()
  "Open eshell on remote host."
  (interactive)
  (bv-tramp-run 'eshell))

(defun bv-tramp-vterm ()
  "Open vterm on remote host."
  (interactive)
  (if (fboundp 'vterm)
      (bv-tramp-run 'vterm)
    (user-error "Vterm is not available")))

(defun bv-tramp-dired ()
  "Open Dired on remote host."
  (interactive)
  (bv-tramp-run 'dired :directory))

(defun bv-tramp-find-file ()
  "Open file on remote host."
  (interactive)
  (bv-tramp-run 'find-file :file))

(defun bv-tramp-sudo-find-file ()
  "Open file as root on remote host."
  (interactive)
  (let* ((host (bv-tramp--read-host))
         (remote-file (read-file-name
                       (format "File (root@%s): " host)
                       (bv-tramp--make-tramp-path host)))
         (sudo-path (replace-regexp-in-string
                     (format "/%s:%s:" bv-tramp-default-method host)
                     (format "/sudo:root@%s:" host)
                     remote-file)))
    (find-file sudo-path)))

;;; Connection status

(defun bv-tramp-cleanup ()
  "Clean up all TRAMP connections."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers)
  (message "All TRAMP connections cleaned up"))

(defun bv-tramp-cleanup-host ()
  "Clean up connection to a specific host."
  (interactive)
  (let ((host (bv-tramp--read-host)))
    (tramp-cleanup-connection
     (tramp-dissect-file-name (bv-tramp--make-tramp-path host)))
    (message "Cleaned up connection to %s" host)))

;;; Status display

(defun bv-tramp-list-connections ()
  "List active TRAMP connections."
  (interactive)
  (let ((connections (tramp-list-connections))
        (buf (get-buffer-create "*TRAMP Connections*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Active TRAMP Connections\n")
      (insert "━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      (if connections
          (dolist (vec connections)
            (let ((method (tramp-file-name-method vec))
                  (user (tramp-file-name-user vec))
                  (host (tramp-file-name-host vec)))
              (insert (format "• %s://%s@%s\n"
                              method
                              (or user "")
                              host))))
        (insert "No active connections\n"))
      (special-mode)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

;;; Configuration

;; Basic TRAMP settings
(setq tramp-verbose 1)
(setq tramp-default-method bv-tramp-default-method)
(setq tramp-persistency-file-name
      (expand-file-name "tramp" user-emacs-directory))
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Performance improvements
(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-chunksize 500)
(setq tramp-default-proxies-alist nil)

;; Add TRAMP own remote path
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;; Consult integration

(defvar bv-tramp-consult-source
  `(:name "SSH Hosts"
    :narrow ?h
    :category ssh-host
    :face bv-themes-salient
    :history ssh-host-history
    :items ,#'bv-tramp--parse-ssh-hosts
    :annotate ,(lambda (host)
                 (let* ((props (bv-tramp--host-metadata host))
                        (hostname (cdr (assoc 'hostname props)))
                        (user (cdr (assoc 'user props))))
                   (when (or hostname user)
                     (format " [%s@%s]"
                             (or user "")
                             (or hostname host)))))
    :action (lambda (host)
              (find-file (bv-tramp--make-tramp-path host))))
  "Consult source for SSH hosts.")

(with-eval-after-load 'consult
  (when (boundp 'consult-buffer-sources)
    (add-to-list 'consult-buffer-sources 'bv-tramp-consult-source 'append)))

;;; Embark integration

(with-eval-after-load 'embark
  (defvar embark-ssh-host-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'bv-tramp-shell)
      (define-key map (kbd "e") #'bv-tramp-eshell)
      (define-key map (kbd "d") #'bv-tramp-dired)
      (define-key map (kbd "f") #'bv-tramp-find-file)
      (define-key map (kbd "r") #'bv-tramp-sudo-find-file)
      (define-key map (kbd "c") #'bv-tramp-cleanup-host)
      map)
    "Embark keymap for SSH host actions.")

  (add-to-list 'embark-keymap-alist '(ssh-host . embark-ssh-host-map)))

;;; Keybindings

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "R f") 'bv-tramp-find-file)
    (define-key bv-app-map (kbd "R d") 'bv-tramp-dired)
    (define-key bv-app-map (kbd "R s") 'bv-tramp-shell)
    (define-key bv-app-map (kbd "R e") 'bv-tramp-eshell)
    (define-key bv-app-map (kbd "R v") 'bv-tramp-vterm)
    (define-key bv-app-map (kbd "R r") 'bv-tramp-sudo-find-file)
    (define-key bv-app-map (kbd "R l") 'bv-tramp-list-connections)
    (define-key bv-app-map (kbd "R c") 'bv-tramp-cleanup)
    (define-key bv-app-map (kbd "R C") 'bv-tramp-cleanup-host)))

(provide 'bv-tramp)
;;; bv-tramp.el ends here
