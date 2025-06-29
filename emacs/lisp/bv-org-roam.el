;;; bv-org-roam.el --- Org-roam knowledge base configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for org-roam knowledge management system.

;;; Code:

(eval-when-compile (require 'eieio))


(autoload 'marginalia--time "marginalia")
(autoload 'oref "eieio")

;; Declare structure for emacsql connection
(cl-defstruct emacsql-connection handle)

(defun bv-patch-emacsql-close (connection &rest _args)
  "Prevent calling emacsql-close if CONNECTION handle is nil."
  (when (ignore-errors (oref connection handle))
    t))

(with-eval-after-load 'emacsql
  (advice-add 'emacsql-close :before-while #'bv-patch-emacsql-close))

(eval-when-compile (let ((org-roam-v2-ack t)) (require 'org-roam)))

(when (boundp 'org-roam-v2-ack)
  (setq org-roam-v2-ack t))
(when (boundp 'org-roam-completion-everywhere)
  (setq org-roam-completion-everywhere t))
(when (boundp 'org-roam-directory)
  (setq org-roam-directory "~/documents/slipbox"))
(when (boundp 'org-roam-db-gc-threshold)
  (setq org-roam-db-gc-threshold most-positive-fixnum))

(defun bv-ensure-org-roam-directories ()
  "Ensure org-roam directories exist."
  (let ((roam-dir (expand-file-name (if (boundp 'org-roam-directory) org-roam-directory "~/documents/slipbox")))
        (cache-dir (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs")))
    (unless (file-exists-p roam-dir)
      (make-directory roam-dir t))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))))

(with-eval-after-load 'org-roam
  (when (boundp 'org-roam-db-location)
    (setq org-roam-db-location
          (concat
           (or (getenv "XDG_CACHE_HOME") "~/.cache")
           "/emacs/org-roam.db")))
  
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE, a directory relative to `org-roam-directory'."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name
            (org-roam-node-file node)
            org-roam-directory))))
      (error "")))
  
  (when (boundp 'org-roam-node-display-template)
    (setq org-roam-node-display-template
          (concat "" "${title:80} " (propertize "${tags:20}" 'face 'org-tag))))
  (when (boundp 'org-roam-node-annotation-function)
    (setq org-roam-node-annotation-function
          (lambda (node) (marginalia--time (org-roam-node-file-mtime node)))))
  
  (bv-ensure-org-roam-directories)
  
  (run-with-idle-timer 1 nil
                       (lambda ()
                         (when (fboundp 'org-roam-db-sync)
                           (org-roam-db-sync))
                         (org-roam-db-autosync-enable)))
  
  (defun bv-org-roam-open-ref ()
    "Prompt for a list of all ROAM_REFS in the current buffer."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (if-let* ((refs (org-property-values "ROAM_REFS"))
                (choices
                 (mapcar
                  (lambda (x) (org-unbracket-string "[[" "]]" x))
                  (split-string
                   (car (org-property-values "ROAM_REFS"))
                   " ")))
                (node-ref
                 (completing-read
                  "Refs: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          (category . org-roam-ref)
                          ,(cons 'display-sort-function 'identity))
                      (complete-with-action action choices string pred)))
                  nil
                  'require-match)))
          node-ref
        (error "No roam refs in this node"))))
  
  (with-eval-after-load 'org
    (when (boundp 'org-mode-map)
      (let ((map org-mode-map))
      (define-key map (kbd "C-TAB") 'completion-at-point)
      (define-key map (kbd "C-c r r") 'org-roam-ref-add)
      (define-key map (kbd "C-c r R") 'org-roam-ref-remove)
      (define-key map (kbd "C-c r f") 'org-roam-ref-find)
      (define-key map (kbd "C-c r t") 'org-roam-tag-add)
      (define-key map (kbd "C-c r T") 'org-roam-tag-remove)
      (define-key map (kbd "C-c r a") 'org-roam-alias-add)
      (define-key map (kbd "C-c r A") 'org-roam-alias-remove)
      (define-key map (kbd "C-c r O") 'bv-org-roam-open-ref))))
  
  (when (boundp 'org-roam-capture-templates)
    (setq org-roam-capture-templates
          '(("s"
           "Slip"
           plain
           "%?"
           :target
           (file+head
            "slips/%<%Y-%m-%d>-${slug}.org"
            ":PROPERTIES:\n:ID: %(org-id-new)\n:CUSTOM_ID: \n:END:\n#+title: ${title}\n#+filetags: \n\n")
           :unnarrowed t)
          ("l"
           "Literature"
           plain
           "%?"
           :target
           (file+head
            "slips/%<%Y-%m-%d>-lit-${slug}.org"
            ":PROPERTIES:\n:ID: %(org-id-new)\n:CUSTOM_ID: \n:ROAM_REFS: @\n:END:\n#+title: ${title}\n#+filetags: :literature:\n\n* Summary\n\n* Key Concepts\n\n* Connections\n\n")
           :unnarrowed t)
          ("c"
           "Concept"
           plain
           "%?"
           :target
           (file+head
            "slips/%<%Y-%m-%d>-concept-${slug}.org"
            ":PROPERTIES:\n:ID: %(org-id-new)\n:CUSTOM_ID: \n:END:\n#+title: ${title}\n#+filetags: :concept:\n\n")
           :unnarrowed t)
          ("f"
           "Fleeting"
           plain
           "%?"
           :target
           (file+head
            "slips/%<%Y-%m-%d>-fleeting-${slug}.org"
            ":PROPERTIES:\n:ID: %(org-id-new)\n:CUSTOM_ID: \n:END:\n#+title: ${title}\n#+filetags: :fleeting:\n\n")
           :unnarrowed t)))))

(with-eval-after-load 'embark
  (defvar-keymap embark-roam-ref-map
    :doc "Keymap for actions on org-roam refs."
    :parent (if (boundp 'embark-url-map) embark-url-map)
    "v" 'bv-mpv-play-url
    "RET" 'browse-url-generic
    "c" 'browse-url-chromium
    "r" 'org-roam-ref-remove)
  (when (boundp 'embark-keymap-alist)
    (add-to-list 'embark-keymap-alist '(org-roam-ref . embark-roam-ref-map)))
  (advice-add 'org-roam-ref-add :around 'bv-browse-url-trace-url))

(when (boundp 'mode-specific-map)
  (let ((map mode-specific-map))
  (define-key map (kbd "n n") 'org-roam-buffer-toggle)
  (define-key map (kbd "n f") 'org-roam-node-find)
  (define-key map (kbd "n i") 'org-roam-node-insert)
  (define-key map (kbd "n r") 'org-roam-ref-find)
  (define-key map (kbd "n C") 'org-roam-capture)))

(provide 'bv-org-roam)
;;; bv-org-roam.el ends here