;;; bv-corfu.el --- Corfu in-buffer completion UI -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Corfu is the in-buffer completion surface.  It follows a precision-auto
;; policy: code gets useful automatic completion, prose and shells stay quiet,
;; and any complex completion can be promoted to the minibuffer via Consult.

;;; Code:

(require 'cl-lib)
(require 'corfu)
(require 'corfu-history)
(require 'subr-x)
(require 'bv-completion)
(require 'bv-cape)

;;; Declarations

(defvar completion-extra-properties)
(defvar completion-in-region--data)
(defvar corfu--index)
(defvar corfu-auto-delay)
(defvar corfu-auto-prefix)
(defvar corfu-excluded-modes)
(defvar corfu-map)
(defvar corfu-popupinfo-delay)
(defvar corfu-popupinfo-hide)
(defvar corfu-popupinfo-max-height)
(defvar corfu-popupinfo-max-width)
(defvar corfu-popupinfo-min-height)
(defvar corfu-popupinfo-min-width)
(defvar corfu-popupinfo-resize)
(defvar corfu-preserve-symlinks)
(defvar global-corfu-minibuffer)
(defvar mct--active)
(defvar read-passwd-map)
(defvar savehist-additional-variables)
(defvar vertico--input)

(declare-function consult-completion-in-region "consult" (start end collection &optional predicate))
(declare-function corfu-info-documentation "corfu-info" ())
(declare-function corfu-info-location "corfu-info" ())
(declare-function corfu-insert-separator "corfu" ())
(declare-function corfu-popupinfo-mode "corfu-popupinfo" (&optional arg))
(declare-function corfu-quick-complete "corfu-quick" ())
(declare-function corfu-quick-insert "corfu-quick" ())
(declare-function corfu-send "corfu" ())

;;; Policy

(defgroup bv-corfu nil
  "BV Corfu configuration."
  :group 'corfu
  :prefix "bv-corfu-")

(defcustom bv-corfu-auto-policy
  '((eglot . (:auto t :prefix 2 :delay 0.15))
    (prog . (:auto t :prefix 3 :delay 0.20))
    (text . (:auto t :prefix 4 :delay 0.35))
    (manual . (:auto nil :prefix 3 :delay 0.20))
    (shell . (:auto nil :prefix 3 :delay 0.20)))
  "Corfu auto-completion profiles."
  :type '(alist :key-type symbol :value-type plist)
  :group 'bv-corfu)

(defun bv-corfu--profile (name key)
  "Return KEY from profile NAME in `bv-corfu-auto-policy'."
  (plist-get (alist-get name bv-corfu-auto-policy) key))

(defun bv-corfu-apply-profile (name)
  "Apply Corfu profile NAME to the current buffer."
  (setq-local corfu-auto (bv-corfu--profile name :auto)
              corfu-auto-prefix (bv-corfu--profile name :prefix)
              corfu-auto-delay (bv-corfu--profile name :delay)))

;;; Core Settings

(setq corfu-min-width 24
      corfu-max-width 100
      corfu-count 10
      corfu-scroll-margin 2
      corfu-cycle t
      corfu-auto nil
      corfu-auto-delay 0.2
      corfu-auto-prefix 3
      corfu-separator ?\s
      corfu-quit-at-boundary 'separator
      corfu-quit-no-match 'separator
      corfu-preview-current nil
      corfu-on-exact-match nil
      corfu-preselect 'valid
      corfu-excluded-modes '(gud-mode)
      corfu-preserve-symlinks t
      global-corfu-minibuffer #'bv-corfu-enable-in-minibuffer-p)

;;; Kind Margin

(defconst bv-corfu--kind-labels
  '((array . ("[]" bv-face-info))
    (boolean . ("bool" bv-face-special))
    (class . ("cls" bv-face-info))
    (color . ("#" bv-face-success))
    (constant . ("const" bv-face-special))
    (constructor . ("ctor" bv-face-salient))
    (enum . ("enum" bv-face-info))
    (enum-member . ("mem" bv-face-info))
    (event . ("evt" bv-face-warning))
    (field . ("fld" bv-face-default))
    (file . ("file" bv-face-success))
    (folder . ("dir" bv-face-success))
    (function . ("fn" bv-face-salient))
    (interface . ("ifc" bv-face-info))
    (keyword . ("kw" bv-face-keybind))
    (method . ("meth" bv-face-salient))
    (module . ("mod" bv-face-info))
    (namespace . ("ns" bv-face-info))
    (number . ("num" bv-face-special))
    (operator . ("op" bv-face-dim))
    (package . ("pkg" bv-face-info))
    (property . ("prop" bv-face-default))
    (reference . ("ref" bv-face-info))
    (snippet . ("snip" bv-face-success))
    (string . ("str" bv-face-success))
    (struct . ("st" bv-face-info))
    (text . ("txt" bv-face-dim))
    (type . ("type" bv-face-info))
    (unit . ("unit" bv-face-dim))
    (value . ("val" bv-face-special))
    (variable . ("var" bv-face-default))
    (cape-dabbrev . ("dab" bv-face-dim))
    (cape-dict . ("dict" bv-face-info))
    (cape-history . ("hist" bv-face-special))
    (cape-keyword . ("kw" bv-face-keybind))
    (cape-yasnippet . ("snip" bv-face-success))
    (project-file . ("proj" bv-face-special))
    (t . ("?" bv-face-dim)))
  "Fixed-width labels for Corfu candidate kinds.")

(defun bv-corfu--candidate-kind (metadata candidate)
  "Return kind for CANDIDATE using completion METADATA."
  (or (when-let ((kind-fn (completion-metadata-get metadata 'company-kind)))
        (ignore-errors (funcall kind-fn candidate)))
      (completion-metadata-get metadata 'category)
      t))

(defun bv-corfu--format-kind (kind)
  "Return a fixed-width label for KIND."
  (let* ((entry (or (assq kind bv-corfu--kind-labels)
                    (assq t bv-corfu--kind-labels)))
         (label (cadr entry))
         (face (caddr entry)))
    (propertize (truncate-string-to-width label 4 0 ?\s t)
                'face face)))

(defun bv-corfu-margin-formatter (metadata)
  "Return a Corfu margin formatter for METADATA."
  (when (bv-completion-icons-enabled-p)
    (lambda (candidate)
      (bv-corfu--format-kind
       (bv-corfu--candidate-kind metadata candidate)))))

(add-to-list 'corfu-margin-formatters #'bv-corfu-margin-formatter)

;;; Popupinfo and Extensions

(defun bv-corfu-setup-history ()
  "Enable Corfu history."
  (setq corfu-history-duplicate 10
        corfu-history-decay 10)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)
    (put 'corfu-history 'history-length 500))
  (corfu-history-mode 1))

(defun bv-corfu-setup-popupinfo (&optional frame)
  "Enable popupinfo on graphical FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (require 'corfu-popupinfo)
      (setq corfu-popupinfo-delay '(1.2 . 0.4)
            corfu-popupinfo-max-width 72
            corfu-popupinfo-max-height 16
            corfu-popupinfo-min-width 30
            corfu-popupinfo-min-height 3
            corfu-popupinfo-resize t
            corfu-popupinfo-hide nil)
      (corfu-popupinfo-mode 1))))

(defun bv-corfu-setup-terminal (&optional frame)
  "Enable terminal Corfu support for FRAME when available."
  (with-selected-frame (or frame (selected-frame))
    (when (and (not (display-graphic-p))
               (require 'corfu-terminal nil t)
               (fboundp 'corfu-terminal-mode))
      (corfu-terminal-mode 1))))

(defun bv-corfu-configure-frame (&optional frame)
  "Configure Corfu helpers for FRAME."
  (bv-corfu-setup-popupinfo frame)
  (bv-corfu-setup-terminal frame))

;;; Commands

(defun bv-corfu--at-separator-p ()
  "Return non-nil if point is after the Corfu separator."
  (and (> (point) (point-min))
       (= (char-before) corfu-separator)))

(defun bv-corfu--at-word-end-p ()
  "Return non-nil if point is at a natural word boundary."
  (or (eobp)
      (not (looking-at-p "[[:alnum:]_]"))))

(defun bv-corfu-smart-sep ()
  "Insert an Orderless separator or commit the selected candidate plus space."
  (interactive)
  (cond
   ((and (bv-corfu--at-separator-p)
         (or (eobp) (memq (char-after) '(?\s ?\n))))
    (delete-char -1)
    (corfu-insert)
    (insert " "))
   ((and (>= corfu--index 0)
         (not (bv-corfu--at-separator-p))
         (bv-corfu--at-word-end-p))
    (corfu-insert)
    (insert " "))
   (t
    (corfu-insert-separator))))

(defun bv-corfu-move-to-minibuffer ()
  "Move the current in-buffer completion session to Consult."
  (interactive)
  (when-let ((data completion-in-region--data))
    (pcase-let ((`(,beg ,end ,table ,pred ,extras) data))
      (let ((completion-extra-properties extras))
        (consult-completion-in-region beg end table pred)))))

;;; Mode Profiles

(defun bv-corfu-prog-settings ()
  "Apply programming-mode Corfu settings."
  (bv-corfu-apply-profile 'prog))

(defun bv-corfu-text-settings ()
  "Apply text-mode Corfu settings."
  (bv-corfu-apply-profile 'text))

(defun bv-corfu-manual-settings ()
  "Apply manual Corfu settings."
  (bv-corfu-apply-profile 'manual))

(defun bv-corfu-shell-settings ()
  "Apply shell Corfu settings."
  (bv-corfu-apply-profile 'shell))

(defun bv-corfu-eglot-settings ()
  "Apply Eglot Corfu settings."
  (bv-corfu-apply-profile 'eglot))

(defun bv-corfu-minibuffer-settings ()
  "Apply minibuffer Corfu settings."
  (setq-local corfu-auto nil
              corfu-popupinfo-delay nil))

(defun bv-corfu-enable-in-minibuffer-p ()
  "Return non-nil if Corfu should be active in this minibuffer."
  (and (not (or (bound-and-true-p vertico--input)
                (bound-and-true-p mct--active)
                (eq (current-local-map) read-passwd-map)))
       (local-variable-p 'completion-at-point-functions)))

;;; Keybindings

(autoload 'corfu-info-documentation "corfu-info" nil t)
(autoload 'corfu-info-location "corfu-info" nil t)
(autoload 'corfu-quick-complete "corfu-quick" nil t)
(autoload 'corfu-quick-insert "corfu-quick" nil t)

(keymap-set corfu-map "TAB" #'corfu-complete)
(keymap-set corfu-map "RET" #'corfu-insert)
(keymap-set corfu-map "M-m" #'bv-corfu-move-to-minibuffer)
(keymap-set corfu-map "SPC" #'bv-corfu-smart-sep)
(keymap-set corfu-map "M-TAB" #'corfu-expand)
(keymap-set corfu-map "C-n" #'corfu-next)
(keymap-set corfu-map "C-p" #'corfu-previous)
(keymap-set corfu-map "M-d" #'corfu-info-documentation)
(keymap-set corfu-map "M-l" #'corfu-info-location)
(keymap-set corfu-map "M-q" #'corfu-quick-complete)
(keymap-set corfu-map "C-q" #'corfu-quick-insert)

(with-eval-after-load 'eshell
  (keymap-set corfu-map "RET" #'corfu-send))

(add-to-list 'corfu-continue-commands #'bv-corfu-move-to-minibuffer)

;;; Activation

(bv-corfu-setup-history)
(bv-corfu-configure-frame)

(add-hook 'after-make-frame-functions #'bv-corfu-configure-frame)
(add-hook 'prog-mode-hook #'bv-corfu-prog-settings)
(add-hook 'text-mode-hook #'bv-corfu-text-settings)
(add-hook 'shell-mode-hook #'bv-corfu-shell-settings)
(add-hook 'eshell-mode-hook #'bv-corfu-shell-settings)
(add-hook 'comint-mode-hook #'bv-corfu-shell-settings)
(add-hook 'git-commit-mode-hook #'bv-corfu-manual-settings)
(add-hook 'minibuffer-setup-hook #'bv-corfu-minibuffer-settings)

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'bv-corfu-eglot-settings))

(global-corfu-mode 1)

(provide 'bv-corfu)
;;; bv-corfu.el ends here
