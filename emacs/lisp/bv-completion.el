;;; bv-completion.el --- Completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:
;; Comprehensive completion configuration using orderless, consult, embark,
;; and marginalia.  Sets up advanced completion styles and custom keybindings.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'marginalia)
  (require 'consult))

;; Variables declared here that are defined in other packages
;; We'll wrap their usage with (when (boundp ...) ...)

(defgroup bv-completion nil
  "Tweaks to the built-in Emacs completion."
  :group 'bv)

(defcustom bv-completion-initial-narrow-alist '()
  "Alist of MODE . KEY to present an initial completion narrowing via `consult'."
  :group 'bv-completion
  :type '(alist :key-type symbol :value-type string))

(with-eval-after-load 'minibuffer
  (when (boundp 'tab-always-indent)
    (setq tab-always-indent 'complete))
  (when (boundp 'minibuffer-prompt-properties)
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt)))
  (when (boundp 'minibuffer-setup-hook)
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))
  (when (boundp 'completion-show-help)
    (setq completion-show-help nil))
  (when (boundp 'completions-format)
    (setq completions-format 'one-column))
  (when (boundp 'completions-header-format)
    (setq completions-header-format nil))

  (when (boundp 'minibuffer-mode-map)
    (let ((map minibuffer-mode-map))
      (define-key map (vector 'remap 'next-line) 'minibuffer-next-completion)
      (define-key map (vector 'remap 'previous-line) 'minibuffer-previous-completion)))

  (when (boundp 'completion-in-region-mode-map)
    (let ((map completion-in-region-mode-map))
      (define-key map (kbd "C-n") 'minibuffer-next-completion)
      (define-key map (kbd "C-p") 'minibuffer-previous-completion)))

  (when (boundp 'rfn-eshadow-update-overlay-hook)
    (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy))

  (when (boundp 'minibuffer-local-completion-map)
    (let ((map minibuffer-local-completion-map))
      (define-key map (kbd "SPC") nil)
      (define-key map (kbd "?") nil)))

  (when (boundp 'orderless-component-separator)
    (setq orderless-component-separator 'orderless-escapable-split-on-space))

  (defun bv-orderless-literal-dispatcher (pattern _index _total)
    "Dispatcher for literal matching when PATTERN ends with '='."
    (cond ((equal "=" pattern) '(orderless-literal . "="))
          ((string-suffix-p "=" pattern)
           (cons 'orderless-literal (substring pattern 0 -1)))))

  (defun bv-orderless-without-literal-dispatcher (pattern _index _total)
    "Dispatcher for exclusion matching when PATTERN ends with '!'."
    (cond ((equal "!" pattern) '(orderless-literal . "!"))
          ((string-suffix-p "!" pattern)
           (cons 'orderless-without-literal (substring pattern 0 -1)))))

  (defun bv-orderless-initialism-dispatcher (pattern _index _total)
    "Dispatcher for initialism matching when PATTERN ends with ','."
    (cond ((equal "," pattern) '(orderless-literal . ","))
          ((string-suffix-p "," pattern)
           (cons 'orderless-initialism (substring pattern 0 -1)))))

  (defun bv-orderless-flex-dispatcher (pattern _index _total)
    "Dispatcher for flex matching when PATTERN ends with '~'."
    (cond ((equal "~" pattern) '(orderless-literal . "~"))
          ((string-suffix-p "~" pattern)
           (cons 'orderless-flex (substring pattern 0 -1)))))

  (defun bv-completion--mode-buffers (&rest modes)
    "Return list of buffers whose major mode derives from any of MODES."
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer (cl-some 'derived-mode-p modes)))
     (buffer-list)))

  (defun bv-completion-initial-narrow ()
    "Automatically narrow consult-buffer based on current buffer's major mode."
    (let* ((buffer-mode-assoc bv-completion-initial-narrow-alist)
           (key (and (eq this-command 'consult-buffer)
                     (or (alist-get
                          (buffer-local-value
                           'major-mode
                           (window-buffer (minibuffer-selected-window)))
                          buffer-mode-assoc)
                         (cdr (seq-find
                               (lambda (mode)
                                 (with-current-buffer
                                     (window-buffer (minibuffer-selected-window))
                                   (derived-mode-p (car mode))))
                               buffer-mode-assoc))))))
      (when key
        (setq unread-command-events
              (append unread-command-events (list key 32))))))

  (when (boundp 'minibuffer-setup-hook)
    (add-hook 'minibuffer-setup-hook 'bv-completion-initial-narrow))

  (when (boundp 'orderless-style-dispatchers)
    (setq orderless-style-dispatchers
          '(bv-orderless-literal-dispatcher
            bv-orderless-without-literal-dispatcher
            bv-orderless-initialism-dispatcher
            bv-orderless-flex-dispatcher)))

  (require 'orderless)
  (when (boundp 'completion-styles)
    (setq completion-styles '(orderless basic)))
  (when (boundp 'completion-category-overrides)
    (setq completion-category-overrides
          '((project-file (styles orderless partial-completion basic))
            (file (styles orderless partial-completion basic)))))
  (when (boundp 'completion-category-defaults)
    (setq completion-category-defaults nil))
  (setq enable-recursive-minibuffers t))

(setq history-length 10000)
(when (boundp 'savehist-file)
  (setq savehist-file
        (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/history")))

(when (boundp 'global-map)
  (define-key global-map (kbd "s-.") 'embark-act)
  (define-key global-map (kbd "s->") 'embark-become))
(when (boundp 'minibuffer-local-map)
  (define-key minibuffer-local-map (kbd "M-r") 'consult-history))
(when (boundp 'global-map)
  (define-key global-map (kbd "M-y") 'consult-yank-pop)
  (define-key global-map (kbd "s-B") 'consult-buffer)
  (define-key global-map (kbd "C-x C-r") 'consult-recent-file))
(when (boundp 'minibuffer-local-map)
  (define-key minibuffer-local-map (kbd "s-g") 'embark-become))

(when (boundp 'goto-map)
  (let ((map goto-map))
    (define-key map (kbd "g") 'consult-goto-line)
    (define-key map (kbd "M-g") 'consult-goto-line)
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "o") 'consult-outline)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "m") 'consult-mark)
    (define-key map (kbd "M") 'consult-global-mark)
    (define-key map (kbd "b") 'consult-bookmark)))

(defun bv-goto-line-relative ()
  "Jump to line using relative numbers when narrowing is active."
  (interactive)
  (let ((consult-line-numbers-widen nil))
    (call-interactively 'consult-goto-line)))

(when (boundp 'narrow-map)
  (define-key narrow-map (kbd "g") 'bv-goto-line-relative))

(when (boundp 'search-map)
  (let ((map search-map))
    (define-key map (kbd "f") 'consult-find)
    (define-key map (kbd "g") 'consult-ripgrep)
    (define-key map (kbd "e") 'consult-isearch-history)
    (define-key map (kbd "l") 'consult-line)))

(autoload 'consult-isearch-history "consult")

(when (boundp 'isearch-mode-map)
  (let ((map isearch-mode-map))
    (define-key map (kbd "M-e") 'consult-isearch-history)
    (define-key map (kbd "M-s e") 'consult-isearch-history)
    (define-key map (kbd "M-s l") 'consult-line)))

(when (boundp 'minibuffer-local-map)
  (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer))

(autoload 'consult-customize "consult" "" nil 'macro)
(autoload 'consult--customize-set "consult")
(autoload 'consult--customize-put "consult")
(autoload 'embark-open-externally "embark")

(with-eval-after-load 'embark
  (require 'embark-consult))

(with-eval-after-load 'xref
  (when (boundp 'xref-show-xrefs-function)
    (setq xref-show-xrefs-function 'consult-xref)))

(with-eval-after-load 'consult
  (require 'embark-consult)
  (when (boundp 'consult-ripgrep-args)
    (setq consult-ripgrep-args
          (replace-regexp-in-string
           "^rg"
           "/run/current-system/profile/bin/rg"
           consult-ripgrep-args)))
  (consult-customize consult-buffer :preview-key "M-.")
  (consult-customize consult-history :category 'consult-history)
  (consult-customize consult-line :inherit-input-method t))

(with-eval-after-load 'marginalia
  (setq marginalia-align 'left))

(autoload 'marginalia-mode "marginalia")
(marginalia-mode 1)

(provide 'bv-completion)
;;; bv-completion.el ends here
