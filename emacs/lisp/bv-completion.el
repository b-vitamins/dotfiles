;;; bv-completion.el --- Completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Completion with orderless, consult, embark, and marginalia.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'marginalia)
  (require 'consult))

;; Better history sorting - recent items first
(defun bv-completion-sort-by-history (candidates)
  "Sort CANDIDATES by history, putting recent items first."
  (let ((hist (and (minibufferp)
                   (symbol-value minibuffer-history-variable))))
    (if hist
        (sort candidates
              (lambda (a b)
                (let ((a-pos (seq-position hist a))
                      (b-pos (seq-position hist b)))
                  (cond ((and a-pos b-pos) (< a-pos b-pos))
                        (a-pos t)
                        (b-pos nil)
                        (t (string< a b))))))
      candidates)))

(defvar bv-completion-initial-narrow-alist '()
  "Mode to key mapping for automatic consult narrowing.")

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

  ;; Enable file path abbreviation (~/... instead of /home/user/...)
  (when (boundp 'file-name-shadow-mode)
    (file-name-shadow-mode 1))
  (when (boundp 'rfn-eshadow-update-overlay)
    (setq file-name-shadow-properties '(face file-name-shadow field shadow))
    (setq file-name-shadow-tty-properties '(before-string "{" after-string "}")))

  ;; Abbreviate file paths in completions
  (add-hook 'rfn-eshadow-setup-minibuffer-hook
            (lambda ()
              (when (eq this-command 'find-file)
                (setq-local minibuffer-default-add-function
                            (lambda ()
                              (let ((def (minibuffer-default-add-completions)))
                                (if (listp def)
                                    (mapcar 'abbreviate-file-name def)
                                  (abbreviate-file-name def))))))))

  (when (boundp 'minibuffer-local-completion-map)
    (let ((map minibuffer-local-completion-map))
      (define-key map (kbd "SPC") nil)
      (define-key map (kbd "?") nil)))

  (when (boundp 'orderless-component-separator)
    (setq orderless-component-separator 'orderless-escapable-split-on-space))

  (defun bv-orderless-literal-dispatcher (pattern _index _total)
    "Match PATTERN literally when it ends with '='."
    (cond ((equal "=" pattern) '(orderless-literal . "="))
          ((string-suffix-p "=" pattern)
           (cons 'orderless-literal (substring pattern 0 -1)))))

  (defun bv-orderless-without-literal-dispatcher (pattern _index _total)
    "Exclude matches for PATTERN when it ends with '!'."
    (cond ((equal "!" pattern) '(orderless-literal . "!"))
          ((string-suffix-p "!" pattern)
           (cons 'orderless-without-literal (substring pattern 0 -1)))))

  (defun bv-orderless-initialism-dispatcher (pattern _index _total)
    "Match PATTERN as initialism when it ends with ','."
    (cond ((equal "," pattern) '(orderless-literal . ","))
          ((string-suffix-p "," pattern)
           (cons 'orderless-initialism (substring pattern 0 -1)))))

  (defun bv-orderless-flex-dispatcher (pattern _index _total)
    "Match PATTERN flexibly when it ends with '~'."
    (cond ((equal "~" pattern) '(orderless-literal . "~"))
          ((string-suffix-p "~" pattern)
           (cons 'orderless-flex (substring pattern 0 -1)))))

  (defun bv-completion--mode-buffers (&rest modes)
    "Return buffers matching MODES."
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer (cl-some 'derived-mode-p modes)))
     (buffer-list)))

  (defun bv-completion-initial-narrow ()
    "Auto-narrow consult-buffer by major mode."
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
          '((file (styles orderless partial-completion basic))
            (project-file (styles orderless partial-completion basic))
            (buffer (styles orderless flex))
            (consult-location (styles orderless))
            (consult-multi (styles orderless))
            (command (styles orderless flex))
            (variable (styles orderless flex))
            (symbol (styles orderless flex)))))
  (when (boundp 'completion-category-defaults)
    (setq completion-category-defaults nil))

  ;; More flexible space matching in orderless
  (when (boundp 'orderless-matching-styles)
    (setq orderless-matching-styles
          '(orderless-literal
            orderless-regexp
            orderless-initialism
            orderless-prefixes
            orderless-flex)))
  (setq enable-recursive-minibuffers t))

(when (boundp 'history-length)
  (setq history-length 10000))
(when (boundp 'savehist-file)
  (setq savehist-file
        (expand-file-name "history" user-emacs-directory)))

(global-set-key (kbd "s-.") 'embark-act)
(global-set-key (kbd "s->") 'embark-become)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "s-B") 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-r") 'consult-line)

(when (boundp 'minibuffer-local-map)
  (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
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
  "Jump to line with relative numbering."
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

;; If you still want to use isearch occasionally, you can access it via M-s prefix
(global-set-key (kbd "M-s i") 'isearch-forward)
(global-set-key (kbd "M-s I") 'isearch-backward)

(when (boundp 'isearch-mode-map)
  (let ((map isearch-mode-map))
    (define-key map (kbd "M-e") 'consult-isearch-history)
    (define-key map (kbd "M-s e") 'consult-isearch-history)
    (define-key map (kbd "M-s l") 'consult-line)))

(when (boundp 'minibuffer-local-map)
  (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer))


(with-eval-after-load 'embark
  (require 'embark-consult))

(with-eval-after-load 'xref
  (when (boundp 'xref-show-xrefs-function)
    (setq xref-show-xrefs-function 'consult-xref)))

(with-eval-after-load 'consult
  (require 'embark-consult)
  (consult-customize
   consult-buffer :preview-key "M-."
   consult-history :category 'consult-history
   consult-line :inherit-input-method t :require-match t
   consult-ripgrep :preview-key "M-."
   consult-grep :preview-key "M-."
   consult-find :preview-key "M-.")

  ;; Better async search performance
  (when (boundp 'consult-async-min-input)
    (setq consult-async-min-input 2))
  (when (boundp 'consult-async-refresh-delay)
    (setq consult-async-refresh-delay 0.2))
  (when (boundp 'consult-async-input-throttle)
    (setq consult-async-input-throttle 0.3))

  ;; Show directories first in file completion
  (when (boundp 'consult-find-args)
    (setq consult-find-args "find . -type f -o -type d")))

(when (boundp 'marginalia-align)
  (setq marginalia-align 'right))

(marginalia-mode 1)
(savehist-mode 1)

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "h") 'consult-history)
    (define-key bv-app-map (kbd "m") 'consult-mode-command)
    (define-key bv-app-map (kbd "k") 'consult-kmacro)))

(provide 'bv-completion)
;;; bv-completion.el ends here
