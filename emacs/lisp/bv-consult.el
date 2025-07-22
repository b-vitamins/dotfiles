;;; bv-consult.el --- Consult configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for consult - practical commands based on Emacs completion.

;;; Code:

(require 'consult)

;; Initial narrow configuration
(defvar bv-completion-initial-narrow-alist '()
  "Mode to key mapping for automatic consult narrowing.")

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

(add-hook 'minibuffer-setup-hook 'bv-completion-initial-narrow)

;; Key bindings
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "s-B") 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-r") 'consult-line)

(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "M-r") 'consult-history))

(with-eval-after-load 'goto-map
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

(with-eval-after-load 'narrow-map
  (define-key narrow-map (kbd "g") 'bv-goto-line-relative))

(with-eval-after-load 'search-map
  (let ((map search-map))
    (define-key map (kbd "f") 'consult-find)
    (define-key map (kbd "g") 'consult-ripgrep)
    (define-key map (kbd "e") 'consult-isearch-history)
    (define-key map (kbd "l") 'consult-line)))

;; If you still want to use isearch occasionally, you can access it via M-s prefix
(global-set-key (kbd "M-s i") 'isearch-forward)
(global-set-key (kbd "M-s I") 'isearch-backward)

(with-eval-after-load 'isearch
  (let ((map isearch-mode-map))
    (define-key map (kbd "M-e") 'consult-isearch-history)
    (define-key map (kbd "M-s e") 'consult-isearch-history)
    (define-key map (kbd "M-s l") 'consult-line)))

;; Xref integration
(with-eval-after-load 'xref
  (setq xref-show-xrefs-function 'consult-xref))

;; Consult customizations
(consult-customize
 consult-buffer :preview-key "M-."
 consult-history :category 'consult-history
 consult-line :inherit-input-method t :require-match t
 consult-ripgrep :preview-key "M-."
 consult-grep :preview-key "M-."
 consult-find :preview-key "M-.")

;; Better async search performance
(setq consult-async-min-input 2)
(setq consult-async-refresh-delay 0.2)
(setq consult-async-input-throttle 0.3)

;; Show directories first in file completion
(setq consult-find-args "find . -type f -o -type d")

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "h") 'consult-history)
    (define-key bv-app-map (kbd "m") 'consult-mode-command)
    (define-key bv-app-map (kbd "k") 'consult-kmacro)))

(provide 'bv-consult)
;;; bv-consult.el ends here