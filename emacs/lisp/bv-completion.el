;;; bv-completion.el --- Core completion configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Core minibuffer and completion settings.
;; This file configures the base completion infrastructure that other
;; completion packages build upon.

;;; Code:

(require 'seq)

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

;; Core minibuffer configuration
(with-eval-after-load 'minibuffer
  ;; Tab completion behavior
  (setq tab-always-indent 'complete)

  ;; Minibuffer prompt properties
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

  ;; Completion display settings
  (setq completion-show-help nil)
  (setq completions-format 'one-column)
  (setq completions-header-format nil)

  ;; Navigation in minibuffer
  (let ((map minibuffer-mode-map))
    (define-key map (vector 'remap 'next-line) 'minibuffer-next-completion)
    (define-key map (vector 'remap 'previous-line) 'minibuffer-previous-completion))

  ;; Navigation in completion popup
  (let ((map completion-in-region-mode-map))
    (define-key map (kbd "C-n") 'minibuffer-next-completion)
    (define-key map (kbd "C-p") 'minibuffer-previous-completion))

  ;; File path handling
  (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

  ;; Enable file path abbreviation (~/... instead of /home/user/...)
  (file-name-shadow-mode 1)
  (setq file-name-shadow-properties '(face file-name-shadow field shadow))
  (setq file-name-shadow-tty-properties '(before-string "{" after-string "}"))

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

  ;; Disable some default bindings
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil))

  ;; Exit minibuffer with s-b
  (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; History configuration
(setq history-length 10000)

(provide 'bv-completion)
;;; bv-completion.el ends here