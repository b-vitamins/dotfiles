;;; bv-eat.el --- Emulate A Terminal configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Terminal emulation with eat.

;;; Code:

(require 'eat nil t)

(defvar bv-terminal-map)
(declare-function term-sentinel "term" (proc msg))

(when (featurep 'eat)
  (when (boundp 'eat-line-input-ring-size)
    (setq eat-line-input-ring-size 4096))
  (when (boundp 'eat-kill-buffer-on-exit)
    (setq eat-kill-buffer-on-exit t))
  (when (boundp 'eat-term-scrollback-size)
    (setq eat-term-scrollback-size nil))
  (when (boundp 'eat-enable-mouse)
    (setq eat-enable-mouse t)))

(defun bv-eat ()
  "Open eat terminal."
  (interactive)
  (if (featurep 'eat)
      (eat)
    (ansi-term (or (getenv "SHELL") "/bin/bash"))))

(defun bv-eat-project ()
  "Open eat terminal in project root."
  (interactive)
  (let ((default-directory (or (and (fboundp 'project-root)
                                   (project-root (project-current)))
                              default-directory)))
    (bv-eat)))

(with-eval-after-load 'bv-bindings
  (when (and (boundp 'bv-app-map) (boundp 'bv-terminal-map))
    (define-key bv-app-map (kbd "e") 'bv-eat)
    (define-key bv-terminal-map (kbd "p") 'bv-eat-project)))

(global-set-key (kbd "s-t") 'bv-eat)

;;; ansi-term fallback

(defun bv-eat--term-sentinel (orig-fun proc msg)
  "Kill ansi-term buffers when their process exits.
ORIG-FUN is the original sentinel, PROC is the terminal process, and MSG is
the process message."
  (funcall orig-fun proc msg)
  (when (memq (process-status proc) '(signal exit))
    (when-let ((buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(with-eval-after-load 'term
  (advice-add #'term-sentinel :around #'bv-eat--term-sentinel))

(provide 'bv-eat)
;;; bv-eat.el ends here
