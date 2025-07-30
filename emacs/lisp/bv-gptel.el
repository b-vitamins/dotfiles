;;; bv-gptel.el --- GPT integration configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; AI assistance with GPTel.

;;; Code:

(require 'auth-source)

(declare-function gptel "gptel")
(declare-function gptel-send "gptel")
(declare-function gptel-quick "gptel")
(declare-function gptel-menu "gptel")
(declare-function gptel-abort "gptel")
(declare-function auth-source-pick-first-password "auth-source")

(defvar gptel-default-mode)
(defvar gptel-use-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-model)
(defvar gptel-api-key)
(defvar gptel-mode-map)
(defvar embark-general-map)

(defgroup bv-gptel nil
  "GPT integration settings."
  :group 'bv)

(defcustom bv-gptel-idle-delay 2.0
  "Idle time before loading gptel."
  :type 'number
  :group 'bv-gptel)

(defcustom bv-gptel-default-model "gpt-4"
  "Default GPT model."
  :type 'string
  :group 'bv-gptel)

;; Load gptel after idle delay
(run-with-idle-timer bv-gptel-idle-delay t
                     (lambda ()
                       (require 'gptel nil t)))

(setq gptel-default-mode 'org-mode
      gptel-use-tools t
      gptel-confirm-tool-calls t
      gptel-model bv-gptel-default-model
      gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))

(defun bv-gptel-chat ()
  "Start a new GPT chat."
  (interactive)
  (gptel (generate-new-buffer "*GPT Chat*")))

(defun bv-gptel-send-region ()
  "Send region to GPT."
  (interactive)
  (if (use-region-p)
      (gptel-send)
    (message "No region selected")))

(defun bv-gptel-rewrite ()
  "Rewrite text at point."
  (interactive)
  (gptel-send t))

(with-eval-after-load 'gptel
  (define-key gptel-mode-map (kbd "C-c RET") 'gptel-send)
  (define-key gptel-mode-map (kbd "C-c C-c") 'gptel-send)
  (define-key gptel-mode-map (kbd "C-c C-k") 'gptel-abort))

(with-eval-after-load 'embark
  (define-key embark-general-map "?" 'gptel-quick))


(provide 'bv-gptel)
;;; bv-gptel.el ends here