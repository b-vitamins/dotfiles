;;; bv-gptel.el --- GPT integration configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; AI assistance with GPTel.

;;; Code:


(declare-function gptel "gptel")
(declare-function gptel-send "gptel")
(declare-function gptel-quick "gptel")
(declare-function gptel-menu "gptel")

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

(defun bv-gptel-transient ()
  "Transient menu for GPTel."
  (interactive)
  (transient-define-prefix bv-gptel-transient-menu ()
    "AI Assistant"
    ["Chat"
     ("c" "New chat" bv-gptel-chat)
     ("s" "Send region" bv-gptel-send-region)
     ("r" "Rewrite" bv-gptel-rewrite)
     ("q" "Quick query" gptel-quick)]
    ["Settings"
     ("m" "Menu" gptel-menu)])
  (bv-gptel-transient-menu))

(global-set-key (kbd "C-c g") 'bv-gptel-transient)

(provide 'bv-gptel)
;;; bv-gptel.el ends here