;;; bv-ellama.el --- LLM interface configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Local LLM integration with Ellama.

;;; Code:


(declare-function make-llm-openai "llm-openai")
(declare-function make-llm-ollama "llm-ollama")
(declare-function ellama-chat "ellama")
(declare-function ellama-ask-about "ellama")
(declare-function ellama-code-review "ellama")
(declare-function ellama-code-improve "ellama")
(declare-function ellama-code-explain "ellama")
(declare-function transient-define-prefix "transient")
(declare-function auth-source-pick-first-password "auth-source")
(declare-function bv-ellama-transient "bv-ellama")

(defvar ellama-keymap-prefix)
(defvar ellama-long-lines-length)
(defvar llm-warn-on-nonfree)
(defvar ellama-provider)
(defvar ellama-providers)

(defgroup bv-ellama nil
  "LLM interface settings."
  :group 'bv)

(defcustom bv-ellama-idle-delay 2.0
  "Idle time before loading ellama."
  :type 'number
  :group 'bv-ellama)

(defcustom bv-ellama-default-provider "local"
  "Default LLM provider."
  :type 'string
  :group 'bv-ellama)

;; Load ellama after idle delay
(run-with-idle-timer bv-ellama-idle-delay t
                     (lambda ()
                       (require 'ellama nil t)))

(setq ellama-keymap-prefix "C-c e"
      ellama-long-lines-length 80
      llm-warn-on-nonfree nil)

(with-eval-after-load 'ellama
  (require 'llm-ollama)
  (require 'llm-openai)

  (setq ellama-provider
        (if (string= bv-ellama-default-provider "local")
            (make-llm-ollama
             :host "localhost"
             :port 11434
             :chat-model "llama3.2")
          (make-llm-openai
           :key (auth-source-pick-first-password :host "api.openai.com")
           :chat-model "gpt-4o-mini"))

        ellama-providers
        `(("local" . ,(make-llm-ollama
                       :host "localhost"
                       :port 11434
                       :chat-model "llama3.2"))
          ("mini" . ,(make-llm-openai
                      :key (auth-source-pick-first-password :host "api.openai.com")
                      :chat-model "gpt-4o-mini"))
          ("full" . ,(make-llm-openai
                      :key (auth-source-pick-first-password :host "api.openai.com")
                      :chat-model "gpt-4o")))))

(defun bv-ellama-chat-local ()
  "Start chat with local LLM."
  (interactive)
  (let ((ellama-provider (alist-get "local" ellama-providers nil nil #'string=)))
    (ellama-chat)))

(defun bv-ellama-switch-provider ()
  "Switch LLM provider."
  (interactive)
  (let* ((providers (mapcar #'car ellama-providers))
         (choice (completing-read "Provider: " providers)))
    (setq ellama-provider (alist-get choice ellama-providers nil nil #'string=))
    (message "Switched to %s" choice)))

(with-eval-after-load 'transient
  (transient-define-prefix bv-ellama-transient ()
    "Local LLM"
    ["Chat"
     ("c" "Chat" ellama-chat)
     ("l" "Local chat" bv-ellama-chat-local)
     ("a" "Ask about" ellama-ask-about)]
    ["Code"
     ("r" "Review" ellama-code-review)
     ("i" "Improve" ellama-code-improve)
     ("e" "Explain" ellama-code-explain)]
    ["Settings"
     ("s" "Switch provider" bv-ellama-switch-provider)])

  (global-set-key (kbd "C-c e") 'bv-ellama-transient))

(provide 'bv-ellama)
;;; bv-ellama.el ends here