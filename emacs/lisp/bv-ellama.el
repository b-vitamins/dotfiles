;;; bv-ellama.el --- Ellama configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module configures Ellama, an LLM client for Emacs.
;; It sets up OpenAI GPT models with authentication and provides
;; multiple model configurations for different use cases.

;;; Code:


(autoload 'make-llm-openai "llm-openai")
(autoload 'auth-source-pick-first-password "auth-source")

(when (boundp 'ellama-keymap-prefix)
  (setopt ellama-keymap-prefix "C-c e"))

(with-eval-after-load 'ellama
  (require 'llm-openai)
  (when (boundp 'llm-warn-on-nonfree)
    (setopt llm-warn-on-nonfree nil))
  (when (boundp 'ellama-long-lines-length)
    (setopt ellama-long-lines-length 80))
  (when (boundp 'ellama-provider)
    (setopt ellama-provider
            (make-llm-openai
             :key (auth-source-pick-first-password :host "openai.com")
             :chat-model "gpt-4o-mini")))
  (when (boundp 'ellama-providers)
    (setopt ellama-providers
            (list (cons "mini"
                        (make-llm-openai
                         :key (auth-source-pick-first-password :host "openai.com")
                         :chat-model "gpt-4o-mini"))
                  (cons "full"
                        (make-llm-openai
                         :key (auth-source-pick-first-password :host "openai.com")
                         :chat-model "gpt-4o"))))))

(provide 'bv-ellama)
;;; bv-ellama.el ends here