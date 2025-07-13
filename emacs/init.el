;;; init.el --- Main initialization file -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Main initialization following nano-emacs architecture.
;; Modules are loaded in specific order to ensure proper setup.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "lisp" (file-name-directory load-file-name)))

(require 'bv-layout)

(require 'bv-theme)
(require 'bv-theme-dark)
(require 'bv-theme-light)

;; Set initial theme (light as fallback)
(bv-theme-set-light)
(bv-refresh-theme)

;; Setup automatic theme switching based on time
(require 'bv-circadian)
(bv-circadian-setup)

(require 'bv-defaults)

;; Load time and battery before modeline so they're available
(require 'bv-time)         ; Time display
(require 'bv-battery)      ; Battery status

(require 'bv-modeline)
(bv-modeline-faces)
(bv-modeline)

(require 'bv-bindings)

(let ((inhibit-message t))
  (message "Welcome to Emacs")
  (message (format "Initialization time: %s" (emacs-init-time))))


;; Optional modules (uncomment as needed)
;; Each module is independent and can be loaded in any order

;; UI enhancements
(require 'bv-which-key)    ; Key binding hints
(require 'bv-keycast)      ; Show keys being pressed
(require 'bv-all-the-icons); Icon support
(require 'bv-dashboard)    ; Startup dashboard

;; Basic functionality
(require 'bv-calendar)     ; Calendar enhancements
(require 'bv-window)       ; Window management

;; File and buffer management
(require 'bv-dired)        ; Directory editor
(require 'bv-tramp)        ; Remote file access
(require 'bv-project)      ; Project management
(require 'bv-perspective)  ; Workspace management
(require 'bv-monocle)      ; Focus mode

;; Completion frameworks
(require 'bv-completion)   ; Base completion setup
(require 'bv-vertico)      ; Vertical completion
(require 'bv-corfu)        ; In-buffer completion
(require 'bv-tempel)       ; Template expansion

;; Shell and terminal
(require 'bv-eat)          ; Terminal emulator
(require 'bv-eshell)       ; Emacs shell
(require 'bv-shell)        ; Shell mode
(require 'bv-comint)       ; Command interpreter

;; Development tools
(require 'bv-elisp)        ; Emacs Lisp
(require 'bv-eglot)        ; LSP client
(require 'bv-dape)         ; Debug adapter
(require 'bv-flymake)      ; On-the-fly checking
(require 'bv-xref)         ; Cross references
(require 'bv-smartparens)  ; Structural editing

;; Version control
(require 'bv-git)          ; Git integration

;; Org mode and related
(require 'bv-org)          ; Org mode setup
(require 'bv-org-agenda)   ; Agenda configuration
(require 'bv-org-dailies)  ; Daily notes
(require 'bv-org-ql)       ; Org query language
(require 'bv-org-agenda-files-track) ; Agenda file tracking
(require 'bv-org-roam)     ; Roam notes
(require 'bv-org-recur)    ; Recurring tasks
(require 'bv-citation)     ; Citation management

;; Documentation and help
(require 'bv-help)         ; Help enhancements
(require 'bv-info)         ; Info reader
(require 'bv-devdocs)      ; Developer documentation

;; External tools
(require 'bv-calc)         ; Calculator
(require 'bv-re-builder)   ; Regex builder
(require 'bv-browse-url)   ; URL browsing
(require 'bv-pdf-tools)    ; PDF viewing
(require 'bv-nov-el)       ; EPUB reader
(require 'bv-graphviz)     ; Graph visualization
(require 'bv-spelling)     ; Spell checking

;; Communication and feeds
(require 'bv-elfeed)       ; RSS/Atom feeds
(require 'bv-ebdb)         ; Contact database
(require 'bv-ednc)         ; Desktop notifications
(require 'bv-webpaste)     ; Web pasting service

;; Media
(require 'bv-emms)         ; Multimedia system
(require 'bv-mpv)          ; MPV integration
(require 'bv-yt-dlp)       ; YouTube downloader
(require 'bv-pulseaudio-control) ; Audio control

;; AI assistants
(require 'bv-gptel)        ; GPT integration
(require 'bv-ellama)       ; LLM interface

;; Guix specific
(require 'bv-geiser)       ; Scheme/Guile
(require 'bv-guix)         ; Guix integration

;; Utilities
(require 'bv-power-menu)   ; Power management
(require 'bv-display-wttr) ; Weather display

;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
