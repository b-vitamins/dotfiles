;;; init.el --- Load all bv-* configuration modules -*- lexical-binding: t -*-

;;; Commentary:
;; This init file loads all bv-* modules for immediate use.
;; Place this in ~/.config/emacs/init.el or ~/.emacs.d/init.el

;;; Code:

(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; Add the directory containing bv-*.el files to load-path
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

;; Core modules - load first
(require 'bv-keymaps)
(require 'bv-startup)
(require 'bv-emacs-base)

;; Appearance and UI
(require 'bv-fonts)
(require 'bv-appearance)
(require 'bv-modus-themes)
(require 'bv-circadian)
(require 'bv-which-key)
(require 'bv-keycast)
(require 'bv-all-the-icons)
(require 'bv-dashboard)

;; Basic functionality
(require 'bv-battery)
(require 'bv-time)
(require 'bv-calendar)
(require 'bv-window)

;; File and buffer management
(require 'bv-dired)
(require 'bv-tramp)
(require 'bv-project)
(require 'bv-perspective)
(require 'bv-monocle)

;; Completion frameworks
(require 'bv-completion)
(require 'bv-vertico)
(require 'bv-corfu)
(require 'bv-tempel)

;; Shell and terminal
(require 'bv-eat)
(require 'bv-eshell)
(require 'bv-shell)
(require 'bv-comint)

;; Development tools
(require 'bv-elisp)
(require 'bv-eglot)
(require 'bv-dape)
(require 'bv-flymake)
(require 'bv-xref)
(require 'bv-smartparens)

;; Version control
(require 'bv-git)

;; Org mode and related
(require 'bv-org)
(require 'bv-org-agenda)
(require 'bv-org-dailies)
(require 'bv-org-ql)
(require 'bv-org-agenda-files-track)
(require 'bv-org-roam)
(require 'bv-emacs-org-recur)
(require 'bv-citation)

;; Documentation and help
(require 'bv-help)
(require 'bv-info)
(require 'bv-devdocs)

;; External tools integration
(require 'bv-calc)
(require 'bv-re-builder)
(require 'bv-browse-url)
(require 'bv-pdf-tools)
(require 'bv-nov-el)
(require 'bv-graphviz)
(require 'bv-spelling)

;; Communication and feeds
(require 'bv-elfeed)
(require 'bv-ebdb)
(require 'bv-ednc)
(require 'bv-webpaste)

;; Media
(require 'bv-emms)
(require 'bv-mpv)
(require 'bv-yt-dlp)
(require 'bv-pulseaudio-control)

;; AI assistants
(require 'bv-gptel)
(require 'bv-ellama)

;; Guix specific
(require 'bv-geiser)
(require 'bv-guix)

;; Utilities
(require 'bv-power-menu)
(require 'bv-display-wttr)

;; Start server if not already running
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
