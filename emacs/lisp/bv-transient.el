;;; bv-transient.el --- Centralized transient menu definitions -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Centralized transient menu definitions for all modules.
;; This file collects all transient menus to avoid loading order issues
;; and provide a consistent interface.

;;; Code:

(require 'transient)

;; Declare functions from various modules
(declare-function bv-gptel-chat "bv-gptel")
(declare-function bv-gptel-send-region "bv-gptel")
(declare-function bv-gptel-rewrite "bv-gptel")
(declare-function gptel-quick "gptel")
(declare-function gptel-menu "gptel")

(declare-function guix-packages-by-name "guix")
(declare-function guix-installed-packages "guix")
(declare-function guix-installed-user-packages "guix")
(declare-function guix-generations "guix")
(declare-function guix-services-from-system-config "guix")
(declare-function bv-guix-build-package "bv-guix")
(declare-function bv-guix-lint-package "bv-guix")
(declare-function bv-guix-format-buffer "bv-guix")

(declare-function geiser-eval-definition "geiser-mode")
(declare-function geiser-eval-region "geiser-mode")
(declare-function geiser-repl "geiser-repl")
(declare-function geiser-repl-clear-buffer "geiser-repl")
(declare-function geiser-doc-symbol-at-point "geiser-doc")
(declare-function geiser-doc-module "geiser-doc")
(declare-function bv-geiser-eval-buffer "bv-geiser")
(declare-function bv-geiser-eval-last-sexp "bv-geiser")
(declare-function bv-geiser-repl-here "bv-geiser")

(declare-function bv-emms-toggle "bv-emms")
(declare-function emms-next "emms")
(declare-function emms-previous "emms")
(declare-function emms-stop "emms")
(declare-function emms-playlist-mode-go "emms-playlist-mode")
(declare-function bv-emms-play-directory "bv-emms")
(declare-function bv-emms-play-url "bv-emms")
(declare-function bv-emms-show-current "bv-emms")
(declare-function emms-toggle-repeat-playlist "emms")
(declare-function emms-toggle-random-playlist "emms")

(declare-function ellama-chat "ellama")
(declare-function bv-ellama-chat-local "bv-ellama")
(declare-function ellama-ask-about "ellama")
(declare-function ellama-code-review "ellama")
(declare-function ellama-code-improve "ellama")
(declare-function ellama-code-explain "ellama")
(declare-function bv-ellama-switch-provider "bv-ellama")

(declare-function ebdb-display-all-records "ebdb")
(declare-function bv-ebdb-search-name "bv-ebdb")
(declare-function bv-ebdb-search-email "bv-ebdb")
(declare-function ebdb-create-record-extended "ebdb")
(declare-function bv-ebdb-add-from-mail "bv-ebdb")

(declare-function bv-ednc-show-notification-log "bv-ednc")
(declare-function bv-ednc-dismiss-last "bv-ednc")
(declare-function bv-ednc-dismiss-all "bv-ednc")
(declare-function bv-ednc-open-app "bv-ednc")

(declare-function bv-power-lock "bv-power-menu")
(declare-function bv-power-suspend "bv-power-menu")
(declare-function async-start-process "async")

(declare-function bv-audio-increase-volume "bv-pulseaudio-control")
(declare-function bv-audio-decrease-volume "bv-pulseaudio-control")
(declare-function bv-audio-toggle-mute "bv-pulseaudio-control")
(declare-function pulseaudio-control-select-sink-by-name "pulseaudio-control")
(declare-function pulseaudio-control-select-source-by-name "pulseaudio-control")

(declare-function bv-webpaste-paste-dwim "bv-webpaste")
(declare-function webpaste-paste-buffer "webpaste")
(declare-function webpaste-paste-region "webpaste")
(declare-function bv-webpaste-paste-defun "bv-webpaste")

(declare-function bv-ytdl-download-url "bv-yt-dlp")
(declare-function bv-ytdl-download-audio "bv-yt-dlp")
(declare-function bv-ytdl-download-video "bv-yt-dlp")
(declare-function ytdl-show-list "ytdl")

;; Variables
(defvar bv-power-loginctl-path)

(defgroup bv-transient nil
  "Centralized transient menu settings."
  :group 'bv)

;;; GPTel transient menu
(transient-define-prefix bv-gptel-transient-menu ()
  "AI Assistant"
  ["Chat"
   ("c" "New chat" bv-gptel-chat)
   ("s" "Send region" bv-gptel-send-region)
   ("r" "Rewrite" bv-gptel-rewrite)
   ("q" "Quick query" gptel-quick)]
  ["Settings"
   ("m" "Menu" gptel-menu)])

;;; Guix transient menu
(transient-define-prefix bv-guix-transient-menu ()
  "GNU Guix"
  [["Packages"
    ("p" "Search packages" guix-packages-by-name)
    ("i" "Installed packages" guix-installed-packages)
    ("u" "Upgradable packages" guix-installed-user-packages)]
   ["Development"
    ("b" "Build package" bv-guix-build-package)
    ("l" "Lint package" bv-guix-lint-package)
    ("f" "Format buffer" bv-guix-format-buffer)]
   ["System"
    ("g" "Generations" guix-generations)
    ("s" "Services" guix-services-from-system-config)]])

;;; Geiser transient menu
(transient-define-prefix bv-geiser-transient-menu ()
  "Scheme Development"
  ["Evaluation"
   ("e" "Eval definition" geiser-eval-definition)
   ("r" "Eval region" geiser-eval-region)
   ("b" "Eval buffer" bv-geiser-eval-buffer)
   ("l" "Eval last sexp" bv-geiser-eval-last-sexp)]
  ["REPL"
   ("z" "Switch to REPL" geiser-repl)
   ("Z" "REPL here" bv-geiser-repl-here)
   ("c" "Clear REPL" geiser-repl-clear-buffer)]
  ["Documentation"
   ("d" "Symbol doc" geiser-doc-symbol-at-point)
   ("m" "Module doc" geiser-doc-module)])

;;; EMMS transient menu
(transient-define-prefix bv-emms-transient-menu ()
  "Music Player"
  ["Playback"
   ("p" "Play/Pause" bv-emms-toggle)
   ("n" "Next" emms-next)
   ("b" "Previous" emms-previous)
   ("s" "Stop" emms-stop)]
  ["Playlist"
   ("l" "Show playlist" emms-playlist-mode-go)
   ("d" "Play directory" bv-emms-play-directory)
   ("u" "Play URL" bv-emms-play-url)
   ("i" "Current track" bv-emms-show-current)]
  ["Options"
   ("r" "Toggle repeat" emms-toggle-repeat-playlist)
   ("R" "Toggle random" emms-toggle-random-playlist)])

;;; Ellama transient menu
(transient-define-prefix bv-ellama-transient-menu ()
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

;;; EBDB transient menu
;;;###autoload (autoload 'bv-ebdb-transient-menu "bv-transient" nil t)
(transient-define-prefix bv-ebdb-transient-menu ()
  "EBDB Contact Database"
  ["Display"
   ("a" "All contacts" ebdb-display-all-records)
   ("s" "Search by name" bv-ebdb-search-name)
   ("e" "Search by email" bv-ebdb-search-email)]
  ["Edit"
   ("c" "Create contact" ebdb-create-record-extended)
   ("m" "Add from mail" bv-ebdb-add-from-mail)])

;;; EDNC transient menu
(transient-define-prefix bv-ednc-transient-menu ()
  "Desktop Notifications"
  ["Actions"
   ("l" "Show log" bv-ednc-show-notification-log)
   ("d" "Dismiss last" bv-ednc-dismiss-last)
   ("D" "Dismiss all" bv-ednc-dismiss-all)
   ("o" "Open app" bv-ednc-open-app)])

;;; Power menu transient
(transient-define-prefix bv-power-transient-menu ()
  "Power Management"
  ["Session"
   ("l" "Lock" bv-power-lock)
   ("o" "Logout" (lambda () (interactive)
                   (when (yes-or-no-p "Logout? ")
                     (async-start-process "logout" bv-power-loginctl-path nil
                                          "terminate-session" (getenv "XDG_SESSION_ID")))))]
  ["System"
   ("s" "Suspend" bv-power-suspend)
   ("h" "Hibernate" (lambda () (interactive)
                      (when (yes-or-no-p "Hibernate? ")
                        (async-start-process "hibernate" bv-power-loginctl-path nil "hibernate"))))
   ("S" "Shutdown" (lambda () (interactive)
                     (when (yes-or-no-p "Shutdown system? ")
                       (async-start-process "shutdown" bv-power-loginctl-path nil "poweroff"))))
   ("r" "Reboot" (lambda () (interactive)
                   (when (yes-or-no-p "Reboot system? ")
                     (async-start-process "reboot" bv-power-loginctl-path nil "reboot"))))])

;;; Audio control transient
(transient-define-prefix bv-audio-transient-menu ()
  "Audio Control"
  ["Volume"
   ("+" "Increase" bv-audio-increase-volume :transient t)
   ("-" "Decrease" bv-audio-decrease-volume :transient t)
   ("m" "Toggle mute" bv-audio-toggle-mute)]
  ["Devices"
   ("s" "Select sink" pulseaudio-control-select-sink-by-name)
   ("i" "Select source" pulseaudio-control-select-source-by-name)])

;;; Webpaste transient menu
(transient-define-prefix bv-webpaste-transient-menu ()
  "Web Paste"
  ["Paste"
   ("p" "Buffer/region" bv-webpaste-paste-dwim)
   ("b" "Buffer" webpaste-paste-buffer)
   ("r" "Region" webpaste-paste-region)
   ("d" "Defun" bv-webpaste-paste-defun)])

;;; Ytdl transient menu
(transient-define-prefix bv-ytdl-transient-menu ()
  "Media Downloader"
  ["Download"
   ("u" "From URL" bv-ytdl-download-url)
   ("a" "Audio only" bv-ytdl-download-audio)
   ("v" "Video" bv-ytdl-download-video)]
  ["Manage"
   ("l" "Show downloads" ytdl-show-list)])

;;; Wrapper functions for lazy loading
(defun bv-gptel-transient ()
  "Transient menu for GPTel."
  (interactive)
  (require 'bv-gptel nil t)
  (bv-gptel-transient-menu))

(defun bv-guix-transient ()
  "Transient menu for Guix."
  (interactive)
  (require 'bv-guix nil t)
  (bv-guix-transient-menu))

(defun bv-geiser-transient ()
  "Transient menu for Geiser."
  (interactive)
  (require 'bv-geiser nil t)
  (bv-geiser-transient-menu))

(defun bv-emms-transient ()
  "Transient menu for EMMS."
  (interactive)
  (require 'bv-emms nil t)
  (bv-emms-transient-menu))

(defun bv-ellama-transient ()
  "Transient menu for Ellama."
  (interactive)
  (require 'bv-ellama nil t)
  (bv-ellama-transient-menu))

(defun bv-ednc-transient ()
  "Transient menu for EDNC."
  (interactive)
  (require 'bv-ednc nil t)
  (bv-ednc-transient-menu))

(defun bv-power-transient ()
  "Transient menu for power management."
  (interactive)
  (require 'bv-power-menu nil t)
  (bv-power-transient-menu))

(defun bv-audio-transient ()
  "Transient menu for audio control."
  (interactive)
  (require 'bv-pulseaudio-control nil t)
  (bv-audio-transient-menu))

(defun bv-webpaste-transient ()
  "Transient menu for webpaste."
  (interactive)
  (require 'bv-webpaste nil t)
  (bv-webpaste-transient-menu))

(defun bv-ytdl-transient ()
  "Transient menu for ytdl."
  (interactive)
  (require 'bv-yt-dlp nil t)
  (bv-ytdl-transient-menu))

;;; Keybindings
(global-set-key (kbd "C-c g") 'bv-gptel-transient)
(global-set-key (kbd "C-c G") 'bv-guix-transient)
(global-set-key (kbd "C-c S") 'bv-geiser-transient)
(global-set-key (kbd "C-c m") 'bv-emms-transient)
(global-set-key (kbd "C-c e") 'bv-ellama-transient)
(global-set-key (kbd "C-c b") 'bv-ebdb-transient-menu)
(global-set-key (kbd "C-c N") 'bv-ednc-transient)
(global-set-key (kbd "C-x q") 'bv-power-transient)
(global-set-key (kbd "C-c v") 'bv-audio-transient)
(global-set-key (kbd "C-c P") 'bv-webpaste-transient)
(global-set-key (kbd "C-c y") 'bv-ytdl-transient)

(provide 'bv-transient)
;;; bv-transient.el ends here