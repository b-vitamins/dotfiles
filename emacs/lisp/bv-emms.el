;;; bv-emms.el --- Multimedia system configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Music playback with EMMS and MPV.

;;; Code:


(declare-function emms-all "emms-setup")
(declare-function emms-playlist-mode-go "emms-playlist-mode")
(declare-function emms-add-directory-tree "emms-source-file")
(declare-function emms-playlist-selected-track "emms")
(declare-function emms-track "emms")
(declare-function emms-track-set "emms")
(declare-function emms-playlist-insert-track "emms")

(defgroup bv-emms nil
  "Multimedia system settings."
  :group 'bv)

(defcustom bv-emms-idle-delay 2.0
  "Idle time before loading EMMS."
  :type 'number
  :group 'bv-emms)

(defcustom bv-emms-music-dir (expand-file-name "~/Music")
  "Default music directory."
  :type 'directory
  :group 'bv-emms)

;; Load EMMS after idle delay
(run-with-idle-timer bv-emms-idle-delay t
                     (lambda ()
                       (require 'emms nil t)))

(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-info-libtag)
  (require 'emms-player-mpv)
  (emms-all)

  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-libtag)
        emms-source-file-default-directory bv-emms-music-dir
        emms-playlist-buffer-name "*Music*"
        emms-playlist-mode-center-when-go t
        emms-history-file (expand-file-name "emacs/emms-history" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
        emms-repeat-playlist t
        emms-mode-line-format " %s"
        emms-mode-line-icon-enabled-p nil)

  (add-to-list 'emms-player-mpv-parameters "--no-video")
  (add-to-list 'emms-player-mpv-parameters "--ytdl-format=bestaudio"))

(defun bv-emms-play-directory ()
  "Play all music files in a directory."
  (interactive)
  (let ((dir (read-directory-name "Music directory: " bv-emms-music-dir)))
    (emms-stop)
    (emms-playlist-clear)
    (emms-add-directory-tree dir)
    (emms-playlist-mode-first)
    (emms-playlist-mode-play-smart)))

(defun bv-emms-play-url ()
  "Play music from URL."
  (interactive)
  (let ((url (read-string "URL: ")))
    (emms-play-url url)))

(defun bv-emms-show-current ()
  "Show current track info."
  (interactive)
  (if emms-player-playing-p
      (message "Now playing: %s"
               (emms-track-description
                (emms-playlist-current-selected-track)))
    (message "No track playing")))

(defun bv-emms-toggle ()
  "Toggle between play and pause."
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

;; Keybindings are provided through the transient menu

(with-eval-after-load 'emms-playlist-mode
  (define-key emms-playlist-mode-map "q" 'quit-window)
  (define-key emms-playlist-mode-map "d" 'emms-playlist-mode-kill-track)
  (define-key emms-playlist-mode-map "c" 'emms-playlist-clear))

(defun bv-emms-transient ()
  "Transient menu for EMMS."
  (interactive)
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
  (bv-emms-transient-menu))

(global-set-key (kbd "C-c m") 'bv-emms-transient)

(provide 'bv-emms)
;;; bv-emms.el ends here