;;; bv-emms.el --- EMMS multimedia configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This module configures EMMS (Emacs Multimedia System) for audio playback.
;; It integrates with MPV player, provides keybindings for media control,
;; and includes utilities for managing playlists and downloading tracks.

;;; Code:

(eval-when-compile (require 'emms))


(autoload 'mpv-live-p "mpv")
(autoload 'mpv--tq-filter "mpv")
(autoload 'xdg-cache-home "xdg")
(autoload 'tq-process "tq")
(autoload 'tq-close "tq")
(autoload 'tq-create "tq")
(autoload 'emms-playlist-mode-go "emms-playlist-mode")


(defvar bv-emms-map nil
  "Keymap for EMMS commands.")
(define-prefix-command 'bv-emms-map)

(autoload 'with-current-emms-playlist "emms" nil t 'macro)
(autoload 'define-emms-source "emms-source-file" nil t 'macro)

(defun bv-emms-source-playlist ()
  "Open EMMS playlist or create one from default directory."
  (interactive)
  (require 'emms)
  (require 'emms-playlist-mode)
  (ignore-errors
    (if (not (and (boundp 'emms-playlist-buffer) emms-playlist-buffer))
        (progn (when (and (boundp 'emms-source-file-default-directory)
                          (fboundp 'emms-add-directory-tree))
                 (emms-add-directory-tree emms-source-file-default-directory))
               (when (fboundp 'emms-playlist-mode-go)
                 (emms-playlist-mode-go)))
      (with-current-emms-playlist
        (unless (and (fboundp 'emms-playlist-selected-track)
                     (emms-playlist-selected-track))
          (when (and (boundp 'emms-source-file-default-directory)
                     (fboundp 'emms-add-directory-tree))
            (emms-add-directory-tree emms-source-file-default-directory)))
        (when (fboundp 'emms-playlist-mode-go)
          (emms-playlist-mode-go))))))

(defun bv-emms-source-track (url title &optional track-length play)
  "Add a track with URL and TITLE to EMMS playlist.
Optional TRACK-LENGTH specifies track duration.  If PLAY is non-nil,
start playing the track immediately."
  (interactive "sURL: \nsTitle: \nP")
  (require 'emms)
  (require 'emms-source-file)
  (if track-length
      (when (fboundp 'emms-source-bv-emms-track)
        (emms-source-bv-emms-track url title track-length))
    (when (fboundp 'emms-source-bv-emms-track)
      (emms-source-bv-emms-track url title nil)))
  (when play
    (when (fboundp 'emms-stop) (emms-stop))
    (when (fboundp 'emms-playlist-current-select-last) (emms-playlist-current-select-last))
    (when (fboundp 'emms-start) (emms-start))))

(defun bv-emms-toggle-random-repeat ()
  "Toggle between random playlist and sequential track repeat modes."
  (interactive)
  (require 'emms)
  (when (fboundp 'emms-toggle-random-playlist)
    (emms-toggle-random-playlist))
  (if (and (boundp 'emms-repeat-track) emms-repeat-track
           (boundp 'emms-random-playlist) emms-random-playlist)
      (progn (when (boundp 'emms-repeat-track)
               (setq emms-repeat-track nil))
             (message "Will play tracks randomly and repeat the track"))
    (when (boundp 'emms-repeat-track)
      (setq emms-repeat-track t))
    (message "Will play tracks sequentially and repeat the track")))

(defun bv-emms-seek-to-beginning ()
  "Seek to the beginning of current track."
  (interactive)
  (require 'emms)
  (emms-seek-to 0))

(defun bv-emms-next ()
  "Play next track, respecting random playlist setting."
  (interactive)
  (require 'emms)
  (if emms-random-playlist (emms-random) (emms-next)))

(defun bv-emms-previous ()
  "Play previous track, respecting random playlist setting."
  (interactive)
  (require 'emms)
  (if (and (boundp 'emms-random-playlist) emms-random-playlist)
      (when (fboundp 'emms-random) (emms-random))
    (when (fboundp 'emms-previous) (emms-previous))))

(define-emms-source bv-emms-track (url title &optional track-length)
  (let ((emms-track (when (fboundp 'emms-track) (emms-track 'url url))))
    (when (and emms-track (fboundp 'emms-track-set))
      (emms-track-set emms-track 'info-title title)
      (when track-length 
        (emms-track-set emms-track 'info-playing-time track-length)))
    (when (and emms-track (fboundp 'emms-playlist-insert-track))
      (emms-playlist-insert-track emms-track))))

(eval-when-compile (require 'ytdl))

(with-eval-after-load 'emms-playlist-mode
  (defun bv-emms-download-track ()
    "Download the current track from playlist using ytdl."
    (interactive)
    (when (fboundp 'emms-playlist-ensure-playlist-buffer)
      (emms-playlist-ensure-playlist-buffer))
    (with-current-emms-playlist
      (let* ((dl-type (when (fboundp 'ytdl--get-download-type) 
                        (ytdl--get-download-type)))
             (track (when (fboundp 'emms-playlist-track-at)
                      (emms-playlist-track-at)))
             (title (when (and track (fboundp 'emms-track-get))
                      (emms-track-get track 'info-title)))
             (source (when (and track (fboundp 'emms-track-get))
                       (emms-track-get track 'name))))
        (if (and track (fboundp 'emms-track-get) 
                 (equal (emms-track-get track 'type) 'url))
            (when (fboundp 'ytdl--download-async)
              (ytdl--download-async
               source
               (expand-file-name title (when (fboundp 'ytdl--eval-field)
                                         (ytdl--eval-field (nth 1 dl-type))))
               (when (fboundp 'ytdl--eval-list)
                 (ytdl--eval-list (when (fboundp 'ytdl--eval-field)
                                    (ytdl--eval-field (nth 2 dl-type)))))
               'ignore
               (car dl-type)))
          (error "Track `%s' is not a remote track to download" title)))))
  (when (boundp 'emms-playlist-mode-map)
    (define-key emms-playlist-mode-map "m" 'bv-emms-download-track)))

(with-eval-after-load 'dired
  (when (boundp 'dired-mode-map)
    (define-key dired-mode-map "e" 'emms-play-dired)))

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "E") 'bv-emms-map)))

(when (boundp 'bv-emms-map)
  (let ((map bv-emms-map))
  (define-key map "b" 'emms-smart-browse)
  (define-key map "h" 'emms-history-save)
  (define-key map "q" 'emms-stop)
  (define-key map "s" 'emms-toggle-random-playlist)
  (define-key map "t" 'emms-seek-to)
  (define-key map (kbd "SPC") 'emms-pause)
  (define-key map "r" 'emms-toggle-repeat-track)
  (define-key map "R" 'emms-toggle-repeat-playlist)
  (define-key map "d" 'bv-emms-toggle-random-repeat)
  (define-key map "l" 'bv-emms-source-playlist)
  (define-key map "n" 'bv-emms-next)
  (define-key map "p" 'bv-emms-previous)
  (define-key map "a" 'bv-emms-seek-to-beginning)))

(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'xdg)
  (require 'env)
  (require 'emms-info-libtag)
  (emms-all)
  
  (defun bv-emms-mpv-kill ()
    "Kill MPV process and clean up associated resources."
    (interactive)
    (require 'mpv)
    (require 'emms-player-mpv)
    (when (and (boundp 'mpv--process) (boundp 'emms-player-mpv-proc)
               (equal mpv--process emms-player-mpv-proc))
      (when (fboundp 'emms-stop) (emms-stop)))
    (when (and (boundp 'mpv--queue) mpv--queue)
      (when (fboundp 'tq-close) (tq-close mpv--queue)))
    (when (and (fboundp 'mpv-live-p) (mpv-live-p)
               (boundp 'mpv--process) (boundp 'emms-player-mpv-proc)
               (not (equal mpv--process emms-player-mpv-proc)))
      (kill-process mpv--process))
    (let ((timeout 0.5)
          (start-time (current-time)))
      (while (and (fboundp 'mpv-live-p) (mpv-live-p)
                  (boundp 'mpv--process) (boundp 'emms-player-mpv-proc)
                  (not (equal mpv--process emms-player-mpv-proc))
                  (< (float-time (time-subtract (current-time) start-time)) timeout))
        (sleep-for 0.05))
      (when (and (fboundp 'mpv-live-p) (mpv-live-p)
                 (boundp 'mpv--process) (boundp 'emms-player-mpv-proc)
                 (not (equal mpv--process emms-player-mpv-proc)))
        (error "Failed to kill mpv")))
    (when (boundp 'mpv--process)
      (setq mpv--process nil))
    (when (boundp 'mpv--queue)
      (setq mpv--queue nil))
    (run-hooks 'mpv-finished-hook))
  
  (defun bv-emms-connect-to-mpv-proc ()
    "Connect to EMMS MPV process for external control."
    (interactive)
    (require 'mpv)
    (when (boundp 'mpv-playing-time-string)
      (setq mpv-playing-time-string ""))
    (when (and (boundp 'mpv--process) (boundp 'emms-player-mpv-proc)
               (not (equal mpv--process emms-player-mpv-proc)))
      (when (fboundp 'mpv-kill) (mpv-kill)))
    (when (boundp 'mpv--process)
      (setq mpv--process (if (boundp 'emms-player-mpv-proc) emms-player-mpv-proc nil)))
    (when (boundp 'mpv--process)
      (set-process-query-on-exit-flag mpv--process nil))
    (when (boundp 'mpv--process)
      (set-process-sentinel
       mpv--process
       (lambda (p _e)
         (when (memq (process-status p) '(exit signal))
           (when (and (boundp 'mpv--process) (boundp 'emms-player-mpv-proc)
                      (not (equal mpv--process emms-player-mpv-proc)))
             (when (fboundp 'mpv-kill) (mpv-kill)))
           (run-hooks 'mpv-on-exit-hook)))))
    (unless (and (boundp 'mpv--queue) mpv--queue)
      (when (boundp 'mpv--queue)
        (setq mpv--queue
              (when (fboundp 'tq-create)
                (tq-create
                 (make-network-process
                  :name "emms-mpv-socket"
                  :family 'local
                  :service (if (boundp 'emms-player-mpv-ipc-socket)
                               emms-player-mpv-ipc-socket
                               nil)
                  :coding '(utf-8 . utf-8)
                  :noquery t
                  :filter 'emms-player-mpv-ipc-filter
                  :sentinel 'emms-player-mpv-ipc-sentinel)))))
      (when (and (boundp 'mpv--queue) mpv--queue)
        (set-process-filter
         (if (fboundp 'tq-process) (tq-process mpv--queue) nil)
         (lambda (_proc string)
           (ignore-errors (when (and (fboundp 'mpv--tq-filter)
                                     (boundp 'mpv--queue))
                            (mpv--tq-filter mpv--queue string)))))))
    (run-hooks 'mpv-on-start-hook)
    (run-hooks 'mpv-started-hook)
    t)
  
  (defun bv-emms-connect-to-mpv-on-startup (data)
    "Connect to MPV process when EMMS starts playing a file.
DATA contains event information from EMMS player."
    (when (string= (alist-get 'event data) "start-file")
      (bv-emms-connect-to-mpv-proc)))
  
  (advice-add 'mpv-kill :override 'bv-emms-mpv-kill)
  (add-hook 'emms-player-mpv-event-functions 'bv-emms-connect-to-mpv-on-startup)
  
  (require 'emms-player-mpv)
  (when (boundp 'emms-player-list)
    (setq emms-player-list '(emms-player-mpv)))
  (when (boundp 'emms-player-mpv-parameters)
    (add-to-list 'emms-player-mpv-parameters "--ytdl-format=best")
    (add-to-list 'emms-player-mpv-parameters "--force-window=no"))
  
  (with-eval-after-load 'emms-tag-editor
    (when (boundp 'emms-tag-editor-tagfile-functions)
      (let ((mp3-function (assoc "mp3" emms-tag-editor-tagfile-functions)))
        (add-to-list 'emms-tag-editor-tagfile-functions
                     `("aac" ,(cadr mp3-function) ,(caddr mp3-function))))
      (add-to-list 'emms-tag-editor-tagfile-functions
                 '("m4a"
                   "/run/current-system/profile/bin/AtomicParsley"
                   ((info-artist . "--artist")
                    (info-title . "--title")
                    (info-album . "--album")
                    (info-tracknumber . "--tracknum")
                    (info-year . "--year")
                    (info-genre . "--genre")
                    (info-note . "--comment")
                    (info-albumartist . "--albumArtist")
                    (info-composer . "--composer")))))
  
  (when (boundp 'emms-playlist-buffer-name)
    (setq emms-playlist-buffer-name "*EMMS Playlist*"))
  (when (boundp 'emms-playlist-mode-center-when-go)
    (setq emms-playlist-mode-center-when-go t))
  (when (boundp 'emms-history-file)
    (setq emms-history-file (expand-file-name "emacs/emms-history" (if (fboundp 'xdg-cache-home) (xdg-cache-home) "~/.cache"))))
  (when (boundp 'emms-seek-seconds)
    (setq emms-seek-seconds 15))
  (when (boundp 'emms-source-file-default-directory)
    (setq emms-source-file-default-directory (substitute-env-vars "$HOME/Music")))
  (when (boundp 'emms-repeat-playlist)
    (setq emms-repeat-playlist t))
  (when (boundp 'emms-info-functions)
    (setq emms-info-functions '(emms-info-libtag)))
  (when (boundp 'emms-mode-line-format)
    (setq emms-mode-line-format "%s"))
  (when (boundp 'emms-mode-line-icon-enabled-p)
    (setq emms-mode-line-icon-enabled-p nil))
  
  (with-eval-after-load 'emms-browser
    (eval-when-compile (require 'emms-browser))
    (with-no-warnings
      (when (fboundp 'emms-browser-make-filter)
        (emms-browser-make-filter "all-files" (when (fboundp 'emms-browser-filter-only-type)
                                                  (emms-browser-filter-only-type 'file)))
        (emms-browser-make-filter "last-week" (when (fboundp 'emms-browser-filter-only-recent)
                                                 (emms-browser-filter-only-recent 7)))))
    (when (boundp 'emms-browser-covers)
      (setq emms-browser-covers 'emms-browser-cache-thumbnail-async))
    (when (boundp 'emms-browser-switch-to-playlist-on-add)
      (setq emms-browser-switch-to-playlist-on-add t))
    (when (boundp 'emms-browser-thumbnail-small-size)
      (setq emms-browser-thumbnail-small-size 64))
    (when (boundp 'emms-browser-thumbnail-medium-size)
      (setq emms-browser-thumbnail-medium-size 128)))))

(provide 'bv-emms)
;;; bv-emms.el ends here
