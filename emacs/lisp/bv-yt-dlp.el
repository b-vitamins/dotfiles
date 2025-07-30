;;; bv-yt-dlp.el --- YouTube downloader configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Media downloading with yt-dlp.

;;; Code:


(declare-function ytdl-show-list "ytdl")
(declare-function ytdl-download "ytdl")
(declare-function ytdl-download-audio "ytdl")
(declare-function ytdl-download-video "ytdl")

(defgroup bv-ytdl nil
  "YouTube downloader settings."
  :group 'bv)

(defcustom bv-ytdl-idle-delay 2.0
  "Idle time before loading ytdl."
  :type 'number
  :group 'bv-ytdl)

(defcustom bv-ytdl-downloads-dir (expand-file-name "~/Downloads")
  "Default download directory."
  :type 'directory
  :group 'bv-ytdl)

(defcustom bv-ytdl-music-dir (expand-file-name "~/Music")
  "Music download directory."
  :type 'directory
  :group 'bv-ytdl)

(defcustom bv-ytdl-video-dir (expand-file-name "~/Videos")
  "Video download directory."
  :type 'directory
  :group 'bv-ytdl)

;; Load ytdl after idle delay
(run-with-idle-timer bv-ytdl-idle-delay t
                     (lambda ()
                       (require 'ytdl nil t)))

(setq ytdl-command "yt-dlp"
      ytdl-download-folder bv-ytdl-downloads-dir
      ytdl-music-folder bv-ytdl-music-dir
      ytdl-video-folder bv-ytdl-video-dir
      ytdl-mode-line nil)

(defun bv-ytdl-download-url ()
  "Download media from URL."
  (interactive)
  (let ((url (read-string "URL: ")))
    (ytdl-download url)))

(defun bv-ytdl-download-audio ()
  "Download audio from URL."
  (interactive)
  (let ((url (read-string "Audio URL: ")))
    (ytdl-download-audio url)))

(defun bv-ytdl-download-video ()
  "Download video from URL."
  (interactive)
  (let ((url (read-string "Video URL: ")))
    (ytdl-download-video url)))

(with-eval-after-load 'ytdl
  (define-key ytdl--dl-list-mode-map "q" 'quit-window)
  (define-key ytdl--dl-list-mode-map "a" 'ytdl-download)
  (define-key ytdl--dl-list-mode-map "d" 'ytdl-download-delete))


(provide 'bv-yt-dlp)
;;; bv-yt-dlp.el ends here