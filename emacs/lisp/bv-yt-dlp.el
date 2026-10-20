;;; bv-yt-dlp.el --- YouTube downloader configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Media downloading with yt-dlp.

;;; Code:


(declare-function ytdl-show-list "ytdl")
(declare-function ytdl-download "ytdl")

;; External variables from ytdl package
(defvar ytdl-command)
(defvar ytdl-download-folder)
(defvar ytdl-music-folder)
(defvar ytdl-video-folder)
(defvar ytdl-mode-line)
(defvar ytdl-download-types)
(defvar ytdl--dl-list-mode-map)

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
(run-with-idle-timer bv-ytdl-idle-delay nil
                     (lambda ()
                       (require 'ytdl nil t)))

(setq ytdl-command "yt-dlp"
      ytdl-download-folder bv-ytdl-downloads-dir
      ytdl-music-folder bv-ytdl-music-dir
      ytdl-video-folder bv-ytdl-video-dir
      ytdl-mode-line nil)

(defun bv-ytdl--download-as (type-name)
  "Run `ytdl-download' offering only the download type TYPE-NAME."
  (require 'ytdl)
  (let ((ytdl-download-types
         (or (seq-filter (lambda (type) (equal (car type) type-name))
                         ytdl-download-types)
             ytdl-download-types)))
    (call-interactively #'ytdl-download)))

(defun bv-ytdl-download-url ()
  "Download media from a URL, prompting for the destination type."
  (interactive)
  (require 'ytdl)
  (call-interactively #'ytdl-download))

(defun bv-ytdl-download-audio ()
  "Download audio from a URL into the music folder."
  (interactive)
  (bv-ytdl--download-as "Music"))

(defun bv-ytdl-download-video ()
  "Download video from a URL into the video folder."
  (interactive)
  (bv-ytdl--download-as "Videos"))

(with-eval-after-load 'ytdl
  (define-key ytdl--dl-list-mode-map "q" 'quit-window)
  (define-key ytdl--dl-list-mode-map "a" 'ytdl-download))


(provide 'bv-yt-dlp)
;;; bv-yt-dlp.el ends here