;;; bv-yt-dlp.el --- YouTube downloader configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for yt-dlp YouTube downloader.

;;; Code:


(autoload 'substitute-env-vars "env")

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "y") 'ytdl-show-list)))

(with-eval-after-load 'ytdl
  (require 'env)
  (when (boundp 'ytdl--dl-list-mode-map)
    (define-key ytdl--dl-list-mode-map "a" 'ytdl-download))
  (when (boundp 'ytdl-command)
    (setq ytdl-command
          "/run/current-system/profile/bin/yt-dlp"))
  (when (boundp 'ytdl-download-folder)
    (setq ytdl-download-folder (substitute-env-vars "$HOME/Downloads")))
  (when (boundp 'ytdl-music-folder)
    (setq ytdl-music-folder (substitute-env-vars "$HOME/Music")))
  (when (boundp 'ytdl-video-folder)
    (setq ytdl-video-folder (substitute-env-vars "$HOME/Videos")))
  (when (boundp 'ytdl-mode-line)
    (setq ytdl-mode-line nil))
  (when (boundp 'ytdl-music-extra-args)
    (setq ytdl-music-extra-args
          (list "--ffmpeg-location"
                "/run/current-system/profile/bin/ffmpeg")))
  (when (boundp 'ytdl-video-extra-args)
    (setq ytdl-video-extra-args
          (list "--ffmpeg-location"
                "/run/current-system/profile/bin/ffmpeg"))))

(provide 'bv-yt-dlp)
;;; bv-yt-dlp.el ends here