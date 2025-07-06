;;; bv-mpv.el --- MPV media player integration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; MPV media player integration with ytdl support.

;;; Code:

(defgroup bv-mpv nil
  "MPV media player integration."
  :group 'bv)

(defcustom bv-mpv-idle-delay 2.0
  "Idle time before loading mpv."
  :type 'number
  :group 'bv-mpv)

(defcustom bv-mpv-seek-step 3
  "Number of seconds to seek forward/backward."
  :type 'integer
  :group 'bv-mpv)

;; Declare functions to avoid warnings
(declare-function mpv-start "mpv")
(declare-function mpv-run-command "mpv")
(declare-function mpv-get-property "mpv")
(declare-function mpv-playlist-append-url "mpv")
(declare-function mpv-seek "mpv")
(declare-function mpv-set-chapter-ab-loop "mpv")
(declare-function mpv-remove-playlist-entry "mpv")
(declare-function mpv-jump-to-chapter "mpv")
(declare-function mpv-jump-to-playlist-entry "mpv")
(declare-function mpv-playlist-next "mpv")
(declare-function mpv-playlist-prev "mpv")
(declare-function mpv-chapter-next "mpv")
(declare-function mpv-chapter-prev "mpv")
(declare-function mpv-seek-forward "mpv")
(declare-function mpv-seek-backward "mpv")
(declare-function mpv-quit "mpv")
(declare-function mpv-set-ab-loop "mpv")
(declare-function mpv-pause "mpv")
(declare-function mpv-toggle-loop "mpv")
(declare-function mpv-toggle-video "mpv")
(declare-function ytdl-select-format "ytdl")
(declare-function ytdl--get-download-type "ytdl")
(declare-function ytdl--download-async "ytdl")
(declare-function ytdl--eval-field "ytdl")
(declare-function ytdl--eval-list "ytdl")

;; Load mpv after idle delay
(run-with-idle-timer bv-mpv-idle-delay t
                     (lambda ()
                       (require 'mpv nil t)
                       (require 'ytdl nil t)))

(define-prefix-command 'bv-mpv-map)

(eval-when-compile (require 'cl-lib))

(cl-defun bv-mpv-play-url (url &optional format &key audio repeat (formats t) (select t) playlist)
  "Play URL in MPV with optional format selection and playback options.
URL is the media URL to play.
FORMAT specifies the ytdl format string.
AUDIO when non-nil plays audio only.
REPEAT when non-nil loops the file infinitely.
FORMATS when non-nil allows format selection.
SELECT when non-nil prompts to play or enqueue if playlist exists.
PLAYLIST when non-nil adds to playlist instead of playing directly."
  (interactive "sURI: ")
  (require 'mpv)
  (let* ((sel-format (or format (and formats (ytdl-select-format url))))
         (extra-args
          (split-string
           (concat
            (format "--ytdl-format=%s" (or sel-format "best"))
            (and audio " --video=no")
            (and repeat " --loop-file=inf")))))
    (if (and select (mpv-get-property "playlist"))
        (pcase (completing-read "Play or Enqueue: " '("Play" "Enqueue"))
          ("Play" (apply 'mpv-start url extra-args))
          ("Enqueue" (apply 'mpv-playlist-append-url url extra-args)))
      (if (and playlist (mpv-get-property "playlist"))
          (apply 'mpv-playlist-append-url url extra-args)
        (apply 'mpv-start url extra-args)))))

(defun bv-mpv-download ()
  "Download the currently playing media using ytdl."
  (interactive)
  (require 'ytdl)
  (if-let* ((dl-type (ytdl--get-download-type))
            (track (mpv-get-property "path"))
            (title (mpv-get-property "media-title")))
      (ytdl--download-async
       track
       (expand-file-name title (ytdl--eval-field (nth 1 dl-type)))
       (ytdl--eval-list (ytdl--eval-field (nth 2 dl-type)))
       'ignore
       (car dl-type))
    (error "Mpv is not currently active")))

(defun bv-mpv-seek-start ()
  "Seek to the beginning of the current media."
  (interactive)
  (mpv-seek 0))

(defun bv-mpv-playlist-shuffle ()
  "Shuffle the MPV playlist."
  (interactive)
  (mpv-run-command "playlist-shuffle"))

(defun bv-mpv-kill-path ()
  "Copy the current media path to the system clipboard."
  (interactive)
  (when-let* ((title (mpv-get-property "media-title"))
              (path (mpv-get-property "path")))
    (kill-new path)
    (message (format "Copied \"%s\" to the system clipboard" title))
    path))

(with-eval-after-load 'embark
  (when (boundp 'embark-url-map)
    (define-key embark-url-map "v" 'bv-mpv-play-url))
  
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'mpv-set-chapter-ab-loop)
    (when (boundp 'embark-keymap-alist)
      (add-to-list 'embark-keymap-alist
                   (cons 'mpv-chapter map))))
  
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'mpv-remove-playlist-entry)
    (when (boundp 'embark-keymap-alist)
      (add-to-list 'embark-keymap-alist
                   (cons 'mpv-file map))))
)

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "m") 'bv-mpv-map)))

(with-eval-after-load 'mpv
  (setq mpv-seek-step bv-mpv-seek-step))

(when (boundp 'bv-mpv-map)
  (let ((map bv-mpv-map))
    (define-key map (kbd "RET") 'bv-mpv-play-url)
    (define-key map (kbd "s") 'bv-mpv-download)
    (define-key map (kbd "a") 'bv-mpv-seek-start)
    (define-key map (kbd "w") 'bv-mpv-kill-path)
    (define-key map (kbd "c") 'mpv-jump-to-chapter)
    (define-key map (kbd "l") 'mpv-jump-to-playlist-entry)
    (define-key map (kbd "n") 'mpv-playlist-next)
    (define-key map (kbd "p") 'mpv-playlist-prev)
    (define-key map (kbd "N") 'mpv-chapter-next)
    (define-key map (kbd "P") 'mpv-chapter-prev)
    (define-key map (kbd "f") 'mpv-seek-forward)
    (define-key map (kbd "b") 'mpv-seek-backward)
    (define-key map (kbd "q") 'mpv-quit)
    (define-key map (kbd "R") 'mpv-set-ab-loop)
    (define-key map (kbd "SPC") 'mpv-pause)
    (define-key map (kbd "r") 'mpv-toggle-loop)
    (define-key map (kbd "v") 'mpv-toggle-video)
    (put 'mpv-seek-forward 'repeat-map 'bv-mpv-map)
    (put 'mpv-seek-backward 'repeat-map 'bv-mpv-map)
    (put 'mpv-pause 'repeat-map 'bv-mpv-map)))

(provide 'bv-mpv)
;;; bv-mpv.el ends here