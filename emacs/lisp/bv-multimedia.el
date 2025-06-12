;;; bv-multimedia.el --- Media playback and external app integration -*- lexical-binding: t -*-

;;; Commentary:
;; Optional multimedia features including media playback and
;; external application integration. Load only if needed.

;;; Code:

(require 'bv-core)

;;;; Custom Variables
(defgroup bv-multimedia nil
  "Multimedia and external app configuration."
  :group 'bv)

;;; EMMS Configuration
(defcustom bv-multimedia-music-directory
  (expand-file-name "~/Music")
  "Default music directory."
  :type 'directory
  :group 'bv-multimedia)

(defcustom bv-multimedia-emms-directory
  (expand-file-name "emms" user-emacs-directory)
  "Directory for EMMS data."
  :type 'directory
  :group 'bv-multimedia)

;;;; Configuration

;;; Basic Media Playback (optional)
(use-package emms
  :defer t
  :init
  (unless (file-exists-p bv-multimedia-emms-directory)
    (make-directory bv-multimedia-emms-directory t))
  
  :config
  (require 'emms-setup)
  (emms-all)
  
  (setq emms-directory bv-multimedia-emms-directory)
  (setq emms-source-file-default-directory bv-multimedia-music-directory)
  
  ;; Player configuration - use simplest available
  (setq emms-player-list
        (cond
         ((executable-find "mpv")
          '(emms-player-mpv))
         ((executable-find "mplayer")
          '(emms-player-mplayer))
         ((executable-find "vlc")
          '(emms-player-vlc))
         (t '(emms-player-mpd))))
  
  (setq emms-info-functions
        '(emms-info-native
          emms-info-metaflac
          emms-info-ogginfo))
  
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-playlist-mode-center-when-go t)
  
  (setq emms-history-file
        (expand-file-name "history" emms-directory))
  
  (setq emms-mode-line-mode-line-function 'emms-mode-line-playlist-current)
  (setq emms-mode-line-format " [%s]")
  
  (setq emms-player-mpd-connect-function 'emms-player-mpd-connect)
  
  :bind (("C-c e e" . emms)
         ("C-c e b" . emms-smart-browse)
         ("C-c e l" . emms-playlist-mode-go)
         ("C-c e s" . emms-start)
         ("C-c e S" . emms-stop)
         ("C-c e n" . emms-next)
         ("C-c e p" . emms-previous)
         ("C-c e P" . emms-pause)))

;;; Simple media controls
(defhydra bv-multimedia-hydra (:color pink :hint nil)
  "
Media Control
_p_: play/pause  _s_: stop  _n_: next  _b_: previous
_+_: volume up   _-_: volume down  _m_: mute
_l_: playlist    _q_: quit
"
  ("p" emms-pause)
  ("s" emms-stop)
  ("n" emms-next)
  ("b" emms-previous)
  ("+" emms-volume-raise)
  ("-" emms-volume-lower)
  ("m" emms-volume-toggle-mute)
  ("l" emms-playlist-mode-go)
  ("q" nil :exit t))

;;; External App Integration

(defun bv-multimedia-open-externally ()
  "Open current file with system's default application."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (when (derived-mode-p 'dired-mode)
                    (dired-get-file-for-visit)))))
    (when file
      (call-process
       (pcase system-type
         ('darwin "open")
         ('windows-nt "start")
         (_ "xdg-open"))
       nil 0 nil file))))

(defun bv-multimedia-play-url (url)
  "Play URL with mpv or browser."
  (interactive "sURL: ")
  (if (executable-find "mpv")
      (start-process "mpv" nil "mpv" url)
    (browse-url url)))

(defun bv-multimedia-screenshot (prefix)
  "Take a screenshot. With PREFIX, select region."
  (interactive "P")
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (expand-file-name 
                    (format "screenshot-%s.png" timestamp)
                    "~/Pictures/"))
         (command (pcase system-type
                    ('darwin (if prefix
                                 "screencapture -i %s"
                               "screencapture %s"))
                    (_ (if prefix
                           "scrot -s %s"
                         "scrot %s"))))
    (shell-command (format command filename))
    (message "Screenshot saved: %s" filename)
    (kill-new filename)))

;;; Global Keybindings
(with-eval-after-load 'bv-core
  (bv-leader
    "M" '(:ignore t :which-key "multimedia")
    "M m" #'bv-multimedia-hydra/body
    "M o" #'bv-multimedia-open-externally
    "M u" #'bv-multimedia-play-url
    "M s" #'bv-multimedia-screenshot))

;;;; Feature Definition
(defun bv-multimedia-load ()
  "Load multimedia configuration."
  (add-to-list 'bv-enabled-features 'multimedia)
  
  ;; Only start EMMS if music directory exists
  (when (file-directory-p bv-multimedia-music-directory)
    (require 'emms-setup))
  
  (message "Multimedia tools loaded"))

(provide 'bv-multimedia)
;;; bv-multimedia.el ends here
