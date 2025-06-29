;;; bv-pulseaudio-control.el --- PulseAudio control configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for pulseaudio-control package with icon support.
;; This module sets up PulseAudio control with Material Design icons
;; for volume and microphone status display in the mode line.

;;; Code:


(autoload 'pulseaudio-control-default-sink-mode "pulseaudio-control")
(autoload 'pulseaudio-control-default-source-mode "pulseaudio-control")

(with-eval-after-load 'bv-keymaps
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "v") 'pulseaudio-control-map)))

(with-eval-after-load 'pulseaudio-control
  (when (boundp 'pulseaudio-control-map)
    (define-key pulseaudio-control-map "L"
      'pulseaudio-control-toggle-sink-input-mute-by-index))
  
  (eval-when-compile (require 'all-the-icons))
  (with-eval-after-load 'all-the-icons
    (let ((all-the-icons-default-adjust -0.15))
      (when (boundp 'pulseaudio-control-sink-mute-string)
        (setq pulseaudio-control-sink-mute-string
              (all-the-icons-material "volume_off" :height 1)))
      (when (boundp 'pulseaudio-control-sink-volume-strings)
        (setq pulseaudio-control-sink-volume-strings
              (list (all-the-icons-material "volume_mute" :height 1)
                    (all-the-icons-material "volume_down" :height 1)
                    (all-the-icons-material "volume_up" :height 1))))
      (when (boundp 'pulseaudio-control-source-mute-string)
        (setq pulseaudio-control-source-mute-string
              (all-the-icons-material "mic_off" :height 1)))
      (when (boundp 'pulseaudio-control-source-volume-strings)
        (setq pulseaudio-control-source-volume-strings
              (list (all-the-icons-material "mic_none" :height 1)
                    (all-the-icons-material "mic" :height 1))))))
  
  (when (boundp 'pulseaudio-control-pactl-path)
    (setq pulseaudio-control-pactl-path
          "/run/current-system/profile/bin/pactl"))
  (when (boundp 'pulseaudio-control--volume-maximum)
    (setq pulseaudio-control--volume-maximum
          '(("percent" . 100) ("decibels" . 10) ("raw" . 98000))))
  (when (boundp 'pulseaudio-control-volume-step)
    (setq pulseaudio-control-volume-step "5%"))
  (when (boundp 'pulseaudio-control-volume-verbose)
    (setq pulseaudio-control-volume-verbose nil))
  
  (pulseaudio-control-default-sink-mode)
  (pulseaudio-control-default-source-mode))

(provide 'bv-pulseaudio-control)
;;; bv-pulseaudio-control.el ends here