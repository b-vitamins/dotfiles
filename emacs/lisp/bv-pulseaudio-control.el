;;; bv-pulseaudio-control.el --- Audio control configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; System audio control with PulseAudio.

;;; Code:


(declare-function pulseaudio-control-default-sink-mode "pulseaudio-control")
(declare-function pulseaudio-control-default-source-mode "pulseaudio-control")
(declare-function pulseaudio-control-increase-volume "pulseaudio-control")
(declare-function pulseaudio-control-decrease-volume "pulseaudio-control")
(declare-function pulseaudio-control-toggle-current-sink-mute "pulseaudio-control")

(defgroup bv-pulseaudio nil
  "Audio control settings."
  :group 'bv)

(defcustom bv-pulseaudio-idle-delay 0.5
  "Idle time before loading pulseaudio-control."
  :type 'number
  :group 'bv-pulseaudio)

(defcustom bv-pulseaudio-volume-step "5%"
  "Volume adjustment step."
  :type 'string
  :group 'bv-pulseaudio)

;; Load pulseaudio-control after idle delay
(run-with-idle-timer bv-pulseaudio-idle-delay t
                     (lambda ()
                       (require 'pulseaudio-control nil t)))

(setq pulseaudio-control-pactl-path "pactl"
      pulseaudio-control-volume-step bv-pulseaudio-volume-step
      pulseaudio-control-volume-verbose nil
      pulseaudio-control-sink-mute-string "[M]"
      pulseaudio-control-sink-volume-strings '("" "" "")
      pulseaudio-control-source-mute-string "[M]"
      pulseaudio-control-source-volume-strings '("" ""))

(with-eval-after-load 'pulseaudio-control
  (pulseaudio-control-default-sink-mode)
  (pulseaudio-control-default-source-mode))

(defun bv-audio-increase-volume ()
  "Increase system volume."
  (interactive)
  (pulseaudio-control-increase-volume))

(defun bv-audio-decrease-volume ()
  "Decrease system volume."
  (interactive)
  (pulseaudio-control-decrease-volume))

(defun bv-audio-toggle-mute ()
  "Toggle audio mute."
  (interactive)
  (pulseaudio-control-toggle-current-sink-mute))


(global-set-key (kbd "<XF86AudioRaiseVolume>") 'bv-audio-increase-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'bv-audio-decrease-volume)
(global-set-key (kbd "<XF86AudioMute>") 'bv-audio-toggle-mute)

(provide 'bv-pulseaudio-control)
;;; bv-pulseaudio-control.el ends here