;;; bv-org-recur.el --- Org recurring tasks configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Configuration for org-recur to manage recurring tasks in Org mode.

;;; Code:

(defgroup bv-org-recur nil
  "Recurring tasks configuration for Org mode."
  :group 'bv)

(defcustom bv-org-recur-idle-delay 2.0
  "Idle time before loading org-recur."
  :type 'number
  :group 'bv-org-recur)

(defcustom bv-org-recur-finish-done t
  "Mark recurring tasks as DONE when finishing."
  :type 'boolean
  :group 'bv-org-recur)

(defcustom bv-org-recur-finish-archive t
  "Archive recurring tasks when finishing."
  :type 'boolean
  :group 'bv-org-recur)

;; Declare functions to avoid warnings
(declare-function org-recur-mode "org-recur")
(declare-function org-recur-agenda-mode "org-recur")
(declare-function org-recur-finish "org-recur")
(declare-function org-recur-schedule-today "org-recur")

;; Load org-recur after idle delay
(run-with-idle-timer bv-org-recur-idle-delay t
                     (lambda ()
                       (require 'org-recur nil t)))

(defun bv-org-recur-setup ()
  "Setup org-recur configuration."
  (setq org-recur-finish-done bv-org-recur-finish-done
        org-recur-finish-archive bv-org-recur-finish-archive))

(with-eval-after-load 'org
  ;; Enable org-recur-mode in org buffers
  (add-hook 'org-mode-hook 'org-recur-mode))

(with-eval-after-load 'org-agenda
  ;; Enable org-recur-agenda-mode in agenda buffers
  (add-hook 'org-agenda-mode-hook 'org-recur-agenda-mode))

(with-eval-after-load 'org-recur
  ;; Apply configuration
  (bv-org-recur-setup)

  ;; Setup keybindings
  (when (boundp 'org-recur-mode-map)
    (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)
    (define-key org-recur-mode-map (kbd "C-c 0") 'org-recur-schedule-today))

  (when (boundp 'org-recur-agenda-mode-map)
    (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
    (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)))

(provide 'bv-org-recur)
;;; bv-org-recur.el ends here