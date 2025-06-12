;;; bv-productivity.el --- Workflow optimization and productivity tools -*- lexical-binding: t -*-

;;; Commentary:
;; Productivity tools including LLM integration, advanced calculator,
;; notifications, pomodoro timer, time tracking, and workflow optimizations.

;;; Code:

(require 'bv-core)

;;;; Custom Variables
(defgroup bv-productivity nil
  "Productivity and workflow configuration."
  :group 'bv)

;;; LLM Integration
(defcustom bv-productivity-gptel-api-key-command
  '("pass" "show" "openai/api-key")
  "Command to retrieve GPT API key."
  :type '(repeat string)
  :group 'bv-productivity)

(defcustom bv-productivity-gptel-default-mode 'org-mode
  "Default mode for GPT responses."
  :type '(choice (const :tag "Org mode" org-mode)
                 (const :tag "Markdown" markdown-mode)
                 (const :tag "Text" text-mode))
  :group 'bv-productivity)

(defcustom bv-productivity-gptel-model "gpt-4"
  "Default GPT model to use."
  :type 'string
  :group 'bv-productivity)

;;; Calculator
(defcustom bv-productivity-calc-currency 'USD
  "Base currency for calculations."
  :type 'symbol
  :group 'bv-productivity)

(defcustom bv-productivity-calc-currency-update-interval 7
  "Days between currency exchange rate updates."
  :type 'integer
  :group 'bv-productivity)

;;; Notifications
(defcustom bv-productivity-notifications-icon nil
  "Icon to use for notifications."
  :type '(choice (const :tag "Default" nil)
                 file)
  :group 'bv-productivity)

(defcustom bv-productivity-notifications-app-name "Emacs"
  "Application name for notifications."
  :type 'string
  :group 'bv-productivity)

;;; Pomodoro
(defcustom bv-productivity-pomodoro-length 25
  "Length of a pomodoro in minutes."
  :type 'integer
  :group 'bv-productivity)

(defcustom bv-productivity-pomodoro-short-break 5
  "Length of short break in minutes."
  :type 'integer
  :group 'bv-productivity)

(defcustom bv-productivity-pomodoro-long-break 15
  "Length of long break in minutes."
  :type 'integer
  :group 'bv-productivity)

(defcustom bv-productivity-pomodoro-long-break-after 4
  "Number of pomodoros before a long break."
  :type 'integer
  :group 'bv-productivity)

;;; Time Tracking
(defcustom bv-productivity-time-tracking-file
  (expand-file-name "time-log.org" org-directory)
  "File for time tracking entries."
  :type 'file
  :group 'bv-productivity)

;;;; Configuration

;;; LLM Integration (GPT)
(use-package gptel
  :defer t
  :config
  (defun bv-productivity-gptel-get-api-key ()
    "Get API key for gptel."
    (string-trim-right
     (with-output-to-string
       (let ((exit (apply #'call-process
                          (car bv-productivity-gptel-api-key-command)
                          nil standard-output nil
                          (cdr bv-productivity-gptel-api-key-command))))
         (unless (zerop exit)
           (error "Failed to get gptel API key")))))
  )
  
  (setq gptel-api-key #'bv-productivity-gptel-get-api-key)
  (setq gptel-default-mode bv-productivity-gptel-default-mode)
  (setq gptel-model bv-productivity-gptel-model)
  
  (setq gptel-directives
        '((default . "You are a helpful assistant.")
          (programming . "You are a programming assistant. Provide concise code examples.")
          (writing . "You are a writing assistant. Help improve clarity and style.")
          (research . "You are a research assistant. Provide accurate, well-sourced information.")))
  
  ;; Keybindings provided via `bv-leader`. Remove conflicting global bindings.
  )

(use-package gptel-quick
  :after gptel)

;;; Calculator
(use-package calc
  :defer t
  :config
  (setq calc-algebraic-mode t)
  (setq calc-symbolic-mode t)
  (setq calc-display-trail nil)
  
  :bind (("C-c k" . calc-dispatch)))

(use-package calc-currency
  :after calc
  :config
  (require 'xdg)
  (setq calc-currency-exchange-rates-file
        (expand-file-name "emacs/calc-currency-rates.el"
                          (xdg-cache-home)))
  (setq calc-currency-base-currency bv-productivity-calc-currency)
  (setq calc-currency-update-interval bv-productivity-calc-currency-update-interval)
  
  (add-hook 'calc-start-hook 'calc-currency-load))

;;; Desktop Notifications
(use-package ednc
  :defer t
  :init
  (defvar bv-ednc-map (make-sparse-keymap)
    "Keymap for EDNC commands.")
  
  :config
  (ednc-mode 1)
  
  (defun bv-ednc--notify ()
    "Display the latest EDNC notification."
    (when (ednc-notifications)
      (ednc-format-notification (car (ednc-notifications)))))
  
  (defun bv-ednc-show-notification-log ()
    "Switch to the EDNC log buffer."
    (interactive)
    (when (get-buffer ednc-log-name)
      (switch-to-buffer ednc-log-name)))
  
  (defun bv-ednc-dismiss-all-notifications ()
    "Dismiss all EDNC notifications."
    (interactive)
    (mapc 'ednc-dismiss-notification (cdr ednc--state)))
  
  (defun bv-ednc-update-notifications (&rest _)
    "Update the display of EDNC notifications."
    (force-mode-line-update t))
  
  (add-hook 'ednc-notification-presentation-functions
            #'bv-ednc-update-notifications)
  
  (with-eval-after-load 'notifications
    (setq notifications-application-name bv-productivity-notifications-app-name)
    (when bv-productivity-notifications-icon
      (setq notifications-application-icon bv-productivity-notifications-icon)))
  
  :bind-keymap ("C-c n" . bv-ednc-map)
  :bind (:map bv-ednc-map
              ("n" . ednc-pop-to-notification-in-log-buffer)
              ("l" . bv-ednc-show-notification-log)
              ("d" . ednc-dismiss-notification)
              ("D" . bv-ednc-dismiss-all-notifications)))

;;; Pomodoro Timer
(use-package pomidor
  :defer t
  :config
  (setq pomidor-seconds (* bv-productivity-pomodoro-length 60))
  (setq pomidor-break-seconds (* bv-productivity-pomodoro-short-break 60))
  (setq pomidor-long-break-seconds (* bv-productivity-pomodoro-long-break 60))
  (setq pomidor-breaks-before-long bv-productivity-pomodoro-long-break-after)
  
  (setq pomidor-sound-tick nil)
  (setq pomidor-sound-tack nil)
  (setq pomidor-sound-overwork nil)
  (setq pomidor-sound-break-over nil)
  
  (add-hook 'pomidor-break-start-hook
            (lambda ()
              (notifications-notify
               :title "Pomodoro Break"
               :body "Time for a break!"
               :app-name "Emacs Pomodoro")))
  
  (add-hook 'pomidor-overwork-hook
            (lambda ()
              (notifications-notify
               :title "Pomodoro Complete"
               :body "Great work! Time to start break."
               :app-name "Emacs Pomodoro")))
  
  :bind (("C-c p p" . pomidor)
         :map pomidor-mode-map
         ("q" . quit-window)
         ("Q" . pomidor-quit)
         ("R" . pomidor-reset)
         ("SPC" . pomidor-break)
         ("RET" . pomidor-stop)))

;; Alternative: org-pomodoro integration
(use-package org-pomodoro
  :after org
  :config
  (setq org-pomodoro-length bv-productivity-pomodoro-length)
  (setq org-pomodoro-short-break-length bv-productivity-pomodoro-short-break)
  (setq org-pomodoro-long-break-length bv-productivity-pomodoro-long-break)
  (setq org-pomodoro-long-break-frequency bv-productivity-pomodoro-long-break-after)
  
  (setq org-pomodoro-finished-sound-p t)
  (setq org-pomodoro-overtime-sound-p t)
  (setq org-pomodoro-clock-break t)
  
  :bind (:map org-mode-map
              ("C-c p o" . org-pomodoro)))

;;; Time Tracking
(use-package org-clock
  :after org
  :config
  (setq org-clock-persist t)
  (setq org-clock-persist-file
        (expand-file-name "org-clock-save.el" user-emacs-directory))
  (setq org-clock-in-resume t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-clock-idle-time 10)
  
  (setq org-clock-mode-line-total 'current)
  (setq org-clock-clocked-in-display 'mode-line)
  
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  
  (org-clock-persistence-insinuate)
  
  :bind (("C-c t i" . org-clock-in)
         ("C-c t o" . org-clock-out)
         ("C-c t c" . org-clock-in-last)
         ("C-c t g" . org-clock-goto)
         ("C-c t d" . org-clock-display)
         ("C-c t r" . org-clock-report)
         ("C-c t e" . org-clock-modify-effort-estimate)))

;; Chronos - Simple timer
(use-package chronos
  :defer t
  :config
  (setq chronos-notification-type 'notifications)
  (setq chronos-shell-notify-command "")
  (setq chronos-expiry-functions '(chronos-desktop-notifications-notify))
  
  :bind (("C-c t t" . chronos-add-timer)
         ("C-c t l" . chronos-list)))

;;; Custom Productivity Commands

(defun bv-productivity-focus-mode ()
  "Enter focus mode - minimal distractions."
  (interactive)
  (when (fboundp 'olivetti-mode)
    (olivetti-mode 1))
  (display-line-numbers-mode -1)
  (setq mode-line-format nil)
  (message "Focus mode enabled"))

(defun bv-productivity-unfocus-mode ()
  "Exit focus mode."
  (interactive)
  (when (fboundp 'olivetti-mode)
    (olivetti-mode -1))
  (display-line-numbers-mode 1)
  (kill-local-variable 'mode-line-format)
  (message "Focus mode disabled"))

(defun bv-productivity-insert-timestamp ()
  "Insert current timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun bv-productivity-insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun bv-productivity-open-time-log ()
  "Open time tracking file."
  (interactive)
  (find-file bv-productivity-time-tracking-file))

(defun bv-productivity-quick-note ()
  "Quickly capture a note with timestamp."
  (interactive)
  (let* ((note (read-string "Quick note: "))
         (timestamp (format-time-string "%Y-%m-%d %H:%M"))
         (entry (format "* %s %s\n" timestamp note)))
    (with-current-buffer (find-file-noselect bv-productivity-time-tracking-file)
      (goto-char (point-max))
      (insert entry)
      (save-buffer))
    (message "Note captured: %s" note)))

(defun bv-productivity-meeting-template ()
  "Insert meeting template."
  (interactive)
  (insert (format "* Meeting: %s\n" (read-string "Meeting title: ")))
  (insert (format "** Date: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
  (insert "** Attendees:\n- \n")
  (insert "** Agenda:\n- \n")
  (insert "** Notes:\n\n")
  (insert "** Action Items:\n- [ ] \n")
  (insert "** Next Steps:\n- \n"))

(defun bv-productivity-daily-review ()
  "Create daily review entry."
  (interactive)
  (let ((date (format-time-string "%Y-%m-%d")))
    (find-file bv-productivity-time-tracking-file)
    (goto-char (point-max))
    (insert (format "\n* Daily Review: %s\n" date))
    (insert "** What went well:\n- \n")
    (insert "** What could be improved:\n- \n")
    (insert "** Key accomplishments:\n- \n")
    (insert "** Tomorrow's priorities:\n- \n")))

(defun bv-productivity-weekly-summary ()
  "Generate weekly time summary."
  (interactive)
  (require 'org-clock)
  (let ((start (org-read-date nil nil "-mon"))
        (end (org-read-date nil nil "+sun")))
    (org-clock-report 
     nil 
     (list :tstart start :tend end :maxlevel 3))))

;;; Hydra for Quick Access
(use-package hydra
  :defer t)

(defhydra bv-productivity-hydra (:color blue :hint nil)
  "
Productivity Tools

Timer           Focus              Quick
-------         -------            -------
_p_: Pomodoro   _f_: Focus mode    _n_: Quick note
_t_: Timer      _F_: Unfocus       _d_: Insert date
_i_: Clock in   _m_: Meeting       _T_: Insert timestamp
_o_: Clock out  _r_: Daily review  _w_: Weekly summary

_q_: Quit
"
  ("p" pomidor)
  ("t" chronos-add-timer)
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("f" bv-productivity-focus-mode)
  ("F" bv-productivity-unfocus-mode)
  ("n" bv-productivity-quick-note)
  ("d" bv-productivity-insert-date)
  ("T" bv-productivity-insert-timestamp)
  ("m" bv-productivity-meeting-template)
  ("r" bv-productivity-daily-review)
  ("w" bv-productivity-weekly-summary)
  ("q" nil :exit t))

;;; Global Keybindings
(with-eval-after-load 'bv-core
  (bv-leader
    "P" '(:ignore t :which-key "productivity")
    "P P" #'bv-productivity-hydra/body
    "P g" #'gptel
    "P c" #'calc
    "P p" #'pomidor
    "P i" #'org-clock-in
    "P o" #'org-clock-out
    "P n" #'bv-productivity-quick-note
    "P f" #'bv-productivity-focus-mode
    "P t" #'chronos-add-timer))

;;; Mode Line Integration
(defun bv-productivity-mode-line-indicator ()
  "Mode line indicator for productivity status."
  (let ((indicators '()))
    ;; Pomodoro indicator
    (when (and (boundp 'pomidor-timer) pomidor-timer)
      (push "[🍅]" indicators))
    ;; Clock indicator
    (when (org-clocking-p)
      (push (format "[⏰ %s]" (org-clock-get-clock-string)) indicators))
    ;; Notification count
    (when (and (fboundp 'ednc-notifications) (ednc-notifications))
      (push (format "[🔔 %d]" (length (ednc-notifications))) indicators))
    (when indicators
      (string-join indicators " "))))

;; Add to mode line
(add-to-list 'global-mode-string '(:eval (bv-productivity-mode-line-indicator)) t)

;;;; Feature Definition
(defun bv-productivity-load ()
  "Load productivity configuration."
  (add-to-list 'bv-enabled-features 'productivity)
  
  ;; Create time log file if needed
  (unless (file-exists-p bv-productivity-time-tracking-file)
    (with-temp-file bv-productivity-time-tracking-file
      (insert "#+TITLE: Time Tracking Log\n")
      (insert "#+STARTUP: logdrawer\n\n")))
  
  ;; Start notification service
  (when (display-graphic-p)
    (ednc-mode 1))
  
  (message "Productivity tools loaded"))

(provide 'bv-productivity)
;;; bv-productivity.el ends here
