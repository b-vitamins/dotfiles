;;; bv-ednc.el --- Desktop notifications configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Desktop notification handling with EDNC.

;;; Code:


(declare-function ednc-notifications "ednc")
(declare-function ednc-format-notification "ednc")
(declare-function ednc--close-notification "ednc")
(declare-function ednc-dismiss-notification "ednc")
(declare-function ednc--update-log-buffer "ednc")

(defgroup bv-ednc nil
  "Desktop notification settings."
  :group 'bv)

(defcustom bv-ednc-idle-delay 0.5
  "Idle time before loading EDNC."
  :type 'number
  :group 'bv-ednc)

(defcustom bv-ednc-modeline-format '(:eval (bv-ednc-modeline))
  "Format for notification indicator in mode line."
  :type 'sexp
  :group 'bv-ednc)

;; Load EDNC after idle delay
(run-with-idle-timer bv-ednc-idle-delay t
                     (lambda ()
                       (require 'ednc nil t)))

(defun bv-ednc-modeline ()
  "Return notification count for mode line."
  (when (and (fboundp 'ednc-notifications)
             (ednc-notifications))
    (concat " [" (number-to-string (length (ednc-notifications))) "]")))

(defun bv-ednc-show-notification-log ()
  "Display notification log."
  (interactive)
  (if-let ((buffer (get-buffer "*ednc-log*")))
      (pop-to-buffer buffer)
    (message "No notifications")))

(defun bv-ednc-dismiss-last ()
  "Dismiss most recent notification."
  (interactive)
  (when-let ((notification (car (ednc-notifications))))
    (ednc-dismiss-notification notification)
    (message "Notification dismissed")))

(defun bv-ednc-dismiss-all ()
  "Dismiss all notifications."
  (interactive)
  (let ((count (length (ednc-notifications))))
    (mapc #'ednc-dismiss-notification (ednc-notifications))
    (message "Dismissed %d notifications" count)))

(defun bv-ednc-open-app ()
  "Open app for most recent notification."
  (interactive)
  (when-let* ((notification (car (ednc-notifications)))
              (app (alist-get 'app-name notification)))
    (cond
     ((string-match-p "firefox\\|chromium" app)
      (browse-url ""))
     ((string-match-p "telegram\\|signal" app)
      (message "Opening %s..." app))
     (t (message "No handler for %s" app)))))

(with-eval-after-load 'ednc
  (ednc-mode 1)
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t))))

(defun bv-ednc-transient ()
  "Transient menu for notifications."
  (interactive)
  (transient-define-prefix bv-ednc-transient-menu ()
    "Desktop Notifications"
    ["Actions"
     ("l" "Show log" bv-ednc-show-notification-log)
     ("d" "Dismiss last" bv-ednc-dismiss-last)
     ("D" "Dismiss all" bv-ednc-dismiss-all)
     ("o" "Open app" bv-ednc-open-app)])
  (bv-ednc-transient-menu))

(global-set-key (kbd "C-c n") 'bv-ednc-transient)

(provide 'bv-ednc)
;;; bv-ednc.el ends here