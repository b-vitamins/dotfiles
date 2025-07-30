;;; bv-power-menu.el --- Power management configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; System power management menu.

;;; Code:


(declare-function async-start-process "async")

(defgroup bv-power nil
  "Power management settings."
  :group 'bv)

(defcustom bv-power-loginctl-path "loginctl"
  "Path to loginctl command."
  :type 'string
  :group 'bv-power)

(defvar bv-power-menu-actions
  '(("Lock"      . ("lock-session"))
    ("Logout"    . ("terminate-session" session-id))
    ("Suspend"   . ("suspend"))
    ("Hibernate" . ("hibernate"))
    ("Shutdown"  . ("poweroff"))
    ("Reboot"    . ("reboot")))
  "Power menu actions.")

(defun bv-power-menu ()
  "Display power management menu."
  (interactive)
  (let* ((action (completing-read "Power action: "
                                  (mapcar 'car bv-power-menu-actions)))
         (command-spec (cdr (assoc action bv-power-menu-actions)))
         (command-args (mapcar (lambda (arg)
                                 (if (eq arg 'session-id)
                                     (getenv "XDG_SESSION_ID")
                                   arg))
                               command-spec)))
    (when (yes-or-no-p (format "%s system? " action))
      (apply 'async-start-process
             "power-menu"
             bv-power-loginctl-path
             nil
             command-args))))

(defun bv-power-lock ()
  "Lock the session."
  (interactive)
  (async-start-process "lock" bv-power-loginctl-path nil "lock-session"))

(defun bv-power-suspend ()
  "Suspend the system."
  (interactive)
  (when (yes-or-no-p "Suspend system? ")
    (async-start-process "suspend" bv-power-loginctl-path nil "suspend")))


(provide 'bv-power-menu)
;;; bv-power-menu.el ends here