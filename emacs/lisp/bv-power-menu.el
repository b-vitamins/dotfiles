;;; bv-power-menu.el --- Power management menu configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for power management menu with icons.

;;; Code:

(autoload 'all-the-icons-faicon "all-the-icons")
(autoload 'all-the-icons-material "all-the-icons")
(autoload 'async-start-process "async")


(defun bv-power-menu--update-candidates ()
  "Update power menu candidates with icons when all-the-icons is available."
  (when (boundp 'bv-power-menu-candidates)
    (setq bv-power-menu-candidates
        (list (cons (concat (all-the-icons-faicon "lock") " lock")
                    (list "/run/current-system/profile/bin/loginctl"
                          "lock-session"))
              (cons (concat (all-the-icons-material "exit_to_app") " logout")
                    (list "/run/current-system/profile/bin/loginctl"
                          "terminate-session"
                          (getenv "XDG_SESSION_ID")))
              (cons (concat (all-the-icons-faicon "pause") " suspend")
                    (list "/run/current-system/profile/bin/loginctl"
                          "suspend"))
              (cons (concat (all-the-icons-faicon "stop") " hibernate")
                    (list "/run/current-system/profile/bin/loginctl"
                          "suspend-then-hibernate"))
              (cons (concat (all-the-icons-faicon "power-off") " shutdown")
                    (list "/run/current-system/profile/bin/loginctl"
                          "poweroff"))
              (cons (concat (all-the-icons-faicon "refresh") " reboot")
                    (list "/run/current-system/profile/bin/loginctl"
                          "reboot"))))))

(with-eval-after-load 'all-the-icons
  (bv-power-menu--update-candidates))

(defun bv-power-menu ()
  "Prompt for an action on the power-menu, and make this action.
Displays a completion interface with iconified power management options
and executes the selected action asynchronously."
  (interactive)
  (when (fboundp 'all-the-icons-faicon)
    (bv-power-menu--update-candidates))
  (let* ((selected (completing-read
                    "power-menu command:"
                    (lambda (string predicate action)
                      (if (eq action 'metadata)
                          `(metadata (display-sort-function . identity))
                        (complete-with-action
                         action
                         (and (boundp 'bv-power-menu-candidates)
                              bv-power-menu-candidates)
                         string
                         predicate)))))
         (command-data (cdr (assoc selected (and (boundp 'bv-power-menu-candidates)
                                                 bv-power-menu-candidates))))
         (command-list (if (and (string-match "logout" selected)
                                (= (length command-data) 2))
                           (append command-data (list (getenv "XDG_SESSION_ID")))
                         command-data)))
    (apply 'async-start-process
           "power-menu"
           (car command-list)
           nil
           (cdr command-list))))

(provide 'bv-power-menu)
;;; bv-power-menu.el ends here