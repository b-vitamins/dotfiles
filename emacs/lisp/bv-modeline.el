;;; bv-modeline.el --- Modeline inspired by nano-modeline  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Mode line format inspired by nano-modeline:
;; [ status | name (primary)               secondary | item1 | item2 ]

;;; Code:

(require 'subr-x)
(require 'cl-lib)

;; External variables
(defvar display-time-string)
(defvar org-mode-line-string)
(defvar Info-current-node)
(defvar no-mode-line)
(defvar eshell-status-in-modeline)
(defvar Info-use-header-line)
(defvar org-capture-mode)
(defvar shell-file-name)

;; External functions
(declare-function vc-backend "vc-hooks" (file))
(declare-function derived-mode-p "subr" (&rest modes))
(declare-function bound-and-true-p "subr" (var))

;; Functions defined in with-eval-after-load blocks
(declare-function calendar-setup-header "bv-modeline")
(declare-function org-capture-turn-off-header-line "bv-modeline")

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

(defun vc-branch ()
  "Return current version control branch."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                               (+ (if (eq backend 'Hg) 2 3) 2))))
    nil))

(defun bv-mode-name ()
  "Return mode name handling list form."
  (if (listp mode-name) (car mode-name) mode-name))

(defun bv-modeline-compose (status name primary secondary)
  "Compose modeline string from STATUS, NAME, PRIMARY and SECONDARY."
  (let* ((char-width    (window-font-width nil 'header-line))
         (space-up      +0.15)
         (space-down    -0.20)
         (edge-pad      (propertize " " 'face 'header-line))
         (edge-pad-width (length edge-pad))
         (prefix (cond ((string= status "RO")
                        (propertize (if (window-dedicated-p) " -- " " RO ")
                                    'face 'bv-themes-header-popout))
                       ((string= status "**")
                        (propertize (if (window-dedicated-p) " -- " " ** ")
                                    'face 'bv-themes-header-critical))
                       ((string= status "RW")
                       (propertize (if (window-dedicated-p) " -- " " RW ")
                                   'face 'bv-themes-header-faded))
                       (t (propertize status 'face 'bv-themes-header-popout))))
         (left (concat
                (propertize " " 'face 'bv-themes-header-default
                            'display `(raise ,space-up))
                (propertize name 'face 'bv-themes-header-strong)
                (propertize " " 'face 'bv-themes-header-default
                            'display `(raise ,space-down))
                (propertize primary 'face 'bv-themes-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width)
                             edge-pad-width edge-pad-width
                             (length prefix) (length left) (length right)
                             (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat edge-pad
            prefix
            left
            (propertize (make-string available-width ?\ )
                        'face 'bv-themes-header-default)
            (propertize right 'face 'bv-themes-header-default)
            edge-pad)))

(defun bv-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)."
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

(defun bv-modeline-default-mode ()
  "Default modeline for `prog-mode' and `text-mode'."
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (bv-mode-name))
        (branch      (vc-branch))
        (position    (format-mode-line "%l:%c"))
        (time-str    (when (and (boundp 'display-time-mode)
                                display-time-mode
                                (boundp 'display-time-string))
                       display-time-string)))
    (bv-modeline-compose (bv-modeline-status)
                         buffer-name
                         (concat "(" mode-name
                                 (if branch (concat ", "
                                                    (propertize branch 'face 'italic)))
                                 ")")
                         (if time-str
                             (concat position " | " time-str)
                           position))))

(defun bv-modeline-org-agenda-mode-p ()
  "Return non-nil if current buffer is in `org-agenda-mode'."
  (derived-mode-p 'org-agenda-mode))

(defun bv-modeline-org-agenda-mode ()
  "Modeline format for `org-agenda-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Agenda"
                       ""
                       (format-time-string "%A %-e %B %Y")))

(defun bv-modeline-org-capture-mode-p ()
  "Return non-nil if current buffer is in `org-capture-mode'."
  (bound-and-true-p org-capture-mode))

(defun bv-modeline-org-capture-mode ()
  "Modeline format for `org-capture-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Capture"
                       "(org)"
                       ""))

(defun bv-modeline-term-mode-p ()
  "Return non-nil if current buffer is in `term-mode'."
  (derived-mode-p 'term-mode))

(defun bv-modeline-vterm-mode-p ()
  "Return non-nil if current buffer is in `vterm-mode'."
  (derived-mode-p 'vterm-mode))

(defun bv-modeline-term-mode ()
  "Modeline format for terminal modes (`term-mode' and `vterm-mode')."
  (bv-modeline-compose " >_ "
                       "Terminal"
                       (concat "(" shell-file-name ")")
                       (shorten-directory default-directory 32)))

(defun bv-modeline-message-mode-p ()
  "Return non-nil if current buffer is in `message-mode'."
  (derived-mode-p 'message-mode))

(defun bv-modeline-message-mode ()
  "Modeline format for `message-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Message" "(draft)" ""))

(defun bv-modeline-info-mode-p ()
  "Return non-nil if current buffer is in `Info-mode'."
  (derived-mode-p 'Info-mode))

(defun bv-modeline-info-mode ()
  "Modeline format for `Info-mode'."
  (bv-modeline-compose (bv-modeline-status)
                       "Info"
                       (concat "(" Info-current-node ")")
                       ""))

(defun bv-modeline-calendar-mode-p ()
  "Return non-nil if current buffer is in `calendar-mode'."
  (derived-mode-p 'calendar-mode))

(defun bv-modeline-calendar-mode ()
  "Empty modeline for calendar."
  "")

(defun bv-modeline-completion-list-mode-p ()
  "Return non-nil if current buffer is in `completion-list-mode'."
  (derived-mode-p 'completion-list-mode))

(defun bv-modeline-completion-list-mode ()
  "Modeline format for `completion-list-mode'."
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (bv-mode-name))
        (position    (format-mode-line "%l:%c")))
    (bv-modeline-compose (bv-modeline-status)
                         buffer-name "" position)))

(defun bv-modeline-prog-mode-p ()
  "Return non-nil if current buffer is in `prog-mode'."
  (derived-mode-p 'prog-mode))

(defun bv-modeline-text-mode-p ()
  "Return non-nil if current buffer is in `text-mode'."
  (derived-mode-p 'text-mode))

(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            (lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun bv-modeline-org-clock-mode-p ()
  "Return non-nil if org-clock is active."
  org-mode-line-string)

(defun bv-modeline-org-clock-mode ()
  "Modeline format when org-clock is active."
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (bv-mode-name))
        (branch      (vc-branch)))
    (bv-modeline-compose (bv-modeline-status)
                         buffer-name
                         (concat "(" mode-name
                                 (if branch (concat ", "
                                                    (propertize branch 'face 'italic)))
                                 ")")
                         org-mode-line-string)))


(defun bv-modeline ()
  "Install a header line whose content is dependent on the major mode."
  (interactive)
  (setq-default header-line-format
                '((:eval
                   (cond ((bv-modeline-prog-mode-p)            (bv-modeline-default-mode))
                         ((bv-modeline-message-mode-p)         (bv-modeline-message-mode))
                         ((bv-modeline-info-mode-p)            (bv-modeline-info-mode))
                         ((bv-modeline-calendar-mode-p)        (bv-modeline-calendar-mode))
                         ((bv-modeline-org-capture-mode-p)     (bv-modeline-org-capture-mode))
                         ((bv-modeline-org-agenda-mode-p)      (bv-modeline-org-agenda-mode))
                         ((bv-modeline-org-clock-mode-p)       (bv-modeline-org-clock-mode))
                         ((bv-modeline-term-mode-p)            (bv-modeline-term-mode))
                         ((bv-modeline-vterm-mode-p)           (bv-modeline-term-mode))
                         ((bv-modeline-text-mode-p)            (bv-modeline-default-mode))
                         ((bv-modeline-completion-list-mode-p) (bv-modeline-completion-list-mode))
                         (t                                    (bv-modeline-default-mode)))))))

(defun bv-modeline-update-windows ()
  "Modify the mode line depending on window configuration."
  (dolist (window (window-list))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (set-window-parameter window 'mode-line-format
                                  (cond ((not mode-line-format) 'none)
                                        ((one-window-p t 'visible) (list ""))
                                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                                        ((not (window-in-direction 'below)) (list ""))
                                        (t 'none))))))))

(add-hook 'window-configuration-change-hook 'bv-modeline-update-windows)

(setq eshell-status-in-modeline nil)
(setq-default mode-line-format nil)

(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

(setq Info-use-header-line nil)

(provide 'bv-modeline)
;;; bv-modeline.el ends here
