;;; bv-all-the-icons.el --- Icon support configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Icon support for completion frameworks and dired.
;; Revised version with more robust fontset configuration.

;;; Code:

(defgroup bv-all-the-icons nil
  "Icon configuration."
  :group 'bv)

(defcustom bv-all-the-icons-scale-factor 0.9
  "Scale factor for all icons."
  :type 'number
  :group 'bv-all-the-icons)

(defcustom bv-all-the-icons-enable-in-terminal nil
  "Enable icons in terminal mode (may require special fonts)."
  :type 'boolean
  :group 'bv-all-the-icons)

(defvar bv-all-the-icons-enabled nil
  "Whether all-the-icons is properly configured.")

(defvar bv-all-the-icons--fontset-configured nil
  "Whether fontsets have been configured.")

(defun bv-all-the-icons-available-p ()
  "Check if all-the-icons fonts are available."
  (or (and (display-graphic-p)
           (or (find-font (font-spec :name "all-the-icons"))
               (find-font (font-spec :name "FontAwesome"))
               (find-font (font-spec :name "github-octicons"))))
      (and bv-all-the-icons-enable-in-terminal
           (not (display-graphic-p)))))

;; Load all-the-icons if available
(when (bv-all-the-icons-available-p)
  (require 'all-the-icons nil t)
  (setq bv-all-the-icons-enabled t)

  (when (boundp 'all-the-icons-scale-factor)
    (setq all-the-icons-scale-factor bv-all-the-icons-scale-factor))
  (when (boundp 'all-the-icons-default-adjust)
    (setq all-the-icons-default-adjust -0.1))
  (when (boundp 'all-the-icons-octicon-scale-factor)
    (setq all-the-icons-octicon-scale-factor 0.85))
  (when (boundp 'all-the-icons-color-icons)
    (setq all-the-icons-color-icons nil)))


(defun bv-all-the-icons-setup-fontsets (&optional frame)
  "Setup fontsets for icon display.
If FRAME is provided, setup fontsets for that frame."
  (when (display-graphic-p frame)
    (let ((target-fontset (if frame
                              (frame-parameter frame 'font)
                            "fontset-default")))
      ;; More aggressive fontset configuration
      ;; Use both the frame-specific and default fontsets
      (dolist (fontset (list target-fontset "fontset-default" t))
        (when fontset
          ;; all-the-icons
          (when (find-font (font-spec :name "all-the-icons"))
            (ignore-errors
              (set-fontset-font fontset '(#xe000 . #xf8ff)
                                "all-the-icons" frame 'prepend)))

          ;; FontAwesome - try multiple names
          (let ((fa-fonts '("FontAwesome" "Font Awesome 5 Free" "Font Awesome 6 Free"))
                (found nil))
            (dolist (fa-font fa-fonts)
              (when (and (not found) (find-font (font-spec :name fa-font)))
                (ignore-errors
                  (set-fontset-font fontset '(#xf000 . #xf2e0)
                                    fa-font frame 'prepend))
                (setq found t))))

          ;; GitHub Octicons - try multiple names
          (let ((octicon-fonts '("github-octicons" "octicons"))
                (found nil))
            (dolist (octicon-font octicon-fonts)
              (when (and (not found) (find-font (font-spec :name octicon-font)))
                (ignore-errors
                  (set-fontset-font fontset '(#xf400 . #xf4e7)
                                    octicon-font frame 'prepend))
                (setq found t))))

          ;; file-icons
          (when (find-font (font-spec :name "file-icons"))
            (ignore-errors
              (set-fontset-font fontset '(#xe900 . #xe9ff)
                                "file-icons" frame 'prepend)))

          ;; Material Icons
          (when (find-font (font-spec :name "Material Icons"))
            (ignore-errors
              (set-fontset-font fontset '(#xe000 . #xebff)
                                "Material Icons" frame 'prepend))))))

    (setq bv-all-the-icons--fontset-configured t)))

(defun bv-all-the-icons-force-fontset-reload ()
  "Force reload of fontsets for all frames."
  (interactive)
  (dolist (frame (frame-list))
    (when (display-graphic-p frame)
      (bv-all-the-icons-setup-fontsets frame)))
  (bv-all-the-icons-setup-fontsets)
  (message "Fontsets reloaded for all frames"))

;; Setup fontsets on various hooks to ensure they're applied
(defun bv-all-the-icons-init ()
  "Initialize all-the-icons with proper timing."
  (when (display-graphic-p)
    (bv-all-the-icons-setup-fontsets)))

;; Hook into frame creation
(add-hook 'after-make-frame-functions #'bv-all-the-icons-setup-fontsets)

;; Hook into various initialization points
(add-hook 'after-init-hook #'bv-all-the-icons-init)
(add-hook 'window-setup-hook #'bv-all-the-icons-init)

;; For daemon mode
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'bv-all-the-icons-init)
  ;; For regular startup, configure immediately if possible
  (when (display-graphic-p)
    (run-with-idle-timer 0.1 nil #'bv-all-the-icons-init)))

;; Fix icon detection for directories and hidden files
(defun bv-all-the-icons-fix-icon-detection ()
  "Fix icon detection issues for directories and dotfiles."
  (when (fboundp 'all-the-icons-icon-for-file)
    ;; Override icon for dotfiles to avoid print icon
    (advice-add 'all-the-icons-icon-for-file :around
                (lambda (orig-fun file &rest args)
                  (if (and (stringp file)
                           (string-match-p "^\\." (file-name-nondirectory file))
                           (not (file-directory-p file)))
                      ;; Use generic file icon for dotfiles
                      (all-the-icons-faicon "file-o"
                                            :height (or (plist-get args :height) 1.0)
                                            :v-adjust (or (plist-get args :v-adjust) 0.0))
                    (apply orig-fun file args))))))

;; Lazy load icon packages when needed
(with-eval-after-load 'vertico
  (when bv-all-the-icons-enabled
    (require 'all-the-icons-completion nil t)
    (when (fboundp 'all-the-icons-completion-mode)
      (all-the-icons-completion-mode 1)
      ;; Ensure fontsets are configured after mode is enabled
      (bv-all-the-icons-force-fontset-reload)
      ;; Fix icon detection
      (bv-all-the-icons-fix-icon-detection))))

(with-eval-after-load 'marginalia
  (when (and bv-all-the-icons-enabled
             (fboundp 'all-the-icons-completion-marginalia-setup))
    (all-the-icons-completion-marginalia-setup)
    ;; Ensure directories show folder icons in marginalia
    (when (boundp 'marginalia-annotators-heavy)
      (setf (alist-get 'file marginalia-annotators-heavy)
            '(marginalia-annotate-file)))))

(with-eval-after-load 'dired
  (when bv-all-the-icons-enabled
    (require 'all-the-icons-dired nil t)
    (when (fboundp 'all-the-icons-dired-mode)
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
      ;; Ensure dired uses correct icons
      (setq all-the-icons-dired-monochrome nil))))

(defun bv-all-the-icons-install-fonts ()
  "Install all-the-icons fonts if not already installed."
  (interactive)
  (when (fboundp 'all-the-icons-install-fonts)
    (all-the-icons-install-fonts t)
    (message "Fonts installed. Please restart Emacs for changes to take effect.")))

(defun bv-all-the-icons-test ()
  "Test if icons are working correctly."
  (interactive)
  (if (fboundp 'all-the-icons-octicon)
      (let ((msg (format "Icons test: %s %s %s %s | Font: %s"
                         (all-the-icons-octicon "file-text")
                         (all-the-icons-faicon "folder")
                         (all-the-icons-material "folder")
                         (all-the-icons-fileicon "elisp")
                         (face-attribute 'default :family))))
        (message "%s" msg)
        (with-current-buffer (get-buffer-create "*icon-test*")
          (erase-buffer)
          (insert msg "\n\n")
          (insert "Octicon file-text: " (all-the-icons-octicon "file-text") "\n")
          (insert "Faicon folder: " (all-the-icons-faicon "folder") "\n")
          (insert "Material folder: " (all-the-icons-material "folder") "\n")
          (insert "Fileicon elisp: " (all-the-icons-fileicon "elisp") "\n\n")
          (insert "If icons show as empty boxes, run M-x bv-all-the-icons-force-fontset-reload\n")
          (pop-to-buffer (current-buffer))))
    (message "all-the-icons functions not available")))

(defun bv-all-the-icons-debug ()
  "Debug icon display issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*icon-debug*")
    (erase-buffer)
    (insert "=== all-the-icons Debug Information ===\n\n")

    (insert "Display type: " (if (display-graphic-p) "GUI" "Terminal") "\n")
    (insert "Fontset configured: " (if bv-all-the-icons--fontset-configured "Yes" "No") "\n\n")

    (insert "Font availability:\n")
    (dolist (font '("all-the-icons" "github-octicons" "octicons"
                    "FontAwesome" "Font Awesome 5 Free" "Font Awesome 6 Free"
                    "file-icons" "Material Icons"))
      (insert (format "  %s: %s\n" font
                      (if (find-font (font-spec :name font)) "Found" "Not found"))))

    (insert "\nDirect character test:\n")
    (insert "  FontAwesome folder: " (propertize "\xf07b" 'face '(:family "FontAwesome")) "\n")
    (insert "  Octicons file: " (propertize "\xf408" 'face '(:family "github-octicons")) "\n")
    (insert "  All-the-icons: " (propertize "\xe907" 'face '(:family "all-the-icons")) "\n")

    (insert "\nFontset information:\n")
    (let ((char-fa #xf07b)    ; FontAwesome folder
          (char-oct #xf408)   ; Octicons file
          (char-ati #xe907))  ; all-the-icons
      (insert (format "  FontAwesome char %X: %S\n" char-fa
                      (ignore-errors (fontset-font "fontset-default" char-fa))))
      (insert (format "  Octicons char %X: %S\n" char-oct
                      (ignore-errors (fontset-font "fontset-default" char-oct))))
      (insert (format "  All-the-icons char %X: %S\n" char-ati
                      (ignore-errors (fontset-font "fontset-default" char-ati)))))

    (insert "\nTroubleshooting:\n")
    (insert "1. Run M-x bv-all-the-icons-force-fontset-reload\n")
    (insert "2. If that doesn't work, restart Emacs\n")
    (insert "3. If still broken, run M-x bv-all-the-icons-install-fonts\n")
    (insert "4. Check that your main font (Fira Code) isn't claiming icon ranges\n")

    (pop-to-buffer (current-buffer))))

;; Dashboard configuration
(with-eval-after-load 'dashboard
  (when (and (boundp 'dashboard-set-file-icons)
             (not (display-graphic-p)))
    (setq dashboard-set-file-icons nil)))

;; Provide install instructions if needed
(unless (or bv-all-the-icons-enabled
            (not (display-graphic-p)))
  (run-with-idle-timer
   2 nil
   (lambda ()
     (message "all-the-icons: Fonts may need installation. Run M-x bv-all-the-icons-install-fonts"))))

;; Immediately configure fontsets if in GUI mode
(when (and (display-graphic-p) bv-all-the-icons-enabled)
  (bv-all-the-icons-setup-fontsets))

(provide 'bv-all-the-icons)
;;; bv-all-the-icons.el ends here
