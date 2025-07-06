;;; bv-all-the-icons.el --- Icon support configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Icon support for completion frameworks and dired.

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

(defun bv-all-the-icons-available-p ()
  "Check if all-the-icons fonts are available."
  (or (and (display-graphic-p)
           (or (find-font (font-spec :name "all-the-icons"))
               (find-font (font-spec :name "FontAwesome"))
               (find-font (font-spec :name "github-octicons"))))
      (and bv-all-the-icons-enable-in-terminal
           (not (display-graphic-p)))))

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

(defun bv-all-the-icons-setup-faces ()
  "Apply muted colors to icons matching theme."
  (when (fboundp 'all-the-icons-faicon)
    (set-face-attribute 'all-the-icons-red nil
                        :foreground bv-color-critical)
    (set-face-attribute 'all-the-icons-lred nil
                        :foreground bv-color-critical)
    (set-face-attribute 'all-the-icons-dred nil
                        :foreground bv-color-critical)
    (set-face-attribute 'all-the-icons-green nil
                        :foreground bv-color-salient)
    (set-face-attribute 'all-the-icons-lgreen nil
                        :foreground bv-color-salient)
    (set-face-attribute 'all-the-icons-dgreen nil
                        :foreground bv-color-salient)
    (set-face-attribute 'all-the-icons-blue nil
                        :foreground bv-color-salient)
    (set-face-attribute 'all-the-icons-lblue nil
                        :foreground bv-color-salient)
    (set-face-attribute 'all-the-icons-dblue nil
                        :foreground bv-color-faded)
    (set-face-attribute 'all-the-icons-yellow nil
                        :foreground bv-color-popout)
    (set-face-attribute 'all-the-icons-orange nil
                        :foreground bv-color-popout)
    (set-face-attribute 'all-the-icons-dorange nil
                        :foreground bv-color-popout)
    (set-face-attribute 'all-the-icons-purple nil
                        :foreground bv-color-faded)
    (set-face-attribute 'all-the-icons-maroon nil
                        :foreground bv-color-critical)))

(defun bv-all-the-icons-setup-fontsets ()
  "Setup fontsets for icon display."
  ;; Define specific unicode ranges for icon fonts
  (when (find-font (font-spec :name "all-the-icons"))
    (set-fontset-font t '(#xe000 . #xf8ff) "all-the-icons"))
  (when (find-font (font-spec :name "github-octicons"))
    (set-fontset-font t '(#xf400 . #xf4e7) "github-octicons"))
  (when (find-font (font-spec :name "FontAwesome"))
    (set-fontset-font t '(#xf000 . #xf2e0) "FontAwesome"))
  (when (find-font (font-spec :name "file-icons"))
    (set-fontset-font t '(#xe900 . #xe9ff) "file-icons"))
  (when (find-font (font-spec :name "Material Icons"))
    (set-fontset-font t '(#xe000 . #xebff) "Material Icons")))

(add-hook 'after-init-hook #'bv-all-the-icons-setup-faces)
(add-hook 'after-init-hook #'bv-all-the-icons-setup-fontsets)
(add-hook 'bv-after-theme-hook #'bv-all-the-icons-setup-faces)

;; Initialize fontsets immediately
(when (display-graphic-p)
  (bv-all-the-icons-setup-fontsets))

;; Lazy load icon packages when needed
(with-eval-after-load 'vertico
  (when bv-all-the-icons-enabled
    (require 'all-the-icons-completion nil t)
    (when (fboundp 'all-the-icons-completion-mode)
      (all-the-icons-completion-mode 1))))

(with-eval-after-load 'marginalia
  (when (and bv-all-the-icons-enabled
             (fboundp 'all-the-icons-completion-marginalia-setup))
    (all-the-icons-completion-marginalia-setup)))

(with-eval-after-load 'dired
  (when bv-all-the-icons-enabled
    (require 'all-the-icons-dired nil t)
    (when (fboundp 'all-the-icons-dired-mode)
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

(defun bv-all-the-icons-install-fonts ()
  "Install all-the-icons fonts if not already installed."
  (interactive)
  (when (and (fboundp 'all-the-icons-install-fonts)
             (not (find-font (font-spec :name "all-the-icons"))))
    (all-the-icons-install-fonts t)))

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
          (insert "Fileicon elisp: " (all-the-icons-fileicon "elisp") "\n")
          (pop-to-buffer (current-buffer))))
    (message "all-the-icons functions not available")))

(defun bv-all-the-icons-debug ()
  "Debug icon display issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*icon-debug*")
    (erase-buffer)
    (insert "Font availability:\n")
    (dolist (font '("all-the-icons" "github-octicons" "FontAwesome" "file-icons" "Material Icons"))
      (insert (format "  %s: %s\n" font (if (find-font (font-spec :name font)) "Found" "Not found"))))
    (insert "\nDirect character test:\n")
    (insert "  FontAwesome folder: " (propertize "\xf07b" 'face '(:family "FontAwesome")) "\n")
    (insert "  Octicons file: " (propertize "\xf408" 'face '(:family "github-octicons")) "\n")
    (insert "  All-the-icons: " (propertize "\xe907" 'face '(:family "all-the-icons")) "\n")
    (pop-to-buffer (current-buffer))))

;; Try alternative dashboard configuration
(with-eval-after-load 'dashboard
  (when (and (boundp 'dashboard-set-file-icons)
             (not (display-graphic-p)))
    (setq dashboard-set-file-icons nil)))

;; Inform user about font installation if needed
(unless (or bv-all-the-icons-enabled
            (not (display-graphic-p)))
  (message "all-the-icons: Fonts not found. Run M-x bv-all-the-icons-install-fonts to install them."))

(provide 'bv-all-the-icons)
;;; bv-all-the-icons.el ends here