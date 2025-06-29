;;; bv-nov-el.el --- Nov.el EPUB reader configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Configuration for nov.el EPUB reader with text justification.

;;; Code:


(autoload 'olivetti-mode "olivetti")
(autoload 'pj-line-width "justify-kp")
(autoload 'shr-pixel-column "shr")
(autoload 'pj-justify "justify-kp")

(when (boundp 'auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(defun bv-nov-post-html-render-hook ()
  "Justify text in nov mode after HTML rendering."
  (if (get-buffer-window)
      (let ((max-width (pj-line-width))
            buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]-]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
    (add-hook 'window-configuration-change-hook
              'bv-nov-window-configuration-change-hook
              nil
              t)))

(defun bv-nov-window-configuration-change-hook ()
  "Handle window configuration change for nov mode text justification."
  (bv-nov-post-html-render-hook)
  (remove-hook 'window-configuration-change-hook
               'bv-nov-window-configuration-change-hook
               t))

(with-eval-after-load 'nov
  (when (boundp 'nov-mode-hook)
    (add-hook 'nov-mode-hook (lambda () (olivetti-mode 1))))
  (when (boundp 'nov-text-width)
    (setq nov-text-width t))
  (when (boundp 'nov-post-html-render-hook)
    (add-hook 'nov-post-html-render-hook 'bv-nov-post-html-render-hook)))

(provide 'bv-nov-el)
;;; bv-nov-el.el ends here