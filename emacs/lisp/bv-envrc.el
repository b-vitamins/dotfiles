;;; bv-envrc.el --- Direnv/envrc integration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Project environment syncing via direnv + envrc.
;; Keeps `exec-path' and `process-environment' aligned with per-project
;; toolchains (Python venvs, Rust toolchains, Guix shells, etc.).

;;; Code:

(defgroup bv-envrc nil
  "Direnv/envrc integration."
  :group 'bv
  :prefix "bv-envrc-")

(defcustom bv-envrc-enable t
  "Whether to enable envrc integration when available."
  :type 'boolean
  :group 'bv-envrc)

(defun bv-envrc--maybe-enable ()
  "Enable envrc if it is available and direnv is installed."
  (when (and bv-envrc-enable
             (require 'envrc nil t)
             (executable-find "direnv"))
    (envrc-global-mode 1)
    (with-eval-after-load 'project
      (when (and (boundp 'project-prefix-map)
                 (boundp 'envrc-command-map))
        ;; Project-local envrc actions under `C-x p E`.
        (define-key project-prefix-map (kbd "E") envrc-command-map)))))

(bv-envrc--maybe-enable)

(provide 'bv-envrc)
;;; bv-envrc.el ends here
