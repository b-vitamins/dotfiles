;;; bv-geiser.el --- Scheme development configuration -*- lexical-binding: t -*-

;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:
;; Scheme development with Geiser and Guile.

;;; Code:


(declare-function geiser-mode "geiser-mode")
(declare-function geiser-eros-mode "geiser-eros")
(declare-function geiser-repl "geiser-repl")
(declare-function geiser-eval-definition "geiser-mode")

(defgroup bv-geiser nil
  "Scheme development settings."
  :group 'bv)

(defcustom bv-geiser-idle-delay 1.0
  "Idle time before loading geiser."
  :type 'number
  :group 'bv-geiser)

(defcustom bv-geiser-default-implementation 'guile
  "Default Scheme implementation."
  :type 'symbol
  :group 'bv-geiser)

;; Load geiser after idle delay
(run-with-idle-timer bv-geiser-idle-delay t
                     (lambda ()
                       (require 'geiser nil t)))

(setq geiser-default-implementation bv-geiser-default-implementation
      geiser-active-implementations '(guile)
      geiser-repl-query-on-kill-p nil
      geiser-repl-history-filename (expand-file-name "emacs/geiser-history" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
      geiser-repl-add-project-paths nil)

(with-eval-after-load 'geiser-mode
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-hook 'geiser-mode-hook 'geiser-eros-mode))

(defun bv-geiser-eval-buffer ()
  "Evaluate entire buffer in Geiser."
  (interactive)
  (geiser-eval-buffer))

(defun bv-geiser-eval-last-sexp ()
  "Evaluate last s-expression."
  (interactive)
  (geiser-eval-last-sexp))

(defun bv-geiser-repl-here ()
  "Start Geiser REPL in current window."
  (interactive)
  (let ((geiser-repl-window-allow-split nil))
    (geiser-repl)))

(with-eval-after-load 'geiser-mode
  (define-key geiser-mode-map (kbd "C-c C-b") 'bv-geiser-eval-buffer)
  (define-key geiser-mode-map (kbd "C-c C-z") 'bv-geiser-repl-here))

(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (require 'ob-scheme))

(with-eval-after-load 'ob-scheme
  (setq org-babel-default-header-args:scheme '((:results . "value"))))

(defun bv-geiser-transient ()
  "Transient menu for Geiser."
  (interactive)
  (transient-define-prefix bv-geiser-transient-menu ()
    "Scheme Development"
    ["Evaluation"
     ("e" "Eval definition" geiser-eval-definition)
     ("r" "Eval region" geiser-eval-region)
     ("b" "Eval buffer" bv-geiser-eval-buffer)
     ("l" "Eval last sexp" bv-geiser-eval-last-sexp)]
    ["REPL"
     ("z" "Switch to REPL" geiser-repl)
     ("Z" "REPL here" bv-geiser-repl-here)
     ("c" "Clear REPL" geiser-repl-clear-buffer)]
    ["Documentation"
     ("d" "Symbol doc" geiser-doc-symbol-at-point)
     ("m" "Module doc" geiser-doc-module)])
  (bv-geiser-transient-menu))

(with-eval-after-load 'scheme-mode
  (define-key scheme-mode-map (kbd "C-c s") 'bv-geiser-transient))

(provide 'bv-geiser)
;;; bv-geiser.el ends here