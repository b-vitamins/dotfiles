;;; bv-corfu.el --- Lightning-fast Corfu with full Cape integration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (corfu "1.0") (cape "2.0"))

;;; Commentary:

;; Optimized Corfu configuration with full Cape integration, safe caching,
;; lazy loading, and performance-focused design following documented best practices.
;;
;; Features:
;; - Full Cape integration with icon support
;; - Bounded caching with periodic cleanup
;; - Mode-specific settings with Cape awareness
;; - Smart separator handling for multi-word completions
;; - Synchronized auto-completion settings

;;; Code:

(require 'corfu)
(require 'cl-lib)

;;; External Variables

(defvar savehist-additional-variables)
(defvar completion-at-point-functions)
(defvar completion-in-region--data)
(defvar completion-extra-properties)
(defvar corfu-history--hash)
(defvar corfu-excluded-modes)
(defvar corfu-preserve-symlinks)
(defvar corfu-history-duplicate)
(defvar corfu-history-decay)
(defvar corfu-popupinfo-delay)
(defvar corfu-popupinfo-max-width)
(defvar corfu-popupinfo-max-height)
(defvar corfu-popupinfo-min-width)
(defvar corfu-popupinfo-min-height)
(defvar corfu-popupinfo-resize)
(defvar corfu-popupinfo-hide)
(defvar corfu-popupinfo--frame)
(defvar corfu-echo-delay)
(defvar corfu-quick1)
(defvar corfu-quick2)
(defvar corfu-map)
(defvar read-passwd-map)
(defvar corfu--index)

;;; External Functions

(declare-function corfu-history-mode "corfu-history" (&optional arg))
(declare-function corfu-popupinfo-mode "corfu-popupinfo" (&optional arg))
(declare-function corfu-echo-mode "corfu-echo" (&optional arg))
(declare-function corfu-info-documentation "corfu-info" ())
(declare-function corfu-info-location "corfu-info" ())
(declare-function corfu-quick-complete "corfu-quick" ())
(declare-function corfu-quick-insert "corfu-quick" ())
(declare-function consult-completion-in-region "consult" (start end collection &optional predicate))
(declare-function bv-cape-clear-caches "bv-cape" ())

;;; Custom Variables

(defcustom bv-completion-auto-modes '(prog-mode text-mode)
  "Modes where auto-completion is enabled."
  :type '(repeat symbol)
  :group 'corfu)

;;; Performance Variables with Bounded Caching

(defvar bv-corfu--kind-cache (make-hash-table :test #'equal :size 128)
  "Bounded cache for kind lookups.")

(defvar bv-corfu--kind-cache-max-size 256
  "Maximum cache size before cleanup.")

(defvar bv-corfu--separator-char 32  ; Space character code
  "Cached separator character code for fast lookup.")

;; History caching
(defvar bv-corfu--history-update-timer nil
  "Timer for deferred history hash update.")

;;; Face Definitions - Set once at load time

(custom-set-faces
 '(corfu-default ((t :background "#1a1b26")))
 '(corfu-current ((t :background "#283457" :weight semibold)))
 '(corfu-bar ((t :background "#7aa2f7")))
 '(corfu-border ((t :background "#3b4261")))
 '(corfu-annotations ((t :foreground "#565f89" :slant italic)))
 '(corfu-deprecated ((t :foreground "#f7768e" :strike-through t)))
 ;; Popupinfo faces
 '(corfu-popupinfo ((t :background "#1f2335" :foreground "#c0caf5")))
 ;; Echo face
 '(corfu-echo ((t :foreground "#9ece6a" :slant italic))))

;;; Core Settings - Following documented defaults with Cape awareness

(setq corfu-min-width 20
      corfu-max-width 100
      corfu-count 10
      corfu-scroll-margin 2
      corfu-cycle t
      corfu-auto t                    ; Off by default for safety
      corfu-auto-delay 0.2               ; Recommended delay
      corfu-auto-prefix 3                ; Documented minimum
      corfu-separator ?\s
      corfu-quit-at-boundary 'separator
      corfu-quit-no-match 'separator
      corfu-preview-current t
      corfu-on-exact-match nil
      corfu-preselect 'valid
      ;; Cape-specific settings
      corfu-excluded-modes '(gud-mode)
      corfu-preserve-symlinks t)

;;; Icon Configuration with Cape Support

(defconst bv-corfu--icons
  '(;; Cape-specific icons
    (cape-abbrev "a" font-lock-string-face)
    (cape-dabbrev "d" font-lock-comment-face)
    (cape-dict "w" font-lock-doc-face)
    (cape-history "h" font-lock-constant-face)
    (cape-keyword "kw" font-lock-keyword-face)
    (cape-yasnippet "y" font-lock-preprocessor-face)
    (project-file "P" font-lock-constant-face)
    ;; Standard kinds
    (array "[]" font-lock-type-face)
    (boolean "◉" font-lock-builtin-face)
    (class "C" font-lock-type-face)
    (color "#" success)
    (constant "π" font-lock-constant-face)
    (constructor "c" font-lock-function-name-face)
    (enum "e" font-lock-builtin-face)
    (enum-member "em" font-lock-builtin-face)
    (event "E" font-lock-warning-face)
    (field "f" font-lock-variable-name-face)
    (file "F" font-lock-string-face)
    (folder "D" font-lock-doc-face)
    (function "λ" font-lock-function-name-face)
    (interface "I" font-lock-type-face)
    (keyword "k" font-lock-keyword-face)
    (method "m" font-lock-function-name-face)
    (module "M" font-lock-type-face)
    (namespace "N" font-lock-type-face)
    (null "∅" font-lock-comment-face)
    (number "n" font-lock-builtin-face)
    (operator "○" font-lock-comment-delimiter-face)
    (package "P" font-lock-builtin-face)
    (property "p" font-lock-variable-name-face)
    (reference "r" font-lock-doc-face)
    (snippet "S" font-lock-string-face)
    (string "s" font-lock-string-face)
    (struct "%" font-lock-type-face)
    (text "·" shadow)
    (type "T" font-lock-type-face)
    (type-parameter "tp" font-lock-type-face)
    (unit "u" shadow)
    (value "v" font-lock-builtin-face)
    (variable "x" font-lock-variable-name-face)
    (t "?" shadow))  ; Default fallback
  "Icon specifications by kind.")

(defun bv-corfu--format-candidate (cand)
  "Format CAND with icon, using bounded cache and Cape support."
  (condition-case nil
      (if (stringp cand)
          (let* ((cache-key (substring-no-properties cand 0 (min 20 (length cand))))
                 (cached-kind (gethash cache-key bv-corfu--kind-cache 'miss)))
            (when (eq cached-kind 'miss)
              ;; Check cache size and clean if needed
              (when (> (hash-table-count bv-corfu--kind-cache)
                       bv-corfu--kind-cache-max-size)
                (clrhash bv-corfu--kind-cache))
              ;; Get kind from text properties, including Cape properties
              (setq cached-kind
                    (or (get-text-property 0 'corfu-kind cand)
                        (plist-get (text-properties-at 0 cand) 'cape-capf-super-kind)
                        (get-text-property 0 'cape-category cand)
                        (and (get-text-property 0 'category cand)
                             (get-text-property 0 'category cand))
                        t))
              (puthash cache-key cached-kind bv-corfu--kind-cache))
            ;; Format with icon
            (let ((icon-spec (or (assq cached-kind bv-corfu--icons)
                                 (assq t bv-corfu--icons))))
              (concat (propertize (cadr icon-spec) 'face (caddr icon-spec)) " ")))
        "   ")
    (error "   ")))  ; Fallback on any error

(defun bv-corfu-margin-formatter (metadata)
  "Safe margin formatter with error handling.
METADATA contains completion metadata used for formatting."
  (when metadata
    #'bv-corfu--format-candidate))

;; Install formatter
(with-eval-after-load 'corfu
  (add-to-list 'corfu-margin-formatters #'bv-corfu-margin-formatter))

;;; History Support with Deferred Updates

(defun bv-corfu--invalidate-history-cache ()
  "Invalidate history cache with debouncing."
  (when bv-corfu--history-update-timer
    (cancel-timer bv-corfu--history-update-timer))
  (setq bv-corfu--history-update-timer
        (run-with-idle-timer 0.5 nil
                             (lambda ()
                               (when (boundp 'corfu-history--hash)
                                 (setq corfu-history--hash nil))))))

(defun bv-corfu--setup-history ()
  "Setup history mode with reasonable defaults."
  (require 'corfu-history)

  ;; Use reasonable settings for performance
  (setq corfu-history-duplicate 10
        corfu-history-decay 10)

  ;; Setup savehist integration
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables)
    ;; Limit history length for performance
    (put 'corfu-history 'history-length 500))

  (corfu-history-mode 1))

;;; Popupinfo Support with Bounded Cache

(defvar bv-corfu--popupinfo-cache (make-hash-table :test #'equal :size 64)
  "Bounded cache for documentation strings.")

(defvar bv-corfu--popupinfo-cache-max-size 128
  "Maximum popupinfo cache size.")

(defun bv-corfu--setup-popupinfo ()
  "Setup popupinfo mode with caching."
  (require 'corfu-popupinfo)

  (setq corfu-popupinfo-delay '(1.0 . 0.3)  ; Conservative delays
        corfu-popupinfo-max-width 70
        corfu-popupinfo-max-height 20
        corfu-popupinfo-min-width 30
        corfu-popupinfo-min-height 3
        corfu-popupinfo-resize t
        corfu-popupinfo-hide nil)

  ;; Add bounded cache to documentation retrieval
  (advice-add 'corfu-popupinfo--get-documentation :around
              #'bv-corfu--popupinfo-doc-cache)

  (corfu-popupinfo-mode 1))

(defun bv-corfu--popupinfo-doc-cache (orig-fun candidate)
  "Cache documentation with bounds checking.
ORIG-FUN is the original documentation function.
CANDIDATE is the completion candidate to get documentation for."
  (condition-case nil
      (or (gethash candidate bv-corfu--popupinfo-cache)
          (when-let ((doc (funcall orig-fun candidate)))
            ;; Check cache size
            (when (> (hash-table-count bv-corfu--popupinfo-cache)
                     bv-corfu--popupinfo-cache-max-size)
              (clrhash bv-corfu--popupinfo-cache))
            (puthash candidate doc bv-corfu--popupinfo-cache)
            doc))
    (error nil)))  ; Return nil on error

;;; Echo Support with Smart Integration

(defun bv-corfu--setup-echo ()
  "Setup echo mode with popupinfo awareness."
  (require 'corfu-echo)

  (setq corfu-echo-delay '(1.0 . 0.3))

  ;; Show echo only when popupinfo is not visible
  (advice-add 'corfu-echo--exhibit :before-while
              #'bv-corfu--echo-check-popupinfo)

  (corfu-echo-mode 1))

(defun bv-corfu--echo-check-popupinfo (&rest _)
  "Only show echo if popupinfo is not visible."
  (not (and (bound-and-true-p corfu-popupinfo-mode)
            (bound-and-true-p corfu-popupinfo--frame)
            (frame-visible-p corfu-popupinfo--frame))))

;;; Smart Completion Functions with Cape Support

(defsubst bv-corfu--at-separator-p ()
  "Check if at separator character."
  (and (> (point) (point-min))
       (= (char-before) bv-corfu--separator-char)))

(defsubst bv-corfu--at-word-end-p ()
  "Check if at word boundary."
  (or (eobp)
      (not (looking-at-p "[[:alnum:]]"))))

(defun bv-corfu--cape-active-p ()
  "Check if Cape function is active in current completion."
  (cl-some (lambda (f)
             (or (memq f '(cape-dabbrev cape-dict cape-keyword cape-line))
                 (and (symbolp f)
                      (string-prefix-p "cape-" (symbol-name f)))))
           completion-at-point-functions))

(defun bv-corfu-smart-sep ()
  "Smart separator insertion with Cape awareness and double-space commit."
  (interactive)
  (cond
   ;; Double space commits
   ((and (bv-corfu--at-separator-p)
         (or (eobp) (memq (char-after) '(?\s ?\n))))
    (delete-char -1)
    (corfu-insert)
    (insert " "))
   ;; Cape dict/dabbrev might have multi-word completions
   ((and (>= corfu--index 0)
         (not (bv-corfu--at-separator-p))
         (bv-corfu--at-word-end-p)
         (bv-corfu--cape-active-p))
    (corfu-insert)
    (insert " "))
   ;; At word boundary with selection
   ((and (>= corfu--index 0)
         (not (bv-corfu--at-separator-p))
         (bv-corfu--at-word-end-p))
    (corfu-insert)
    (insert " "))
   ;; Normal separator - important for orderless with Cape
   (t (corfu-insert-separator))))

(defun bv-corfu-move-to-minibuffer ()
  "Move completion to minibuffer for richer interaction."
  (interactive)
  (when-let ((data completion-in-region--data))
    (pcase-let ((`(,beg ,end ,table ,pred ,extras) data))
      (let ((completion-extra-properties extras))
        (consult-completion-in-region beg end table pred)))))

;;; Extension Loading

(defvar bv-corfu--extensions-loaded nil
  "Whether extensions have been loaded.")

(defun bv-corfu--load-extensions ()
  "Lazy load extensions on first use."
  (unless bv-corfu--extensions-loaded
    (setq bv-corfu--extensions-loaded t)

    ;; Core extensions
    (bv-corfu--setup-history)
    (bv-corfu--setup-popupinfo)
    (bv-corfu--setup-echo)

    ;; Quick selection - autoload for performance
    (autoload 'corfu-quick-complete "corfu-quick" nil t)
    (autoload 'corfu-quick-insert "corfu-quick" nil t)

    ;; Configure quick keys when loaded
    (with-eval-after-load 'corfu-quick
      (setq corfu-quick1 "asdfgh"
            corfu-quick2 "jkluionm"))))

;;; Mode-specific Settings with Cape Integration

(defun bv-corfu-prog-settings ()
  "Settings for programming modes with Cape awareness."
  ;; Ensure Cape setup runs first
  (run-with-timer 0 nil
    (lambda ()
      (setq-local corfu-auto t
                  corfu-auto-prefix 3
                  corfu-auto-delay 0.2
                  ;; Add flex for better Cape integration
                  completion-styles '(orderless basic flex)
                  ;; Cape-specific: allow partial completion for files
                  completion-category-overrides
                  '((file (styles partial-completion))
                    (project-file (styles partial-completion orderless)))))))

(defun bv-corfu-text-settings ()
  "Settings for text modes with Cape awareness."
  (run-with-timer 0 nil
    (lambda ()
      (setq-local corfu-auto t
                  corfu-auto-prefix 3
                  corfu-auto-delay 0.3
                  completion-styles '(orderless basic flex)))))

(defun bv-corfu-shell-settings ()
  "Conservative settings for shell modes."
  (setq-local corfu-auto nil             ; Manual completion
              corfu-quit-at-boundary t
              corfu-quit-no-match t
              completion-styles '(basic partial-completion)))

(defun bv-corfu-minibuffer-settings ()
  "Minimal settings for minibuffer."
  (setq-local corfu-echo-delay nil
              corfu-popupinfo-delay nil
              corfu-auto nil))

;;; Auto-completion Synchronization

(defun bv-corfu-maybe-enable-auto ()
  "Enable auto-completion based on major mode."
  (when (apply #'derived-mode-p bv-completion-auto-modes)
    (setq-local corfu-auto t)))

;;; Minibuffer Support

(defun bv-corfu--minibuffer-p ()
  "Check if Corfu should be enabled in minibuffer."
  (and (not (or (bound-and-true-p vertico--input)
                (bound-and-true-p mct--active)
                (eq (current-local-map) read-passwd-map)))
       (or completion-at-point-functions
           (where-is-internal #'completion-at-point
                              (list (current-local-map))))))

(defun bv-corfu-enable-in-minibuffer ()
  "Enable Corfu in minibuffer when appropriate."
  (when (bv-corfu--minibuffer-p)
    (bv-corfu-minibuffer-settings)
    (corfu-mode 1)))

;;; Keybindings

(with-eval-after-load 'corfu
  ;; Core bindings
  (keymap-set corfu-map "TAB" #'corfu-complete)
  (keymap-set corfu-map "RET" #'corfu-insert)
  (keymap-set corfu-map "M-m" #'bv-corfu-move-to-minibuffer)
  (keymap-set corfu-map "SPC" #'bv-corfu-smart-sep)
  (keymap-set corfu-map "M-TAB" #'corfu-expand)
  (keymap-set corfu-map "C-n" #'corfu-next)
  (keymap-set corfu-map "C-p" #'corfu-previous)

  ;; Documentation
  (keymap-set corfu-map "M-d" #'corfu-info-documentation)
  (keymap-set corfu-map "M-l" #'corfu-info-location)

  ;; Quick selection
  (keymap-set corfu-map "M-q" #'corfu-quick-complete)
  (keymap-set corfu-map "C-q" #'corfu-quick-insert))

;;; Periodic Cleanup with Cape Integration

(defun bv-corfu--periodic-cleanup ()
  "Consolidated cleanup function with Cape awareness."
  ;; Clean kind cache if too large
  (when (> (hash-table-count bv-corfu--kind-cache)
           bv-corfu--kind-cache-max-size)
    (clrhash bv-corfu--kind-cache))

  ;; Clean popupinfo cache if too large
  (when (and (boundp 'bv-corfu--popupinfo-cache)
             (> (hash-table-count bv-corfu--popupinfo-cache)
                bv-corfu--popupinfo-cache-max-size))
    (clrhash bv-corfu--popupinfo-cache))

  ;; Trigger Cape cache cleanup if available and needed
  (when (and (fboundp 'bv-cape-clear-caches)
             (or (> (hash-table-count bv-corfu--kind-cache) 512)
                 (> (random 100) 95)))  ; 5% chance each cleanup
    (bv-cape-clear-caches)))

;; Run cleanup every 5 minutes
(run-with-timer 300 300 #'bv-corfu--periodic-cleanup)

;;; Hook Setup

(defun bv-corfu--init-hook ()
  "One-time initialization when Corfu is first activated."
  (bv-corfu--load-extensions)
  (remove-hook 'corfu-mode-hook #'bv-corfu--init-hook))

;; Core hooks
(add-hook 'corfu-mode-hook #'bv-corfu--init-hook)

;; Mode-specific hooks with Cape coordination
(add-hook 'prog-mode-hook #'bv-corfu-prog-settings)
(add-hook 'text-mode-hook #'bv-corfu-text-settings)
(add-hook 'shell-mode-hook #'bv-corfu-shell-settings)
(add-hook 'eshell-mode-hook #'bv-corfu-shell-settings)
(add-hook 'minibuffer-setup-hook #'bv-corfu-enable-in-minibuffer)

;; Auto-completion synchronization
(add-hook 'after-change-major-mode-hook #'bv-corfu-maybe-enable-auto)

;; Shell-specific configuration
(with-eval-after-load 'eshell
  (keymap-set corfu-map "RET" #'corfu-send))

;; Add continue commands
(with-eval-after-load 'corfu
  (add-to-list 'corfu-continue-commands #'bv-corfu-move-to-minibuffer))

;;; Cape Integration Check

(defun bv-corfu-check-cape-integration ()
  "Check if Cape is properly integrated."
  (interactive)
  (if (featurep 'bv-cape)
      (message "Cape integration active. %d Capfs available in current buffer."
               (length completion-at-point-functions))
    (message "Cape not loaded. Consider loading bv-cape for enhanced completion.")))

;;; Load Order Handling

;; Ensure Cape is loaded first if available
(when (locate-library "bv-cape")
  (require 'bv-cape nil t))

;;; Activation

;; Enable global Corfu mode
(global-corfu-mode 1)

(provide 'bv-corfu)
;;; bv-corfu.el ends here
