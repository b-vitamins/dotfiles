;;; bv-bindings.el --- Semantic BV keybinding surface  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; The BV command surface uses native Emacs keymaps, not a modal editing layer.
;; `C-c' is the canonical user leader.  Its first key selects a semantic domain;
;; modules then populate the relevant domain map.  Native Emacs conventions
;; (`C-x', `C-h', `M-g', `M-s', mode-local `C-c C-*') are left intact unless a
;; package is deliberately being upgraded in place.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function recentf-open-files "recentf" ())

(defgroup bv-bindings nil
  "Semantic keybinding maps and live keymap inspection."
  :group 'bv
  :prefix "bv-bindings-")

(defcustom bv-bindings-leader-key "C-c"
  "Canonical BV leader prefix."
  :type 'string
  :group 'bv-bindings)

(defconst bv-bindings-domain-specs
  '((notes      "n"   bv-notes-map      "notes/slipbox")
    (org        "o"   bv-org-map        "org/agenda")
    (citation   "c"   bv-citation-map   "citations")
    (terminal   "t"   bv-terminal-map   "terminal/shell")
    (ai         "a"   bv-ai-map         "AI")
    (project    "p"   bv-project-map    "project")
    (git        "g"   bv-git-map        "git/VC")
    (dev        "d"   bv-dev-map        "diagnostics/dev")
    (search     "s"   bv-search-map     "search/spelling")
    (window     "w"   bv-window-map     "windows/workspaces")
    (file       "f"   bv-file-map       "files/remote")
    (help       "h"   bv-help-map       "help/docs")
    (complete   "TAB" bv-completion-map "completion providers")
    (action     "."   bv-action-map     "context actions")
    (system     "x"   bv-system-map     "system/extras"))
  "Top-level BV leader domains.

Each item is (DOMAIN KEY MAP-SYMBOL LABEL).")

;;; Prefix maps

(defvar bv-notes-map (make-sparse-keymap)
  "BV notes and slipbox commands.")
(defvar bv-org-map (make-sparse-keymap)
  "BV Org, agenda, capture, and clocking commands.")
(defvar bv-citation-map (make-sparse-keymap)
  "BV citation and bibliography commands.")
(defvar bv-terminal-map (make-sparse-keymap)
  "BV terminal and shell commands.")
(defvar bv-ai-map (make-sparse-keymap)
  "BV AI assistant commands.")
(defvar bv-project-map (make-sparse-keymap)
  "BV project commands.")
(defvar bv-git-map (make-sparse-keymap)
  "BV Git and VC commands.")
(defvar bv-dev-map (make-sparse-keymap)
  "BV diagnostics, LSP, debug, and developer commands.")
(defvar bv-search-map (make-sparse-keymap)
  "BV search, grep, line filtering, and spelling commands.")
(defvar bv-window-map (make-sparse-keymap)
  "BV window, frame, and workspace commands.")
(defvar bv-file-map (make-sparse-keymap)
  "BV file, Dired, recent-file, and remote-file commands.")
(defvar bv-help-map (make-sparse-keymap)
  "BV help, docs, and discovery commands.")
(defvar bv-completion-map (make-sparse-keymap)
  "BV completion-provider commands.")
(defvar bv-action-map (make-sparse-keymap)
  "BV contextual action commands.")
(defvar bv-system-map (make-sparse-keymap)
  "BV system, media, notification, and miscellaneous commands.")
(defvar bv-toggle-map (make-sparse-keymap)
  "BV toggle commands.")
(defvar bv-remote-map (make-sparse-keymap)
  "BV TRAMP and remote-host commands.")
(defvar bv-media-map (make-sparse-keymap)
  "BV media commands.")
(defvar bv-debug-map (make-sparse-keymap)
  "BV debug adapter commands.")

;; Historical symbol retained only so older modules that are loaded out of tree
;; fail soft.  It is intentionally not bound to a key.
(defvar bv-app-map (make-sparse-keymap)
  "Deprecated unbound map; use the semantic BV domain maps instead.")

(defun bv-bindings--kbd (key)
  "Return KEY as a parsed key sequence."
  (kbd key))

(defun bv-bindings--define (map key command)
  "Bind KEY to COMMAND in MAP."
  (define-key map (bv-bindings--kbd key) command))

(defun bv-bindings--global (key command)
  "Bind global KEY to COMMAND."
  (global-set-key (bv-bindings--kbd key) command))

(defun bv-bindings--leader-key (key)
  "Return KEY under the BV leader."
  (string-join (list bv-bindings-leader-key key) " "))

(defun bv-bindings--install-domain-prefixes ()
  "Install all top-level BV leader domain maps."
  (dolist (spec bv-bindings-domain-specs)
    (pcase-let ((`(,_domain ,key ,map-symbol ,_label) spec))
      (bv-bindings--global
       (bv-bindings--leader-key key)
       (symbol-value map-symbol)))))

;;; Frame/session helpers

(defun new-frame ()
  "Create a new frame with `*scratch*' selected."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun bv-delete-frame-or-kill-emacs ()
  "Delete this frame, or kill Emacs when it is the last frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))

;;; Live keymap audit

(defun bv-bindings--domain-row (spec)
  "Return a display row for domain SPEC."
  (pcase-let* ((`(,domain ,key ,map-symbol ,label) spec)
               (full-key (bv-bindings--leader-key key))
               (binding (key-binding (bv-bindings--kbd full-key) t)))
    (list domain full-key map-symbol label binding)))

(defun bv-bindings-audit ()
  "Show the live BV leader map state.

The report uses the currently loaded keymaps, so it catches load-order drift
that a regex scan of the source tree cannot see."
  (interactive)
  (let ((rows (mapcar #'bv-bindings--domain-row bv-bindings-domain-specs)))
    (with-current-buffer (get-buffer-create "*BV Keybinding Audit*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "BV Keybinding Audit\n\n")
        (insert (format "Leader: %s\n\n" bv-bindings-leader-key))
        (insert (format "%-12s %-10s %-22s %-24s %s\n"
                        "Domain" "Key" "Map" "Label" "Live binding"))
        (insert (make-string 92 ?-) "\n")
        (dolist (row rows)
          (pcase-let ((`(,domain ,key ,map-symbol ,label ,binding) row))
            (insert
             (format "%-12s %-10s %-22s %-24s %S\n"
                     domain key map-symbol label binding))))
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer (current-buffer)))))

(defun bv-bindings-leader-help ()
  "Show BV leader domains through Which-Key when available."
  (interactive)
  (cond
   ((fboundp 'which-key-show-keymap)
    (which-key-show-keymap "BV leader"
                            (lookup-key global-map
                                        (kbd bv-bindings-leader-key))))
   ((fboundp 'which-key-show-top-level)
    (which-key-show-top-level))
   (t
    (bv-bindings-audit))))

;;; Canonical bindings

(bv-bindings--install-domain-prefixes)

;; Native upgrades and frame/window basics.
(bv-bindings--global "C-x k" #'kill-current-buffer)
(bv-bindings--global "C-x C-c" #'bv-delete-frame-or-kill-emacs)
(bv-bindings--global "M-n" #'new-frame)
(bv-bindings--global "M-`" #'other-frame)

;; Window map keeps discoverable aliases for built-in high-frequency actions.
(bv-bindings--define bv-window-map "0" #'delete-window)
(bv-bindings--define bv-window-map "1" #'delete-other-windows)
(bv-bindings--define bv-window-map "2" #'split-window-below)
(bv-bindings--define bv-window-map "3" #'split-window-right)
(bv-bindings--define bv-window-map "o" #'other-window)
(bv-bindings--define bv-window-map "n" #'new-frame)
(bv-bindings--define bv-window-map "`" #'other-frame)

;; File map covers global file conveniences; modules add Dired/TRAMP extras.
(bv-bindings--define bv-file-map "f" #'find-file)
(bv-bindings--define bv-file-map "s" #'save-buffer)
(bv-bindings--define bv-file-map "w" #'write-file)
(bv-bindings--define bv-file-map "r" #'recentf-open-files)
(bv-bindings--define bv-file-map "R" bv-remote-map)

;; Discovery and self-audit.
(bv-bindings--define bv-help-map "b" #'bv-bindings-audit)
(bv-bindings--define bv-help-map "k" #'bv-bindings-leader-help)

;; Toggle and media subdomains live under the system/extras map.
(bv-bindings--define bv-system-map "t" bv-toggle-map)
(bv-bindings--define bv-system-map "m" bv-media-map)

;; Debugging is a dev subdomain populated by `bv-dape'.
(bv-bindings--define bv-dev-map "d" bv-debug-map)

(provide 'bv-bindings)
;;; bv-bindings.el ends here
