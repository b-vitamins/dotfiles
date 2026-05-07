;;; bv-keybindings-tests.el --- Tests for BV keybinding policy -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Code:

(require 'ert)
(require 'bv-bindings)
(require 'bv-keylog)

(ert-deftest bv-keybindings-leader-domains-are-installed ()
  "The live leader prefixes should resolve to the canonical domain maps."
  :tags '(bv-keybindings)
  (should (eq (lookup-key global-map (kbd "C-c n")) bv-notes-map))
  (should (eq (lookup-key global-map (kbd "C-c o")) bv-org-map))
  (should (eq (lookup-key global-map (kbd "C-c c")) bv-citation-map))
  (should (eq (lookup-key global-map (kbd "C-c t")) bv-terminal-map))
  (should (eq (lookup-key global-map (kbd "C-c a")) bv-ai-map))
  (should (eq (lookup-key global-map (kbd "C-c p")) bv-project-map))
  (should (eq (lookup-key global-map (kbd "C-c g")) bv-git-map))
  (should (eq (lookup-key global-map (kbd "C-c d")) bv-dev-map))
  (should (eq (lookup-key global-map (kbd "C-c s")) bv-search-map))
  (should (eq (lookup-key global-map (kbd "C-c w")) bv-window-map))
  (should (eq (lookup-key global-map (kbd "C-c f")) bv-file-map))
  (should (eq (lookup-key global-map (kbd "C-c h")) bv-help-map))
  (should (eq (lookup-key global-map (kbd "C-c TAB")) bv-completion-map))
  (should (eq (lookup-key global-map (kbd "C-c .")) bv-action-map))
  (should (eq (lookup-key global-map (kbd "C-c x")) bv-system-map)))

(ert-deftest bv-keybindings-subdomains-are-nested ()
  "Secondary maps should live under their semantic parent maps."
  :tags '(bv-keybindings)
  (should (eq (lookup-key bv-system-map (kbd "t")) bv-toggle-map))
  (should (eq (lookup-key bv-system-map (kbd "m")) bv-media-map))
  (should (eq (lookup-key bv-file-map (kbd "R")) bv-remote-map))
  (should (eq (lookup-key bv-dev-map (kbd "d")) bv-debug-map)))

(ert-deftest bv-keybindings-keylog-is-discoverable ()
  "Key telemetry commands should be reachable from the leader."
  :tags '(bv-keybindings)
  (should (eq (lookup-key bv-help-map (kbd "K")) #'bv-keylog-open-current-log))
  (should (eq (lookup-key bv-help-map (kbd "L")) #'bv-keylog-report))
  (should (eq (lookup-key bv-toggle-map (kbd "K")) #'bv-keylog-mode)))

(ert-deftest bv-keybindings-old-app-drawer-is-not-canonical ()
  "The retired `bv-app-map' should not own the AI leader."
  :tags '(bv-keybindings)
  (should-not (eq (lookup-key global-map (kbd "C-c a")) bv-app-map)))

(ert-deftest bv-keylog-skips-text-insertion ()
  "Key telemetry must not log ordinary text insertion."
  :tags '(bv-keybindings)
  (should (bv-keylog--skip-p nil))
  (should (bv-keylog--skip-p 'self-insert-command))
  (should (bv-keylog--skip-p 'org-self-insert-command))
  (should-not (bv-keylog--skip-p 'find-file)))

(ert-deftest bv-keylog-redacts-prompted-input ()
  "Key telemetry should not persist minibuffer payloads."
  :tags '(bv-keybindings)
  (should
   (equal (bv-keylog--safe-key-description-from-vector
           (kbd "C-x C-f / t m p / s e c r e t RET")
           #'find-file)
          "C-x C-f"))
  (should
   (equal (bv-keylog--safe-key-description-from-vector
           (kbd "M-x o r g - a g e n d a RET a")
           #'org-agenda)
          "M-x")))

(provide 'bv-keybindings-tests)
;;; bv-keybindings-tests.el ends here
