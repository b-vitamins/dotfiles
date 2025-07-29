;;; bv-lang-lua.el --- Lua development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Lua development environment with tree-sitter, LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar lua-indent-level)
(defvar lua-indent-string-contents)
(defvar lua-indent-nested-block-content-align)
(defvar lua-indent-close-paren-align)
(defvar electric-pair-pairs)
(defvar prettify-symbols-alist)
(defvar lua-mode-map)
(defvar lua-ts-mode-map)

;; External functions
(declare-function lua-start-process "lua-mode" (&optional name program startfile switches))
(declare-function lua-send-region "lua-mode" (start end))
(declare-function lua-send-buffer "lua-mode" ())
(declare-function lua-send-defun "lua-mode" (&optional arg))
(declare-function lua-send-current-line "lua-mode" ())

;; Optional package loading
(autoload 'lua-mode "lua-mode" nil t)

;; Enable tree-sitter for Lua when available
(when (treesit-language-available-p 'lua)
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.rockspec\\'" . lua-mode))

;; Lua settings
(setq lua-indent-level 2)
(setq lua-indent-string-contents t)
(setq lua-indent-nested-block-content-align nil)
(setq lua-indent-close-paren-align nil)

;; Configure lua-language-server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((lua-mode lua-ts-mode) . ("lua-language-server"))))

;; Lua-specific eglot settings
(defun bv-lua-eglot-config ()
  "Configure eglot for Lua development."
  (setq-local eglot-workspace-configuration
              '(:Lua (:runtime (:version "Lua 5.4"
                               :path ["?.lua" "?/init.lua"])
                     :diagnostics (:globals ["vim" "love" "awesome"])
                     :workspace (:library [])
                     :telemetry (:enable :json-false)))))

(add-hook 'lua-mode-hook #'bv-lua-eglot-config)
(when (fboundp 'lua-ts-mode)
  (add-hook 'lua-ts-mode-hook #'bv-lua-eglot-config))

;; REPL interaction
(defun bv-lua-start-repl ()
  "Start Lua REPL."
  (interactive)
  (lua-start-process))

(defun bv-lua-send-region ()
  "Send region to Lua REPL."
  (interactive)
  (lua-send-region (region-beginning) (region-end)))

(defun bv-lua-send-buffer ()
  "Send buffer to Lua REPL."
  (interactive)
  (lua-send-buffer))

(defun bv-lua-send-defun ()
  "Send current function to Lua REPL."
  (interactive)
  (lua-send-defun))

(defun bv-lua-send-line ()
  "Send current line to Lua REPL."
  (interactive)
  (lua-send-current-line))

;; Running Lua scripts
(defun bv-lua-run-file ()
  "Run current Lua file."
  (interactive)
  (compile (format "lua %s" (buffer-file-name))))

(defun bv-lua-run-love ()
  "Run current project with LÖVE."
  (interactive)
  (let ((project-root (locate-dominating-file default-directory "main.lua")))
    (if project-root
        (compile (format "love %s" project-root))
      (message "No LÖVE project found (missing main.lua)"))))

;; Formatting
(defun bv-lua-format-buffer ()
  "Format current Lua buffer."
  (interactive)
  (cond
   ((executable-find "lua-format")
    (shell-command-on-region (point-min) (point-max)
                            "lua-format" nil t))
   ((executable-find "stylua")
    (shell-command-on-region (point-min) (point-max)
                            "stylua -" nil t))
   (t
    (eglot-format-buffer))))

;; Linting
(defun bv-lua-check ()
  "Check Lua syntax."
  (interactive)
  (compile (format "luac -p %s" (buffer-file-name))))

(defun bv-lua-lint ()
  "Lint Lua file with luacheck."
  (interactive)
  (if (executable-find "luacheck")
      (compile (format "luacheck %s" (buffer-file-name)))
    (message "luacheck not found")))

;; Documentation
(defun bv-lua-search-documentation ()
  "Search Lua documentation."
  (interactive)
  (browse-url "https://www.lua.org/manual/5.4/"))

(defun bv-lua-search-love-documentation ()
  "Search LÖVE documentation."
  (interactive)
  (browse-url "https://love2d.org/wiki/Main_Page"))

;; LuaRocks package management
(defun bv-lua-rocks-install (package)
  "Install LuaRocks PACKAGE."
  (interactive "sPackage: ")
  (compile (format "luarocks install %s" package)))

(defun bv-lua-rocks-search (query)
  "Search LuaRocks packages for QUERY."
  (interactive "sSearch query: ")
  (compile (format "luarocks search %s" query)))

(defun bv-lua-rocks-list ()
  "List installed LuaRocks packages."
  (interactive)
  (compile "luarocks list"))

;; Busted testing framework
(defun bv-lua-test-file ()
  "Run Busted test for current file."
  (interactive)
  (if (executable-find "busted")
      (compile (format "busted %s" (buffer-file-name)))
    (message "busted not found")))

(defun bv-lua-test-project ()
  "Run all Busted test in project."
  (interactive)
  (if (executable-find "busted")
      (compile "busted")
    (message "busted not found")))

;; Electric pairs
(defun bv-lua-electric-pairs ()
  "Setup electric pairs for Lua."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?{ . ?})))))

(add-hook 'lua-mode-hook #'electric-pair-local-mode)
(add-hook 'lua-mode-hook #'bv-lua-electric-pairs)
(when (fboundp 'lua-ts-mode)
  (add-hook 'lua-ts-mode-hook #'electric-pair-local-mode)
  (add-hook 'lua-ts-mode-hook #'bv-lua-electric-pairs))

;; Compilation setup
(defun bv-lua-setup-compilation ()
  "Setup compilation for Lua."
  (setq-local compile-command
              (cond
               ((file-exists-p "main.lua") "love .")
               ((file-exists-p "Makefile") "make")
               (t (format "lua %s" (buffer-file-name))))))

(add-hook 'lua-mode-hook #'bv-lua-setup-compilation)
(when (fboundp 'lua-ts-mode)
  (add-hook 'lua-ts-mode-hook #'bv-lua-setup-compilation))

;; Pretty symbols
(defun bv-lua-pretty-symbols ()
  "Enable pretty symbols for Lua."
  (setq prettify-symbols-alist
        '(("function" . ?ƒ)
          ("end" . ?∎)
          ("~=" . ?≠)
          ("<=" . ?≤)
          (">=" . ?≥)
          ("==" . ?≡)))
  (prettify-symbols-mode 1))

(add-hook 'lua-mode-hook #'bv-lua-pretty-symbols)
(when (fboundp 'lua-ts-mode)
  (add-hook 'lua-ts-mode-hook #'bv-lua-pretty-symbols))

;; Keybindings
(defvar bv-lua-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-lua-start-repl)
    ;; Evaluation
    (define-key map (kbd "C-c C-r") #'bv-lua-send-region)
    (define-key map (kbd "C-c C-b") #'bv-lua-send-buffer)
    (define-key map (kbd "C-c C-f") #'bv-lua-send-defun)
    (define-key map (kbd "C-c C-l") #'bv-lua-send-line)
    ;; Running
    (define-key map (kbd "C-c C-c") #'bv-lua-run-file)
    (define-key map (kbd "C-c C-L") #'bv-lua-run-love)
    ;; Formatting/Linting
    (define-key map (kbd "C-c C-F") #'bv-lua-format-buffer)
    (define-key map (kbd "C-c C-v") #'bv-lua-check)
    (define-key map (kbd "C-c C-V") #'bv-lua-lint)
    ;; Testing
    (define-key map (kbd "C-c t f") #'bv-lua-test-file)
    (define-key map (kbd "C-c t p") #'bv-lua-test-project)
    ;; LuaRocks
    (define-key map (kbd "C-c r i") #'bv-lua-rocks-install)
    (define-key map (kbd "C-c r s") #'bv-lua-rocks-search)
    (define-key map (kbd "C-c r l") #'bv-lua-rocks-list)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-lua-search-documentation)
    (define-key map (kbd "C-c C-D") #'bv-lua-search-love-documentation)
    map)
  "Keymap for Lua mode commands.")

(with-eval-after-load 'lua-mode
  (define-key lua-mode-map (kbd "C-c l") bv-lua-mode-map))

(when (fboundp 'lua-ts-mode)
  (with-eval-after-load 'lua-ts-mode
    (define-key lua-ts-mode-map (kbd "C-c l") bv-lua-mode-map)))

(provide 'bv-lang-lua)
;;; bv-lang-lua.el ends here