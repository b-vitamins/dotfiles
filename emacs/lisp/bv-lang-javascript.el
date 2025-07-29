;;; bv-lang-javascript.el --- JavaScript/TypeScript development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; JavaScript/TypeScript development environment with tree-sitter, LSP, and modern tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar js-indent-level)
(defvar typescript-indent-level)
(defvar js2-basic-offset)
(defvar web-mode-markup-indent-offset)
(defvar web-mode-css-indent-offset)
(defvar web-mode-code-indent-offset)
(defvar electric-pair-pairs)
(defvar js-ts-mode-map)
(defvar typescript-ts-mode-map)
(defvar tsx-ts-mode-map)
(defvar json-ts-mode-map)

;; Optional package loading
(autoload 'js2-mode "js2-mode" nil t)
(autoload 'typescript-mode "typescript-mode" nil t)
(autoload 'web-mode "web-mode" nil t)
(autoload 'prettier-js "prettier" nil t)
(autoload 'eslint-fix "eslint-fix" nil t)

;; Remap to tree-sitter modes
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))

;; File associations
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;; JavaScript/TypeScript settings
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq js2-basic-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; Configure TypeScript language server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode) .
                 ("typescript-language-server" "--stdio"))))

;; JavaScript/TypeScript-specific eglot settings
(defun bv-javascript-eglot-config ()
  "Configure eglot for JavaScript/TypeScript development."
  (setq-local eglot-workspace-configuration
              '(:typescript (:preferences (:includeInlayParameterNameHints "all"
                                          :includeInlayParameterNameHintsWhenArgumentMatchesName :json-false
                                          :includeInlayFunctionParameterTypeHints t
                                          :includeInlayVariableTypeHints t
                                          :includeInlayPropertyDeclarationTypeHints t
                                          :includeInlayFunctionLikeReturnTypeHints t
                                          :includeInlayEnumMemberValueHints t))
                :javascript (:preferences (:includeInlayParameterNameHints "all"
                                          :includeInlayParameterNameHintsWhenArgumentMatchesName :json-false
                                          :includeInlayFunctionParameterTypeHints t
                                          :includeInlayVariableTypeHints t
                                          :includeInlayPropertyDeclarationTypeHints t
                                          :includeInlayFunctionLikeReturnTypeHints t)))))

(add-hook 'js-ts-mode-hook #'bv-javascript-eglot-config)
(add-hook 'typescript-ts-mode-hook #'bv-javascript-eglot-config)
(add-hook 'tsx-ts-mode-hook #'bv-javascript-eglot-config)

;; Formatting with prettier
(defun bv-javascript-format-buffer ()
  "Format current buffer with prettier."
  (interactive)
  (cond
   ((fboundp 'prettier-js)
    (prettier-js))
   ((executable-find "prettier")
    (save-buffer)
    (shell-command-on-region (point-min) (point-max)
                            "prettier --stdin-filepath " (buffer-file-name) nil t))
   (t
    (eglot-format-buffer))))

;; ESLint integration
(defun bv-javascript-eslint-fix ()
  "Fix current buffer with ESLint."
  (interactive)
  (if (fboundp 'eslint-fix)
      (eslint-fix)
    (when (executable-find "eslint")
      (save-buffer)
      (shell-command (format "eslint --fix %s" (buffer-file-name)))
      (revert-buffer t t))))

;; Flymake ESLint integration
(with-eval-after-load 'flymake
  (when (fboundp 'flymake-eslint-enable)
    (add-hook 'js-ts-mode-hook #'flymake-eslint-enable)
    (add-hook 'typescript-ts-mode-hook #'flymake-eslint-enable)
    (add-hook 'tsx-ts-mode-hook #'flymake-eslint-enable)))

;; Node.js REPL
(defun bv-javascript-node-repl ()
  "Start Node.js REPL."
  (interactive)
  (pop-to-buffer
   (make-comint "node-repl" "node" nil "--interactive")))

;; Run current file
(defun bv-javascript-run-file ()
  "Run current JavaScript/TypeScript file."
  (interactive)
  (compile
   (cond
    ((eq major-mode 'typescript-ts-mode)
     (format "npx ts-node %s" (buffer-file-name)))
    (t
     (format "node %s" (buffer-file-name))))))

;; NPM commands
(defun bv-javascript-npm-install ()
  "Run npm install."
  (interactive)
  (compile "npm install"))

(defun bv-javascript-npm-run (script)
  "Run npm SCRIPT."
  (interactive "sScript name: ")
  (compile (format "npm run %s" script)))

(defun bv-javascript-npm-test ()
  "Run npm test."
  (interactive)
  (compile "npm test"))

(defun bv-javascript-npm-build ()
  "Run npm build."
  (interactive)
  (compile "npm run build"))

;; Package.json helpers
(defun bv-javascript-find-package-json ()
  "Find and open package.json."
  (interactive)
  (let ((package-json (locate-dominating-file default-directory "package.json")))
    (when package-json
      (find-file (expand-file-name "package.json" package-json)))))

;; Import management
(defun bv-javascript-organize-imports ()
  "Organize imports using LSP."
  (interactive)
  (if (eglot-managed-p)
      (eglot-code-action-organize-imports (point-min) (point-max))
    (message "Not connected to LSP server")))

;; Documentation lookup
(defun bv-javascript-mdn-lookup ()
  "Look up JavaScript documentation on MDN."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (browse-url (format "https://developer.mozilla.org/search?q=%s" symbol)))))

;; Jest testing
(defun bv-javascript-jest-test-file ()
  "Run Jest on current test file."
  (interactive)
  (compile (format "npx jest %s" (buffer-file-name))))

(defun bv-javascript-jest-test-current ()
  "Run Jest on test at point."
  (interactive)
  (let ((test-name (save-excursion
                    (re-search-backward "\\(test\\|it\\|describe\\)[[:space:]]*(\\(['\"]\\)\\([^'\"]+\\)" nil t)
                    (match-string 3))))
    (when test-name
      (compile (format "npx jest %s -t \"%s\"" (buffer-file-name) test-name)))))

;; JSON mode enhancements
(defun bv-javascript-json-format ()
  "Format JSON buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                          "jq ." nil t))

;; Electric pairs
(defun bv-javascript-electric-pairs ()
  "Setup electric pairs for JavaScript."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?` . ?`)))))

(add-hook 'js-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'typescript-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'tsx-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'js-ts-mode-hook #'bv-javascript-electric-pairs)
(add-hook 'typescript-ts-mode-hook #'bv-javascript-electric-pairs)
(add-hook 'tsx-ts-mode-hook #'bv-javascript-electric-pairs)

;; Keybindings
(defvar bv-javascript-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-javascript-format-buffer)
    (define-key map (kbd "C-c C-e") #'bv-javascript-eslint-fix)
    ;; Running
    (define-key map (kbd "C-c C-r") #'bv-javascript-run-file)
    (define-key map (kbd "C-c C-z") #'bv-javascript-node-repl)
    ;; NPM
    (define-key map (kbd "C-c n i") #'bv-javascript-npm-install)
    (define-key map (kbd "C-c n r") #'bv-javascript-npm-run)
    (define-key map (kbd "C-c n t") #'bv-javascript-npm-test)
    (define-key map (kbd "C-c n b") #'bv-javascript-npm-build)
    (define-key map (kbd "C-c n p") #'bv-javascript-find-package-json)
    ;; Testing
    (define-key map (kbd "C-c t f") #'bv-javascript-jest-test-file)
    (define-key map (kbd "C-c t t") #'bv-javascript-jest-test-current)
    ;; LSP
    (define-key map (kbd "C-c C-o") #'bv-javascript-organize-imports)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-javascript-mdn-lookup)
    map)
  "Keymap for JavaScript/TypeScript mode commands.")

(with-eval-after-load 'js-ts-mode
  (define-key js-ts-mode-map (kbd "C-c C-c") bv-javascript-mode-map))

(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-mode-map (kbd "C-c C-c") bv-javascript-mode-map))

(with-eval-after-load 'tsx-ts-mode
  (define-key tsx-ts-mode-map (kbd "C-c C-c") bv-javascript-mode-map))

;; JSON mode keybindings
(with-eval-after-load 'json-ts-mode
  (define-key json-ts-mode-map (kbd "C-c C-f") #'bv-javascript-json-format))

(provide 'bv-lang-javascript)
;;; bv-lang-javascript.el ends here