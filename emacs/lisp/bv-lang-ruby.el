;;; bv-lang-ruby.el --- Ruby development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Ruby development environment with tree-sitter, LSP, and comprehensive tooling.

;;; Code:

(require 'eglot)

;; External variables
(defvar ruby-indent-level)
(defvar ruby-indent-tabs-mode)
(defvar enh-ruby-add-encoding-comment-on-save)
(defvar enh-ruby-deep-indent-paren)
(defvar enh-ruby-hanging-brace-indent-level)
(defvar inf-ruby-default-implementation)
(defvar inf-ruby-first-prompt-pattern)
(defvar inf-ruby-prompt-pattern)
(defvar electric-pair-pairs)
(defvar ruby-mode-map)
(defvar enh-ruby-mode-map)
(defvar ruby-ts-mode-map)

;; External functions
(declare-function ruby-send-region "inf-ruby" (start end))
(declare-function ruby-send-buffer "inf-ruby" ())
(declare-function ruby-send-block "inf-ruby" ())
(declare-function ruby-beginning-of-defun "ruby-mode" (&optional arg))
(declare-function inf-ruby-minor-mode "inf-ruby" (&optional arg))

;; Optional package loading
(autoload 'enh-ruby-mode "enh-ruby-mode" nil t)
(autoload 'inf-ruby "inf-ruby" nil t)
(autoload 'robe-mode "robe" nil t)

;; Remap to enhanced ruby mode and tree-sitter
(add-to-list 'major-mode-remap-alist '(ruby-mode . enh-ruby-mode))
(when (treesit-language-available-p 'ruby)
  (add-to-list 'major-mode-remap-alist '(enh-ruby-mode . ruby-ts-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.arb\\'" . ruby-mode))

;; Ruby settings
(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)
(setq enh-ruby-add-encoding-comment-on-save nil)
(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-hanging-brace-indent-level 2)

;; Configure solargraph LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((ruby-mode enh-ruby-mode ruby-ts-mode) . ("solargraph" "stdio"))))

;; Ruby-specific eglot settings
(defun bv-ruby-eglot-config ()
  "Configure eglot for Ruby development."
  (setq-local eglot-workspace-configuration
              '(:solargraph (:diagnostics t
                            :completion t
                            :hover t
                            :formatting t
                            :symbols t
                            :definitions t
                            :references t
                            :rename t))))

(add-hook 'ruby-mode-hook #'bv-ruby-eglot-config)
(add-hook 'enh-ruby-mode-hook #'bv-ruby-eglot-config)
(add-hook 'ruby-ts-mode-hook #'bv-ruby-eglot-config)

;; Enable inf-ruby
(add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
(add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)
(add-hook 'ruby-ts-mode-hook #'inf-ruby-minor-mode)

;; REPL interaction
(defun bv-ruby-console ()
  "Start Ruby REPL."
  (interactive)
  (inf-ruby))

(defun bv-ruby-rails-console ()
  "Start Rails console."
  (interactive)
  (inf-ruby "rails console"))

(defun bv-ruby-send-region ()
  "Send region to Ruby REPL."
  (interactive)
  (ruby-send-region (region-beginning) (region-end)))

(defun bv-ruby-send-buffer ()
  "Send buffer to Ruby REPL."
  (interactive)
  (ruby-send-buffer))

(defun bv-ruby-send-block ()
  "Send current block to Ruby REPL."
  (interactive)
  (ruby-send-block))

;; Formatting
(defun bv-ruby-format-buffer ()
  "Format current Ruby buffer."
  (interactive)
  (cond
   ((executable-find "rubocop")
    (shell-command-on-region (point-min) (point-max)
                            "rubocop -x --stdin untitled.rb" nil t))
   ((executable-find "rufo")
    (shell-command-on-region (point-min) (point-max)
                            "rufo" nil t))
   (t
    (eglot-format-buffer))))

;; Testing
(defun bv-ruby-test-file ()
  "Run tests for current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((string-match-p "_spec\\.rb$" file)
      (compile (format "rspec %s" file)))
     ((string-match-p "_test\\.rb$" file)
      (compile (format "ruby -I test %s" file)))
     (t
      (message "Not a test file")))))

(defun bv-ruby-test-at-point ()
  "Run test at point."
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos)))
    (cond
     ((string-match-p "_spec\\.rb$" file)
      (compile (format "rspec %s:%d" file line)))
     ((string-match-p "_test\\.rb$" file)
      (let ((test-name (save-excursion
                        (ruby-beginning-of-defun)
                        (re-search-forward "def \\(test_[^ ]*\\)" nil t)
                        (match-string 1))))
        (if test-name
            (compile (format "ruby -I test %s -n %s" file test-name))
          (message "No test found at point"))))
     (t
      (message "Not a test file")))))

(defun bv-ruby-rake-test ()
  "Run rake test."
  (interactive)
  (compile "rake test"))

(defun bv-ruby-rspec ()
  "Run RSpec."
  (interactive)
  (compile "rspec"))

;; Bundler
(defun bv-ruby-bundle-install ()
  "Run bundle install."
  (interactive)
  (compile "bundle install"))

(defun bv-ruby-bundle-update ()
  "Run bundle update."
  (interactive)
  (compile "bundle update"))

(defun bv-ruby-bundle-exec (command)
  "Run COMMAND with bundle exec."
  (interactive "sCommand: ")
  (compile (format "bundle exec %s" command)))

;; Gem management
(defun bv-ruby-gem-build ()
  "Build gem."
  (interactive)
  (compile "gem build *.gemspec"))

(defun bv-ruby-gem-install ()
  "Install gem."
  (interactive)
  (compile "gem install *.gem"))

;; Rails commands
(defun bv-ruby-rails-server ()
  "Start Rails server."
  (interactive)
  (compile "rails server"))

(defun bv-ruby-rails-generate (generator)
  "Run Rails generator."
  (interactive "sGenerator: ")
  (compile (format "rails generate %s" generator)))

(defun bv-ruby-rails-migrate ()
  "Run Rails migrations."
  (interactive)
  (compile "rails db:migrate"))

(defun bv-ruby-rails-routes ()
  "Show Rails routes."
  (interactive)
  (compile "rails routes"))

;; Documentation
(defun bv-ruby-ri (query)
  "Look up Ruby documentation."
  (interactive "sRI query: ")
  (compile (format "ri %s" query)))

(defun bv-ruby-ri-at-point ()
  "Look up documentation for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (bv-ruby-ri symbol))))

;; Code navigation with robe
(defun bv-ruby-maybe-enable-robe ()
  "Enable robe-mode if available."
  (when (fboundp 'robe-mode)
    (robe-mode 1)))

(add-hook 'ruby-mode-hook #'bv-ruby-maybe-enable-robe)
(add-hook 'enh-ruby-mode-hook #'bv-ruby-maybe-enable-robe)
(add-hook 'ruby-ts-mode-hook #'bv-ruby-maybe-enable-robe)

;; Syntax checking
(defun bv-ruby-check-syntax ()
  "Check Ruby syntax."
  (interactive)
  (compile (format "ruby -c %s" (buffer-file-name))))

;; IRB configuration
(setq inf-ruby-default-implementation "irb")
(setq inf-ruby-first-prompt-pattern "^>>\\|^\\[[0-9]+\\] pry\\(.*\\)> *")
(setq inf-ruby-prompt-pattern "^>>\\|^\\[[0-9]+\\] pry\\(.*\\)> *")

;; Electric pairs
(defun bv-ruby-electric-pairs ()
  "Setup electric pairs for Ruby."
  (setq-local electric-pair-pairs
              (append electric-pair-pairs
                      '((?| . ?|)))))

(add-hook 'ruby-mode-hook #'electric-pair-local-mode)
(add-hook 'ruby-mode-hook #'bv-ruby-electric-pairs)
(add-hook 'enh-ruby-mode-hook #'electric-pair-local-mode)
(add-hook 'enh-ruby-mode-hook #'bv-ruby-electric-pairs)
(add-hook 'ruby-ts-mode-hook #'electric-pair-local-mode)
(add-hook 'ruby-ts-mode-hook #'bv-ruby-electric-pairs)

;; Compilation setup
(defun bv-ruby-setup-compilation ()
  "Setup compilation for Ruby."
  (setq-local compile-command
              (cond
               ((file-exists-p "Rakefile") "rake")
               ((file-exists-p "Gemfile") "bundle exec ruby")
               (t (format "ruby %s" (buffer-file-name))))))

(add-hook 'ruby-mode-hook #'bv-ruby-setup-compilation)
(add-hook 'enh-ruby-mode-hook #'bv-ruby-setup-compilation)
(add-hook 'ruby-ts-mode-hook #'bv-ruby-setup-compilation)

;; Keybindings
(defvar bv-ruby-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL
    (define-key map (kbd "C-c C-z") #'bv-ruby-console)
    (define-key map (kbd "C-c C-Z") #'bv-ruby-rails-console)
    ;; Evaluation
    (define-key map (kbd "C-c C-r") #'bv-ruby-send-region)
    (define-key map (kbd "C-c C-b") #'bv-ruby-send-buffer)
    (define-key map (kbd "C-c C-x") #'bv-ruby-send-block)
    ;; Testing
    (define-key map (kbd "C-c t f") #'bv-ruby-test-file)
    (define-key map (kbd "C-c t t") #'bv-ruby-test-at-point)
    (define-key map (kbd "C-c t r") #'bv-ruby-rake-test)
    (define-key map (kbd "C-c t s") #'bv-ruby-rspec)
    ;; Bundler
    (define-key map (kbd "C-c b i") #'bv-ruby-bundle-install)
    (define-key map (kbd "C-c b u") #'bv-ruby-bundle-update)
    (define-key map (kbd "C-c b e") #'bv-ruby-bundle-exec)
    ;; Rails
    (define-key map (kbd "C-c r s") #'bv-ruby-rails-server)
    (define-key map (kbd "C-c r g") #'bv-ruby-rails-generate)
    (define-key map (kbd "C-c r m") #'bv-ruby-rails-migrate)
    (define-key map (kbd "C-c r r") #'bv-ruby-rails-routes)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'bv-ruby-format-buffer)
    ;; Documentation
    (define-key map (kbd "C-c C-d") #'bv-ruby-ri-at-point)
    (define-key map (kbd "C-c C-h") #'bv-ruby-ri)
    ;; Syntax
    (define-key map (kbd "C-c C-v") #'bv-ruby-check-syntax)
    ;; Gems
    (define-key map (kbd "C-c g b") #'bv-ruby-gem-build)
    (define-key map (kbd "C-c g i") #'bv-ruby-gem-install)
    map)
  "Keymap for Ruby mode commands.")

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c C-c") bv-ruby-mode-map))

(with-eval-after-load 'enh-ruby-mode
  (define-key enh-ruby-mode-map (kbd "C-c C-c") bv-ruby-mode-map))

(when (fboundp 'ruby-ts-mode)
  (with-eval-after-load 'ruby-ts-mode
    (define-key ruby-ts-mode-map (kbd "C-c C-c") bv-ruby-mode-map)))

(provide 'bv-lang-ruby)
;;; bv-lang-ruby.el ends here