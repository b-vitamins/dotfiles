;;; bv-themes-roles.el --- Semantic roles for BV theme faces -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; Roles are the stable semantic contract between tokens and package adapters.
;; Package faces should map to roles, not raw colors, unless the package
;; behavior needs an explicit exception.

;;; Code:

(require 'cl-lib)

(defconst bv-themes-roles
  '((default :fg fg-main :bg bg-main
             :family-var bv-themes-font-family-monospaced
             :height-var bv-themes-font-size)
    (fixed-pitch :fg fg-main :bg bg-main
                 :family-var bv-themes-font-family-monospaced)
    (variable-pitch :fg fg-main :bg bg-main
                    :family-var bv-themes-font-family-proportional)
    (variable-pitch-text :fg fg-main :bg bg-main
                         :family-var bv-themes-font-family-proportional
                         :height 1.08)
    (default-plain :fg fg-main :bg bg-main)
    (muted :fg fg-muted)
    (dim :fg fg-dim)
    (faint :fg fg-faint)
    (strong :fg fg-main :weight semibold)
    (bold :fg fg-main :weight bold)
    (italic :slant italic)
    (salient :fg fg-salient :weight semibold)
    (special :fg fg-special :weight semibold)
    (inverse :fg fg-inverse :bg fg-main)
    (critical :fg error-strong :weight bold)
    (critical-block :fg critical-fg :bg critical-bg :weight bold :extend t)
    (cursor :bg cursor)
    (fringe :fg fg-faint :bg bg-main)
    (border :fg border)
    (border-subtle :fg border-subtle)
    (border-strong :fg border-strong)
    (shadow :fg fg-dim)
    (secondary :fg fg-muted :bg bg-alt)
    (escape :fg orange-strong :weight semibold)
    (trailing :bg error)
    (region :fg fg-main :bg bg-region :extend t)
    (secondary-selection :fg fg-main :bg bg-region-subtle :extend t)
    (highlight :fg fg-main :bg bg-hover :extend t)
    (hl-line :bg bg-hl-line :extend t)
    (selection :fg fg-main :bg bg-selected :extend t)
    (match :fg fg-main :bg bg-match :weight semibold)
    (search :fg fg-search :bg bg-search :weight semibold)
    (search-current :fg fg-search-current :bg bg-search-current :weight bold)
    (search-lazy :fg fg-main :bg bg-search-lazy)
    (paren :fg fg-paren :bg bg-paren :weight bold)
    (link :fg fg-link :underline (:style line :color fg-link))
    (link-muted :fg fg-link-faint)
    (link-visited :fg fg-link-visited :underline (:style line :color fg-link-visited))
    (prompt :fg prompt :weight semibold)
    (keybind :fg keybind :weight semibold)
    (success :fg success :weight semibold)
    (success-subtle :fg success-strong :bg success-subtle :extend t)
    (warning :fg warning-strong :weight semibold)
    (warning-subtle :fg warning-strong :bg warning-subtle :extend t)
    (error :fg error-strong :weight semibold)
    (error-subtle :fg error-strong :bg error-subtle :extend t)
    (info :fg info-strong :weight semibold)
    (info-subtle :fg info-strong :bg info-subtle :extend t)
    (syntax-comment :fg syntax-comment :slant italic)
    (syntax-doc :fg syntax-doc :slant italic)
    (syntax-string :fg syntax-string)
    (syntax-regexp :fg syntax-regexp :weight semibold)
    (syntax-escape :fg syntax-escape :weight semibold)
    (syntax-keyword :fg syntax-keyword :weight semibold)
    (syntax-builtin :fg syntax-builtin :weight medium)
    (syntax-function :fg syntax-function :weight semibold)
    (syntax-variable :fg syntax-variable)
    (syntax-variable-use :fg fg-main)
    (syntax-constant :fg syntax-constant :weight medium)
    (syntax-number :fg syntax-number)
    (syntax-type :fg syntax-type :weight semibold)
    (syntax-operator :fg syntax-operator)
    (syntax-preprocessor :fg syntax-preprocessor :weight semibold)
    (syntax-property :fg syntax-property)
    (syntax-bracket :fg syntax-bracket)
    (syntax-delimiter :fg syntax-delimiter)
    (syntax-warning :fg warning-strong :wave warning)
    (syntax-error :fg error-strong :wave error)
    (line-number :fg fg-faint :bg bg-main)
    (line-number-current :fg fg-main :bg bg-hl-line :weight semibold)
    (header-default :fg fg-header :bg bg-header :extend t)
    (header-muted :fg fg-header-muted :bg bg-header :extend t)
    (header-strong :fg fg-header :bg bg-header :weight bold :extend t)
    (header-salient :fg fg-salient :bg bg-header :weight semibold :extend t)
    (header-info :fg info-strong :bg bg-header :weight semibold :extend t)
    (header-warning :fg warning-strong :bg bg-header :weight semibold
                    :extend t)
    (header-error :fg error-strong :bg bg-header :weight semibold
                  :extend t)
    (header-popout :fg fg-header-inverse :bg modeline-bg-active :weight bold :extend t)
    (header-critical :fg modeline-fg-critical :bg modeline-bg-critical
                     :weight bold :extend t)
    (header-inactive-default :fg modeline-fg-inactive :bg modeline-bg-inactive
                             :extend t)
    (header-inactive-muted :fg fg-dim :bg modeline-bg-inactive :extend t)
    (header-inactive-strong :fg modeline-fg-inactive :bg modeline-bg-inactive
                            :weight semibold :extend t)
    (header-inactive-salient :fg fg-muted :bg modeline-bg-inactive
                             :weight semibold :extend t)
    (header-inactive-info :fg fg-muted :bg modeline-bg-inactive
                          :weight semibold :extend t)
    (header-inactive-warning :fg fg-muted :bg modeline-bg-inactive
                             :weight semibold :extend t)
    (header-inactive-error :fg fg-muted :bg modeline-bg-inactive
                           :weight semibold :extend t)
    (header-inactive-popout :fg modeline-fg-inactive :bg modeline-bg-inactive
                            :weight semibold :extend t)
    (header-inactive-critical :fg modeline-fg-critical :bg modeline-bg-inactive
                              :weight semibold :extend t)
    (mode-line-active :fg modeline-fg-active :bg modeline-bg-active
                      :box (:line-width 1 :color modeline-border-active)
                      :weight semibold)
    (mode-line-inactive :fg modeline-fg-inactive :bg modeline-bg-inactive
                        :box (:line-width 1 :color modeline-border-inactive))
    (mode-line-accent :fg modeline-fg-accent :bg modeline-bg-accent
                      :weight bold)
    (tab-bar :fg fg-dim :bg bg-tab-bar)
    (tab-current :fg fg-main :bg bg-tab-current
                 :box (:line-width 1 :color border-subtle)
                 :weight semibold)
    (tab-other :fg fg-dim :bg bg-tab-other)
    (tooltip :fg fg-main :bg bg-tooltip
             :box (:line-width 1 :color border-subtle))
    (popup :fg fg-main :bg bg-popup
           :box (:line-width 1 :color border-subtle))
    (completion :fg fg-main :bg bg-completion)
    (completion-current :fg fg-main :bg bg-completion-current
                        :weight semibold :extend t)
    (completion-annotation :fg fg-dim)
    (completion-match :fg fg-salient :weight bold)
    (button :fg fg-link :bg bg-hover
            :box (:line-width 1 :color border-subtle)
            :weight semibold)
    (heading-1 :fg prose-heading-1 :weight bold :height 1.34)
    (heading-2 :fg prose-heading-2 :weight bold :height 1.24)
    (heading-3 :fg prose-heading-3 :weight semibold :height 1.16)
    (heading-4 :fg prose-heading-4 :weight semibold :height 1.10)
    (heading-5 :fg prose-heading-5 :weight semibold :height 1.05)
    (heading-6 :fg prose-heading-6 :weight semibold)
    (heading-7 :fg prose-heading-7 :weight semibold)
    (heading-8 :fg prose-heading-8 :weight semibold)
    (prose-title :fg prose-heading-1 :weight bold :height 1.60
                 :family-var bv-themes-font-family-proportional)
    (prose-subtitle :fg fg-muted :height 1.16
                    :family-var bv-themes-font-family-proportional)
    (prose-block :fg fg-main :bg bg-prose-block-contents :extend t)
    (prose-block-begin :fg fg-dim :bg bg-prose-block :extend t)
    (prose-code :fg prose-code :bg bg-markup-code
                :family-var bv-themes-font-family-monospaced)
    (prose-verbatim :fg prose-verbatim
                    :family-var bv-themes-font-family-monospaced)
    (prose-metadata :fg prose-metadata)
    (prose-metadata-value :fg prose-metadata-value)
    (prose-table :fg prose-table)
    (prose-todo :fg prose-todo :weight bold)
    (prose-done :fg prose-done :weight semibold)
    (diff-added :fg fg-added :bg bg-added :extend t)
    (diff-added-refine :fg fg-added :bg bg-added-refine :weight bold)
    (diff-removed :fg fg-removed :bg bg-removed :extend t)
    (diff-removed-refine :fg fg-removed :bg bg-removed-refine :weight bold)
    (diff-changed :fg fg-changed :bg bg-changed :extend t)
    (diff-changed-refine :fg fg-changed :bg bg-changed-refine :weight bold)
    (diff-context :fg fg-muted :bg bg-main :extend t)
    (diff-header :fg fg-main :bg bg-alt :weight semibold :extend t)
    (diff-file-header :fg fg-salient :bg bg-alt-2 :weight bold :extend t)
    (vc-added :fg fg-added :weight semibold)
    (vc-removed :fg fg-removed :weight semibold)
    (vc-changed :fg fg-changed :weight semibold)
    (diagnostic-error :fg error-strong :wave error)
    (diagnostic-warning :fg warning-strong :wave warning)
    (diagnostic-info :fg info-strong :wave info)
    (diagnostic-note :fg fg-salient :wave blue)
    (terminal-default :fg fg-main :bg bg-main)
    (terminal-black :fg term-black)
    (terminal-red :fg term-red)
    (terminal-green :fg term-green)
    (terminal-yellow :fg term-yellow)
    (terminal-blue :fg term-blue)
    (terminal-magenta :fg term-magenta)
    (terminal-cyan :fg term-cyan)
    (terminal-white :fg term-white)
    (terminal-bright-black :fg term-bright-black)
    (terminal-bright-red :fg term-bright-red)
    (terminal-bright-green :fg term-bright-green)
    (terminal-bright-yellow :fg term-bright-yellow)
    (terminal-bright-blue :fg term-bright-blue)
    (terminal-bright-magenta :fg term-bright-magenta)
    (terminal-bright-cyan :fg term-bright-cyan)
    (terminal-bright-white :fg term-bright-white)
    (calendar-header :fg prose-heading-1 :weight bold)
    (calendar-today :fg blue-strong :weight bold)
    (calendar-current :fg fg-main :bg bg-region :weight semibold)
    (calendar-weekend :fg fg-dim)
    (calendar-holiday :fg red-strong :weight semibold))
  "Semantic role specifications for BV themes.")

(defun bv-themes-roles-build (&optional overrides)
  "Return semantic roles with profile OVERRIDES applied."
  (let ((roles (copy-sequence bv-themes-roles)))
    (dolist (override overrides)
      (setf (alist-get (car override) roles)
            (cdr override)))
    roles))

(defun bv-themes-roles-known-p (role &optional roles)
  "Return non-nil if ROLE exists in ROLES."
  (assq role (or roles bv-themes-roles)))

(defun bv-themes-roles-spec (role &optional roles)
  "Return the plist for semantic ROLE from ROLES."
  (or (cdr (assq role (or roles bv-themes-roles)))
      (error "Unknown BV theme role: %S" role)))

(provide 'bv-themes-roles)
;;; bv-themes-roles.el ends here
