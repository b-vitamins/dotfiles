;;; bv-theme.el --- Theme face configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; This file derives all faces in various modes from the fundamental faces
;; defined in bv-faces.

;;; Code:

(require 'bv-faces)

(defcustom bv-theme-var nil
  "Current theme variant ('light' or 'dark')."
  :group 'bv
  :type 'string)

(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (if (facep face)
      (set-face-attribute face nil
                          :foreground 'unspecified :background 'unspecified
                          :family     'unspecified :slant      'unspecified
                          :weight     'unspecified :height     'unspecified
                          :underline  'unspecified :overline   'unspecified
                          :box        'unspecified :inherit    style)))

(defun bv-theme--basics ()
  "Derive basic Emacs faces from bv-faces."

  (set-foreground-color bv-color-foreground)
  (set-background-color bv-color-background)

  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'light
                      :family     (face-attribute 'bv-face-default :family)
                      :height     (face-attribute 'bv-face-default :height))

  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  (set-face 'bold                                     'bv-face-strong)
  (set-face 'italic                                    'bv-face-faded)
  (set-face 'bold-italic                              'bv-face-strong)
  (set-face 'region                                   'bv-face-subtle)
  (set-face 'highlight                                'bv-face-subtle)
  (set-face 'fixed-pitch-serif                       'bv-face-default)
  (set-face 'cursor                                  'bv-face-default)
  (if bv-font-family-proportional
      (set-face-attribute 'variable-pitch nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)
                          :family     (face-attribute 'bv-face-variable-pitch :family)
                          :height     (face-attribute 'bv-face-variable-pitch :height))
    (set-face 'variable-pitch                     'bv-face-default))

  (set-face-attribute 'cursor nil
                      :background (face-foreground 'bv-face-default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'bv-face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground bv-color-background)
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground bv-color-background)
  (set-face-foreground 'vertical-border bv-color-subtle)

  (set-face 'shadow                                    'bv-face-faded)
  (set-face 'success                                 'bv-face-salient)
  (set-face 'warning                                  'bv-face-popout)
  (set-face 'error                                  'bv-face-critical)
  (set-face 'match                                    'bv-face-popout)

  (set-face 'buffer-menu-buffer                       'bv-face-strong)
  (set-face 'minibuffer-prompt                        'bv-face-strong)
  (set-face 'link                                    'bv-face-salient)
  (set-face 'fringe                                    'bv-face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'bv-face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'bv-face-strong)
  (set-face 'isearch-fail                              'bv-face-faded)
  (set-face 'lazy-highlight                           'bv-face-subtle)
  (set-face 'trailing-whitespace                      'bv-face-subtle)
  (set-face 'show-paren-match                         'bv-face-popout)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'bv-face-subtle)
  (set-face 'completions-common-part                   'bv-face-faded)
  (set-face 'completions-first-difference            'bv-face-default))

(defun bv-theme--font-lock ()
  "Derive font-lock faces from bv-faces."
  (set-face 'font-lock-comment-face                    'bv-face-faded)
  (set-face 'font-lock-doc-face                        'bv-face-faded)
  (set-face 'font-lock-string-face                    'bv-face-popout)
  (set-face 'font-lock-constant-face                 'bv-face-salient)
  (set-face 'font-lock-warning-face                   'bv-face-popout)
  (set-face 'font-lock-function-name-face             'bv-face-strong)
  (set-face 'font-lock-variable-name-face             'bv-face-strong)
  (set-face 'font-lock-builtin-face                  'bv-face-salient)
  (set-face 'font-lock-type-face                     'bv-face-salient)
  (set-face 'font-lock-keyword-face                  'bv-face-salient))

(defun bv-theme--mode-line ()
  "Derive mode-line and header-line faces from bv-faces."
  (set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'bv-face-default)
                                    (face-foreground 'bv-face-default))
                      :background (face-background 'bv-face-default)
                      :underline nil
                      :overline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'bv-face-default)
                                    (face-foreground 'bv-face-default))
                      :background (face-background 'bv-face-default)
                      :underline nil
                      :overline nil
                      :inherit nil
                      :box nil)

  (set-face-attribute 'header-line nil
                      :weight 'light
                      :foreground (face-foreground 'bv-face-default)
                      :background (face-background 'bv-face-default)
                      :overline nil
                      :underline nil
                      :box nil
                      :box `(:line-width 1
                                         :color ,(face-background 'bv-face-default)
                                         :style nil)
                      :inherit nil)

  (set-face-attribute 'internal-border nil
                      :background (face-background 'bv-face-default)))

(defun bv-theme--hl-line ()
  "Derive hl-line faces from bv faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                        :background bv-color-highlight)))

(defun bv-theme--org ()
  "Derive org faces from bv faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'bv-face-faded)
    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'bv-face-faded)
    (set-face 'org-block-end-line                      'bv-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))
    (set-face 'org-checkbox                            'bv-face-faded)
    (set-face 'org-checkbox-statistics-done            'bv-face-faded)
    (set-face 'org-checkbox-statistics-todo            'bv-face-faded)
    (set-face 'org-clock-overlay                       'bv-face-faded)
    (set-face 'org-code                                'bv-face-faded)
    (set-face 'org-column                              'bv-face-faded)
    (set-face 'org-column-title                        'bv-face-faded)
    (set-face 'org-date                                'bv-face-faded)
    (set-face 'org-date-selected                       'bv-face-faded)
    (set-face 'org-default                             'bv-face-faded)
    (set-face 'org-document-info                       'bv-face-faded)
    (set-face 'org-document-info-keyword               'bv-face-faded)
    (set-face 'org-document-title                      'bv-face-faded)
    (set-face 'org-done                              'bv-face-default)
    (set-face 'org-drawer                              'bv-face-faded)
    (set-face 'org-ellipsis                            'bv-face-faded)
    (set-face 'org-footnote                            'bv-face-faded)
    (set-face 'org-formula                             'bv-face-faded)
    (set-face 'org-headline-done                       'bv-face-faded)
    (set-face 'org-latex-and-related                   'bv-face-faded)
    (set-face 'org-level-1                            'bv-face-strong)
    (set-face 'org-level-2                            'bv-face-strong)
    (set-face 'org-level-3                            'bv-face-strong)
    (set-face 'org-level-4                            'bv-face-strong)
    (set-face 'org-level-5                            'bv-face-strong)
    (set-face 'org-level-6                            'bv-face-strong)
    (set-face 'org-level-7                            'bv-face-strong)
    (set-face 'org-level-8                            'bv-face-strong)
    (set-face 'org-link                              'bv-face-salient)
    (set-face 'org-list-dt                             'bv-face-faded)
    (set-face 'org-macro                               'bv-face-faded)
    (set-face 'org-meta-line                           'bv-face-faded)
    (set-face 'org-mode-line-clock                     'bv-face-faded)
    (set-face 'org-mode-line-clock-overrun             'bv-face-faded)
    (set-face 'org-priority                            'bv-face-faded)
    (set-face 'org-property-value                      'bv-face-faded)
    (set-face 'org-quote                               'bv-face-faded)
    (set-face 'org-scheduled                           'bv-face-faded)
    (set-face 'org-scheduled-previously                'bv-face-faded)
    (set-face 'org-scheduled-today                   '(bv-face-salient
                                                       bv-face-strong))
    (set-face 'org-sexp-date                           'bv-face-faded)
    (set-face 'org-special-keyword                     'bv-face-faded)
    (set-face 'org-table                               'bv-face-faded)
    (set-face 'org-tag                                'bv-face-popout)
    (set-face 'org-tag-group                           'bv-face-faded)
    (set-face 'org-target                              'bv-face-faded)
    (set-face 'org-time-grid                           'bv-face-faded)
    (set-face 'org-todo                              'bv-face-salient)
    (set-face 'org-upcoming-deadline                 'bv-face-default)
    (set-face 'org-verbatim                           'bv-face-popout)
    (set-face 'org-verse                               'bv-face-faded)
    (set-face 'org-warning                            'bv-face-popout)))

(defun bv-theme ()
  "Derive many faces from the core bv faces."
  (bv-theme--basics)
  (bv-theme--font-lock)
  (bv-theme--mode-line)
  (bv-theme--hl-line)
  (bv-theme--org))

(defun bv-refresh-theme ()
  "Refresh the current theme."
  (interactive)
  (bv-faces)
  (bv-theme)
  (when (fboundp 'bv-modeline-faces)
    (bv-modeline-faces)))

(defun bv-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((string= bv-theme-var "light")
         (bv-theme-set-dark))
        ((string= bv-theme-var "dark")
         (bv-theme-set-light)))
  (bv-refresh-theme))

(provide 'bv-theme)
;;; bv-theme.el ends here