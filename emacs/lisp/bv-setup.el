;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/lisp/bv-setup.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ;; [Reddit user] 7 years ago
;; "We all love Emacs but I think some of these people might be taking it a [tiny] bit too seriously.
;; It's a [freaking] text editor [for heaven's sake], not the cure for cancer lol."
;;
;; ;; [deleted] 7 years ago
;; "M-x doctor"
;; 
;; (Moderation by ChatGPT-4)

;;; Code:

(require 'setup)

(setup-define :set
  (lambda (name val)
    `(setq ,name ,val))
  :documentation
  "Directly set the option NAME to VAL. Uses `setq' to directly set the value,
affecting the current buffer for buffer-local variables, or globally for others."
  :debug '(sexp form)
  :repeatable t)

(declare-function :set "ext:setup" t)

(setup-define :set-default
  (lambda (name val)
    `(setq-default ,name ,val))
  :documentation
  "Set the default option NAME to VAL. Uses `setq-default' to set the default/global
value for buffer-local variables, affecting new buffers or those without a local
override."
  :debug '(sexp form)
  :repeatable t)

(declare-function :set-default "ext:setup" t)

;; Borrowed from Philip Kaludercic <philipk@posteo.net>
;; URL: https://www.emacswiki.org/emacs/SetupEl
;;
(setup-define :option*
  (lambda (name val)
    `(customize-set-variable
      ',(intern (format "%s-%s" (setup-get 'feature) name))
      ,val
      ,(format "Set for %s's setup block" (setup-get 'feature))))
  :documentation "Set the option NAME to VAL.
NAME is not the name of the option itself, but of the option with
the feature prefix."
  :debug '(sexp form)
  :repeatable t)

(declare-function :option* "ext:setup" t)

;; Borrowed with modification from Philip Kaludercic <philipk@posteo.net>
;; URL: https://www.emacswiki.org/emacs/SetupEl
;;
(setup-define :straight-if
  (lambda (recipe condition)
    `(if ,condition
         (straight-use-package ',recipe)
       nil))
  :documentation
  "Install RECIPE with `straight-use-package' when CONDITION is met.
If CONDITION is false, stop evaluating the body.  This macro can
be used as HEAD, and will replace itself with the RECIPE's
package.  This macro is not repeatable."
  :repeatable nil
  :indent 1
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe) (car recipe) recipe))))

(declare-function :straight-if "ext:setup" t)

(provide 'bv-setup)
;;; bv-setup.el ends here