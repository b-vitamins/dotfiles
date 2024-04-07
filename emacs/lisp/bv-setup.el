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
    (let ((pkg (if (consp recipe) (car recipe) recipe))) ;; Extract package name from recipe if it's a list
      `(if ,condition
           (straight-use-package ',recipe)
         ;; If the condition fails, check if the package is installed or the feature is loaded.
         (unless (or (package-installed-p ',pkg) (featurep ',pkg))
           ;; If not installed or loaded, then use straight.el as a fallback.
           (straight-use-package ',recipe)))))
  :documentation
  "Conditionally install RECIPE with `straight-use-package' if CONDITION is met.
If CONDITION is false, it checks if the package (or feature) PKG is installed or loaded, and if not, installs it using straight.el.
This macro can be used as a HEAD in setup blocks and replaces itself with the RECIPE's package. This macro is not repeatable."
  :repeatable nil
  :indent 1
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe) (car recipe) recipe))))

(declare-function :straight-if "ext:setup" t)

(setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES.")

(declare-function :load-after "ext:setup" t)

(setup-define :local-or-package
  (lambda (feature-or-package)
    `(unless (locate-file ,(symbol-name feature-or-package)
			  load-path
			  (get-load-suffixes))
       (:package ',feature-or-package)))
  :documentation "Install PACKAGE if it is not available locally.
This macro can be used as NAME, and it will replace itself with
the first PACKAGE."
  :repeatable t
  :shorthand #'cadr)

(declare-function :load-or-package "ext:setup" t)

(setup-define :push-to
  (lambda (list elements)
    (let (bodies)
      (dolist (el (if (listp elements) elements (list elements)))
        (cond
         ((symbolp el)
          (when (not (keywordp el))
            (push `(push ',el ,list) bodies)))
         ((listp el)
          (push `(push ',el ,list) bodies))
         ((stringp el)
          (push `(push ,el ,list) bodies))))
      (macroexp-progn (nreverse bodies))))
  :documentation "Push each of the elements onto the given list."
  :debug '(sexp [&or ([&rest sexp]) sexp])
  :repeatable t)

(declare-function :push-to "ext:setup" t)

(provide 'bv-setup)
;;; bv-setup.el ends here