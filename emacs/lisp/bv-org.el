;;; bv-org.el --- Org mode configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/bv-essentials.el
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
;; “Org-mode markup deserves to be adopted beyond Emacs.”
;;                                                    - @UnixToolTip on X.

;;; Code:

(require 'org)

(defun bv-setup-org-fonts ()
  "Configure font settings for Org-mode, checking font availability."
  (let ((dejavu-available (member "DejaVu Sans" (font-family-list)))
        (iosevka-available (member "Iosevka Comfy" (font-family-list))))
    ;; Set the document title font if DejaVu Sans is available
    (when dejavu-available
      (set-face-attribute 'org-document-title nil
                          :font "DejaVu Sans" :height 1.0))
    ;; Set heading fonts if Iosevka Comfy is available
    (when iosevka-available
      (dolist (face '((org-level-1 . 1.1)
                      (org-level-2 . 1.05)
                      (org-level-3 . 1.0)
                      (org-level-4 . 1.0)
                      (org-level-5 . 1.0)
                      (org-level-6 . 1.0)
                      (org-level-7 . 1.0)
                      (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil
                            :font "Iosevka Comfy"
                            :weight 'bold
                            :height (* 1.0 (cdr face)))))
    ;; Set fixed-pitch attributes for specific elements if either font is available
    (when (or dejavu-available iosevka-available)
      (dolist (face '(org-block org-table org-formula org-code org-verbatim
                                org-special-keyword org-meta-line org-checkbox))
        (set-face-attribute face nil :inherit 'fixed-pitch)))
    (set-face-attribute 'org-block nil
                        :background (face-background 'default))))

(defun bv-org-todo-find-sequence (keyword)
  "Return the sequence in `org-todo-keywords' that contain KEYWORD.
Uses prefix matching to allow for fast-selection keywords."
  (seq-find (lambda (seq)
              ;; Check each sequence and attempt prefix matching
              (seq-some (lambda (kw)
                          (string-prefix-p keyword kw))
                        (cdr seq)))
            org-todo-keywords
            nil))

(defun bv-org-todo-jump-from-edge-to-edge (keyword direction)
  "Cycle TODO keyword to the opposite end of its sequence based on DIRECTION.
If DIRECTION is `left, and KEYWORD is its sequence's first, return the last
keyword.  If DIRECTION is `right, and KEYWORD is its sequence's last, return
the first keyword.  If KEYWORD is 'ABANDONED', return itself, enforcing
sequence closure.  Otherwise, return nil to permit normal cycling."
  (if (string= keyword "ABANDONED")
      keyword  ;; Special case: Return 'ABANDONED' itself to prevent cycling
    (let ((sequence (bv-org-todo-find-sequence keyword)))
      (when sequence
        (let* ((keywords (cdr sequence))  ;; Skip "sequence" symbol
               (first-keyword (car (split-string (car keywords) "(")))  ;; Extract base keyword of the first
               (last-keyword (car (split-string (car (last keywords)) "("))))  ;; Extract base keyword of the last
          (cond
           ;; If direction is 'left and at the first, jump to the last
           ((and (eq direction 'left) (string= keyword first-keyword))
            last-keyword)
           ;; If direction is 'right and at the last, jump to the first
           ((and (eq direction 'right) (string= keyword last-keyword))
            first-keyword)
           ;; Otherwise, permit normal cycling
           (t nil)))))))

(defun bv-org-todo-cycle-left ()
  "Restrict TODO cycling to jump from the first to last keyword when moving left."
  (let* ((current-state (substring-no-properties (org-get-todo-state)))
         (next-state (bv-org-todo-jump-from-edge-to-edge current-state 'left)))
    (when next-state
      (org-todo next-state)
      t)))  ;; Return non-nil to exit hook processing if a change is made

(defun bv-org-todo-cycle-right ()
  "Restrict TODO cycling to jump from the last to first keyword when moving right."
  (let* ((current-state (substring-no-properties (org-get-todo-state)))
         (next-state (bv-org-todo-jump-from-edge-to-edge current-state 'right)))
    (when next-state
      (org-todo next-state)
      t)))  ;; Return non-nil to exit hook processing if a change is made

(provide 'bv-org)
;;; bv-org.el ends here
