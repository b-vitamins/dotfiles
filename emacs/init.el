;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ayan Das <bvits@riseup.net>

;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/.config/emacs/init.el
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
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars.  It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;              - Neal Stephenson, In the Beginning was the Command Line

;;; Code:

;; Set the user-emacs-directory to the custom Emacs configuration directory.
;; This ensures that Emacs looks for configuration files in the specified
;; directory instead of the default "~/.emacs.d/" directory.
;;
(let ((custom-emacs-dir (substitute-in-file-name "$HOME/.config/emacs/")))
  (when (file-directory-p custom-emacs-dir)
    (setq user-emacs-directory (directory-file-name custom-emacs-dir))))

;; Add the "lisp" directory within the user-emacs-directory to the load path.
;; This allows Emacs to find and load Lisp files located in this directory.
;;
(when user-emacs-directory
  (let* ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))))

;; Start the Emacs server to allow for client-server communication.
;;
(server-start)

;; If the 'setup' package is available and the custom Emacs directory is set,
;; require the 'setup' module. This ensures that any additional setup procedures
;; are executed if the corresponding package is available.
;;
(when (and (featurep 'setup)
           (require 'setup nil 'noerror)
           user-emacs-directory)
  (require 'setup))

(provide 'init)
;;; init.el ends here
