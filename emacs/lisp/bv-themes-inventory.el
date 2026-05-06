;;; bv-themes-inventory.el --- Live face coverage for BV themes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; This library audits faces that already exist in the running Emacs session.
;; It does not load optional packages.  Package ownership comes from the BV
;; adapter rules, so the inventory can run late in init or from `bv-doctor'
;; after the active configuration has created its faces.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bv-themes-adapters)
(require 'bv-themes-compile)

(defgroup bv-themes-inventory nil
  "Live face inventory for BV themes."
  :group 'bv-themes
  :prefix "bv-themes-inventory-")

(defcustom bv-themes-inventory-ignored-faces nil
  "Live faces intentionally excluded from BV theme coverage gates."
  :type '(repeat symbol)
  :group 'bv-themes-inventory)

(defcustom bv-themes-inventory-ignored-prefixes nil
  "Live face name prefixes intentionally excluded from coverage gates."
  :type '(repeat string)
  :group 'bv-themes-inventory)

(defun bv-themes-inventory--face< (left right)
  "Return non-nil when face LEFT sorts before face RIGHT."
  (string< (symbol-name left) (symbol-name right)))

(defun bv-themes-inventory--sort-faces (faces)
  "Return FACES sorted by symbol name."
  (sort (copy-sequence faces) #'bv-themes-inventory--face<))

(defun bv-themes-inventory--ignored-face-p (face)
  "Return non-nil when FACE is intentionally excluded."
  (or (memq face bv-themes-inventory-ignored-faces)
      (let ((name (symbol-name face)))
        (cl-some (lambda (prefix)
                   (string-prefix-p prefix name))
                 bv-themes-inventory-ignored-prefixes))))

(defun bv-themes-inventory-owner (face)
  "Return the BV package owner for live FACE, or nil."
  (bv-themes-adapters-package-for-face face))

(defun bv-themes-inventory--covered-faces (artifact)
  "Return the face symbols covered by ARTIFACT."
  (mapcar #'car (plist-get artifact :faces)))

(defun bv-themes-inventory--face-entry (face)
  "Return an inventory entry for FACE."
  (list :face face
        :package (bv-themes-inventory-owner face)))

(defun bv-themes-inventory--group-packages (entries covered)
  "Return package coverage rows from ENTRIES and COVERED faces."
  (let ((table (make-hash-table :test 'eq))
        rows)
    (dolist (entry entries)
      (let* ((package (plist-get entry :package))
             (face (plist-get entry :face))
             (row (or (gethash package table)
                      (puthash package
                               (list :package package
                                     :live 0
                                     :covered 0
                                     :unthemed nil)
                               table))))
        (plist-put row :live (1+ (plist-get row :live)))
        (if (memq face covered)
            (plist-put row :covered (1+ (plist-get row :covered)))
          (plist-put row :unthemed
                     (cons face (plist-get row :unthemed))))))
    (maphash (lambda (_ row)
               (push row rows))
             table)
    (dolist (row rows)
      (plist-put row :unthemed
                 (bv-themes-inventory--sort-faces
                  (plist-get row :unthemed))))
    (sort rows
          (lambda (left right)
            (string< (symbol-name (plist-get left :package))
                     (symbol-name (plist-get right :package)))))))

(defun bv-themes-inventory-scan (&optional artifact live-faces)
  "Return live face coverage data for ARTIFACT.
When LIVE-FACES is non-nil, use it instead of `face-list'."
  (let* ((artifact (or artifact
                       (if-let ((theme (caar bv-themes-token-profiles)))
                           (bv-themes-compile theme)
                         (error "No BV theme profile is registered"))))
         (covered (bv-themes-inventory--covered-faces artifact))
         (live (bv-themes-inventory--sort-faces
                (or live-faces (face-list))))
         relevant
         ignored
         unowned
         unthemed)
    (dolist (face live)
      (cond
       ((bv-themes-inventory--ignored-face-p face)
        (push face ignored))
       ((bv-themes-inventory-owner face)
        (let ((entry (bv-themes-inventory--face-entry face)))
          (push entry relevant)
          (unless (memq face covered)
            (push entry unthemed))))
       (t
        (push face unowned))))
    (setq relevant (nreverse relevant)
          unthemed (nreverse unthemed))
    (list :theme (plist-get artifact :theme)
          :live-count (length live)
          :adapter-count (length covered)
          :relevant-count (length relevant)
          :covered-live-count (- (length relevant) (length unthemed))
          :unthemed unthemed
          :ignored (bv-themes-inventory--sort-faces ignored)
          :unowned (bv-themes-inventory--sort-faces unowned)
          :unused-adapters
          (bv-themes-inventory--sort-faces
           (cl-set-difference covered live :test #'eq))
          :packages (bv-themes-inventory--group-packages relevant covered))))

(defun bv-themes-inventory-assert (&optional inventory)
  "Signal an error if INVENTORY contains uncovered live faces."
  (let* ((inventory (or inventory (bv-themes-inventory-scan)))
         (unthemed (plist-get inventory :unthemed))
         (unowned (plist-get inventory :unowned)))
    (when (or unthemed unowned)
      (error "BV theme live face inventory failed: unthemed=%d unowned=%d relevant=%d live=%d"
             (length unthemed)
             (length unowned)
             (plist-get inventory :relevant-count)
             (plist-get inventory :live-count)))
    inventory))

(defun bv-themes-inventory--insert-list (title items formatter)
  "Insert TITLE and ITEMS into the current buffer using FORMATTER."
  (insert (format "* %s (%d)\n" title (length items)))
  (if items
      (dolist (item items)
        (insert (format "- %s\n" (funcall formatter item))))
    (insert "- none\n"))
  (insert "\n"))

(defun bv-themes-inventory-report (&optional inventory)
  "Display a live face coverage INVENTORY."
  (interactive)
  (let ((inventory (or inventory (bv-themes-inventory-scan)))
        (buffer (get-buffer-create "*BV Theme Face Inventory*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# BV Theme Face Inventory: %S\n\n"
                        (plist-get inventory :theme)))
        (insert (format "- live faces: %d\n"
                        (plist-get inventory :live-count)))
        (insert (format "- adapter specs: %d\n"
                        (plist-get inventory :adapter-count)))
        (insert (format "- relevant live faces: %d\n"
                        (plist-get inventory :relevant-count)))
        (insert (format "- covered relevant faces: %d\n"
                        (plist-get inventory :covered-live-count)))
        (insert (format "- unthemed relevant faces: %d\n\n"
                        (length (plist-get inventory :unthemed))))
        (insert "* Packages\n")
        (dolist (row (plist-get inventory :packages))
          (insert (format "- %S live=%d covered=%d unthemed=%d\n"
                          (plist-get row :package)
                          (plist-get row :live)
                          (plist-get row :covered)
                          (length (plist-get row :unthemed)))))
        (insert "\n")
        (bv-themes-inventory--insert-list
         "Unthemed Relevant Live Faces"
         (plist-get inventory :unthemed)
         (lambda (entry)
           (format "%S (%S)"
                   (plist-get entry :face)
                   (plist-get entry :package))))
        (bv-themes-inventory--insert-list
         "Ignored Live Faces"
         (plist-get inventory :ignored)
         (lambda (face) (format "%S" face)))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

(provide 'bv-themes-inventory)
;;; bv-themes-inventory.el ends here
