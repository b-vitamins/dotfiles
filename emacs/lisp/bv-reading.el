;;; bv-reading.el --- Document and feed reading -*- lexical-binding: t -*-

;;; Commentary:
;; Reading environment for RSS feeds, PDFs, EPUBs, and info documents.
;; Includes elfeed with saved searches, pdf-tools, nov.el, and enhanced info mode.

;;; Code:

(require 'bv-core)

;;;; Custom Variables
(defgroup bv-reading nil
  "Document and feed reading configuration."
  :group 'bv)

(defcustom bv-reading-directory "~/reading"
  "Directory for reading materials."
  :type 'directory
  :group 'bv-reading)

(defcustom bv-reading-elfeed-directory "~/reading/elfeed"
  "Directory for elfeed database and configuration."
  :type 'directory
  :group 'bv-reading)

(defcustom bv-reading-elfeed-org-files
  (list (expand-file-name "feeds.org" bv-reading-elfeed-directory))
  "Org files containing elfeed subscriptions."
  :type '(repeat file)
  :group 'bv-reading)

(defcustom bv-reading-elfeed-default-filter "@2-weeks-ago +unread"
  "Default filter for elfeed."
  :type 'string
  :group 'bv-reading)

(defcustom bv-reading-pdf-continuous-scroll t
  "Enable continuous scroll in PDF viewing."
  :type 'boolean
  :group 'bv-reading)

(defcustom bv-reading-nov-text-width t
  "Text width for nov.el. t means use full window width."
  :type '(choice (const :tag "Full width" t)
                 (integer :tag "Fixed width"))
  :group 'bv-reading)

;;;; Elfeed - RSS/Atom Reader
(use-package elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("q" . bv-reading-elfeed-quit)
         ("Q" . kill-buffer)
         ("g" . elfeed-update)
         ("G" . elfeed-search-update--force)
         ("f" . bv-reading-elfeed-search-filter)
         ("F" . bv-reading-elfeed-reset-filter)
         ("s" . bv-reading-elfeed-save-search)
         ("S" . bv-reading-elfeed-load-search)
         ("t" . elfeed-search-set-tag)
         ("T" . elfeed-search-unset-tag)
         ("r" . elfeed-search-untag-all-unread)
         ("R" . elfeed-mark-all-as-read)
         ("b" . elfeed-search-browse-url)
         ("y" . elfeed-search-yank)
         ("+" . elfeed-search-tag-all)
         ("-" . elfeed-search-untag-all)
         :map elfeed-show-mode-map
         ("q" . bv-reading-elfeed-kill-buffer)
         ("Q" . kill-current-buffer)
         ("n" . elfeed-show-next)
         ("p" . elfeed-show-prev)
         ("b" . elfeed-show-visit)
         ("y" . elfeed-show-yank)
         ("t" . elfeed-show-tag)
         ("T" . elfeed-show-untag)
         ("s" . bv-reading-elfeed-show-save)
         ("S" . bv-reading-elfeed-show-save-to-org))
  :custom
  (elfeed-db-directory
   (expand-file-name "db" bv-reading-elfeed-directory))
  (elfeed-enclosure-default-dir
   (expand-file-name "enclosures" bv-reading-elfeed-directory))
  (elfeed-search-filter bv-reading-elfeed-default-filter)
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 30)
  (elfeed-search-trailing-width 30)
  (elfeed-show-unique-buffers t)
  (elfeed-sort-order 'descending)
  :config
  ;; Create directories
  (dolist (dir (list elfeed-db-directory
                     elfeed-enclosure-default-dir
                     bv-reading-elfeed-directory))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  
  ;; Custom faces
  (custom-set-faces
   '(elfeed-search-title-face ((t (:foreground "gray50"))))
   '(elfeed-search-unread-title-face ((t (:weight bold))))
   '(elfeed-search-date-face ((t (:foreground "gray60"))))
   '(elfeed-search-feed-face ((t (:foreground "gray70"))))
   '(elfeed-search-tag-face ((t (:foreground "gray80")))) )
  
  ;; Saved searches
  (defvar bv-reading-elfeed-saved-searches
    '(("All unread" . "@2-weeks-ago +unread")
      ("Today" . "@1-day-ago")
      ("This week" . "@1-week-ago")
      ("Programming" . "@2-weeks-ago +unread +programming")
      ("Research" . "@2-weeks-ago +unread +research")
      ("News" . "@1-day-ago +unread +news")
      ("Videos" . "@1-week-ago +unread +video")
      ("Starred" . "+starred")
      ("Later" . "+later +unread"))
    "Saved elfeed searches.")
  
  (defun bv-reading-elfeed-quit ()
    "Quit elfeed and bury buffers."
    (interactive)
    (elfeed-search-quit-window)
    (when (get-buffer "*elfeed-log*")
      (kill-buffer "*elfeed-log*")))
  
  (defun bv-reading-elfeed-kill-buffer ()
    "Kill buffer and return to search."
    (interactive)
    (let ((buf (current-buffer)))
      (elfeed-show-refresh)
      (elfeed-search-show-entry)
      (kill-buffer buf)))
  
  (defun bv-reading-elfeed-search-filter ()
    "Set a custom filter interactively."
    (interactive)
    (let ((filter (read-string "Filter: " elfeed-search-filter)))
      (elfeed-search-set-filter filter)))
  
  (defun bv-reading-elfeed-reset-filter ()
    "Reset filter to default."
    (interactive)
    (elfeed-search-set-filter bv-reading-elfeed-default-filter))
  
  (defun bv-reading-elfeed-save-search ()
    "Save current search filter."
    (interactive)
    (let ((name (read-string "Save search as: "))
          (filter elfeed-search-filter))
      (add-to-list 'bv-reading-elfeed-saved-searches
                   (cons name filter))
      (message "Saved search: %s" name)))
  
  (defun bv-reading-elfeed-load-search ()
    "Load a saved search."
    (interactive)
    (let* ((searches bv-reading-elfeed-saved-searches)
           (choice (completing-read "Load search: "
                                    (mapcar #'car searches))))
      (when-let ((filter (cdr (assoc choice searches))))
        (elfeed-search-set-filter filter))))
  
  (defun bv-reading-elfeed-show-save ()
    "Save current entry to reading list."
    (interactive)
    (elfeed-show-tag 'later)
    (message "Saved for later reading"))
  
  (defun bv-reading-elfeed-show-save-to-org ()
    "Save current entry to org file."
    (interactive)
    (let ((entry elfeed-show-entry))
      (org-capture nil "e")
      (insert (elfeed-entry-title entry))
      (org-capture-finalize)))
  
  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)))

;;;; Elfeed Org
(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files bv-reading-elfeed-org-files)
  
  ;; Create initial feeds.org
  (let ((feeds-file (car bv-reading-elfeed-org-files)))
    (unless (file-exists-p feeds-file)
      (with-temp-file feeds-file
        (insert "#+TITLE: Elfeed Subscriptions\n\n")
        (insert "* Feeds :elfeed:\n")
        (insert "** Programming :programming:\n")
        (insert "*** [[https://news.ycombinator.com/rss][Hacker News]]\n")
        (insert "*** [[https://lobste.rs/rss][Lobsters]]\n")
        (insert "** Research :research:\n")
        (insert "*** [[https://arxiv.org/rss/cs.AI][arXiv AI]]\n")
        (insert "** News :news:\n")
        (insert "** Blogs :blog:\n")))))

;;;; Elfeed Capture Template
(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("e" "Elfeed" entry
                 (file+headline bv-reading-elfeed-org-files "Saved")
                 "*** %:annotation\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
                 :immediate-finish t)))

;;;; PDF Tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation)
              ("D" . pdf-annot-delete)
              ("n" . pdf-view-next-page)
              ("p" . pdf-view-previous-page)
              ("d" . pdf-view-next-page)
              ("u" . pdf-view-previous-page)
              ("g" . pdf-view-goto-page)
              ("G" . pdf-view-last-page)
              ("l" . image-forward-hscroll)
              ("h" . image-backward-hscroll)
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("0" . image-bol)
              ("$" . image-eol)
              ("m" . pdf-view-set-slice-using-mouse)
              ("r" . pdf-view-reset-slice)
              ("R" . pdf-view-rotate)
              ("+" . pdf-view-enlarge)
              ("-" . pdf-view-shrink)
              ("=" . pdf-view-fit-page-to-window)
              ("f" . pdf-view-fit-page-to-window)
              ("w" . pdf-view-fit-width-to-window)
              ("W" . pdf-view-fit-height-to-window)
              ("/" . pdf-occur)
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("q" . kill-current-buffer))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.05)
  (pdf-view-continuous-scroll-mode bv-reading-pdf-continuous-scroll)
  (pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
  :config
  (pdf-tools-install :no-query)
  
  (defun bv-reading-pdf-view-theme-toggle ()
    "Toggle between day and night themes in PDF view."
    (interactive)
    (if pdf-view-midnight-minor-mode
        (pdf-view-midnight-minor-mode -1)
      (pdf-view-midnight-minor-mode 1)))
  
  (define-key pdf-view-mode-map "M" 'bv-reading-pdf-view-theme-toggle)
  
  ;; Theme integration
  (bv-with-feature modus-themes
    (defun bv-reading-pdf-view-themed ()
      "Apply theme to PDF based on current theme."
      (when (derived-mode-p 'pdf-view-mode)
        (if (bv-modus-themes--dark-theme-p)
            (pdf-view-midnight-minor-mode 1)
          (pdf-view-midnight-minor-mode -1))))
    
    (add-hook 'pdf-view-mode-hook 'bv-reading-pdf-view-themed)
    (with-eval-after-load 'pdf-view
      (add-hook 'bv-modus-themes-after-enable-theme-hook
                (lambda ()
                  (dolist (buf (buffer-list))
                    (with-current-buffer buf
                      (when (derived-mode-p 'pdf-view-mode)
                        (bv-reading-pdf-view-themed)))))))))

;;;; SavePlace PDF View
(use-package saveplace-pdf-view
  :after pdf-tools
  :config
  (save-place-mode 1))

;;;; Nov.el - EPUB Reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("n" . nov-next-document)
              ("p" . nov-previous-document)
              ("t" . nov-goto-toc)
              ("l" . nov-history-back)
              ("r" . nov-history-forward)
              ("m" . nov-display-metadata)
              ("f" . nov-toggle-font-type)
              ("+" . nov-increase-font-size)
              ("-" . nov-decrease-font-size)
              ("=" . nov-default-font-size)
              ("S" . nov-save-place)
              ("q" . kill-current-buffer))
  :custom
  (nov-text-width bv-reading-nov-text-width)
  (nov-save-place-file (expand-file-name "nov-places" bv-cache-directory))
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  
  (bv-with-feature monocle
    (add-hook 'nov-mode-hook (lambda () (olivetti-mode 1))))
  
  (setq nov-shr-rendering-functions
        '(nov-render-title
          nov-render-author
          nov-render-metadata
          nov-render-content))
  
  ;; Justify text
  (use-package justify-kp
    :hook (nov-mode . justify-kp-mode)
    :config
    (defun bv-reading-nov-window-configuration-change-hook ()
      (bv-reading-nov-post-html-render-hook)
      (remove-hook 'window-configuration-change-hook
                   'bv-reading-nov-window-configuration-change-hook
                   t))
    
    (defun bv-reading-nov-post-html-render-hook ()
      "Justify text after rendering."
      (if (get-buffer-window)
          (let ((max-width (pj-line-width))
                buffer-read-only)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (not (looking-at "^[[:space:]-]*$"))
                  (goto-char (line-end-position))
                  (when (> (shr-pixel-column) max-width)
                    (goto-char (line-beginning-position))
                    (pj-justify)))
                (forward-line 1))))
        (add-hook 'window-configuration-change-hook
                  'bv-reading-nov-window-configuration-change-hook
                  nil t)))
    
    (add-hook 'nov-post-html-render-hook
              'bv-reading-nov-post-html-render-hook)) )

;;;; Info Mode Enhancement
(use-package info
  :bind (:map Info-mode-map
              ("n" . Info-next)
              ("p" . Info-prev)
              ("u" . Info-up)
              ("b" . Info-history-back)
              ("f" . Info-history-forward)
              ("s" . Info-search)
              ("S" . Info-search-case-sensitively)
              ("i" . Info-index)
              ("I" . Info-virtual-index)
              ("g" . Info-goto-node)
              ("t" . Info-top-node)
              ("T" . Info-toc)
              ("m" . Info-menu)
              ("d" . Info-directory)
              ("?" . Info-summary)
              ("q" . kill-this-buffer))
  :custom
  (Info-use-header-line nil)
  (Info-fontify-maximum-menu-size 100000)
  :config
  (set-face-attribute 'info-title-1 nil :height 1.3)
  (set-face-attribute 'info-title-2 nil :height 1.2)
  (set-face-attribute 'info-title-3 nil :height 1.1)
  
  (add-hook 'Info-mode-hook 'visual-line-mode))

;;;; Info Plus
(use-package info+
  :after info
  :config
  (setq Info-fontify-quotations t)
  (setq Info-fontify-reference-items-flag t)
  (setq Info-saved-history-file
        (expand-file-name "info-history" bv-cache-directory))
  (setq Info-persist-history-mode t))

;;;; Enhanced Info Faces
(bv-with-feature modus-themes
  (with-eval-after-load 'info
    (defun bv-reading-info-set-custom-faces ()
      "Apply custom faces to Info mode."
      (face-remap-add-relative 'default :inherit 'variable-pitch)
      (face-remap-add-relative 'fixed-pitch :inherit 'default))
    
    (add-hook 'Info-mode-hook 'bv-reading-info-set-custom-faces)))

;;;; Reading List Management
(defvar bv-reading-list-file
  (expand-file-name "reading-list.org" bv-reading-directory)
  "File to store reading list.")

(defun bv-reading-add-to-list (title url &optional notes)
  "Add TITLE with URL to reading list with optional NOTES."
  (interactive "sTitle: \nsURL: ")
  (with-current-buffer (find-file-noselect bv-reading-list-file)
    (goto-char (point-max))
    (insert (format "* TODO %s\n" title))
    (insert (format "  :PROPERTIES:\n  :URL: %s\n  :ADDED: %s\n  :END:\n"
                    url (format-time-string "%Y-%m-%d %H:%M")))
    (when notes
      (insert (format "  %s\n" notes)))
    (save-buffer))
  (message "Added to reading list: %s" title))

;;;; Unified Reading Interface
(defun bv-reading-open-url (url)
  "Open URL in appropriate reader based on content type."
  (interactive "sURL: ")
  (cond
   ((string-match-p "\\.pdf\\'" url)
    (url-copy-file url (make-temp-file "pdf-" nil ".pdf") t)
    (find-file (make-temp-file "pdf-" nil ".pdf")))
   ((string-match-p "\\.epub\\'" url)
    (url-copy-file url (make-temp-file "epub-" nil ".epub") t)
    (find-file (make-temp-file "epub-" nil ".epub")))
   ((string-match-p "^\\(https?\\|feed\\)://" url)
    (browse-url url))
   (t
    (message "Unknown URL type: %s" url))))

;;;; Keybindings
(with-eval-after-load 'bv-core
  (define-prefix-command 'bv-reading-map)
  (define-key bv-app-map "R" 'bv-reading-map)
  
  (define-key bv-reading-map "e" 'elfeed)
  (define-key bv-reading-map "p" 'pdf-tools-install)
  (define-key bv-reading-map "i" 'info)
  (define-key bv-reading-map "l" (lambda () (interactive)
                                    (find-file bv-reading-list-file)))
  (define-key bv-reading-map "a" 'bv-reading-add-to-list)
  (define-key bv-reading-map "u" 'bv-reading-open-url))

;;;; Feature Definition
(defun bv-reading-load ()
  "Load reading configuration."
  (add-to-list 'bv-enabled-features 'reading)
  
  ;; Create directories
  (dolist (dir (list bv-reading-directory
                     bv-reading-elfeed-directory))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  
  ;; Create reading list file
  (unless (file-exists-p bv-reading-list-file)
    (with-temp-file bv-reading-list-file
      (insert "#+TITLE: Reading List\n")
      (insert "#+STARTUP: overview\n\n")))
  
  ;; Add Info directories
  (with-eval-after-load 'info
    (add-to-list 'Info-directory-list
                 (expand-file-name "info" bv-reading-directory))))

(provide 'bv-reading)
;;; bv-reading.el ends here
