;;; bv-calendar.el --- Dynamic calendar display  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; Calendar that adapts to window width (1-3 months).

;;; Code:
(require 'calendar)
(require 'holidays)

;; Declare external functions and variables to avoid elint warnings
(declare-function special-mode "simple" ())
(defvar bv-themes-default)

;; Silence byte-compiler warning for missing function
(eval-when-compile (require 'simple))

(setq calendar-week-start-day 1
      calendar-date-style 'iso)
(defface bv-calendar-header
  '((t :weight bold))
  "Face for calendar headers.")

(defface bv-calendar-today
  '((t :foreground "blue" :weight bold))
  "Face for today.")

(defface bv-calendar-current
  '((t :inherit region :weight medium))
  "Face for current selected date.")

(defface bv-calendar-weekend
  '((t :foreground "gray"))
  "Face for weekends.")

(defface bv-calendar-holiday
  '((t :foreground "red"))
  "Face for holidays.")

(defconst bv-calendar-month-width 23
  "Width of a single month display.")

(defconst bv-calendar-month-spacing 5
  "Spacing between months.")
(defvar-local bv-calendar-current-date nil
  "Currently displayed date.")

(defun bv-calendar ()
  "Display calendar in dedicated window."
  (interactive)
  (let* ((buffer (get-buffer-create "*Calendar*"))
         (window (display-buffer buffer
                                 '((display-buffer-reuse-window
                                    display-buffer-below-selected)
                                   (window-height . 12)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq bv-calendar-current-date (calendar-current-date))
        (bv-calendar-render)
        (goto-char (point-min))
        (bv-calendar-mode)))
    (set-window-dedicated-p window t)
    (select-window window)))

(defun bv-calendar-render ()
  "Render calendar based on window width."
  (let* ((window-width (window-width))
         (usable-width (- window-width 4))
         (months-possible (/ usable-width (+ bv-calendar-month-width bv-calendar-month-spacing)))
         (num-months (min 3 (max 1 months-possible)))
         (current-date (or bv-calendar-current-date (calendar-current-date)))
         (month (calendar-extract-month current-date))
         (year (calendar-extract-year current-date)))
    (let ((months-data
           (cond
            ((= num-months 1)
             (list (list month year)))
            ((= num-months 2)
             (let* ((next-month (if (= month 12) 1 (1+ month)))
                    (next-year (if (= month 12) (1+ year) year)))
               (list (list month year)
                     (list next-month next-year))))
            (t
             (let* ((prev-month (if (= month 1) 12 (1- month)))
                    (prev-year (if (= month 1) (1- year) year))
                    (next-month (if (= month 12) 1 (1+ month)))
                    (next-year (if (= month 12) (1+ year) year)))
               (list (list prev-month prev-year)
                     (list month year)
                     (list next-month next-year)))))))
      (let* ((total-width (* num-months bv-calendar-month-width))
             (total-spacing (* (1- num-months) bv-calendar-month-spacing))
             (content-width (+ total-width total-spacing))
             (left-margin (/ (- usable-width content-width) 2)))
        (bv-calendar-render-months months-data left-margin)))))

(defun bv-calendar-render-months (months-data margin-left)
  "Render MONTHS-DATA with MARGIN-LEFT."
  (let ((month-strings (mapcar (lambda (data)
                                 (bv-calendar-format-month
                                  (car data) (cadr data)))
                               months-data)))
    (let ((max-lines (apply #'max (mapcar #'length month-strings))))
      (dotimes (line max-lines)
        (insert (make-string (max 0 margin-left) ?\s))
        (dotimes (m (length month-strings))
          (let* ((month-lines (nth m month-strings))
                 (month-line (or (nth line month-lines) "")))
            (insert month-line)
            (when (< m (1- (length month-strings)))
              (insert (make-string bv-calendar-month-spacing ?\s)))))
        (insert "\n")))))

(defun bv-calendar-format-month (month year)
  "Format MONTH and YEAR as list of lines."
  (let* ((first-day (calendar-day-of-week (list month 1 year)))
         (first-day (mod (- first-day calendar-week-start-day) 7))
         (last-day (calendar-last-day-of-month month year))
         (today (calendar-current-date))
         (current-date (or bv-calendar-current-date today))
         (month-name (calendar-month-name month))
         (lines '()))
    (push "" lines)
    (push (bv-calendar-center-text
           (propertize (format "%s %d" month-name year)
                       'face 'bv-calendar-header)
           bv-calendar-month-width) lines)
    (push "" lines)
    (let ((day-line ""))
      (dotimes (i 7)
        (let* ((day (mod (+ i calendar-week-start-day) 7))
               (name (aref calendar-day-abbrev-array day))
               (weekend (or (= day 0) (= day 6))))
          (setq day-line
                (concat day-line
                        (propertize (format "%-3s" (substring name 0 2))
                                    'face (if weekend
                                              'bv-calendar-weekend
                                            'bv-themes-default))))))
      (push day-line lines))
    (let ((current-line (make-string (* 3 first-day) ?\s)))
      (dotimes (day last-day)
        (let* ((d (1+ day))
               (date (list month d year))
               (dow (calendar-day-of-week date))
               (weekend (or (= dow 0) (= dow 6)))
               (is-today (equal date today))
               (is-current (equal date current-date))
               (holiday (calendar-check-holidays date)))
          (when (and (> d 1)
                     (= (mod (+ first-day d -1) 7) 0))
            (push current-line lines)
            (setq current-line ""))

          (let ((day-text (format "%2d " d))
                (map (make-sparse-keymap)))
            (define-key map [mouse-1]
              `(lambda () (interactive)
                 (setq bv-calendar-current-date ',date)
                 (bv-calendar-refresh)))
            (setq current-line
                  (concat current-line
                          (propertize day-text
                                      'face (cond
                                             ((and is-current is-today)
                                              '(:inherit (bv-calendar-current bv-calendar-today)))
                                             (is-current 'bv-calendar-current)
                                             (is-today 'bv-calendar-today)
                                             (holiday 'bv-calendar-holiday)
                                             (weekend 'bv-calendar-weekend)
                                             (t 'bv-themes-default))
                                      'mouse-face 'highlight
                                      'help-echo (format-time-string "%A, %B %e, %Y"
                                                                     (encode-time 0 0 0 d month year))
                                      'calendar-date date
                                      'keymap map))))))
      (when (> (length current-line) 0)
        (push current-line lines)))
    (nreverse (mapcar (lambda (line)
                        (if (< (length line) bv-calendar-month-width)
                            (concat line (make-string (- bv-calendar-month-width (length line)) ?\s))
                          line))
                      lines))))

(defun bv-calendar-center-text (text width)
  "Center TEXT within WIDTH."
  (let* ((text-width (length text))
         (padding (/ (- width text-width) 2)))
    (concat (make-string padding ?\s) text)))
(defvar bv-calendar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "<escape>") 'quit-window)
    (define-key map (kbd "g") 'bv-calendar-refresh)
    (define-key map (kbd ".") 'bv-calendar-goto-today)
    (define-key map (kbd "t") 'bv-calendar-goto-today)
    (define-key map (kbd "<left>") 'bv-calendar-backward-day)
    (define-key map (kbd "<right>") 'bv-calendar-forward-day)
    (define-key map (kbd "<up>") 'bv-calendar-backward-week)
    (define-key map (kbd "<down>") 'bv-calendar-forward-week)
    (define-key map (kbd "M-<left>") 'bv-calendar-backward-month)
    (define-key map (kbd "M-<right>") 'bv-calendar-forward-month)
    (define-key map (kbd "M-<up>") 'bv-calendar-backward-year)
    (define-key map (kbd "M-<down>") 'bv-calendar-forward-year)
    map)
  "Keymap for calendar.")

(defvar-local bv-calendar-last-window-width nil
  "Last window width to detect real size changes.")

(define-derived-mode bv-calendar-mode special-mode "Calendar"
  "Mode for calendar display.
\\{bv-calendar-mode-map}"
  (setq mode-line-format nil
        header-line-format nil
        cursor-type nil
        line-spacing 0.2)
  (use-local-map bv-calendar-mode-map)
  (add-hook 'window-configuration-change-hook #'bv-calendar-maybe-refresh nil t))

(defun bv-calendar-maybe-refresh ()
  "Refresh calendar only if window width actually changed."
  (when (and (eq major-mode 'bv-calendar-mode)
             (not (equal (window-width) bv-calendar-last-window-width)))
    (setq bv-calendar-last-window-width (window-width))
    (bv-calendar-refresh)))

(defun bv-calendar-refresh ()
  "Refresh calendar display."
  (interactive)
  (when (eq major-mode 'bv-calendar-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless bv-calendar-current-date
        (setq bv-calendar-current-date (calendar-current-date)))
      (bv-calendar-render)
      (goto-char (point-min)))))

(defun bv-calendar-goto-today ()
  "Go to today's date."
  (interactive)
  (setq bv-calendar-current-date (calendar-current-date))
  (bv-calendar-refresh))

(defun bv-calendar-forward-month ()
  "Move forward one month."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (new-month (if (= month 12) 1 (1+ month)))
         (new-year (if (= month 12) (1+ year) year))
         (last-day (calendar-last-day-of-month new-month new-year))
         (new-day (min day last-day)))
    (setq bv-calendar-current-date (list new-month new-day new-year))
    (bv-calendar-refresh)))

(defun bv-calendar-backward-month ()
  "Move backward one month."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (new-month (if (= month 1) 12 (1- month)))
         (new-year (if (= month 1) (1- year) year))
         (last-day (calendar-last-day-of-month new-month new-year))
         (new-day (min day last-day)))
    (setq bv-calendar-current-date (list new-month new-day new-year))
    (bv-calendar-refresh)))

(defun bv-calendar-forward-year ()
  "Move forward one year."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (new-year (1+ year))
         (last-day (calendar-last-day-of-month month new-year))
         (new-day (min day last-day)))
    (setq bv-calendar-current-date (list month new-day new-year))
    (bv-calendar-refresh)))

(defun bv-calendar-backward-year ()
  "Move backward one year."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (new-year (1- year))
         (last-day (calendar-last-day-of-month month new-year))
         (new-day (min day last-day)))
    (setq bv-calendar-current-date (list month new-day new-year))
    (bv-calendar-refresh)))

(defun bv-calendar-forward-day ()
  "Move forward one day."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day (calendar-last-day-of-month month year)))
    (cond
     ((< day last-day)
      (setq bv-calendar-current-date (list month (1+ day) year)))
     ((< month 12)
      (setq bv-calendar-current-date (list (1+ month) 1 year)))
     (t
      (setq bv-calendar-current-date (list 1 1 (1+ year)))))
    (bv-calendar-refresh)))

(defun bv-calendar-backward-day ()
  "Move backward one day."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date)))
    (cond
     ((> day 1)
      (setq bv-calendar-current-date (list month (1- day) year)))
     ((> month 1)
      (let ((prev-month (1- month)))
        (setq bv-calendar-current-date
              (list prev-month (calendar-last-day-of-month prev-month year) year))))
     (t
      (setq bv-calendar-current-date (list 12 31 (1- year)))))
    (bv-calendar-refresh)))

(defun bv-calendar-forward-week ()
  "Move forward one week."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (absolute (calendar-absolute-from-gregorian date))
         (new-date (calendar-gregorian-from-absolute (+ absolute 7))))
    (setq bv-calendar-current-date new-date)
    (bv-calendar-refresh)))

(defun bv-calendar-backward-week ()
  "Move backward one week."
  (interactive)
  (let* ((date (or bv-calendar-current-date (calendar-current-date)))
         (absolute (calendar-absolute-from-gregorian date))
         (new-date (calendar-gregorian-from-absolute (- absolute 7))))
    (setq bv-calendar-current-date new-date)
    (bv-calendar-refresh)))

(with-eval-after-load 'bv-bindings
  (when (boundp 'bv-app-map)
    (define-key bv-app-map (kbd "c") 'bv-calendar)))

(provide 'bv-calendar)
;;; bv-calendar.el ends here
