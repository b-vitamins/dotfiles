(use-modules (ice-9 ftw)
             (guix scripts style))

;; Function to apply guix style to a file and keep count
(define (apply-guix-style file count)
  (guix-style "--whole-file" file)
  (+ count 1))

;; Function to process each file or directory using file-system-fold
(define (process-files root-dir)
  (define (enter? path stat result)
    ;; Always enter directories
    (eq? 'directory
         (stat:type stat)))

  (define (leaf path stat result)
    ;; Apply guix style if it's a .scm file and count it
    (if (and (string-suffix? ".scm" path)
             (eq? 'regular
                  (stat:type stat)))
        (apply-guix-style path result) result))

  (define (down path stat result)
    result)

  (define (up path stat result)
    result)

  (define (skip path stat result)
    result)

  (define (handle-error path stat errno result)
    result)

  ;; Start the file-system-fold traversal with initial count 0
  (file-system-fold enter?
                    leaf
                    down
                    up
                    skip
                    handle-error
                    0
                    root-dir))

;; Get the script directory and move to its parent
(define script-path
  (car (command-line)))
(define script-dir
  (dirname script-path))
(define target-dir
  (dirname script-dir))

;; Process the target directory recursively and count formatted files
(define total-files
  (process-files target-dir))

;; Print a confirmation message with the total number of files formatted
(format #t "~a scheme files in ~a have been formatted with guix style.~%"
        total-files target-dir)
