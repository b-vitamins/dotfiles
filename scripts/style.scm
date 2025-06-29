(use-modules (ice-9 ftw)
             (guix scripts style)
             (ice-9 getopt-long))

;; Function to apply guix style to a file and keep count
(define (apply-guix-style file count)
  (guix-style "--whole-file" file)
  (format #t "Styled: ~a~%" file)
  (+ count 1))

;; Function to process a single .scm file
(define (process-single-file file)
  (if (and (string-suffix? ".scm" file)
           (file-exists? file)
           (eq? 'regular
                (stat:type (stat file))))
      (begin
        (apply-guix-style file 0) 1)
      (begin
        (format #t "Skipping non-Scheme file: ~a~%" file) 0)))

;; Function to process each file or directory using file-system-fold
(define (process-directory root-dir)
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
    (format #t "Error processing ~a: ~a~%" path
            (strerror errno)) result)

  ;; Start the file-system-fold traversal with initial count 0
  (file-system-fold enter?
                    leaf
                    down
                    up
                    skip
                    handle-error
                    0
                    root-dir))

;; Main function to handle command line arguments
(define (main args)
  (let* ((opts '((help (single-char #\h))
                 (version (single-char #\v))))
         (result (getopt-long args opts))
         (remaining (assoc-ref result
                               '())))
    
    ;; Handle help option
    (when (assoc-ref result
                     'help)
      (display
       "Usage: style.scm [OPTION] [FILE-OR-DIRECTORY]...
Apply guix style formatting to Scheme files.

If FILE-OR-DIRECTORY is a file, style that file (if it's a .scm file).
If FILE-OR-DIRECTORY is a directory, recursively style all .scm files in it.
If no arguments are provided, style all .scm files in the parent directory.

Options:
  -h, --help     Display this help message
  -v, --version  Display version information

Examples:
  style.scm file.scm           # Style a single file
  style.scm src/               # Style all .scm files in src/ directory
  style.scm file1.scm dir/     # Style file1.scm and all .scm files in dir/
  style.scm                    # Style all .scm files in parent directory
")
      (exit 0))

    ;; Handle version option
    (when (assoc-ref result
                     'version)
      (display "style.scm 1.0.0\n")
      (exit 0))

    ;; Determine target paths
    (let ((targets (if (null? remaining)
                       ;; No arguments - use parent directory as default
                       (list (dirname (dirname (car (command-line)))))
                       ;; Use provided arguments
                       remaining)))
      
      ;; Process each target
      (let ((total-files 0))
        (for-each (lambda (target)
                    (cond
                      ((not (file-exists? target))
                       (format #t "Error: ~a does not exist~%" target))
                      ((eq? 'directory
                            (stat:type (stat target)))
                       (let ((dir-count (process-directory target)))
                         (set! total-files
                               (+ total-files dir-count))
                         (format #t "Processed ~a files in directory: ~a~%"
                                 dir-count target)))
                      ((eq? 'regular
                            (stat:type (stat target)))
                       (let ((file-count (process-single-file target)))
                         (set! total-files
                               (+ total-files file-count))))
                      (else (format #t
                                    "Skipping ~a (not a file or directory)~%"
                                    target)))) targets)

        ;; Print summary
        (format #t "~%Total: ~a Scheme file(s) formatted with guix style.~%"
                total-files)))))

;; Execute the main function with command-line arguments
(main (command-line))
