(use-modules (guix packages)
             (guix utils)
             (srfi srfi-1)
             (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 textual-ports)
             (srfi srfi-26)
             (ice-9 getopt-long))
; Added for command-line options parsing

;; Read the entire content of a file and return it as a string.
(define (read-file-to-string file)
  (call-with-input-file file
    (lambda (port)
      (get-string-all port))))

;; Isolate and return a list of package definitions from the content string.
(define (extract-package-definitions content)
  (let* ((port (open-input-string content))
         (packages '()))
    (let loop
      ((expr (read port)))
      (cond
        ((eof-object? expr)
         (reverse packages))
        ((and (pair? expr)
              (eq? (car expr)
                   'define-public))
         (loop (begin
                 (set! packages
                       (cons expr packages))
                 (read port))))
        (else (loop (read port)))))))

;; Find and return a list of unique packages, keeping only the first occurrence.
(define (filter-unique-packages packages)
  (let loop
    ((remaining-packages packages)
     (unique-packages '()))
    (match remaining-packages
      (() (reverse unique-packages))
      ((first . rest) (if (any (cut equal? first <>) unique-packages)
                          ;; Skip this duplicate
                          (loop rest unique-packages)
                          ;; Keep the package
                          (loop rest
                                (cons first unique-packages)))))))

;; Save the deduplicated packages along with the non-package content.
(define (save-to-file non-package-content packages output-file)
  (call-with-output-file output-file
    (lambda (port)
      ;; Write the non-package content
      (display non-package-content port)
      ;; Write the deduplicated packages
      (for-each (lambda (package
                          )
                  (pretty-print package port)
                  (format port "\n")) packages))))

;; Split the content into non-package and package sections.
(define (split-content content)
  (let ((package-start (string-contains content "(define-public ")))
    (if package-start
        (values (substring content 0 package-start)
                (substring content package-start))
        (values content ""))))

;; Main script execution
(define (main args)
  (let* ((opts `((input (single-char #\i)
                        (value #t))
                 (output (single-char #\o)
                         (value #t))))
         (result (getopt-long args opts))
         (input-file (assoc-ref result
                                'input))
         (output-file (assoc-ref result
                                 'output)))
    
    ;; Ensure input file is provided
    (unless input-file
      (error
       "No input file specified! Use -i or --input to specify the input file."))

    ;; Set default output file if not provided
    (unless output-file
      (set! output-file
            (string-append (string-drop-right input-file 4) ".dedup.scm")))

    ;; Read and process the file
    (let* ((content (read-file-to-string input-file))
           (non-package-content "")
           (packages '())
           (unique-packages '()))
      
      ;; Split the content into non-package and package sections
      (call-with-values (lambda ()
                          (split-content content))
                        (lambda (np-content pkg-content)
                          (set! non-package-content np-content)
                          (set! packages
                                (extract-package-definitions pkg-content))
                          (set! unique-packages
                                (filter-unique-packages packages))))

      ;; Save the deduplicated content to a new file
      (save-to-file non-package-content unique-packages output-file)

      ;; Inform the user
      (format #t "Deduplicated packages saved to ~a~%" output-file))))

;; Execute the main function with command-line arguments
(main (command-line))
