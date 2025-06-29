(use-modules (ice-9 format)
             (ice-9 match)
             (guix build utils))

(define %user
  (or (getenv "SUDO_USER")
      (getenv "USER")))

(define %user-id
  (passwd:uid (getpwnam %user)))

(define %group-id
  (passwd:gid (getpwnam %user)))

(define %CA-directory
  "/etc/ssl-ca")

(define subject-template
  "/C=DE/ST=Berlin/L=Berlin/O=GNU Guix/OU=Cuirass/CN=~a")

(define CA-key
  (string-append %CA-directory "/private/ca.key"))
(define CA-cert
  (string-append %CA-directory "/certs/ca.crt"))

(define* (output who file)
  (string-append (getcwd) "/" who file))

(define (key-file who)
  "Return the absolute file name of the key file for WHO."
  (output who ".key"))

(define (csr-file who)
  "Return the absolute file name of the CSR file for WHO."
  (output who ".csr"))

(define (client-cert-file who)
  "Return the absolute file name of the client certificate file for
WHO."
  (output who ".crt"))

(define (exported-cert-file who)
  "Return the absolute file name of the pkcs12 client certificate file
for WHO.  This is the file that users should import into their
browsers."
  (output who ".p12"))

(define (generate-ca!)
  "Generate a private certificate authority (CA) valid for 10 years."
  (mkdir-p (dirname CA-key))
  (mkdir-p (dirname CA-cert))
  (invoke "openssl"
          "req"
          "-newkey"
          "rsa"
          "-x509"
          "-days"
          "3650"
          "-noenc" ;no password
          "-subj"
          (format #f "~@?" subject-template "Cuirass CA")
          "-keyout"
          CA-key
          "-out"
          CA-cert))

(define (generate-csr! who)
  "Generate a new certificate signing request and key for WHO."
  (let ((key (key-file who))
        (csr (csr-file who)))
    (invoke "openssl"
            "req"
            "-newkey"
            "rsa"
            "-noenc" ;no password
            "-subj"
            (format #f "~@?" subject-template who)
            "-keyout"
            key
            "-out"
            csr)
    (chown key %user-id %group-id)
    (chown csr %user-id %group-id)))

(define* (generate-client-certificate! who
                                       #:key (expiry 365))
  "Generate a client certificate for WHO."
  (let ((cert (client-cert-file who)))
    (invoke "openssl"
            "x509"
            "-req"
            "-in"
            (csr-file who)
            "-CA"
            CA-cert
            "-CAkey"
            CA-key
            "-out"
            cert
            "-days"
            (number->string expiry))
    (chown cert %user-id %group-id)))

(define (export-p12! who)
  (let ((key (key-file who))
        (exported-cert (exported-cert-file who)))
    (invoke "openssl"
            "pkcs12"
            "-export"
            "-in"
            (client-cert-file who)
            "-inkey"
            key
            "-out"
            exported-cert)
    (chown key %user-id %group-id)
    (chown exported-cert %user-id %group-id)))

(define (main args)
  (match (command-line)
    ((script)
     (set-program-arguments (list script %user))
     (apply main args))
    ((script "--generate-ca")
     (generate-ca!))
    ((script who)
     (generate-csr! who)
     (generate-client-certificate! who)
     (export-p12! who))
    ((script . rest) (format (current-error-port)
                             "usage: ~a [--generate-ca|name]~%" script))))
