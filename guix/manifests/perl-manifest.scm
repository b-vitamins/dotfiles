(use-modules (gnu packages perl))

;; Define the Perl package manifest with a broader range of useful modules
(specifications->manifest '("perl" ;Core Perl language
                            "perl-json" ;JSON parsing and generating
                            "perl-json-any" ;Wrapper module that chooses a JSON implementation
                            "perl-json-parse" ;Simple, correct JSON parser
                            "perl-lwp-useragent-determined" ;LWP::UserAgent with retries
                            "perl-lwp-useragent-cached" ;Caching for LWP::UserAgent
                            "perl-lwp-online" ;Check if LWP can connect to the Internet
                            "perl-lwp-protocol-https" ;HTTPS support for LWP::UserAgent
                            "perl-parallel-forkmanager" ;Simple parallel processing fork manager
                            "perl-image-magick" ;Interface to ImageMagick for image manipulation
                            "perl-readonly" ;Facility for creating read-only scalars, arrays, hashes
                            "perl-readonly-xs" ;Improved performance for Readonly using XS
                            "perl-dbi" ;Database independent interface for Perl
                            "perl-dbd-sqlite" ;Self-contained RDBMS in a DBI Driver (SQLite)
                            "perl-xml-simple" ;Easy API to maintain XML (esp. config files)
                            "perl-cgi" ;Simple Common Gateway Interface class
                            "perl-carp-assert" ;Executable comments like the ANSI C assert macro
                            "perl-date-manip" ;Date manipulation routines
                            "perl-file-slurp" ;Simple and efficient reading/writing of complete files
                            "perl-net-ssleay" ;Perl extension for using OpenSSL
                            "perl-uri" ;Uniform Resource Identifiers (URI) management
                            "perl-www-mechanize" ;Handy web browsing in a Perl object
                            "perl-html-parser" ;HTML parsing class
                            "perl-email-simple" ;Simple parsing of RFC2822 message format and headers
                            "perl-log-log4perl" ;Log4j implementation for Perl
                            "perl-test-simple" ;Basic utilities for writing tests
                            "perl-yaml-tiny" ;Read/Write YAML files with as little code as possible
                            "perl-try-tiny" ;Minimal try/catch with no dependencies
                            ))
