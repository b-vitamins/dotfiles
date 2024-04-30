;; -*- mode: scheme; -*-
;; guile-manifest.scm
(use-modules (gnu packages guile)
             (gnu packages guile-xyz))

(specifications->manifest '("guile-lib" "guile-studio"
                            "guile-readline"
                            "guile-colorized"
                            "guile-sqlite3"
                            "guile-json"
                            "guile-goblins"
                            "guile-fibers"
                            "guile-openai"
                            "guile-xapian"))
