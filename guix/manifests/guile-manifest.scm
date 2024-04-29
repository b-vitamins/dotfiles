;; -*- mode: scheme; -*-
;; guile-manifest.scm
(use-modules (gnu packages guile)
             (gnu packages guile-xyz))

(specifications->manifest '("guile-lib"
                            "guile-studio"
                            "guile-sqlite3"
                            "guile-json"
                            "guile-goblins"
                            "guile-fibers"
                            "guile-dbi"
                            "guile-openai"
                            "guile-lens"
                            "guile-xapian"))
