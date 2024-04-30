;; -*- mode: scheme; -*-
;; fonts-manifest.scm
(use-modules (gnu packages fonts))

(specifications->manifest '("font-gnu-unifont" "font-gnu-freefont"
                            "font-liberation"
                            "font-hack"
                            "font-fira-go"
                            "font-fira-mono"
                            "font-fira-code"
                            "font-fira-sans"
                            "font-tamzen"
                            "font-terminus"
                            "font-openmoji"
                            "font-mathjax"
                            "font-libertinus"
                            "font-lato"
                            "font-jetbrains-mono"))
