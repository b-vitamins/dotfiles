;; -*- mode: scheme; -*-
;; texlive-manifest.scm
(use-modules (gnu packages texlive))

(specifications->manifest '("texlive" ;The TeX Live package, a comprehensive TeX system
                            "texlive-biblatex" ;Complete reimplementation of bibliographic facilities for LaTeX
                            "texlive-biber" ;A BibTeX replacement for users of BibLaTeX
                            "texlive-tex-gyre" ;A collection of fonts based on the URW++ fonts
                            "texlive-tex-gyre-math" ;Maths fonts to match tex-gyre text fonts
                            ))

