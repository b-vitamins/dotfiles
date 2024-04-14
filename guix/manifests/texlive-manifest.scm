(use-modules (gnu packages texlive))

;; Define the Emacs package manifest
(specifications->manifest '("texlive" "texlive-biblatex" "texlive-biber"
                            "texlive-tex-gyre" "texlive-tex-gyre-math"))
