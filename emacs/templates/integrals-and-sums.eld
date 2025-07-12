latex-mode org-mode

;; Basic integrals and sums
(intg "\\int ")                    ; integral
(intl "\\int_{" p "}^{" p "} ")  ; integral with limits
(oint "\\oint ")                  ; contour integral
(ointl "\\oint_{" p "}^{" p "} ") ; contour integral with limits

(sums "\\sum ")                    ; summation
(suml "\\sum_{" p "}^{" p "} ")  ; summation with limits
(prods "\\prod ")                  ; product
(prodl "\\prod_{" p "}^{" p "} ") ; product with limits

;; Limit placement modifiers
(limt "\\lim_{" p " \\to " p "} ") ; limit
(limsup "\\limsup_{" p " \\to " p "} ") ; limit superior
(liminf "\\liminf_{" p " \\to " p "} ") ; limit inferior

;; Display style operators
(dint "\\displaystyle\\int_{" p "}^{" p "} ") ; display style integral
(dsum "\\displaystyle\\sum_{" p "}^{" p "} ") ; display style sum
(dprod "\\displaystyle\\prod_{" p "}^{" p "} ") ; display style product

;; Limits and nolimits
(intlim "\\int\\limits_{" p "}^{" p "} ")     ; force limits above/below
(intnolim "\\int\\nolimits_{" p "}^{" p "} ") ; force limits to side
(sumlim "\\sum\\limits_{" p "}^{" p "} ")     ; force limits above/below
(sumnolim "\\sum\\nolimits_{" p "}^{" p "} ") ; force limits to side

;; Common limit patterns
(limx "\\lim_{x \\to " p "} ")   ; limit as x approaches
(limn "\\lim_{n \\to \\infty} ") ; limit as n approaches infinity
(limzero "\\lim_{" p " \\to 0} ") ; limit approaching zero
(liminfty "\\lim_{" p " \\to \\infty} ") ; limit approaching infinity

;; Common sum/product patterns
(sumn "\\sum_{n=1}^{\\infty} ")  ; sum from n=1 to infinity
(sumi "\\sum_{i=1}^{n} ")        ; sum from i=1 to n
(sumk "\\sum_{k=0}^{n} ")        ; sum from k=0 to n
(prodi "\\prod_{i=1}^{n} ")      ; product from i=1 to n

;; Operators with subscripts
(maxs "\\max_{" p "} ")           ; max with subscript
(mins "\\min_{" p "} ")           ; min with subscript
(sups "\\sup_{" p "} ")           ; supremum with subscript
(infim "\\inf_{" p "} ")           ; infimum with subscript
(argmax "\\arg\\max_{" p "} ")      ; argmax
(argmin "\\arg\\min_{" p "} ")   ; argmin

latex-mode org-mode

;; Multiple integrals
(iint "\\iint ")                  ; double integral
(iintl "\\iint_{" p "}^{" p "} ") ; double integral with limits
(iiint "\\iiint ")                ; triple integral
(iiintl "\\iiint_{" p "}^{" p "} ") ; triple integral with limits
(iiiint "\\iiiint ")              ; quadruple integral
(idotsint "\\idotsint ")          ; multiple integral with dots

;; Multiple integrals with domains
(iintd "\\iint_{" p "} ")        ; double integral over domain
(iiintd "\\iiint_{" p "} ")      ; triple integral over domain
(iintxy "\\iint_{" p "} " p " \\, dx \\, dy") ; double integral with dx dy
(iiintxyz "\\iiint_{" p "} " p " \\, dx \\, dy \\, dz") ; triple integral

;; Surface and volume integrals
(oiint "\\oiint ")                ; surface integral (closed)
(oiintl "\\oiint_{" p "}^{" p "} ") ; surface integral with limits
(oiiint "\\oiiint ")              ; volume integral (closed)

;; Common multiple integral patterns
(iintR "\\iint_\\mathbb{R}^2 ")  ; integral over R^2
(iiintR "\\iiint_\\mathbb{R}^3 ") ; integral over R^3
(iintD "\\iint_D ")               ; integral over region D
(iintS "\\oiint_S ")              ; surface integral over S
(iiintV "\\iiint_V ")             ; volume integral over V

;; With measures
(iintdA "\\iint_{" p "} " p " \\, dA") ; with area element
(iiintdV "\\iiint_{" p "} " p " \\, dV") ; with volume element
(oiintdS "\\oiint_{" p "} " p " \\, dS") ; with surface element

;; Repeated integrals
(intint "\\int \\int ")           ; two separate integrals
(intintint "\\int \\int \\int ")  ; three separate integrals
(intxy "\\int_{" p "}^{" p "} \\int_{" p "}^{" p "} ") ; iterated integral
(intxyz "\\int_{" p "}^{" p "} \\int_{" p "}^{" p "} \\int_{" p "}^{" p "} ") ; triple iterated

;; Fancy integration patterns
(intcirc "\\oint ")               ; circular integral
(intcircC "\\oint_C ")            ; integral around curve C
(intsurf "\\iint_S ")             ; surface integral
(intvol "\\iiint_V ")             ; volume integral

latex-mode org-mode

;; Multiline subscripts/superscripts using subarray
(subarray "_{\\subarray{l} " p " \\\\ " p " \\endsubarray}")  ; left-aligned subscript array
(subarrayc "_{\\subarray{c} " p " \\\\ " p " \\endsubarray}") ; centered subscript array
(subarrayr "_{\\subarray{r} " p " \\\\ " p " \\endsubarray}") ; right-aligned subscript array

(suparray "^{\\subarray{l} " p " \\\\ " p " \\endsubarray}")  ; left-aligned superscript array
(suparrayc "^{\\subarray{c} " p " \\\\ " p " \\endsubarray}") ; centered superscript array
(suparrayr "^{\\subarray{r} " p " \\\\ " p " \\endsubarray}") ; right-aligned superscript array

;; Common multiline patterns
(sumlinesub "\\sum_{\\subarray{l} " p " \\\\ " p " \\endsubarray}") ; sum with multiline subscript
(prodlinesub "\\prod_{\\subarray{c} " p " \\\\ " p " \\endsubarray}") ; product with multiline subscript
(intlinesub "\\int_{\\subarray{l} " p " \\\\ " p " \\endsubarray}") ; integral with multiline subscript

;; Using substack (amsmath)
(substack "_{\\substack{" p " \\\\ " p "}}") ; substack subscript
(supstack "^{\\substack{" p " \\\\ " p "}}") ; substack superscript
(sumstack "\\sum_{\\substack{" p " \\\\ " p "}}") ; sum with substack
(prodstack "\\prod_{\\substack{" p " \\\\ " p "}}") ; product with substack

;; Common use cases
(sumij "\\sum_{\\substack{i=1 \\\\ j=1}}^{\\substack{m \\\\ n}}") ; double index sum
(sumnk "\\sum_{\\substack{1 \\le i \\le n \\\\ 1 \\le j \\le m}}") ; sum with conditions
(maxsub "\\max_{\\substack{x \\in A \\\\ y \\in B}}") ; max with constraints
(limsub "\\lim_{\\substack{x \\to 0 \\\\ y \\to 0}}") ; limit with multiple variables

;; Alternative stacking methods (see fractions section for atop)
(above "{" p " \\above 0pt " p "}") ; TeX primitive stacking

;; Aligned subscripts/superscripts
(subalign "_{\\begin{aligned} " p " &= " p " \\\\ " p " &= " p " \\end{aligned}}") ; aligned subscript
(supalign "^{\\begin{aligned} " p " &= " p " \\\\ " p " &= " p " \\end{aligned}}") ; aligned superscript

latex-mode org-mode

;; Basic sideset usage
(sideset "\\sideset{" p "}{" p "}" p) ; general sideset
(sidesetl "\\sideset{_{" p "}^{" p "}}{}" p) ; left sub/superscript
(sidesetr "\\sideset{}{_{" p "}^{" p "}}" p) ; right sub/superscript
(sidesetlr "\\sideset{_{" p "}^{" p "}}{_{" p "}^{" p "}}" p) ; both sides

;; Common sideset patterns
(sidesum "\\sideset{}{'}\\sum")   ; sum with prime
(sideprod "\\sideset{}{'}\\prod") ; product with prime
(sideint "\\sideset{}{'}\\int")   ; integral with prime

;; Multiple primes and indices
(sideprime "\\sideset{}{'}" p)    ; add prime to operator
(sidedprime "\\sideset{}{''}" p)  ; add double prime
(sidetprime "\\sideset{}{'''}" p) ; add triple prime

;; Complex sideset examples
(sidesumfull "\\sideset{_{" p "}^{" p "}}{_{" p "}^{" p "}}\\sum") ; sum with all indices
(sideprodfull "\\sideset{_{" p "}^{" p "}}{_{" p "}^{" p "}}\\prod") ; product with all indices

;; With specific operators
(sidebigop "\\sideset{}{'}\\bigop") ; generic big operator with prime
(sidebigoplus "\\sideset{}{'}\\bigoplus") ; direct sum with prime
(sidebigotimes "\\sideset{}{'}\\bigotimes") ; tensor product with prime

;; Practical examples
(sumstar "\\sideset{}{^*}\\sum")  ; sum with star
(prodstar "\\sideset{}{^*}\\prod") ; product with star
(sumab "\\sideset{^a_b}{}\\sum")  ; sum with left indices
(prodcd "\\sideset{}{^c_d}\\prod") ; product with right indices

;; Combining with limits
(sidesumij "\\sideset{}{'}\\sum_{i=1}^{n}") ; sideset sum with limits
(sideprodkl "\\sideset{}{'}\\prod_{k,l}") ; sideset product with double index
