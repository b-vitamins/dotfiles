latex-mode org-mode

;; Math style commands
(dstyle "\\displaystyle ")            ; display style (full size)
(textstyle "\\textstyle ")               ; text style (inline size)
(scriptstyle "\\scriptstyle ")             ; script style (subscript size)
(sss "\\scriptscriptstyle ")      ; scriptscript style (sub-subscript size)

;; Common applications
(dsfrac "{\\displaystyle\\frac{" p "}{" p "}}") ; display style fraction
(dssum "{\\displaystyle\\sum_{" p "}^{" p "}}") ; display style sum
(dsprod "{\\displaystyle\\prod_{" p "}^{" p "}}") ; display style product
(dsint "{\\displaystyle\\int_{" p "}^{" p "}}") ; display style integral

;; With nolimits
(dsnl "{\\displaystyle " p "\\nolimits}") ; display style with side limits
(dssuml "{\\displaystyle\\sum\\nolimits_{" p "}^{" p "}}") ; sum with side limits
(dsprodl "{\\displaystyle\\prod\\nolimits_{" p "}^{" p "}}") ; product with side limits

;; Text style variants
(tsfrac "{\\textstyle\\frac{" p "}{" p "}}") ; text style fraction
(tssum "{\\textstyle\\sum_{" p "}^{" p "}}") ; text style sum
(tsint "{\\textstyle\\int_{" p "}^{" p "}}") ; text style integral

;; Script style
(ssfrac "{\\scriptstyle\\frac{" p "}{" p "}}") ; script style fraction
(sssfrac "{\\scriptscriptstyle\\frac{" p "}{" p "}}") ; scriptscript fraction

;; Mixed styles in fractions
(fracds "\\frac{\\displaystyle " p "}{\\displaystyle " p "}") ; both parts display
(fracdsts "\\frac{\\displaystyle " p "}{\\textstyle " p "}") ; numerator display
(fractsds "\\frac{\\textstyle " p "}{\\displaystyle " p "}") ; denominator display

;; Sizing in specific contexts
(dsub "_{\\displaystyle " p "}")  ; display style subscript
(dsup "^{\\displaystyle " p "}")  ; display style superscript
(tssub "_{\\textstyle " p "}")     ; text style subscript
(tsup "^{\\textstyle " p "}")     ; text style superscript

;; Common patterns
(bigfrac "\\displaystyle\\frac{" p "}{" p "}") ; big fraction
(bigsum "\\displaystyle\\sum_{" p "}^{" p "} ") ; big summation
(bigint "\\displaystyle\\int_{" p "}^{" p "} ") ; big integral
(bigprod "\\displaystyle\\prod_{" p "}^{" p "} ") ; big product

;; Inline operators forced to display size
(inlinesum "$\\displaystyle\\sum_{" p "}^{" p "}$ ") ; inline display sum
(inlineint "$\\displaystyle\\int_{" p "}^{" p "}$ ") ; inline display integral

;; Resizing delimiters with content
(lr "\\left(" p "\\right)")    ; auto-sized parentheses
(Biglr "\\bigl(" p "\\bigr)")     ; manually sized parentheses
(bigglr "\\biggl(" p "\\biggr)")  ; larger parentheses

;; Math operator size
(limits "\\limits ")              ; force limits above/below
(nolimits "\\nolimits ")          ; force limits to the side

;; Size declarations (scope-based)
(dscope "{\\displaystyle " r "}") ; display style scope
(tscope "{\\textstyle " r "}")    ; text style scope
(sscope "{\\scriptstyle " r "}")  ; script style scope

;; Common mistakes to avoid (for reference)
;; Wrong: \displaystyle{...}
;; Right: {\displaystyle ...}
;; These templates follow the correct pattern
