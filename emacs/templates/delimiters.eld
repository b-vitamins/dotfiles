latex-mode org-mode

;; Basic delimiter commands
(lp "\\left(" p "\\right)")      ; auto-sized parentheses
(lbr "\\left[" p "\\right]")      ; auto-sized brackets
(lbc "\\left\\{" p "\\right\\}") ; auto-sized braces
(lang "\\left\\langle" p "\\right\\rangle") ; auto-sized angle brackets
(lvb "\\left|" p "\\right|")      ; auto-sized vertical bars
(lVb "\\left\\|" p "\\right\\|")  ; auto-sized double vertical bars

;; Manual sizing - small to large
(bigp "\\big(" p "\\big)")         ; big parentheses
(bgg "\\Big(" p "\\Big)")        ; Big parentheses
(bggg "\\bigg(" p "\\bigg)")     ; bigg parentheses
(bgggg "\\Bigg(" p "\\Bigg)")    ; Bigg parentheses

;; Left/right variants for manual sizing
(biglr "\\bigl(" p "\\bigr)")      ; big left/right
(bggl "\\Bigl(" p "\\Bigr)")     ; Big left/right
(bgggl "\\biggl(" p "\\biggr)")  ; bigg left/right
(bggggl "\\Biggl(" p "\\Biggr)") ; Bigg left/right

;; Other delimiter types with sizing
(bgb "\\bigl[" p "\\bigr]")      ; big brackets
(bggb "\\Bigl[" p "\\Bigr]")     ; Big brackets
(bgggb "\\biggl[" p "\\biggr]")  ; bigg brackets
(bggggb "\\Biggl[" p "\\Biggr]") ; Bigg brackets

(bgc "\\bigl\\{" p "\\bigr\\}")  ; big braces
(bggc "\\Bigl\\{" p "\\Bigr\\}") ; Big braces
(bgggc "\\biggl\\{" p "\\biggr\\}") ; bigg braces
(bggggc "\\Biggl\\{" p "\\Biggr\\}") ; Bigg braces

(bga "\\bigl\\langle" p "\\bigr\\rangle") ; big angle
(bgga "\\Bigl\\langle" p "\\Bigr\\rangle") ; Big angle
(bggga "\\biggl\\langle" p "\\biggr\\rangle") ; bigg angle
(bgggga "\\Biggl\\langle" p "\\Biggr\\rangle") ; Bigg angle

(bgv "\\bigl|" p "\\bigr|")      ; big vert
(bggv "\\Bigl|" p "\\Bigr|")     ; Big vert
(bgggv "\\biggl|" p "\\biggr|")  ; bigg vert
(bggggv "\\Biggl|" p "\\Biggr|") ; Bigg vert

(bgV "\\bigl\\|" p "\\bigr\\|")  ; big double vert
(bggV "\\Bigl\\|" p "\\Bigr\\|") ; Big double vert
(bgggV "\\biggl\\|" p "\\biggr\\|") ; bigg double vert
(bggggV "\\Biggl\\|" p "\\Biggr\\|") ; Bigg double vert

;; Common patterns
(lps "\\left(\\sum_{" p "}^{" p "} " r " \\right)") ; sum with auto-sized parens
(bgs "\\bigl(\\sum_{" p "}^{" p "} " r " \\bigr)")  ; sum with big parens
(bggs "\\Bigl[\\sum_{" p "}^{" p "} " r " \\Bigr]") ; sum with Big brackets

;; Nested delimiters
(nest2 "\\bigl(" p "\\left(" r "\\right)" p "\\bigr)") ; two levels
(nest3 "\\Bigl[" p "\\bigl(" p "\\left(" r "\\right)" p "\\bigr)" p "\\Bigr]") ; three levels

;; Special delimiter combinations
(floor "\\lfloor " p " \\rfloor") ; floor function
(ceil "\\lceil " p " \\rceil")    ; ceiling function
(lfloor "\\left\\lfloor " p " \\right\\rfloor") ; auto-sized floor
(lceil "\\left\\lceil " p " \\right\\rceil")    ; auto-sized ceiling

latex-mode org-mode

;; Proper paired delimiters (recommended)
(abs "\\lvert " p " \\rvert")    ; absolute value |x|
(norm "\\lVert " p " \\rVert")   ; norm ||x||
(labs "\\left\\lvert " p " \\right\\rvert")   ; auto-sized absolute value
(lnorm "\\left\\lVert " p " \\right\\rVert")  ; auto-sized norm

;; Common mathematical constructs with vertical bars
(set "\\{" p " \\mid " p "\\}")  ; set notation {x | condition}
(lset "\\left\\{" p " \\,\\middle\\vert\\, " p "\\right\\}") ; auto-sized set
(cond "P(" p " \\mid " p ")")    ; conditional probability P(A|B)
(divides " \\mid ")                  ; divides relation a|b
(ndiv " \\nmid ")                ; does not divide

;; Sized vertical bar notations
(bgabs "\\bigl\\lvert " p " \\bigr\\rvert")     ; big absolute value
(bggabs "\\Bigl\\lvert " p " \\Bigr\\rvert")    ; Big absolute value
(bgggabs "\\biggl\\lvert " p " \\biggr\\rvert") ; bigg absolute value
(bggggabs "\\Biggl\\lvert " p " \\Biggr\\rvert") ; Bigg absolute value

(bgnorm "\\bigl\\lVert " p " \\bigr\\rVert")    ; big norm
(bggnorm "\\Bigl\\lVert " p " \\Bigr\\rVert")   ; Big norm
(bgggnorm "\\biggl\\lVert " p " \\biggr\\rVert") ; bigg norm
(bggggnorm "\\Biggl\\lVert " p " \\Biggr\\rVert") ; Bigg norm

;; Evaluation bars
(eval "\\bigl." p "\\bigr|_{" p "}^{" p "}") ; evaluation from a to b
(evalat "\\bigl." p "\\bigr|_{" p "}")        ; evaluation at point
(at "\\big|_{" p "}")                         ; simple "at" notation

;; Definition in preamble (for reference)
(defabs "\\providecommand{\\abs}[1]{\\lvert#1\\rvert}") ; define \abs command
(defnorm "\\providecommand{\\norm}[1]{\\lVert#1\\rVert}") ; define \norm command

;; Matrix determinant
(detm "\\begin{vmatrix} " r " \\end{vmatrix}") ; determinant with vert bars
(Det "\\begin{Vmatrix} " r " \\end{Vmatrix}") ; matrix norm with double bars

;; Common patterns
(absfrac "\\left\\lvert\\frac{" p "}{" p "}\\right\\rvert") ; |a/b|
(normsum "\\left\\lVert\\sum_{" p "}^{" p "} " p "\\right\\rVert") ; ||sum||
(innerprod "\\langle " p " \\mid " p " \\rangle") ; inner product <a|b>
(braket "\\langle " p " | " p " | " p " \\rangle") ; bra-ket notation <ψ|H|ψ>
