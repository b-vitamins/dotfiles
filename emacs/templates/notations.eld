latex-mode org-mode

;; Over/under embellishments (already defined in math accents)
;; See math accents section for: what, wtil, olin, ulin, obr, ubr
;; See math accents section for: ola, ora, olra, ula, ura, ulra

;; Labels on embellishments
(obl "\\overbrace{" p "}^{" p "}") ; overbrace with label
(ubl "\\underbrace{" p "}_{" p "}") ; underbrace with label

latex-mode org-mode

;; Extensible arrows with super/subscripts
(xla "\\xleftarrow{" p "}")      ; xleftarrow (superscript only)
(xra "\\xrightarrow{" p "}")     ; xrightarrow (superscript only)
(xlasub "\\xleftarrow[" p "]{" p "}") ; xleftarrow with sub and super
(xrasub "\\xrightarrow[" p "]{" p "}") ; xrightarrow with sub and super

;; Extensible arrows from mathtools package
(xlra "\\xleftrightarrow{" p "}") ; xleftrightarrow
(xLa "\\xLeftarrow{" p "}")      ; xLeftarrow
(xRa "\\xRightarrow{" p "}")     ; xRightarrow
(xLra "\\xLeftrightarrow{" p "}") ; xLeftrightarrow
(xmt "\\xmapsto{" p "}")         ; xmapsto
(xhra "\\xhookrightarrow{" p "}") ; xhookrightarrow
(xhla "\\xhookleftarrow{" p "}") ; xhookleftarrow

;; With optional subscripts (mathtools)
(xlrasub "\\xleftrightarrow[" p "]{" p "}") ; with sub and super
(xLasub "\\xLeftarrow[" p "]{" p "}") ; with sub and super
(xRasub "\\xRightarrow[" p "]{" p "}") ; with sub and super
(xLrasub "\\xLeftrightarrow[" p "]{" p "}") ; with sub and super

latex-mode org-mode

;; Stacking symbols
(os "\\overset{" p "}{" p "}")   ; overset - symbol above
(us "\\underset{" p "}{" p "}")  ; underset - symbol below
(stk "\\stackrel{" p "}{" p "}")  ; stackrel (older, prefer overset)

;; Advanced affixing (see sideset in section 8.4)
(ss "\\sideset{" p "}{" p "}" p) ; sideset for corner symbols

latex-mode org-mode

;; Basic matrix environments
(matrix "\\begin{matrix}" n> r> n> "\\end{matrix}")     ; no delimiters
(pmat "\\begin{pmatrix}" n> r> n> "\\end{pmatrix}")  ; ( ) parentheses
(bmat "\\begin{bmatrix}" n> r> n> "\\end{bmatrix}")  ; [ ] brackets
(Bmat "\\begin{Bmatrix}" n> r> n> "\\end{Bmatrix}")  ; { } braces
(vmat "\\begin{vmatrix}" n> r> n> "\\end{vmatrix}")  ; | | single bars
(Vmat "\\begin{Vmatrix}" n> r> n> "\\end{Vmatrix}")  ; || || double bars

;; Small matrices (inline)
(smat "\\begin{smallmatrix}" n> r> n> "\\end{smallmatrix}") ; small matrix
(spmat "\\bigl(\\begin{smallmatrix}" n> r> n> "\\end{smallmatrix}\\bigr)") ; small pmatrix
(sbmat "\\bigl[\\begin{smallmatrix}" n> r> n> "\\end{smallmatrix}\\bigr]") ; small bmatrix

;; Matrix templates with content
(mat22 "\\begin{pmatrix}" n> p " & " p " \\\\" n> p " & " p n> "\\end{pmatrix}") ; 2x2
(mat33 "\\begin{pmatrix}" n> p " & " p " & " p " \\\\" n> p " & " p " & " p " \\\\" n> p " & " p " & " p n> "\\end{pmatrix}") ; 3x3

;; Row operations
(hdf "\\hdotsfor{" p "}")        ; horizontal dots for n columns
(nr "\\\\")                       ; new row
(col " & ")                        ; column separator

;; Cases environment
(cases "\\begin{cases}" n> r> n> "\\end{cases}")      ; cases
(cas2 "\\begin{cases}" n> p " & \\text{if } " p " \\\\" n> p " & \\text{otherwise}" n> "\\end{cases}") ; 2-case template

;; Array environment (general purpose)
(arr "\\begin{array}{" p "}" n> r> n> "\\end{array}") ; array with spec

latex-mode org-mode

;; Positive spacing
(thins "\\,")                   ; thin space (3mu)
(meds "\\:")                    ; medium space (4mu)
(thicksp "\\;")                  ; thick space (5mu)
(quad "\\quad")                     ; 1em space
(qq "\\qquad")                   ; 2em space

;; Negative spacing
(negthinsp "\\!")                ; negative thin space (-3mu)
(nts "\\negthinspace")           ; same as \!
(nms "\\negmedspace")            ; negative medium space
(nks "\\negthickspace")          ; negative thick space

;; Named spacing
(thinspace "\\thinspace")               ; thin space
(medspace "\\medspace")                ; medium space
(ks "\\thickspace")              ; thick space

;; Custom spacing
(hsp "\\hspace{" p "}")           ; horizontal space
(hs* "\\hspace*{" p "}")         ; non-breakable hspace
(vs "\\vspace{" p "}")           ; vertical space
(vs* "\\vspace*{" p "}")         ; non-breakable vspace
(msp "\\mspace{" p "mu}")        ; math space in mu units

;; Phantom spacing
(phantom "\\phantom{" p "}")          ; full phantom (width + height)
(hph "\\hphantom{" p "}")        ; horizontal phantom (width only)
(vph "\\vphantom{" p "}")        ; vertical phantom (height only)

;; Fine-tuning
(mk "\\mkern" p "mu")            ; math kern
(ke "\\kern" p)                  ; general kern

latex-mode org-mode

;; Basic dots (already in punctuation - see punctuation section)

;; Semantic dots (amsmath - preferred)
(dc "\\dotsc")                   ; dots with commas
(db "\\dotsb")                   ; dots with binary operators/relations
(dotsm "\\dotsm")                   ; dots with multiplication
(di "\\dotsi")                   ; dots with integrals
(dotso "\\dotso")                   ; other dots
(dt "\\dots")                    ; auto dots (context sensitive)

;; Special usage
(dts "\\ldots,")                 ; dots followed by comma
(dtb "\\cdots +")                ; dots in binary context

latex-mode org-mode

;; Nonbreaking dash
(nbd "\\nobreakdash")            ; prevent line break after dash
(nbd- "\\nobreakdash-")          ; nonbreaking hyphen
(nbd-- "\\nobreakdash--")        ; nonbreaking en-dash
(nbd--- "\\nobreakdash---")      ; nonbreaking em-dash

;; Common abbreviations (as suggested in guide)
(padic "$p$\\nobreakdash-adic") ; p-adic
(ndim "$n$\\nobreakdash-dimensional") ; n-dimensional
(Ndash "\\nobreakdash\\textendash") ; en-dash for page ranges

;; With following space control
(nbds "$n$\\nobreakdash-\\hspace{0pt}") ; allow hyphenation after

latex-mode org-mode

;; Square roots
(sqrt "\\sqrt{" p "}")             ; square root
(sqn "\\sqrt[" p "]{" p "}")     ; nth root

;; Common roots
(sq2 "\\sqrt{2}")                ; sqrt(2)
(sq3 "\\sqrt{3}")                ; sqrt(3)
(sqrn "\\sqrt{n}")               ; sqrt(n)
(sqx "\\sqrt{x}")                ; sqrt(x)
(cb "\\sqrt[3]{" p "}")          ; cube root
(4th "\\sqrt[4]{" p "}")         ; fourth root

;; Complex expressions
(sqf "\\sqrt{\\frac{" p "}{" p "}}") ; sqrt of fraction
(sqs "\\sqrt{" p "^2 + " p "^2}") ; sqrt of sum of squares

latex-mode org-mode

;; Boxed formulas
(bx "\\boxed{" p "}")            ; boxed formula
(fb "\\fbox{" p "}")             ; fbox (text mode)
(fbx "\\fbox{$" p "$}")          ; fbox with math

;; Framed environments (requires additional packages)
(fr "\\framebox{" p "}")         ; framebox
(frb "\\framebox[" p "]{" p "}") ; framebox with width

;; Common boxed expressions
(bxeq "\\boxed{" p " = " p "}")  ; boxed equation
(bxres "\\boxed{\\text{Answer: } " p "}") ; boxed answer
