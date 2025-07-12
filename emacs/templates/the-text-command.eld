latex-mode org-mode

;; Text in math mode
(txt "\\text{" p "}")              ; text in math mode
(tb "\\textbf{" p "}")           ; bold text in math
(tit "\\textit{" p "}")           ; italic text in math
(tt "\\texttt{" p "}")           ; typewriter text in math
(trm "\\textrm{" p "}")          ; roman text in math
(tsf "\\textsf{" p "}")          ; sans-serif text in math
(tsc "\\textsc{" p "}")          ; small caps in math

;; Common text patterns in math
(twhere " \\text{ where } ")     ; where clause
(tand " \\text{ and } ")         ; and conjunction
(tor " \\text{ or } ")           ; or conjunction
(tfor " \\text{ for } ")         ; for clause
(tif " \\text{ if } ")           ; if clause
(twith " \\text{ with } ")       ; with clause
(tsuchthat " \\text{ such that } ") ; such that
(totherwise " \\text{ otherwise}") ; otherwise

;; Text with spacing
(tquad " \\quad\\text{" p "}\\quad ") ; text with quad spaces
(tqquad " \\qquad\\text{" p "}\\qquad ") ; text with qquad spaces

;; Modulo operations
(mod " \\bmod ")                 ; binary mod operator (spacing)
(pmod " \\pmod{" p "}")          ; modulo with parentheses (mod n)
(pod " \\pod{" p "}")            ; just parentheses, no "mod"
(modd " \\mod{" p "}")           ; modulo without parentheses

;; Common modulo patterns
(equivmod " \\equiv " p " \\pmod{" p "}") ; a ≡ b (mod n)
(notequivmod " \\not\\equiv " p " \\pmod{" p "}") ; a ≢ b (mod n)
(remmod " \\bmod " p " = ")      ; remainder when divided by

;; GCD and related
(gcdf "\\gcd(" p ", " p ")")      ; greatest common divisor
(lcm "\\operatorname{lcm}(" p ", " p ")") ; least common multiple

;; Text subscripts and superscripts
(tsub "_{\\text{" p "}}")        ; text subscript
(tsup "^{\\text{" p "}}")        ; text superscript
(tsubsup "_{\\text{" p "}}^{\\text{" p "}}") ; both

;; Common mathematical text
(tmax "_{\\text{max}}")          ; max subscript
(tmin "_{\\text{min}}")          ; min subscript
(topt "_{\\text{opt}}")          ; optimal subscript
(tinit "_{\\text{init}}")        ; initial subscript
(tfinal "_{\\text{final}}")      ; final subscript
(ttotal "_{\\text{total}}")      ; total subscript
(tavg "_{\\text{avg}}")          ; average subscript

;; Units and dimensions
(tunits " \\text{ " p "}")       ; units (with leading space)
(tm " \\text{ m}")               ; meters
(tcm " \\text{ cm}")             ; centimeters
(tmm " \\text{ mm}")             ; millimeters
(tkm " \\text{ km}")             ; kilometers
(tkg " \\text{ kg}")             ; kilograms
(tsec " \\text{ s}")               ; seconds
(tms " \\text{ ms}")             ; milliseconds
(thz " \\text{ Hz}")             ; hertz
(tkhz " \\text{ kHz}")           ; kilohertz
(tmhz " \\text{ MHz}")           ; megahertz
