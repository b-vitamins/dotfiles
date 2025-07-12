latex-mode org-mode

;; Basic fractions
(frac "\\frac{" p "}{" p "}")       ; fraction
(df "\\dfrac{" p "}{" p "}")     ; display fraction (larger)
(tf "\\tfrac{" p "}{" p "}")     ; text fraction (smaller)

;; Common fractions
(f2 "\\frac{1}{2}")              ; 1/2
(f3 "\\frac{1}{3}")              ; 1/3
(f4 "\\frac{1}{4}")              ; 1/4
(fn "\\frac{1}{n}")              ; 1/n
(fx "\\frac{1}{x}")              ; 1/x
(f12 "\\frac{" p "}{2}")         ; x/2
(fab "\\frac{a}{b}")             ; a/b
(fxy "\\frac{x}{y}")             ; x/y

;; Complex fractions
(fpi "\\frac{\\pi}{" p "}")      ; pi/x
(f2pi "\\frac{" p "}{2\\pi}")    ; x/2pi
(fsq "\\frac{\\sqrt{" p "}}{" p "}") ; sqrt in numerator
(fpar "\\frac{\\partial " p "}{\\partial " p "}") ; partial derivative
(fdd "\\frac{d" p "}{d" p "}")   ; derivative
(fd2 "\\frac{d^2" p "}{d" p "^2}") ; second derivative

;; Nested fractions
(ff "\\frac{\\frac{" p "}{" p "}}{" p "}") ; fraction over x
(ffu "\\frac{" p "}{\\frac{" p "}{" p "}}") ; x over fraction
(dff "\\dfrac{\\dfrac{" p "}{" p "}}{" p "}") ; display nested

latex-mode org-mode

;; Basic binomial coefficients
(binom "\\binom{" p "}{" p "}")     ; binomial coefficient
(dbi "\\dbinom{" p "}{" p "}")   ; display binomial (larger)
(tbi "\\tbinom{" p "}{" p "}")   ; text binomial (smaller)

;; Common binomial coefficients
(bin "\\binom{n}{" p "}")        ; n choose k
(bik "\\binom{" p "}{k}")        ; x choose k
(bink "\\binom{n}{k}")           ; n choose k
(binm "\\binom{n}{m}")           ; n choose m
(bi2 "\\binom{" p "}{2}")        ; x choose 2
(binn "\\binom{n}{n}")           ; n choose n (=1)
(bin0 "\\binom{n}{0}")           ; n choose 0 (=1)
(bin1 "\\binom{n}{1}")           ; n choose 1 (=n)

;; Complex binomial expressions
(binp "\\binom{n+1}{k}")         ; n+1 choose k
(binm1 "\\binom{n-1}{k}")        ; n-1 choose k
(bink1 "\\binom{n}{k+1}")        ; n choose k+1
(bink- "\\binom{n}{k-1}")        ; n choose k-1
(bi2n "\\binom{2n}{n}")          ; central binomial coefficient

;; Generalized binomial (negative/fractional)
(bihalf "\\binom{1/2}{" p "}")   ; half choose k
(bineg "\\binom{-n}{k}")         ; -n choose k
(bialpha "\\binom{\\alpha}{k}")  ; alpha choose k

latex-mode org-mode

;; General fraction command
(gf "\\genfrac{" p "}{" p "}{" p "}{" p "}{" p "}{" p "}") ; full genfrac

;; Common genfrac patterns
(gf() "\\genfrac{(}{)}{0pt}{}{" p "}{" p "}") ; parentheses (like binom)
(gf[] "\\genfrac{[}{]}{0pt}{}{" p "}{" p "}") ; brackets
(gf{} "\\genfrac{\\{}{\\}}{0pt}{}{" p "}{" p "}") ; braces
(gf|| "\\genfrac{|}{|}{0pt}{}{" p "}{" p "}") ; single bars
(gf|||| "\\genfrac{\\|}{\\|}{0pt}{}{" p "}{" p "}") ; double bars

;; Style overrides
(gfd "\\genfrac{}{}{}{0}{" p "}{" p "}") ; display style
(gft "\\genfrac{}{}{}{1}{" p "}{" p "}") ; text style
(gfs "\\genfrac{}{}{}{2}{" p "}{" p "}") ; script style
(gfss "\\genfrac{}{}{}{3}{" p "}{" p "}") ; scriptscript style

;; No line fractions (like \atop)
(gfnl "\\genfrac{}{}{0pt}{}{" p "}{" p "}") ; no line
(atop "\\genfrac{}{}{0pt}{}{" p "}{" p "}") ; atop replacement

;; Custom thickness
(gfth "\\genfrac{}{}{" p "pt}{}{" p "}{" p "}") ; custom thickness

;; Combinations
(gfdb "\\genfrac{(}{)}{0pt}{0}{" p "}{" p "}") ; display binom
(gfb "\\genfrac{[}{]}{0pt}{}{" p "}{" p "}") ; bracket fraction

latex-mode org-mode

;; Continued fraction
(cf "\\cfrac{" p "}{" p "}")     ; continued fraction
(cfl "\\cfrac[l]{" p "}{" p "}") ; left-aligned numerator
(cfr "\\cfrac[r]{" p "}{" p "}") ; right-aligned numerator

;; Common continued fraction patterns
(cf2 "\\cfrac{1}{" p " + \\cfrac{1}{" p "}}") ; 1/(a + 1/b)
(cf3 "\\cfrac{1}{" p " + \\cfrac{1}{" p " + \\cfrac{1}{" p "}}}") ; three levels
(cfinf "\\cfrac{1}{" p " + \\cfrac{1}{" p " + \\cfrac{1}{" p " + \\dotsb}}}") ; infinite

;; Classic continued fractions
(cfpi "\\cfrac{4}{1 + \\cfrac{1^2}{2 + \\cfrac{3^2}{2 + \\cfrac{5^2}{2 + \\dotsb}}}}") ; pi
(cfe "\\cfrac{1}{1 + \\cfrac{1}{2 + \\cfrac{1}{1 + \\cfrac{1}{1 + \\cfrac{1}{4 + \\dotsb}}}}}") ; e-1
(cfgold "\\cfrac{1}{1 + \\cfrac{1}{1 + \\cfrac{1}{1 + \\dotsb}}}") ; golden ratio

;; Generalized continued fractions
(cfgen "\\cfrac{" p "}{" p " + \\cfrac{" p "}{" p " + \\cfrac{" p "}{" p " + \\dotsb}}}") ; a0/(b0 + a1/(b1 + ...))

;; Nested continued fractions
(cfn "\\cfrac{" p "}{" p " + " r "}") ; template for nesting
