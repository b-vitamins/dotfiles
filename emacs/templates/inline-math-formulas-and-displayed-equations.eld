org-mode

;; Basic inline and display math
(im "\\(" p "\\)")                                     ; inline math \(...\)
(M "$" p "$")                                          ; inline math $...$
(dm "\\[" n> r> n> "\\]")                               ; display math \[...\]
(eq "\\begin{equation}" n> r> n> "\\end{equation}")     ; numbered equation
(eq* "\\begin{equation*}" n> r> n> "\\end{equation*}")  ; unnumbered equation

;; Multiline environments - primary (can be numbered)
(sp "\\begin{split}" n> r> n> "\\end{split}")          ; split (use inside equation)
(ml "\\begin{multline}" n> r> n> "\\end{multline}")     ; multline
(ml* "\\begin{multline*}" n> r> n> "\\end{multline*}")  ; multline*
(ga "\\begin{gather}" n> r> n> "\\end{gather}")        ; gather
(ga* "\\begin{gather*}" n> r> n> "\\end{gather*}")     ; gather*
(al "\\begin{align}" n> r> n> "\\end{align}")          ; align
(al* "\\begin{align*}" n> r> n> "\\end{align*}")       ; align*
(ala "\\begin{alignat}{" p "}" n> r> n> "\\end{alignat}")    ; alignat (requires number)
(ala* "\\begin{alignat*}{" p "}" n> r> n> "\\end{alignat*}") ; alignat*
(fl "\\begin{flalign}" n> r> n> "\\end{flalign}")      ; flalign (full length)
(fl* "\\begin{flalign*}" n> r> n> "\\end{flalign*}")   ; flalign*

;; Subordinate environments (for use within other math)
(gad "\\begin{gathered}" n> r> n> "\\end{gathered}")    ; gathered
(ald "\\begin{aligned}" n> r> n> "\\end{aligned}") ; aligned
(alad "\\begin{alignedat}{" p "}" n> r> n> "\\end{alignedat}") ; alignedat

;; Array environment (general purpose)
(ar "\\begin{array}{" p "}" n> r> n> "\\end{array}")    ; array with column spec

;; Subequations
(se "\\begin{subequations}" p n> r> n> "\\end{subequations}") ; subequations

org-mode

;; Labels and references
(lb "\\label{" p "}")                    ; label
(eqref "\\eqref{" p "}")                    ; equation reference (with parentheses)
(ref "\\ref{" p "}")                      ; general reference (no parentheses)
(tg "\\tag{" p "}")                      ; manual tag
(tg* "\\tag*{" p "}")                    ; tag without parentheses

;; Common label patterns
(leq "\\label{eq:" p "}")                ; equation label
(lfig "\\label{fig:" p "}") ; figure label
(ltab "\\label{tab:" p "}")              ; table label
(lsec "\\label{sec:" p "}")              ; section label
(lthm "\\label{thm:" p "}")              ; theorem label
(llem "\\label{lem:" p "}")              ; lemma label
(lprop "\\label{prop:" p "}")            ; proposition label
(lcor "\\label{cor:" p "}")              ; corollary label
(ldef "\\label{def:" p "}")              ; definition label

;; Cross-referencing with prefixes
(req "\\eqref{eq:" p "}")                ; equation reference
(rfig "\\ref{fig:" p "}")                ; figure reference
(rtab "\\ref{tab:" p "}")                ; table reference
(rsec "\\ref{sec:" p "}")                ; section reference

;; Numbering control
(nw "\\numberwithin{equation}{" p "}")    ; number equations within sections
(nn "\\nonumber")                         ; suppress equation number on current line
(nt "\\notag")                            ; same as \nonumber
