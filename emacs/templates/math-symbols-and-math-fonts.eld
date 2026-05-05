latex-mode org-mode

(tblc
 "\\begin{tabular}{|l|l|l|l|}" n
 "\\hline Class number & Mnemonic & Description (part of speech) & Examples \\\\" n
 "\\hline 0 & Ord & simple/ordinary (\"noun\") & $A 0 \\Phi \\infty$ \\\\" n
 "\\hline 1 & Op & prefix operator & $\\sum \\Pi$ \\\\" n
 "\\hline 2 & Bin & binary operator (conjunction) & $+ \\cup \\wedge$ \\\\" n
 "\\hline 3 & Rel & relation/comparison (verb) & $= < C$ \\\\" n
 "\\hline 4 & Open & left/opening delimiter & ( [ \\{ \\lbrace \\rbrace \\} >) \\\\" n
 "\\hline 5 & Close & right/closing delimiter & ) ] \\} > \\\\" n
 "\\hline 6 & Punct & postfix/punctuation & ., , ! \\\\" n
 "\\hline" n
 "\\end{tabular}")

(dfs
 "\\DeclareFontShape{OT2}{wncyr}{m}{n}{<->wncyr10}{}")
(dsf
 "\\DeclareSymbolFont{cyr}{OT2}{wncyr}{m}{n}")
(dms
 "\\DeclareMathSymbol{\\Sh}{\\mathalpha}{cyr}{88}")

;;; Some symbols intentionally omitted here (placeholder)

latex-mode org-mode

;; Greek letters - lowercase
(alpha "\\alpha")                    ; α alpha
(beta "\\beta")                     ; β beta
(gamma "\\gamma")                    ; γ gamma
(delta "\\delta")                    ; δ delta
(epsilon "\\epsilon")                  ; ε epsilon (lunate)
(varepsilon "\\varepsilon")              ; ε varepsilon (script)
(zeta "\\zeta")                     ; ζ zeta
(et "\\eta")                     ; η eta
(th "\\theta")                   ; θ theta
(vth "\\vartheta")               ; ϑ vartheta
(io "\\iota")                     ; ι iota
(kappa "\\kappa")                    ; κ kappa
(vk "\\varkappa")                ; ϰ varkappa
(lambda "\\lambda")                   ; λ lambda
(mu "\\mu")                      ; μ mu
(nu "\\nu")                       ; ν nu
(xi "\\xi")                       ; ξ xi
(pi "\\pi")                       ; π pi
(vp "\\varpi")                   ; ϖ varpi
(rh "\\rho")                     ; ρ rho
(vrh "\\varrho")                 ; ϱ varrho
(sig "\\sigma")                    ; σ sigma
(vs "\\varsigma")                ; ς varsigma
(tau "\\tau")                    ; τ tau
(upsilon "\\upsilon")                  ; υ upsilon
(phi "\\phi")                     ; φ phi
(vph "\\varphi")                 ; φ varphi
(ch "\\chi")                     ; χ chi
(ps "\\psi")                     ; ψ psi
(omega "\\omega")                    ; ω omega
(dg "\\digamma")                 ; ϝ digamma

;; Greek letters - uppercase
(Gamma "\\Gamma")                    ; Γ Gamma
(Delta "\\Delta")                    ; Δ Delta
(Th "\\Theta")                   ; Θ Theta
(Lambda "\\Lambda")                   ; Λ Lambda
(Xi "\\Xi")                       ; Ξ Xi
(Pi "\\Pi")                       ; Π Pi
(Sigma "\\Sigma")                    ; Σ Sigma
(Upsilon "\\Upsilon")                  ; Υ Upsilon
(Ph "\\Phi")                     ; Φ Phi
(Ps "\\Psi")                     ; Ψ Psi
(Omega "\\Omega")                    ; Ω Omega

;; Other alphabetic symbols
(aleph "\\aleph")                ; ℵ aleph
(be "\\beth")                    ; ℶ beth
(gi "\\gimel")                   ; ℷ gimel
(daleth "\\daleth")                  ; ℸ daleth
(el "\\ell")                     ; ℓ ell
(eth "\\eth")                    ; ð eth
(hb "\\hbar")                    ; ℏ hbar
(hslash "\\hslash")                  ; ℏ hslash variant
(wp "\\wp")                      ; ℘ wp (Weierstrass p)
(mho "\\mho")                    ; ℧ mho
(co "\\complement")              ; ∁ complement
(cs "\\circledS")                ; Ⓢ circled S
(fi "\\Finv")                    ; Ⅎ Finv
(Game "\\Game")                  ; ⅁ Game
(bk "\\Bbbk")                    ; 𝕜 Bbbk

;; Partial derivatives
(pa "\\partial")                 ; ∂ partial

;; Special math mode letters
(imath "\\imath")                   ; ı dotless i
(jm "\\jmath")                   ; ȷ dotless j

;; Real and Imaginary parts
(Re "\\Re")                      ; ℜ Real part
(Im "\\Im")                      ; ℑ Imaginary part

;; Math font switches
(mb "\\mathbf{" p "}")           ; bold
(mr "\\mathrm{" p "}")           ; roman
(mi "\\mathit{" p "}")           ; italic
(mc "\\mathcal{" p "}")          ; calligraphic
(mscr "\\mathscr{" p "}")          ; script
(mf "\\mathfrak{" p "}")         ; fraktur
(bb "\\mathbb{" p "}")           ; blackboard bold
(sf "\\mathsf{" p "}")           ; sans serif
(mtt "\\mathtt{" p "}")           ; typewriter
(bs "\\boldsymbol{" p "}")       ; bold symbol
(pmb "\\pmb{" p "}")             ; poor man's bold

latex-mode org-mode

;; Basic symbols
(hash "\\#")                     ; # hash
(amp "\\&")                      ; & ampersand
(an "\\angle")                   ; ∠ angle
(bp "\\backprime")               ; ‵ backprime
(pr "\\prime")                   ; ′ prime
(em "\\emptyset")                ; ∅ emptyset
(vem "\\varnothing")             ; ∅ varnothing (alternative)
(ex "\\exists")                  ; ∃ exists
(nex "\\nexists")                ; ∄ nexists
(fo "\\forall")                  ; ∀ forall
(infty "\\infty")                  ; ∞ infinity
(na "\\nabla")                   ; ∇ nabla (del)
(neg "\\neg")                    ; ¬ neg (logical not)
(lnot "\\lnot")                  ; ¬ lnot (synonym)
(sr "\\surd")                    ; √ surd
(bo "\\bot")                     ; ⊥ bot
(top "\\top")                    ; ⊤ top
(tri "\\triangle")               ; △ triangle
(td "\\triangledown")            ; ▽ triangledown

;; Card suits
(cl "\\clubsuit")                ; ♣ clubsuit
(dsuit "\\diamondsuit")          ; ♦ diamondsuit
(he "\\heartsuit")               ; ♥ heartsuit
(ssuit "\\spadesuit")            ; ♠ spadesuit

;; Musical symbols
(flat "\\flat")                  ; ♭ flat
(nat "\\natural")                ; ♮ natural
(sh "\\sharp")                   ; ♯ sharp

;; Geometric shapes
(lo "\\lozenge")                 ; ◊ lozenge
(dia "\\diamond")                ; ⋄ diamond (smaller)
(square "\\square")                  ; □ square
(bst "\\bigstar")                ; ★ bigstar
;; Black shapes (amssymb)
(blo "\\blacklozenge")           ; ⧫ blacklozenge
(bsq "\\blacksquare")            ; ■ blacksquare
(bt "\\blacktriangle")           ; ▲ blacktriangle
(bltd "\\blacktriangledown")     ; ▼ blacktriangledown

;; Angle symbols
(ma "\\measuredangle")           ; ∡ measuredangle
(sa "\\sphericalangle")          ; ∢ sphericalangle

;; Diagonal lines
(dd "\\diagdown")                ; ╲ diagdown
(du "\\diagup")                  ; ╱ diagup

latex-mode org-mode

;; Basic arithmetic
(times "*")                      ; * times (implicit)
(plus "+")                       ; + plus (implicit)
(minus "-")                      ; - minus (implicit)
(ti "\\times")                   ; × times
(div "\\div")                    ; ÷ div
(pm "\\pm")                      ; ± plus-minus
(mp "\\mp")                      ; ∓ minus-plus

;; Dots and circles
(cdot "\\cdot")                    ; ⋅ cdot
(ci "\\circ")                    ; ∘ circ
(bu "\\bullet")                  ; • bullet
(bc "\\bigcirc")                 ; ◯ bigcirc

;; Set operations
(cu "\\cup")                     ; ∪ cup (union)
(ca "\\cap")                     ; ∩ cap (intersection)
(upl "\\uplus")                  ; ⊎ uplus
(scu "\\sqcup")                  ; ⊔ sqcup
(sca "\\sqcap")                  ; ⊓ sqcap
(Cu "\\Cup")                     ; ⋓ Cup (double union)
(Ca "\\Cap")                     ; ⋒ Cap (double intersection)
(vee "\\vee")                    ; ∨ vee (or)
(lor "\\lor")                    ; ∨ lor (synonym)
(we "\\wedge")                   ; ∧ wedge (and)
(land "\\land")                   ; ∧ land (synonym)

;; Advanced set/lattice operations
(bw "\\barwedge")                ; ⊼ barwedge
(dbw "\\doublebarwedge")         ; ⩞ doublebarwedge
(cv "\\curlyvee")                ; ⋎ curlyvee
(cw "\\curlywedge")              ; ⋏ curlywedge
(vb "\\veebar")                  ; ⊻ veebar

;; Triangles
(btu "\\bigtriangleup")          ; △ bigtriangleup
(btd "\\bigtriangledown")        ; ▽ bigtriangledown
(tl "\\triangleleft")            ; ◁ triangleleft
(trig "\\triangleright")         ; ▷ triangleright

;; Box operations (amssymb)
(bpl "\\boxplus")                ; ⊞ boxplus
(bmi "\\boxminus")               ; ⊟ boxminus
(bti "\\boxtimes")               ; ⊠ boxtimes
(bdo "\\boxdot")                 ; ⊡ boxdot

;; Circle operations
(opl "\\oplus")                  ; ⊕ oplus
(omi "\\ominus")                 ; ⊖ ominus
(oti "\\otimes")                 ; ⊗ otimes
(osl "\\oslash")                 ; ⊘ oslash
(odo "\\odot")                   ; ⊙ odot

;; Special operators
(as "\\ast")                     ; ∗ ast
(star "\\star")                    ; ⋆ star
(dag "\\dagger")                 ; † dagger
(ddag "\\ddagger")               ; ‡ ddagger
(am "\\amalg")                   ; ⨿ amalg
(wr "\\wr")                      ; ≀ wr (wreath product)

;; Additional operations (amssymb)
(dpl "\\dotplus")                ; ∔ dotplus
(ce "\\centerdot")               ; · centerdot
(cda "\\circleddash")            ; ⊝ circleddash
(cas "\\circledast")             ; ⊛ circledast
(cci "\\circledcirc")            ; ⊚ circledcirc
(intc "\\intercal")               ; ⊺ intercal
(dio "\\divideontimes")          ; ⋇ divideontimes
(ltt "\\leftthreetimes")         ; ⋋ leftthreetimes
(rtt "\\rightthreetimes")        ; ⋌ rightthreetimes
(lti "\\ltimes")                 ; ⋉ ltimes
(rti "\\rtimes")                 ; ⋊ rtimes
(ldo "\\lessdot")                ; ⋖ lessdot
(gdo "\\gtrdot")                 ; ⋗ gtrdot

;; Backslash operators
(sm "\\setminus")                ; ∖ setminus
(ssm "\\smallsetminus")          ; ∖ smallsetminus

latex-mode org-mode

;; Basic comparisons
(lt "<")                         ; < less than
(gt ">")                         ; > greater than
(eqsign "=")                     ; = equals
(le "\\leq")                     ; ≤ leq
(LEQ "\\le")                     ; ≤ le (synonym)
(ge "\\geq")                     ; ≥ geq
(GEQ "\\ge")                     ; ≥ ge (synonym)
(ll "\\ll")                      ; ≪ ll (much less)
(gg "\\gg")                      ; ≫ gg (much greater)
(lll "\\lll")                    ; ⋘ lll (very much less)
(ggg "\\ggg")                    ; ⋙ ggg (very much greater)
(ne "\\neq")                     ; ≠ neq (not equal)
(NEQ "\\ne")                     ; ≠ ne (synonym)

;; Equivalence and similarity
(equiv "\\equiv")                ; ≡ equiv
(si "\\sim")                     ; ∼ sim
(sieq "\\simeq")                 ; ≃ simeq
(ap "\\approx")                  ; ≈ approx
(apeq "\\approxeq")              ; ⊜ approxeq
(asy "\\asymp")                  ; ≍ asymp
(cong "\\cong")                 ; ≅ cong
(doteq "\\doteq")                   ; ≐ doteq
(deqd "\\doteqdot")             ; ≑ doteqdot
(Do "\\Doteq")                   ; ≑ Doteq (synonym)

;; Advanced equivalences (amssymb)
(bsi "\\backsim")                ; ∽ backsim
(bsieq "\\backsimeq")            ; ⋍ backsimeq
(beq "\\bumpeq")                 ; ≏ bumpeq
(Bu "\\Bumpeq")                  ; ≎ Bumpeq
(cieq "\\circeq")                ; ≗ circeq
(ecir "\\eqcirc")                ; ≖ eqcirc
(esim "\\eqsim")                 ; ≂ eqsim
(esg "\\eqslantgtr")             ; ⪖ eqslantgtr
(esl "\\eqslantless")            ; ⪕ eqslantless
(fd "\\fallingdotseq")           ; ≒ fallingdotseq
(rd "\\risingdotseq")            ; ≓ risingdotseq
(tap "\\thickapprox")            ; ≈ thickapprox
(tsi "\\thicksim")               ; ∼ thicksim
(treq "\\triangleq")             ; ≜ triangleq

;; Order relations
(prec "\\prec")                  ; ≺ prec
(su "\\succ")                    ; ≻ succ
(preq "\\preceq")                ; ⪯ preceq
(sueq "\\succeq")                ; ⪰ succeq
(prsi "\\precsim")               ; ≾ precsim
(susi "\\succsim")               ; ≿ succsim
(prap "\\precapprox")            ; ⪷ precapprox
(suap "\\succapprox")            ; ⪸ succapprox
(prce "\\preccurlyeq")           ; ≼ preccurlyeq
(suce "\\succcurlyeq")           ; ≽ succcurlyeq
(cepr "\\curlyeqprec")           ; ⋞ curlyeqprec
(cesu "\\curlyeqsucc")           ; ⋟ curlyeqsucc

;; Less/greater variants (amssymb)
(leqq "\\leqq")                  ; ≦ leqq
(geqq "\\geqq")                  ; ≧ geqq
(leqs "\\leqslant")              ; ⩽ leqslant
(geqs "\\geqslant")              ; ⩾ geqslant
(lsi "\\lesssim")                ; ≲ lesssim
(gsi "\\gtrsim")                 ; ≳ gtrsim
(lap "\\lessapprox")             ; ⪅ lessapprox
(gap "\\gtrapprox")              ; ⪆ gtrapprox
(leg "\\lesseqgtr")              ; ⋚ lesseqgtr
(gel "\\gtreqless")              ; ⋛ gtreqless
(legg "\\lesseqqgtr")            ; ⪋ lesseqqgtr
(geql "\\gtreqqless")            ; ⪌ gtreqqless
(lsg "\\lessgtr")                ; ≶ lessgtr
(gls "\\gtrless")                ; ≷ gtrless

;; Negated relations
(nle "\\nleq")                   ; ≰ nleq
(nge "\\ngeq")                   ; ≱ ngeq
(nleqq "\\nleqq")                ; ≦̸ nleqq
(ngeqq "\\ngeqq")                ; ≧̸ ngeqq
(nleqs "\\nleqslant")            ; ≰ nleqslant
(ngeqs "\\ngeqslant")            ; ≱ ngeqslant
(nless "\\nless")                ; ≮ nless
(ngtr "\\ngtr")                  ; ≯ ngtr
(nprec "\\nprec")                ; ⊀ nprec
(nsucc "\\nsucc")                ; ⊁ nsucc
(npreq "\\npreceq")              ; ⋠ npreceq
(nsueq "\\nsucceq")              ; ⋡ nsucceq
(nsi "\\nsim")                   ; ≁ nsim
(nco "\\ncong")                  ; ≆ ncong
(nap "\\napprox")                ; ≉ napprox

;; Additional negated (amssymb)
(prnap "\\precnapprox")          ; ⪹ precnapprox
(sunap "\\succnapprox")          ; ⪺ succnapprox
(prneq "\\precneqq")             ; ⪵ precneqq
(suneq "\\succneqq")             ; ⪶ succneqq
(prnsi "\\precnsim")             ; ⋨ precnsim
(sunsi "\\succnsim")             ; ⋩ succnsim
(lnap "\\lnapprox")              ; ⪉ lnapprox
(gnap "\\gnapprox")              ; ⪊ gnapprox
(lneq "\\lneq")                  ; ⪇ lneq
(gneq "\\gneq")                  ; ⪈ gneq
(lneqq "\\lneqq")                ; ≨ lneqq
(gneqq "\\gneqq")                ; ≩ gneqq
(lnsi "\\lnsim")                 ; ⋦ lnsim
(gnsi "\\gnsim")                 ; ⋧ gnsim
(lveq "\\lvertneqq")             ; ≨ lvertneqq
(gveq "\\gvertneqq")             ; ≩ gvertneqq

latex-mode org-mode

;; Basic arrows
(la "\\leftarrow")               ; ← leftarrow
(ra "\\rightarrow")              ; → rightarrow
(lrar "\\leftrightarrow")       ; ↔ leftrightarrow
(La "\\Leftarrow")               ; ⇐ Leftarrow
(Ra "\\Rightarrow")              ; ⇒ Rightarrow
(Lra "\\Leftrightarrow")        ; ⇔ Leftrightarrow
(ua "\\uparrow")                 ; ↑ uparrow
(da "\\downarrow")               ; ↓ downarrow
(uda "\\updownarrow")            ; ↕ updownarrow
(Ua "\\Uparrow")                 ; ⇑ Uparrow
(Da "\\Downarrow")               ; ⇓ Downarrow
(Uda "\\Updownarrow")            ; ⇕ Updownarrow

;; Long arrows
(llar "\\longleftarrow")         ; ⟵ longleftarrow
(lra "\\longrightarrow")         ; ⟶ longrightarrow
(llra "\\longleftrightarrow")    ; ⟷ longleftrightarrow
(Lla "\\Longleftarrow")          ; ⟸ Longleftarrow
(Lrar "\\Longrightarrow")        ; ⟹ Longrightarrow
(Llra "\\Longleftrightarrow")    ; ⟺ Longleftrightarrow

;; Maps to
(mt "\\mapsto")                  ; ↦ mapsto
(lmt "\\longmapsto")             ; ⟼ longmapsto

;; Hooks
(hla "\\hookleftarrow")          ; ↩ hookleftarrow
(hra "\\hookrightarrow")         ; ↪ hookrightarrow

;; Tails (amssymb)
(lta "\\leftarrowtail")          ; ↢ leftarrowtail
(rta "\\rightarrowtail")         ; ↣ rightarrowtail

;; Harpoons
(lhu "\\leftharpoonup")          ; ↼ leftharpoonup
(lhdown "\\leftharpoondown")        ; ↽ leftharpoondown
(rhu "\\rightharpoonup")         ; ⇀ rightharpoonup
(rhdown "\\rightharpoondown")       ; ⇁ rightharpoondown
(uhu "\\upharpoonleft")          ; ↿ upharpoonleft
(uhr "\\upharpoonright")         ; ↾ upharpoonright
(dhl "\\downharpoonleft")        ; ⇃ downharpoonleft
(dhr "\\downharpoonright")       ; ⇂ downharpoonright

;; Left-right harpoons
(lrh "\\leftrightharpoons")      ; ⇋ leftrightharpoons
(rlh "\\rightleftharpoons")      ; ⇌ rightleftharpoons

;; Double arrows (amssymb)
(llas "\\leftleftarrows")        ; ⇇ leftleftarrows
(rra "\\rightrightarrows")       ; ⇉ rightrightarrows
(uua "\\upuparrows")             ; ⇈ upuparrows
(dda "\\downdownarrows")         ; ⇊ downdownarrows
(lras "\\leftrightarrows")       ; ⇆ leftrightarrows
(rla "\\rightleftarrows")        ; ⇄ rightleftarrows

;; Two-headed arrows (amssymb)
(thla "\\twoheadleftarrow")      ; ↞ twoheadleftarrow
(thra "\\twoheadrightarrow")     ; ↠ twoheadrightarrow

;; Triple arrows (amssymb)
(LLa "\\Lleftarrow")             ; ⇚ Lleftarrow
(Rra "\\Rrightarrow")            ; ⇛ Rrightarrow

;; Curved arrows (amssymb)
(cla "\\curvearrowleft")         ; ↶ curvearrowleft
(cra "\\curvearrowright")        ; ↷ curvearrowright
(cila "\\circlearrowleft")       ; ↺ circlearrowleft
(cira "\\circlearrowright")      ; ↻ circlearrowright

;; Diagonal arrows
(nea "\\nearrow")                ; ↗ nearrow
(sea "\\searrow")                ; ↘ searrow
(swa "\\swarrow")                ; ↙ swarrow
(nwa "\\nwarrow")                ; ↖ nwarrow

;; Loop arrows (amssymb)
(lola "\\looparrowleft")         ; ↫ looparrowleft
(lora "\\looparrowright")        ; ↬ looparrowright

;; Squiggly arrows (amssymb)
(lsq "\\leftrightsquigarrow")    ; ↭ leftrightsquigarrow
(rsq "\\rightsquigarrow")        ; ⇝ rightsquigarrow
(lea "\\leadsto")                ; ⇝ leadsto (synonym)

;; Special arrows (amssymb)
(Lsh "\\Lsh")                    ; ↰ Lsh
(Rsh "\\Rsh")                    ; ↱ Rsh
(mum "\\multimap")               ; ⊸ multimap

;; Negated arrows (amssymb)
(nla "\\nleftarrow")             ; ↚ nleftarrow
(nra "\\nrightarrow")            ; ↛ nrightarrow
(nlra "\\nleftrightarrow")       ; ↮ nleftrightarrow
(nLa "\\nLeftarrow")             ; ⇍ nLeftarrow
(nRa "\\nRightarrow")            ; ⇏ nRightarrow
(nLra "\\nLeftrightarrow")       ; ⇎ nLeftrightarrow

;; Gets and to synonyms
(gets "\\gets")                  ; ← gets (leftarrow)
(res "\\restriction")            ; ↾ restriction (upharpoonright)

latex-mode org-mode

;; Basic relations
(in "\\in")                      ; ∈ in
(ni "\\ni")                      ; ∋ ni (contains)
(own "\\owns")                   ; ∋ owns (synonym)
(nin "\\notin")                  ; ∉ notin
(sub "\\subset")                 ; ⊂ subset
(supset "\\supset")                 ; ⊃ supset
(sube "\\subseteq")              ; ⊆ subseteq
(supe "\\supseteq")              ; ⊇ supseteq
(ssub "\\sqsubset")              ; ⊏ sqsubset
(ssup "\\sqsupset")              ; ⊐ sqsupset
(ssube "\\sqsubseteq")           ; ⊑ sqsubseteq
(ssupe "\\sqsupseteq")           ; ⊒ sqsupseteq

;; Advanced subsets (amssymb)
(Sub "\\Subset")                 ; ⋐ Subset
(Sup "\\Supset")                 ; ⋑ Supset
(subeqq "\\subseteqq")           ; ⫅ subseteqq
(supeqq "\\supseteqq")           ; ⫆ supseteqq
(subne "\\subsetneq")            ; ⊊ subsetneq
(supne "\\supsetneq")            ; ⊋ supsetneq
(subneqq "\\subsetneqq")         ; ⫋ subsetneqq
(supneqq "\\supsetneqq")         ; ⫌ supsetneqq
(vsubne "\\varsubsetneq")        ; ⊊ varsubsetneq
(vsupne "\\varsupsetneq")        ; ⊋ varsupsetneq
(vsubneqq "\\varsubsetneqq")     ; ⫋ varsubsetneqq
(vsupneqq "\\varsupsetneqq")     ; ⫌ varsupsetneqq

;; Negated subsets (amssymb)
(nsub "\\nsubseteq")             ; ⊈ nsubseteq
(nsup "\\nsupseteq")             ; ⊉ nsupseteq
(nsubeqq "\\nsubseteqq")         ; ⊈ nsubseteqq
(nsupeqq "\\nsupseteqq")         ; ⊉ nsupseteqq

;; Logic and lattice
(vd "\\vdash")                   ; ⊢ vdash
(dv "\\dashv")                   ; ⊣ dashv
(Vd "\\Vdash")                   ; ⊩ Vdash
(vD "\\vDash")                   ; ⊨ vDash
(mo "\\models")                  ; ⊨ models (synonym)
(VD "\\Vvdash")                  ; ⊪ Vvdash
(nvd "\\nvdash")                 ; ⊬ nvdash
(nVd "\\nVdash")                 ; ⊮ nVdash
(nvD "\\nvDash")                 ; ⊭ nvDash
(nVD "\\nVDash")                 ; ⊯ nVDash

;; Parallel
(par "\\parallel")               ; ∥ parallel
(npar "\\nparallel")             ; ∦ nparallel
(spar "\\shortparallel")         ; ∥ shortparallel
(nspar "\\nshortparallel")       ; ⊬ nshortparallel

;; Mid
(mid "\\mid")                    ; ∣ mid
(nmid "\\nmid")                  ; ∤ nmid
(smid "\\shortmid")              ; ∣ shortmid
(nsmid "\\nshortmid")            ; ⊬ nshortmid

;; Misc relations
(perp "\\perp")                  ; ⊥ perp
(bow "\\bowtie")                 ; ⋈ bowtie
(Join "\\Join")                  ; ⋈ Join (synonym)
(ltri "\\ltriangle")             ; ⊲ ltriangle (same as vartriangleleft)
(rtri "\\rtriangle")             ; ⊳ rtriangle (same as vartriangleright)
(vltri "\\vartriangleleft")      ; ⊲ vartriangleleft
(vrtri "\\vartriangleright")     ; ⊳ vartriangleright
(bltri "\\blacktriangleleft")    ; ◀ blacktriangleleft
(brtri "\\blacktriangleright")   ; ▶ blacktriangleright
(tleq "\\trianglelefteq")        ; ⊴ trianglelefteq
(trieq "\\trianglerighteq")       ; ⊵ trianglerighteq
(nltri "\\ntriangleleft")        ; ⋪ ntriangleleft
(nrtri "\\ntriangleright")       ; ⋫ ntriangleright
(ntleq "\\ntrianglelefteq")      ; ⋬ ntrianglelefteq
(ntreq "\\ntrianglerighteq")     ; ⋭ ntrianglerighteq

;; Other relations (amssymb)
(bet "\\between")                ; ≬ between
(pf "\\pitchfork")               ; ⋔ pitchfork
(smi "\\smile")                  ; ⌣ smile
(fro "\\frown")                  ; ⌢ frown
(ssmi "\\smallsmile")            ; ⌣ smallsmile
(sfro "\\smallfrown")            ; ⌢ smallfrown
(prop "\\propto")                ; ∝ propto
(vprop "\\varpropto")            ; ∝ varpropto
(bec "\\because")                ; ∵ because
(the "\\therefore")              ; ∴ therefore
(beps "\\backepsilon")           ; ϶ backepsilon
(unlhd "\\unlhd")                ; ⊴ unlhd (synonym for trianglelefteq)
(unrhd "\\unrhd")                ; ⊵ unrhd (synonym for trianglerighteq)
(lhd "\\lhd")                    ; ⊲ lhd (synonym for vartriangleleft)
(rhd "\\rhd")                    ; ⊳ rhd (synonym for vartriangleright)
(vtri "\\vartriangle")           ; △ vartriangle

latex-mode org-mode

;; Basic operators
(sum "\\sum")                    ; ∑ sum
(prod "\\prod")                  ; ∏ prod
(cp "\\coprod")                  ; ∐ coprod
(int "\\int")                    ; ∫ int
(oi "\\oint")                    ; ∮ oint
(sint "\\smallint")              ; ∫ smallint

;; Set operations
(bcu "\\bigcup")                 ; ⋃ bigcup (union)
(bca "\\bigcap")                 ; ⋂ bigcap (intersection)
(bscu "\\bigsqcup")              ; ⨆ bigsqcup
(bsca "\\bigsqcap")              ; ⨅ bigsqcap
(bve "\\bigvee")                 ; ⋁ bigvee
(bwe "\\bigwedge")               ; ⋀ bigwedge
(bupl "\\biguplus")              ; ⨄ biguplus

;; Circle operations
(bodo "\\bigodot")               ; ⨀ bigodot
(bopl "\\bigoplus")              ; ⨁ bigoplus
(boti "\\bigotimes")             ; ⨂ bigotimes

;; Multiple integrals (amsmath)
(ii "\\iint")                    ; ∬ iint
(iii "\\iiint")                  ; ∭ iiint
(iiii "\\iiiint")                ; ⨌ iiiint
(ido "\\idotsint")               ; ∫⋯∫ idotsint

latex-mode org-mode

;; Basic punctuation
(comma ",")                      ; , comma
(semicolon ";")                  ; ; semicolon
(colon "\\colon")                ; : colon (proper spacing)
(period ".")                     ; . period
(excl "!")                       ; ! exclamation
(quest "?")                      ; ? question

;; Dots
(cdots "\\cdots")                   ; ⋯ cdots (centered dots)
(ld "\\ldots")                   ; … ldots (low dots)
(ddots "\\ddots")                 ; ⋱ ddots (diagonal dots)
(vdot "\\vdots")                 ; ⋮ vdots (vertical dots)

;; Semantic dots (amsmath - preferred)
(dsc "\\dotsc")                  ; dots with commas
(dsb "\\dotsb")                  ; dots with binary operators
(dsm "\\dotsm")                  ; dots with multiplication
(dsi "\\dotsi")                  ; dots with integrals
(dso "\\dotso")                  ; other dots
(ds "\\dots")                    ; generic dots (auto-chooses)

latex-mode org-mode

;; Basic delimiters (these auto-scale with \left \right)
;; Note: Prefer \lvert/\rvert, \lVert/\rVert for proper spacing
(lpar "(")                       ; ( left paren
(rpar ")")                       ; ) right paren
(lbra "[")                       ; [ left bracket
(rbra "]")                       ; ] right bracket
(lbrc "\\{")                     ; { left brace
(rbrc "\\}")                     ; } right brace
(lan "\\langle")                 ; ⟨ langle
(ran "\\rangle")                 ; ⟩ rangle
(lc "\\lceil")                   ; ⌈ lceil
(rc "\\rceil")                   ; ⌉ rceil
(lf "\\lfloor")                  ; ⌊ lfloor
(rf "\\rfloor")                  ; ⌋ rfloor
(slash "/")                      ; / slash
(bsl "\\backslash")              ; \ backslash

;; Vertical bars (use these for proper spacing)
(lv "\\lvert")                   ; | lvert
(rv "\\rvert")                   ; | rvert
(lV "\\lVert")                   ; ‖ lVert
(rV "\\rVert")                   ; ‖ rVert

;; Double brackets (stmaryrd)
(llb "\\llbracket")              ; ⟦ llbracket
(rrb "\\rrbracket")              ; ⟧ rrbracket

;; Upper/lower corners (amssymb)
(ulc "\\ulcorner")               ; ⌜ ulcorner
(urc "\\urcorner")               ; ⌝ urcorner
(llc "\\llcorner")               ; ⌞ llcorner
(lrc "\\lrcorner")               ; ⌟ lrcorner

;; Group delimiters (amssymb)
(lgr "\\lgroup")                 ; ⟮ lgroup
(rgr "\\rgroup")                 ; ⟯ rgroup
(lmo "\\lmoustache")             ; ⎰ lmoustache
(rmo "\\rmoustache")             ; ⎱ rmoustache

;; Auto-sizing delimiters
(lrp "\\left" p " " r " \\right" p) ; \left...\right pair
(big "\\big" p)                   ; \big
(Bi "\\Big" p)                   ; \Big
(bg "\\bigg" p)                  ; \bigg
(Bg "\\Bigg" p)                  ; \Bigg
(bil "\\bigl" p)                 ; \bigl
(bir "\\bigr" p)                 ; \bigr
(Bil "\\Bigl" p)                 ; \Bigl
(Bir "\\Bigr" p)                 ; \Bigr
(bgl "\\biggl" p)                ; \biggl
(bgr "\\biggr" p)                ; \biggr
(Bgl "\\Biggl" p)                ; \Biggl
(Bgr "\\Biggr" p)                ; \Biggr

latex-mode org-mode

;; Vertical bars and arrows
(vbar "|")                      ; | vert
(vert "\\vert")                   ; | vert (explicit)
(Vert "\\|")                    ; ‖ Vert (double bar)
(Ve "\\Vert")                   ; ‖ Vert (explicit)
(ava "\\arrowvert")             ; | arrowvert
(Ava "\\Arrowvert")             ; ‖ Arrowvert
(bvert "\\bracevert")             ; ⎪ bracevert

;; Extensible arrows (defined in arrows section, removed duplicates)

latex-mode org-mode

;; These are same as in arrows section, removed duplicates

latex-mode org-mode

;; Standard accents
(hat "\\hat{" p "}")             ; â hat
(chk "\\check{" p "}")            ; ǎ check
(til "\\tilde{" p "}")           ; ã tilde
(ac "\\acute{" p "}")            ; á acute
(gr "\\grave{" p "}")            ; à grave
(dot "\\dot{" p "}")             ; ȧ dot
(ddot "\\ddot{" p "}")            ; ä ddot
(brv "\\breve{" p "}")            ; ă breve
(bar "\\bar{" p "}")             ; ā bar
(vec "\\vec{" p "}")             ; a⃗ vec
(mring "\\mathring{" p "}")        ; å mathring

;; Wide accents
(what "\\widehat{" p "}")        ; âbc widehat
(wti "\\widetilde{" p "}")       ; ãbc widetilde
(wbar "\\overline{" p "}")       ; a̅b̅c̅ overline
(ubar "\\underline{" p "}")      ; abc underline

;; Triple/quadruple dots (amsmath)
(ddd "\\dddot{" p "}")           ; ⃛a dddot
(dddd "\\ddddot{" p "}")         ; ⃜a ddddot

;; Over/under arrows and braces
(ola "\\overleftarrow{" p "}")   ; ←abc overleftarrow
(ora "\\overrightarrow{" p "}")  ; →abc overrightarrow
(olla "\\overleftrightarrow{" p "}") ; ↔abc overleftrightarrow
(ula "\\underleftarrow{" p "}")  ; abc← underleftarrow
(ura "\\underrightarrow{" p "}") ; abc→ underrightarrow
(ulla "\\underleftrightarrow{" p "}") ; abc↔ underleftrightarrow
(obr "\\overbrace{" p "}")       ; ⏞abc overbrace
(ubr "\\underbrace{" p "}")      ; abc⏟ underbrace

latex-mode org-mode

;; Trigonometric
(sin "\\sin")                    ; sin
(cos "\\cos")                    ; cos
(tan "\\tan")                    ; tan
(cot "\\cot")                    ; cot
(sec "\\sec")                    ; sec
(csc "\\csc")                    ; csc

;; Inverse trigonometric
(arcsin "\\arcsin")              ; arcsin
(arccos "\\arccos")              ; arccos
(arctan "\\arctan")              ; arctan

;; Hyperbolic
(sinh "\\sinh")                  ; sinh
(cosh "\\cosh")                  ; cosh
(tanh "\\tanh")                  ; tanh
(coth "\\coth")                  ; coth

;; Logarithms and exponentials
(ln "\\ln")                      ; ln
(log "\\log")                    ; log
(lg "\\lg")                      ; lg
(exp "\\exp")                    ; exp

;; Limits
(lim "\\lim")                    ; lim
(liminf0 "\\liminf")              ; liminf
(limsup0 "\\limsup")              ; limsup
(varliminf "\\varliminf")        ; varliminf
(varlimsup "\\varlimsup")        ; varlimsup

;; Other operators
(arg "\\arg")                    ; arg
(deg "\\deg")                    ; deg
(det "\\det")                    ; det
(dim "\\dim")                    ; dim
(gcd "\\gcd")                    ; gcd
(hom "\\hom")                    ; hom
(inf "\\inf")                    ; inf
(ker "\\ker")                    ; ker
(max "\\max")                    ; max
(min "\\min")                    ; min
(Pr "\\Pr")                      ; Pr
(sup "\\sup")                    ; sup

;; Projective limits (amsmath)
(injlim "\\injlim")              ; injlim
(projlim "\\projlim")            ; projlim
(varinjlim "\\varinjlim")        ; varinjlim
(varprojlim "\\varprojlim")      ; varprojlim

;; Custom operators
(dmo "\\DeclareMathOperator{" p "}{" p "}") ; declare math operator
(dmo* "\\DeclareMathOperator*{" p "}{" p "}") ; with limits
(op "\\operatorname{" p "}")     ; inline operator
(op* "\\operatorname*{" p "}")   ; inline with limits
