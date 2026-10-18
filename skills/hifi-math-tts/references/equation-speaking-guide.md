# Equation Speaking Guide

Use this when a section contains nontrivial displayed equations.

## Goal

Make dense equations listenable without losing formal meaning.

The failure mode to avoid is token soup: a faithful but exhausting line-by-line reading that preserves symbols yet sounds mechanical and is hard to follow.

## Non-Negotiables

- Preserve symbol identity exactly.
- Preserve index identity exactly.
- Preserve mathematical structure exactly.
- Preserve chapter-wide spoken naming exactly.
- Do not collapse an equation into a vague prose paraphrase.

Examples of forbidden drift:

- `beta sub one` becoming `beta sub t`
- `z sub t minus one` becoming `z sub t`
- dropping a conditioning bar
- flattening a denominator into surrounding prose
- attaching a cumulant or connected-average subscript to the variable rather than to the expectation
- deleting adjacent notation when applying a pronunciation note inside a larger expression

## Recommended Speaking Pattern

For a complex displayed equation:

1. Say the equation number.
2. Give one short orienting phrase if needed.
3. Speak the expression with explicit grouping.
4. If the expression is especially hairy, stage it instead of flattening it.

If the source uses an explicit LaTeX `\tag{...}`, that tag is the printed equation number. Speak `Equation ...` using the tag itself. Do not invent a running equation counter and do not say `source tag`.

For ranged tags such as `4.1.3--4.1.4`, speak `Equation four point one point three through four point one point four`.

Useful grouping phrases:

- `the quantity ...`
- `all over ...`
- `inside the norm ...`
- `times the quantity ...`
- `the numerator is ...`
- `the denominator is ...`
- `the first line is ...`
- `equivalently ...`
- `minus the quantity ...`
- `plus the quantity ...`

For very dense expressions, it is often better to say:

- `the numerator is ...`
- `the denominator is ...`
- `inside the brackets ...`
- `inside the braces ...`
- `the whole quantity ...`

## Fractions

Prefer explicit grouping over flat token order.

Instead of:

- `beta sub t over two times one minus alpha sub t times one minus beta sub t`

Prefer:

- `beta sub t, all over two times the quantity one minus alpha sub t, times the quantity one minus beta sub t`

When a denominator has products or sums, say `the quantity` around each major factor as needed.

If a denominator contains a product, sum, power, factorial, norm, or absolute value, prefer the explicit form:

- `the numerator is ...`
- `the denominator is ...`

Do not say `one over four pi epsilon zero times absolute value ...` when the absolute value is also in the denominator. Say `one all over the product of four pi, epsilon zero, and the absolute value of ...`.

If the fraction is very long, narrate it in two stages:

- `the numerator is ...`
- `all over ...`

If a fraction is followed by an external multiplicative factor, make the
boundary audible. Say `the fraction whose numerator is ... and denominator is
..., then multiplied by ...` or `all over ..., times ...` only when the listener
cannot mistake the final factor as part of the denominator.

## Powers and Scope

When a power applies to a parenthesized expression or a function value, make the base audible.

- `(D-\alpha)^2` -> `the quantity D minus alpha, squared`
- `(2\pi)^3` -> `the quantity two pi, cubed`
- `(\ln a)^2` -> `the quantity natural log of a, squared`
- `(2n+1)!` -> `the factorial of the quantity two n plus one`
- `(-2t)^{-1/2}` -> `the whole quantity negative two t, raised to negative one half`

Do not say `D minus alpha squared`, `two pi cubed`, `natural log of a squared`, or `two n plus one factorial` for these forms.

For exponentials with grouped exponents, speak the sign and scope:

- `e^{-(n\pi/L)^2t}` -> `e to the negative of the quantity n pi over L, squared, times t`

For negative or multi-token bases with fractional exponents, prefer `the whole
quantity ... raised to ...` over a flat phrase that lets the TTS listener attach
the exponent only to the final symbol.

## Norms

Say what is inside the norm explicitly.

Instead of:

- `the squared norm of g of ... minus epsilon`

Prefer:

- `the squared norm of the quantity g of ... minus epsilon`

If there are multiple terms inside the norm, use `the quantity` to make the scope audible.

## Multiline Equations

When a displayed equation has multiple aligned lines:

- say `the first line is ...`
- then `equivalently ...` or `the second line is ...`
- speak every source row that carries mathematical content
- do not replace a row with `paired with the preceding line`, `the companion relation`, or similar shorthand

This usually sounds much better than flattening both lines into one uninterrupted sentence.

## Matrices and Column-Vector Equations

For matrix equations, do not collapse the matrix into undifferentiated token soup.

Prefer one of these patterns:

- `the matrix whose first row is ... and whose second row is ...`
- `the column vector with entries ...`
- `the top-right entry is ...`

If the source only needs the matrix as a linear map, a short orienting phrase is often enough before reading the entries.

## Indexed Symbols

Always say indexed symbols in full.

- `beta sub one`
- `epsilon sub one`
- `z sub t minus one`
- `alpha sub t`
- `sigma sub x y`, not `sigma sub xy`
- `R sub i a`, not `R sub ia`

Do not rely on nearby context to repair an index mentally for the listener.

Do not alternate naming styles across equations. If the source brief says `vector x`, do not say `bold x` in a later section.

For simple braced indices such as `z_{t-1}` or `z_{\tau-1}`, speak the index directly as `z sub t minus one` or `z sub tau minus one`. Do not say `z sub parenthesized t minus one`; the braces are LaTeX grouping, not mathematical parentheses.

For standard Gaussian notation, prefer semantic speech. Read `\mathcal{N}(x\mid \mu,\Sigma)` as a normal distribution or as `x is normally distributed with mean ... and covariance ...`; do not say `script n of ...`. Read `\mathbf{I}` as `identity matrix` when it denotes the identity.

## Functions and Conditionals

Use stable spoken forms:

- `p of vector x, given vector z sub one and vector w`
- `q of vector z sub t, given vector x`
- `the expectation with respect to q`

Be explicit about conditioning bars and argument order.

For function evaluation with conditions inside the argument, say `evaluated at`:

- `\psi(x=0,t)` -> `psi evaluated at x equals zero and time t`
- `f(x=L)` -> `f evaluated at x equals L`

For decorated symbols:

- `\widehat{\nu}` -> keep one stable spoken form such as `nu hat`
- `\underline{x}` -> keep one stable spoken form such as `underline x`
- `\mathfrak{F}` -> keep one stable spoken form such as `fraktur f`
- `\hat{\imath}` and `\hat{\jmath}` -> say `i hat` and `j hat`, not `imath hat` or `jmath hat`
- `\vec{\imath}` and `\vec{\jmath}` -> say `vector i` and `vector j`, or a context-specific phrase such as `current density vector j`

Do not alternate among multiple spoken variants within the same chapter.

## Integrals, Sums, and Products

- For integrals, explicitly say `the integral of ... with respect to ...`
- For sums, say `the sum from ... to ... of ...`
- For products, say `the product from ... to ... of ...`
- Do not bury limits or measures in the surrounding sentence.

If the measure is nontrivial, say it explicitly:

- `d-dimensional q over the quantity two pi raised to d`
- `d-dimensional x`
- `d minus one-dimensional vector x`
- `with respect to phi`
- `with respect to the Gaussian measure D u`

Avoid mechanical measure readings such as `d to the d x` or `d to the d
minus one vector x` when the source clearly means a volume measure over a
coordinate or momentum variable. These are faithful enough to parse, but they
sound like TeX rather than a technical narrator.

If the sum or product is constrained, narrate the condition:

- `the sum over alpha not equal beta`
- `the sum over subsets U of U sub i a and S of S sub i a such that the size of U is greater than the size of S`

## Cases and Piecewise Definitions

For equations with cases:

- say `is given by the following cases`
- read each branch separately
- use `if` and `otherwise` explicitly when present

## Operators and Asymptotics

Use stable spoken forms for common operators and asymptotic symbols:

- `Tr` or `operatorname{Tr}` -> `trace`
- `argmax` -> `arg max`
- `lim` -> `the limit as ...`
- `\sim` in asymptotic contexts -> `scales as` or `is asymptotically proportional to`, depending on source context
- `\doteq` -> `equal to leading exponential order`
- `\stackrel{d}{=}` -> `equal in distribution`

Do not improvise operator names differently across sections.

## When to Add a Brief Orientation

Add a one-line orientation only when it helps comprehension:

- `This writes the lower bound as a sum of a reconstruction term and consistency terms.`
- `This is the same expression, but now written in terms of the predicted noise.`

Do not add conceptual commentary inside the equation reading itself.

## Cross-References

When the prose includes a meaningful cross-reference, keep it in the narration.

- `see Eq.~\eqref{...}` -> `see Equation ...`
- `from Eq.~\eqref{...}` -> `from Equation ...`
- `as shown in Fig.~\ref{...}` -> `as shown in Figure ...`

Omit only standalone navigation or drill markers that the source brief explicitly treats as non-content.

## Bra-Ket Notation

Bra-ket expressions should sound like matrix elements, not like raw delimiters.

- `\langle f|U|i\rangle` -> `the matrix element of U between final state f and initial state i`
- `\langle \psi|A|\psi\rangle` -> `the expectation value of A in state psi`, when the source context supports that meaning

Avoid token-order artifacts such as `tensor final state`, `bar final state`, or `ket final state acted on by`. If the source context is unclear, preserve the states and operator explicitly: `the bra f, operator U, ket i matrix element`.

## Placeholder Arguments

When a placeholder symbol appears inside notation, describe its role rather than its glyph.

- `\langle \bullet \rangle` -> `angle brackets denote the thermal average of the argument`
- `f(\cdot)` -> `the function f of its argument`

Say `bullet` or `dot` only when the source is explicitly discussing the symbol itself.

## Absolute Values

Make the scope of absolute values and powers audible.

- `|\phi|^2` -> `absolute value squared of phi`
- `|\Psi(\mathbf{x})|^4` -> `absolute value of Psi of position vector x, raised to the fourth power`
- `|x-y|^{d+\sigma}` -> `the absolute value of x minus y, raised to d plus sigma`

Avoid ambiguous forms such as `absolute value of phi squared` when the source means the square of the absolute value.

## Cumulants and Connected Averages

For expectation notation with a subscript, attach the subscript to the expectation.

- `\langle A\rangle_c` -> `the connected average of A` or `the cumulant average of A`
- `\langle A^2\rangle_c` -> `the connected average of A squared`
- `\langle \phi_i\phi_j\rangle_c` -> `the connected correlation of phi sub i and phi sub j`

Do not say `A sub c`, `A squared sub c`, or imply that the random variable itself carries the `c` subscript.

## Functional Measures

Preserve the measure and the field together.

- `\int \mathcal{D}\phi(x)` -> `the integral over the functional measure D phi of x`
- `\mathcal{D}\phi_{\ell}(x)` -> `functional measure D phi sub ell of x`

Do not apply a `\mathcal{D}` pronunciation note as a blind substitution that deletes the `D`, the field, or the argument. Avoid `functional measure phi of x` when the source contains `\mathcal{D}\phi(x)`.

## No Invented Equation Commentary

Use grouping words to make equations audible, but do not add unsupported interpretation.

Allowed:

- `the numerator is ... and the denominator is ...`
- `the first case is ...; the second case is ...`
- `the whole expression is multiplied by ...`

Not allowed unless the source says it:

- `this denominator keeps the powers grouped`
- `this is a squared Gaussian form`
- `the final term is a squared source contribution`
- `the second case is the squared fluctuation quantity`
