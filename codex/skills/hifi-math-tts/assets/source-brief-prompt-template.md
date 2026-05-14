Create a source-local director's brief for TTS script generation.

Inputs:

- Source TeX: `{source_tex}`
- Section plan: `{plan_file}`
- Checkpoint ledger: `{checkpoint_file}`
- Source evidence packet: `{source_packet_file}`
- Output brief: `{source_brief_file}`

Read the source evidence packet first, then inspect the source TeX wherever the packet is insufficient. This is a model steering step, not a transcript and not a rule engine.

Write `{source_brief_file}` as Markdown with these sections:

1. `Narration Contract`
   - The chapter's mathematical texture and the desired spoken style.
   - State the core task plainly: produce the spoken English script a technically competent voice actor would read while preserving complete mathematical fidelity.
   - What should stay close to source prose.
   - What may be lightly rewritten for TTS pacing.

2. `Scope And Omission Policy`
   - Which source objects must be narrated.
   - Which source objects are layout, navigation, repeated drill markers, TeX bookkeeping, or default-out-of-scope material and should not become spoken paragraphs.
   - End-of-chapter problems or exercises, front matter, answers or solutions, bibliography or references, and index are out of scope by default unless the user explicitly asks for them.
   - Appendices are in scope when selected as canonical sources.
   - If the section plan contains default-out-of-scope rows and the user did not explicitly request them, stop and rebuild the plan instead of writing a brief that authorizes those rows.
   - How to handle captions, tables, algorithms, lists, and appendix material in this specific source.
   - Boundary-coherence notes for figures, tables, algorithms, displayed equations, or captions whose logical prose anchor is just outside their planned section because of LaTeX placement. If an artifact should be read before a heading or attached to adjacent prose for audiobook coherence, say so explicitly.

3. `Semantic Notation Decisions`
   - A table with columns: `source form`, `preferred spoken form`, `forbidden mechanical forms`, `source evidence`, `notes`.
   - Include only decisions grounded in this source. Do not generalize from another book.
   - Prefer semantic speech when the source names the object or the convention is unambiguous in context.
   - Leave genuinely ambiguous symbols generic, and say what local evidence a section worker should look for.

4. `Equation Speaking Policy`
   - How displayed equations should be introduced.
   - Verify the checkpoint ledger's equation identifiers against explicit
     `\tag{...}` commands, nearby prose references, and any source-local
     numbering resets before approving generation.
   - If two different displayed equations would receive the same spoken
     equation number, stop and report the numbering ambiguity instead of
     drafting final scripts from a conflicted ledger.
   - If the source has stale hard-coded cross-references because the local
     equation-numbering contract has been corrected, say how workers should
     map those references to the displayed equation they actually refer to.
     A spoken script must not point a listener to a different equation merely
     to preserve an obsolete numeral from the TeX prose.
   - Spell out the source-local rule for lettered subequations. If the source
     prints or references suffixes such as `(12.4.4a)` and `(12.4.4b)`, workers
     must speak those suffixes and must not collapse them to `Equation twelve
     point four point four`.
   - Warn workers if theorem, remark, definition, proposition, example,
     exercise, or problem numbers share the same chapter prefix as equations;
     those numbers must not be used to advance the equation counter.
   - Any recurring structures that need special handling, such as distributions, path integrals, Hamiltonians, matrix recurrences, cases, traces, norms, or variational bounds.
   - Include source-local rules for cross-references, bra-kets, placeholder arguments, absolute-value powers, connected or cumulant expectations, and functional measures when those structures occur.

5. `Source-Local Few-Shot Examples`
   - Provide at least five examples from this source.
   - Each example should include: source line reference, compact source snippet, bad mechanical wording to avoid, preferred spoken wording.
   - Cover the highest-risk patterns for this source, such as semantic notation, dense equations, standalone callouts, captions, tables, algorithms, appendix material, or inline math in prose.
   - If the source contains bra-kets, placeholders like `\bullet`, absolute-value powers, cumulant or connected-average notation, or functional measures, include at least one few-shot example for each present pattern.

6. `Section-Level Risk Notes`
   - Brief notes for sections likely to need extra care.
   - Call out where ordinary prose dominates versus where dense equations dominate.
   - Include any source-order anomalies that section workers must preserve or repair in spoken order, especially artifacts referenced by the previous paragraph but placed after a heading.

Do not write final scripts. Do not mechanically convert every symbol. The goal is to give later workers enough source-local judgment to produce scripts that sound like a careful technical narrator, not a parser.
