# Cart-Pole Equations Article

Last updated: 2026-07-13

## Current Status

- Canonical article: `public/research/cart-pole/cart-pole-equations.html`.
- The completed revision was promoted to the canonical filename on 2026-07-12; no separate staging HTML remains.
- The mathematical, notation, source-comparison, prose, Figure 1, and numerical-artifact review passes recorded below were completed on 2026-07-12.
- A derivation and pedagogy review at revision `d07bb73` is recorded in `plans/cart-pole-derivation-review.md`. It found no incorrect final equation and opened prioritized work required to fully meet the intended audience goal. The reader may be assumed to understand basic calculus and trigonometry. Findings F1, F3, F5, and F14 were resolved on 2026-07-13; F9 is partly resolved and the remaining findings stay open.
- Section 3.6 now uses one cart-acceleration-first route: derive (19), substitute it directly into the horizontal balance, and derive (20), without dividing by `cos(theta)`. Section 3.7.3 repeats that nonsingular pattern step by step for friction in (34)-(35). The unused alternative main-paper formulas were removed, all main equation tags were renumbered continuously from (1) through (51), and the recommended evaluation order is stated after both final equation pairs.
- Supporting C# repository: `/mnt/d/home/projects/code/cartpole-physics/main`.
- The corrected C# solution builds with zero warnings and errors, and its analysis results match the article.
- The article now uses MathJax 4's TeX/CommonHTML component from jsDelivr; the migrated rendering has been visually verified.
- A fresh independent step-by-step derivation review on 2026-07-12 found no substantive mathematical errors. The review started from the stated coordinate/sign conventions, reconstructed the mass-matrix equations, and symbolically verified the explicit single-pole, friction, uniform-pole, and multiple-pole forms. Its proof-domain concern about the former `cos(theta)` divisions was subsequently resolved by the 2026-07-13 rewrite.
- Pedagogical simplification pass 1 was completed on 2026-07-12: four displayed factoring steps with empty right-hand sides were completed so that no equality looks truncated. The equations and numbering are unchanged.
- Pedagogical simplification pass 2 was completed on 2026-07-12: repeated inertia explanations in sections 3.5, 3.6, and the opening of section 4 were consolidated. The complete first explanation, indexed multiple-pole definition, uniform-pole calculation, conclusion, and source-specific appendix evidence were retained.
- Pedagogical simplification pass 3 was completed on 2026-07-12 by distinguishing the recommended and alternative routes. That presentation was superseded on 2026-07-13: the main paper now retains only the nonsingular cart-acceleration-first route.
- Pedagogical simplification pass 4 was completed on 2026-07-12: adjacent explanatory prose in sections 3.1 and 3.5 was consolidated while retaining every differentiation step, the centripetal/tangential explanation, cross-product order and right-hand-rule guidance, and the physical limiting-case and sign checks.
- Pedagogical simplification pass 5 was completed on 2026-07-12: section 3.7 now separates detailed Coulomb models, optional simplifications, adopted linear damping, and optional combined models. The wheel-bearing interpretation was moved alongside cart linear damping and relabelled from (29-R) to (34-R); the generic friction equations now point explicitly to the adopted substitutions in section 5. All friction equations and physical definitions were retained.
- Pedagogical simplification pass 6 was completed on 2026-07-12: the derivation of the multiple-pole equation (47) retains substitution, expansion, combined-mass definition, factoring, and division, but replaces one very long unfactored intermediate equality with a direct plain-language instruction. Equations (42)-(49) and their numbering are unchanged.
- The total-mass shorthand was standardized as `m_{\mathrm{tot}}` on 2026-07-12, replacing both single-pole `m_t` and multiple-pole `m_{\Sigma}` in local notation. Source-faithful equations retain their original explicit mass expressions or source symbols.
- Pedagogical simplification pass 7 was completed on 2026-07-12: the conclusion now marks the end of the main instructional path, identifies Appendices A-B as optional background and Appendices C-E as optional historical verification, and states that the source comparisons are not required to implement the recommended equations. Appendix content was unchanged.
- Pedagogical simplification pass 8 was completed on 2026-07-12: the whole paper passed structural, notation, numeric equation-reference, accessibility, and symbolic-equivalence checks. Beginner-facing wording and pole/angle notation were standardized, every image now has alternative text, and legacy unmatched paragraph/table tags were corrected. The intentionally detailed mathematical derivations were retained.
- A review of Figure 1 (`cart-pole-model.svg`) on 2026-07-13 found its geometry and force/sign conventions consistent with the paper: positive `theta` is clockwise from upright, `R` is radial, `T` is the positive tangential direction, `r=PG`, `f` points right, and `mg` points downward. The user considers the existing `T/R` line styling and `P/C` layout appropriate, so both should be preserved. The angle indicator was changed to a conventional clockwise arc centred on pivot `P`, with the `theta` label inside the sector. The caption, HTML alternative text, and standalone SVG `title`/`desc` now explicitly identify a centre-of-mass schematic and state that line PG does not necessarily show the full physical pole length; the drawing was not extended beyond G because that could incorrectly imply the uniform-pole specialization. The stale Inkscape document name was updated. Legacy Inkscape definitions were retained to avoid risking marker or pattern dependencies.
- The ground hatching was confirmed to be vector-based rather than an embedded bitmap. Its Inkscape-generated chain of indirect and unused pattern definitions was replaced with one direct `groundHatch` SVG pattern containing a single diagonal path. The direction, density, and visual weight were rendered and verified; the SVG decreased from 44,060 to 40,842 bytes, with no raster data or dangling old pattern references.
- A cautious stage-1 internal SVG cleanup on 2026-07-13 removed twelve unused marker definitions, merged three duplicate arrow-end markers into the retained shared marker, removed nine editor-only guides and the `namedview`, removed redundant RDF metadata and unused namespaces, and removed an empty flow-text object. Required marker and hatch references remain valid. A headless-browser before/after comparison at 900 by 1200 pixels found zero differing pixels. After LF normalization the SVG is 27,652 bytes; a pre-cleanup copy for session rollback is at `/tmp/cart-pole-model.pre-structural-cleanup.svg`.
- Stage-2 SVG normalization on 2026-07-13 removed all remaining Inkscape/Sodipodi attributes and namespaces, removed 61 unreferenced element IDs while preserving accessibility and drawing targets, consolidated eleven byte-identical repeated styles into named CSS classes, and removed browser-ignored Inkscape font hints. Geometry, coordinate precision, unique style properties, and transforms were deliberately left unchanged. Each subpass was compared with the pre-stage-2 browser render and produced zero differing pixels. The SVG is now 15,220 bytes (down from the original 44,060 bytes); rollback snapshots are `/tmp/cart-pole-model.pre-stage2.svg` and `/tmp/cart-pole-model.pre-style-classes.svg`.
- A final conservative style-cleanup pass on 2026-07-13 removed explicit CSS declarations equal to SVG/browser defaults and obsolete `xml:space="preserve"` attributes from single-glyph text elements. Non-default fonts, sizes, italics, line heights, fills, strokes, widths, dash patterns, markers, and paint order were retained. The render again had zero differing pixels from the pre-pass baseline. The SVG is now 12,388 bytes, about 72% smaller than the original; geometry conversion, transform flattening, and coordinate rounding were intentionally not attempted. Additional rollback snapshots are `/tmp/cart-pole-model.pre-css-default-cleanup.svg` and `/tmp/cart-pole-model.pre-xml-space-cleanup.svg`.

## Summary of the Revision

### Physical Model and Inertia

- Corrected the interpretation of `J`: it is the pole body's moment of inertia about centre of mass `G`, not total inertia about pivot `P`.
- Kept the two pivot-inertia contributions explicit: centre-of-mass motion contributes `mr^2`, body rotation contributes `J`, and total pivot inertia is `mr^2+J`.
- For an ideal point mass, `J=0`.
- For a uniform pole, `r=L/2` and `J=(1/3)mr^2`, giving total pivot inertia `(4/3)mr^2`.
- Replaced the former `7/3` and `3/7` factors with the correct uniform-pole `4/3` and `3/4` factors.
- Removed the model-specific `alpha` inertia shorthand and retained `mr^2+J` or `m_i r_i^2+J_i` in general equations.
- Reframed section 4 as a specialization of the general rigid-body equations for a uniform pole, rather than a separate hybrid model.

### Symbols and Equation Presentation

- Defined `r`/`r_i` as pivot-to-centre-of-mass distance wherever the centre of mass lies. Only a uniform pole imposes `r=L/2`.
- Removed local `l`, `hat l`, and indexed variants. Source `l` remains only in source-faithful Appendix C-E displays and maps to local `r`.
- Retained `L` for full physical pole length and `J` for body inertia about `G`.
- Adopted uppercase geometric-point subscripts: `x_G`, `y_G`, `x_{G|P}`, `y_{G|P}`, `F^i_G`, `N_P`, and `M_G`.
- Reserved bold typography for genuine vectors; signed planar forces and moments such as `F_f`, `M_f`, `M_g`, and `M_i` are scalars.
- Clarified Table 1's coordinate-frame labels, origin `O`, cart coordinate `x`, point labels, and sign conventions.
- Used `m_{\mathrm{tot}}` for total system mass: `m_c+m` for a single pole and `m_c+sum_i(m_i)` for multiple poles.
- Used pole-specific `M_{f,i}` and `b_{p,i}` in multiple-pole equations; identical pivots may use equal values.

### Friction and Multiple Poles

- Distinguished dimensionless Coulomb coefficients `mu_c`/`mu_p` from dimensional damping coefficients `b_c`/`b_p`.
- Added effective pivot-friction radius `r_p` where required by the Coulomb moment model.
- Treated `N_c` as the nonnegative upward track reaction and made the contact-model validity condition explicit.
- Distinguished vector pivot reaction `N_P` from scalar cart reaction `N_c`.
- Retained velocity-proportional cart and pivot damping in the recommended equations for numerical continuity.
- Extended the corrected general and uniform-pole equations consistently to multiple poles.

### Numerical Integration and Artifacts

- Rewrote the Euler procedure using saved timestep state and conventional indices `n`, `n+1`, and pole index `i`; all next-state values are applied simultaneously.
- Regenerated the six Euler/RK4 CSV files and Figures 2-3 using the corrected C# equations.
- Recalculated Table 5 against a converged RK4 reference and documented the exact-duration `15/N` search and strict `|e|<0.01` rad criterion.
- Quantified single-versus-double precision differences in section 6.4.
- Corrected the experiment cart damping parameter to the source-backed `b_c=0.1` N s/m.

### Figure 1 Artwork

- Kept the cart-pole geometry and the fixed `XY`/rotating `TR` construction unchanged while simplifying the SVG internals and retaining a fully vector ground hatch.
- Added accessible SVG title/description metadata and clarified that `PG` locates the centre of mass rather than necessarily showing the full physical pole length.
- Used a restrained colour treatment: muted slate blue for the cart, blue for the applied force `f`, and muted red for weight `mg`; the pole, coordinate frames, dimensions, labels, and ground remain neutral.
- Force arrows have separate colour-matched SVG marker definitions, so shared geometric arrowheads remain black.

### Appendices and Prose

- Rewrote Appendix A's d'Alembert explanation and Appendix B's torque/sign-convention explanation to match the main derivation.
- Appendix C now retains Cannon-labelled source notation, maps it explicitly to local notation, confirms Cannon's `J=ml^2/3` is centre-of-mass body inertia, and follows the same cart-acceleration-first route as the main paper in (C3)-(C4).
- Appendix D labels Barto's unnumbered equations `(Barto pole)` and `(Barto cart)`, combines source values with mappings, and reserves (D1)-(D2) for local derivations. It is the one appendix that derives the opposite, angular-acceleration-first elimination because that form is required to compare directly with Barto's source pole equation. It presents broad structural agreement, isolates the listed gravity sign as the apparent discrepancy, and treats inertia/friction primarily as interpretation.
- Appendix E labels source displays `(Wieland 9)`, `(Wieland 10)`, `(Wieland force)`, and `(Wieland mass)`, combines values with mappings, and reserves (E1)-(E2) for local derivations. Its local comparison remains aligned with the cart-first form.
- Source `mu_c` and `mu_p` are retained only in source contexts; mapped equations use local signed `F_f` and `M_f`.
- Reframed the introduction and conclusion around the corrected derivation and evidence-supported source comparison rather than presumed source errors.
- Corrected and strengthened the Barto, Cannon, Lundberg/Barton, Michie/Chambers, and Wieland references.

## Invariants for Future Edits

- Do not redefine `J` as total pivot inertia; total pivot inertia is `mr^2+J`.
- Do not restore `7/3`, `3/7`, local `l`/`hat l`, or the removed inertia-factor shorthand.
- Keep local `g` as a positive gravitational-acceleration magnitude; signs in the equations encode direction.
- Preserve literal source notation in source-labelled Appendix C-E displays and map it explicitly before using local equations.
- Keep source dimensional `mu` parameters distinct from local dimensionless Coulomb coefficients.
- Keep `r` general; `r=L/2` applies only to a uniform pole.
- Keep vector/scalar typography and uppercase point subscripts consistent.
- Keep multiple-pole pivot damping indexed as `b_{p,i}`.
- Keep the nonsingular cart-acceleration-first route as the main instructional path. Derive the opposite elimination only where a source comparison requires it, currently Appendix D.

## Numerical Reproduction

- Solution: `/mnt/d/home/projects/code/cartpole-physics/main/src/CartPolePhysics.sln`.
- Analysis command, from the source directory:

  ```text
  dotnet run --project CartPoleConsole/CartPoleConsole.csproj --no-build --no-restore -- --analyse
  ```

- Article experiment: `m_c=1`, `m=0.1`, `L=1`, `r=0.5`, `b_c=0.1`, `b_p=0.001`, initial `theta=pi/2`, all other initial state values and applied force zero, duration 15 seconds.
- RK4 reference: `tau=0.000005`, `theta(15)=2.8896449084416695` rad.
- Table 5 thresholds: RK4 168 steps, RK2 426 steps, Euler 1,203,384 steps.
- Expected signed errors at the thresholds: `+0.0096937296`, `-0.0097898068`, and `-0.0099999965` rad, respectively.
- At `tau=0.001`, maximum absolute single-versus-double angle differences are approximately `6.81e-5` rad for Euler, `1.04e-5` for RK2, and `4.12e-5` for RK4.

## Source and Editing Guidance

- Primary-source evidence and URLs are recorded in `sources.md`; durable notation/model decisions are in `decisions.md`.
- Stay close to the article's established section structure and wording.
- Grammar and readability improvements are welcome; avoid broad redesign or restructuring unless required for correctness or genuine clarity.
- Preserve the static HTML/CSS approach and avoid unrelated worktree changes.

## Resume

Read this file, `decisions.md`, and any relevant entries in `sources.md`, then inspect the canonical article. Read the article itself rather than historical revision logs when reviewing equations or prose.
