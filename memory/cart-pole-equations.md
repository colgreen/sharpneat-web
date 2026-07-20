# Cart-Pole Equations Article

Last updated: 2026-07-20

## Current Status

- Canonical article: `public/research/cart-pole/cart-pole-equations.html`.
- The completed revision was promoted to the canonical filename on 2026-07-12; no separate staging HTML remains.
- The mathematical, notation, source-comparison, prose, Figure 1, and numerical-artifact review passes recorded below were completed on 2026-07-12.
- The derivation and pedagogy review at revision `d07bb73`, recorded in `plans/cart-pole-derivation-review.md`, is complete. It found no incorrect final equation. The reader may be assumed to understand basic calculus, trigonometry, and summation notation. Findings F1-F6, F11, and F14 were resolved on 2026-07-13; F7-F10, F12-F13, and F15-F16 on 2026-07-14; and F17 on 2026-07-20.
- Section 3.6 now uses one cart-acceleration-first route: derive (19), substitute it directly into the horizontal balance, and derive (20), without dividing by `cos(theta)`. Section 3.7.3 repeats that nonsingular pattern step by step for friction in (34)-(35). The unused alternative main-paper formulas were removed, all main equation tags were renumbered continuously from (1) through (51), and the recommended evaluation order is stated after both final equation pairs.
- The paper now uses “moment” as its primary term for the turning effect of a force, with “torque” introduced as its synonym. Do not call a moment a “rotational force”: force has units N, while moment has units N m. Appendix B may retain conventional torque-vector notation while the planar equations use signed scalar moments `M`.
- Section 3.5 now introduces the planar identity `[r cross F]_z=r_x F_y-r_y F_x`, substitutes the radial-vector and inertial-force components explicitly, derives the gravity moment through the same rule, and explains why the ideal pivot reaction has zero moment about P. Appendix A states the compact rotational equation about the centre of mass, explains that the inertial force has zero moment at G but gains lever arm `r_PG` when moments are taken about P, and distinguishes that force moment from the reference-point-independent body inertial couple; do not restore the former unrestricted “chosen axis” wording.
- The F4 revision makes every later specialization operation inspectable without repeating the full core derivation. Section 3.8 derives the multiple-pole friction terms from the indexed balances through (42)-(43); section 4 expands the uniform-pole inertia identity and shows the numerator, denominator, and indexed cancellations through (44)-(47); Appendix E expands Wieland's effective force and mass before substitution into (E1). Preserve these explicit bridges when editing the later equations.
- Section 3.8 defines the multiple-pole system as $N$ separate, unjointed poles attached directly to one cart. Each pivot coordinate is $x$ plus a fixed offset and therefore has acceleration $\ddot x$, while each pole retains separate indexed angular states and parameters. Figure 2 is a focused topology schematic showing two distinct pivots on one cart; its pole bodies have unequal illustrative lengths and extend beyond internal $G_i$ markers, while it deliberately omits Figure 1's forces and coordinate frames. The section defines $i=1,\ldots,N$ and $\sum_i$ as shorthand for $\sum_{i=1}^{N}$ without teaching the summation operator. Its equal-length discussion is qualified: the near-upright difference mode of dynamically identical equal-length poles is uncontrollable, while identical angular states form a synchronized exception. The former numerical Figures 2 and 3 are now Figures 3 and 4.
- Section 3.2 now states the single-pole pivot kinematics explicitly: $x_P=x+d_x$, $y_P=d_y$, $\ddot x_P=\ddot x$, and $\ddot y_P=0$ for fixed offsets. Section 3.7.1 derives the upward track reaction from the combined vertical balance before (22). Section 3.7.2 defines $\mathbf N_P$ as the pole-on-cart pivot reaction, identifies $-\mathbf N_P$ as the cart-on-pole force, and derives components (28)-(29) from the pole-only balance. Preserve these action-reaction directions when editing the friction models.
- The general cart-acceleration denominators are now proved positive in sections 3.6 and 3.8. For one pole,
  $D=mr^2[m_c+m\sin^2\theta]+J(m_c+m)>0$; for multiple poles, the denominator of (41) is at least $m_c>0$.
  Sections 5.2 and 5.3 restate the uniform-pole bounds $m_c+m/4$ and $m_c+\frac14\sum_i m_i$, respectively. They also inventory
  the current state, input, and parameters required by (48)-(51), and require every right-hand-side value to come from one instantaneous
  state without advancing any state variable between the cart- and pole-acceleration evaluations.
- Section 6 now identifies RK2 as Heun's method and separates visual trajectory agreement in Figure 4 from the endpoint-only metric in Table 5. Table 5 measures absolute pole-angle error at exactly 15 seconds against the fine RK4 reference; it does not establish a full-trajectory error bound or a universal timestep. The accepted RK2 and RK4 timesteps are respectively about 2,825 and exactly 7,163 times Euler's timestep, so both exceed it by more than three orders of magnitude for this endpoint experiment.
- The two Cannon detours in the main derivation are explicitly labelled as optional source notes. They tell readers that the main route does not depend on the rotating-frame notation or applied-force-sign comparison and link to Appendix C through its stable `appendixC` fragment. The first identifies equation (9) as an adaptation of Cannon's (22.51), retains his unit-vector symbols, and writes scalar multiplication explicitly; equation (13) uses the matching radial-vector form $r\mathbf{1}_R$. The second note distinguishes Cannon's positive applied traction force from the opposing d'Alembert inertial force and describes the §2.2 force convention as his usual rather than universal choice.
- The F15 editorial pass distinguishes physical parameters from initial state in section 6.2, standardizes indexed angular-velocity squares as $\dot\theta_i^2$, defines $g$ as a positive magnitude rather than a “directionless” one, states that table 2's $\theta$ is in radians, corrects the numerical-plot alternative text, and displays both the original 2020 publication date and the 2026 revision date.
- The F16 friction pass treats $m_cg$ in (25) and $mg/2$ in (31) as empirical constant-load choices to calibrate rather than generally preferable approximations. It scopes the discontinuity warning to simple fixed-step smooth-ODE integration, retains velocity-proportional damping as the adopted smooth model for that workflow, and states that cart and pivot damping act on distinct generalized velocities and are not substitutes.
- F17 deliberately retains clockwise-positive $\theta$, $\dot\theta$, and $\ddot\theta$ alongside anticlockwise-positive right-hand-rule moment vectors. This aligns with Cannon and Barto directly and with the sign structure of Wieland's equation (10). Table 1 and Appendix B now give the explicit bridge $\boldsymbol\omega=-\dot\theta\hat{\mathbf z}$ and $\boldsymbol\alpha=-\ddot\theta\hat{\mathbf z}$, where $\hat{\mathbf z}$ is the outward unit vector; section 3.5 uses it to derive $M_i=J\ddot\theta$, and Appendix B applies it to $M_f=b_p\dot\theta$. Preserve both conventions and their bridge together.
- Follow-up finding F18 was resolved on 2026-07-20. Equations (34)-(35) and (42)-(43) are an explicit cart-acceleration-first evaluation sequence only when the selected friction loads can be calculated without the unknown accelerations, as with the adopted linear damping models. Exact reaction-dependent Coulomb models using (23), (27), (30), or (33) leave an implicit, generally nonlinear and piecewise system that must be solved simultaneously. Preserve this qualification whenever describing generic friction substitutions.
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
- A review of Figure 1 (`cart-pole-model.svg`) on 2026-07-13 found its geometry and force/sign conventions consistent with the paper: positive `theta` is clockwise from upright, `R` is radial, `T` is the positive tangential direction, `r=PG`, `f` points right, and `mg` points downward. The user considers the existing `T/R` line styling and `P/C` layout appropriate, so both should be preserved. The angle indicator was changed to a conventional clockwise arc centred on pivot `P`, with the `theta` label inside the sector. The stale Inkscape document name was updated. Legacy Inkscape definitions were retained to avoid risking marker or pattern dependencies. On 2026-07-15, the pole was extended beyond the internal G marker; the caption, HTML alternative text, and standalone SVG description explain that its endpoint is schematic and does not imply the uniform-pole specialization.
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
- Distinguished vector pivot reaction `N_P` from scalar cart reaction `N_c`; `N_P` is specifically pole-on-cart, while the force on the pole is `-N_P`.
- Retained velocity-proportional cart and pivot damping in the recommended equations for numerical continuity.
- Labelled simplified constant Coulomb loads as calibrated effective-load choices, not generally superior approximations to the state-dependent reactions.
- Scoped the Coulomb-discontinuity warning to simple fixed-step smooth-ODE methods and acknowledged event-aware, switching, and nonsmooth alternatives.
- Kept cart and pivot losses conceptually separate because they act on `xdot` and `thetadot`, respectively, despite dynamic coupling.
- Extended the corrected general and uniform-pole equations consistently to multiple poles.

### Numerical Integration and Artifacts

- Rewrote the Euler procedure using saved timestep state and conventional indices `n`, `n+1`, and pole index `i`; all next-state values are applied simultaneously.
- Regenerated the six Euler/RK4 CSV files and Figures 2-3 using the corrected C# equations.
- Recalculated Table 5 against a converged RK4 reference and documented the exact-duration `15/N` search and strict `|e|<0.01` rad criterion.
- Identified RK2 as Heun's method and narrowed all Table 5 claims to its final-angle metric; trajectory-wide comparisons require maximum errors for each state component over the sampled interval.
- Quantified single-versus-double precision differences in section 6.4.
- Corrected the experiment cart damping parameter to the source-backed `b_c=0.1` N s/m.

### Figure Artwork

- Kept the cart-pole geometry and the fixed `XY`/rotating `RT` construction unchanged while simplifying the SVG internals and retaining a fully vector ground hatch.
- On 2026-07-14, Figure 1 adopted Figure 2's wheel and ground treatment: larger symmetric wheels overlap the cart body and sit on the track, each wheel has a contrasting light hub, and the ground uses the same square diagonal-hatch construction scaled to Figure 1. The pole, coordinate-frame, force, dimension, and label geometry was unchanged.
- On 2026-07-14, Figure 1's pivot pedestal was redrawn as a precise half-elliptical D profile, about 12% wider and 5% lower than the former generated path. Pivot P and all connected pole, guide, dimension, and label coordinates remain fixed.
- Figure 1's cart body now uses the same subtle corner-radius proportion as Figure 2: 5% of the body height. Its dimensions and position are unchanged.
- On 2026-07-15, Figure 1's text was normalized to Figure 2's serif font model and physical display sizes: point labels and $\theta$ use the equivalent of Figure 2's 24-unit size, while $r$, $f$, $mg$, and the cart-position $x$ use its 28-unit mathematical-symbol size. Plain italic serif characters replace the former mixed sans-serif and Unicode mathematical-italic glyphs. The `xy` and `RT` coordinate-frame labels intentionally remain smaller and muted; the cart-position dimension $x$ remains a full-weight model symbol. Its pole now also uses Figure 2's dark blue-grey, rounded stroke and equivalent physical thickness. Point G is 70% of the way from P to the illustrated endpoint, matching Figure 2; the gravity vector and the RT-aligned $r=PG$ dimension move with it. The $\theta$ arc retains its Figure 1 geometry but now uses Figure 2's equivalent physical stroke weight and solid triangular arrowhead.
- Figure 2's two pivot pedestals now use the same precise half-elliptical D profile as Figure 1, scaled from the fixed pivot-to-body distance. The pivots, poles, guides, labels, and cart remain fixed.
- Figure 2's cart body was shortened symmetrically by about 23%, from 380 to 293.502 SVG units. Its centre remains fixed at C, and its wheel-to-edge clearance now matches Figure 1 relative to wheel radius; the wheels, pivots, pedestals, poles, guides, and labels remain fixed.
- Figure 2 now draws unequal illustrative full pole lengths of 250 and 185 SVG units, so the right pole is 74% as long as the left. Each $G_i$ marker is 70% of the way from its pivot and the pole body continues beyond it, giving the $G_i$ and $r_i$ annotations clear space above the angle indicators. The pivot positions and pole angles are unchanged. The SVG description, HTML alternative text, and caption clarify that $r_i=P_iG_i$ is a centre-of-mass distance and need not be half the full physical length. Figure 1 subsequently adopted the same internal-G convention.
- Figure 2's angle indicators are true radius-110 arcs centred on their respective pivots and ending exactly on the pole axes. Their arrowheads were reduced, the $\theta_i$ labels use a slightly smaller dedicated size and sit well inside their angular sectors, and each $r_i$ label sits beside the corresponding $P_iG_i$ segment near $G_i$.
- Figure 2's pivot labels now face inward: $P_1$ sits above-right of the left pivot and $P_2$ above-left of the right pivot. This keeps both labels close to their housings and clear of the outer $r_i$ dimension constructions.
- A user refinement placed both $G_i$ labels toward the central gap and both $r_i$ labels on the outer sides of the poles; these placements were retained. Each $r_i$ now accompanies a double-headed dimension line parallel to the radial $R_i$ direction $P_iG_i$. Its dashed projection guides are exactly perpendicular to $R_i$ and therefore parallel to the implied tangential $T_i$ direction, matching Figure 1's distance-indicator convention even though Figure 2 does not draw the RT frames. The lower guides are layered behind the pivot housings. The SVG description and HTML alternative text identify the marked pivot-to-centre-of-mass distances.
- After the final Figure 2 label refinement, editor-only Inkscape/Sodipodi metadata, namespaces, and generated element IDs were removed while preserving every user-adjusted coordinate. Accessibility IDs and referenced marker/pattern IDs remain.
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
- Keep “moment” as the primary paper term and “torque” as its accepted synonym; never describe either as a rotational force.
- Keep the fixed-pivot kinematic bridge before (7)-(8): $x_P=x+d_x$, $y_P=d_y$, $\ddot x_P=\ddot x$, and $\ddot y_P=0$.
- Keep reaction directions explicit: $N_c$ is the upward track-on-cart scalar reaction, while $\mathbf N_P$ is the pole-on-cart pivot-reaction vector and $-\mathbf N_P$ acts on the pole.
- Preserve the denominator guarantees: (20) has denominator $mr^2[m_c+m\sin^2\theta]+J(m_c+m)>0$; (41) has denominator at least $m_c>0$; the uniform-pole denominators in (48) and (50) are at least $m_c+m/4$ and $m_c+\frac14\sum_i m_i$.
- When implementing (48)-(51), evaluate every right-hand-side quantity from the same instantaneous state. Compute $\ddot x$ first, pass only that intermediate into the angular-acceleration equation(s), and do not advance the cart or pole state between those evaluations.
- Treat Table 5 only as an endpoint experiment: its metric is the absolute pole-angle error at exactly 15 seconds against the fine RK4 reference. Do not infer a whole-trajectory accuracy bound or a universal timestep from it. If trajectory accuracy matters, report maximum absolute errors in $x$, $\dot x$, $\theta$, and $\dot\theta$ over the sampled interval. Keep the RK2 label tied to Heun's method.
- Keep the Cannon blocks after (8) and (12) visibly labelled as optional source notes, with clear skip language and links to Appendix C. Do not let their source notation or convention become an unexplained branch of the main derivation.
- Retain explicit scalar-times-unit-vector notation in (9) and (13): $\ddot x\mathbf{1}_x$, $r\ddot\theta\mathbf{1}_T$, $-r\dot\theta^2\mathbf{1}_R$, and $r\mathbf{1}_R$. Use `RT` consistently for the rotating coordinate frame.
- Keep physical parameters distinct from initial state variables in numerical experiment descriptions; write indexed angular-velocity squares as $\dot\theta_i^2$; describe $g$ as a positive magnitude whose direction is encoded by equation signs; and retain both the original-publication and last-revised dates.
- Treat the constant $m_cg$ cart load and $mg/2$ pivot load in the optional Coulomb models as empirical calibration choices, not generally more accurate replacements for the state-dependent reactions. Keep the adopted linear damping tied to the paper's fixed-step smooth-ODE workflow, and do not treat cart damping as a substitute for pivot damping.
- Do not present $\sum M=I\alpha$ as valid about an arbitrary accelerating point. About pivot P, retain both the moment of the inertial force at G and the separate body inertial moment.
- Preserve the clockwise-positive angular-coordinate scalars, anticlockwise-positive right-hand-rule vectors, and the relations $\boldsymbol\omega=-\dot\theta\hat{\mathbf z}$ and $\boldsymbol\alpha=-\ddot\theta\hat{\mathbf z}$ for outward unit vector $\hat{\mathbf z}$. Do not change either convention or remove their bridge in isolation.

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
