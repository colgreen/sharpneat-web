# Cart-Pole Equations Revision Handoff

Last updated: 2026-07-12

## Goal

Create a conservative replacement draft for `public/research/cart-pole/cart-pole-equations.html`, preserving the original article structure and wording where possible, while correcting the rotational inertia interpretation and associated equations.

## Active Draft

- Draft HTML: `public/research/cart-pole/cart-pole-equations-revision.html`
- Working notes: `memory/cart-pole-equations-revision-draft.md`
- Original article is still untouched.

## Current Progress

- Reviewed and revised through equation (59), focused reviews of appendices A-E, a detailed section 6 prose pass, and Figure 1 SVG labels.
- Most recent work: completed the section 6 numerical artifact regeneration. The corrected CSVs and Figures 2-3 were regenerated; the finished PNGs were copied into the website staging path. Table 5 was recalculated against a converged RK4 reference at `tau=0.000005` seconds, using exact-duration candidate timesteps `15/N`, strict acceptance `|e|<0.01` rad, and signed error `candidate-reference`. Section 6.4 now reports measured single-versus-double precision differences at `tau=0.001` instead of the provisional qualitative statement. The supporting console has a reproducible `--analyse` mode for these measurements.
- Table 1 now uses:
  - `G`: pole/pendulum centre of mass.
  - `r`: distance from pivot P to G.
  - `L`: full physical pole length.
  - `J`: pole body inertia about centre of mass G.
- `l` and `\hat l` are no longer main symbols in Table 1.
- Sections 3.1-3.5 have been moved from old `Q,l,F^i_q,M_q` notation to `G,r,F^i_g,M_G` notation.
- Notes after equations (6), (12), and (14) have been revised for clarity.
- Equation (16) commentary now states `J` is the pendulum/pole body's moment of inertia about centre of mass G, not total pivot inertia.
- Section 3.6 equations (19)-(24) now use `r` and `mr^2 + J`.
- The former section 3.7 and redundant equations (25)-(28) have been removed. Section 3.6 now ends with the model-choice note and evaluation guidance; equation (24) carries the positive-denominator cart-acceleration form directly.
- Friction is now section 3.7 and multiple poles section 3.8. Their general equations retain `J` and `J_i` explicitly; no model-specific `\alpha` parameter remains. The friction extensions are labelled (19-F), (21-F), (23-F), and (24-F).
- Section 3.7 now uses distinct friction notation: local `\mu_c`/`\mu_p` are dimensionless Coulomb coefficients, `b_c` is the linear cart damping coefficient in N s/m, `b_p` is the linear pivot damping coefficient in N m s/rad, and Coulomb pivot moments include an effective radius `r_p`. The wheel-bearing example uses rotational damping coefficient `b_w`.
- Section 3.7 now has numbered subsections for cart-track friction, pivot friction, and the friction-extended equations. Its opening was condensed; detailed Coulomb and bearing-damping explanations were moved into their relevant subsections; and static-friction scope is stated concisely.
- Section 3.4 now presents equation (12) as the horizontal dynamic-equilibrium balance for the combined cart-pole system. The unknown pivot interaction is internal and cancels, and gravity is absent because the balance is horizontal.
- Section 3.8 explicitly resets from friction to the frictionless balances. Equation (10) is identified as the inertial force at G rather than the physical pole-on-cart contact force; $\tilde F_i$ is defined as each pole's contribution to the combined-system balance.
- Section 4.1's short uniform-pole inertia calculation was retained because it makes the specialization self-contained without excessive repetition.
- Section 5 now presents the uniform-pole and linear-damping selections as explicit model assumptions used to obtain one complete simulation equation set.
- Section 6.3 now saves the start-of-timestep state, evaluates all accelerations from that state, calculates all Euler updates from the saved values, and applies the updates simultaneously.
- Table 4 now specifies full pole length $L=1$ m, hence $r=\hat l=0.5$ m, and cart damping $b_c=0.1$ N s/m. The latter corrects the paper's former `0.01` value to match the artifact-generating C# defaults and their repository history; use `0.1` when regenerating the corrected experiments.
- Equations (29)-(31) now define `N_c` conventionally as the nonnegative upward normal reaction exerted by the track on the cart. Absolute-value notation was removed. The text states that the contact model ceases to apply if the calculated reaction reaches zero, and explains clearly why the `\ddot\theta` term in (31) creates an additional simultaneous coupling.
- Section 4 uniform-pole equations and section 5 recommended equations now use the corrected `4/3` and `3/4` factors instead of the old `7/3` and `3/7` factors.
- Section 4 has been reframed from a "hybrid model" to a specialization of the general equations for a uniform pole. With `J` resolved as centre-of-mass body inertia, no separate hybrid approximation is required.
- Equations (24), (24-F), (53), (56), (58), and (E5) use positive-denominator forms derived directly from (C5). For a uniform pole the coupling ratio `(mr)^2/(mr^2+J)` reduces to `3m/4`, making the `3/4` factors explicit.
- Appendix C now treats Cannon's `J = ml^2/3` as body inertia about the mass centre, yielding total pivot inertia `4/3 ml^2`; the unsubstantiated Cannon sign-error claim was removed.
- Appendices D and E now describe Barto/Wieland `4/3` factors as consistent with the corrected uniform-pole model, rather than as problematic inertia terms.
- Appendix D has received a focused source comparison pass. It explicitly maps Barto's `l` and `F_t` to local `\hat l` and `f`, includes the source parameter values, limits the negative-`g` discussion to claims supported by the displayed equation and angle convention, and distinguishes Barto's constant cart friction force from its velocity-proportional pivot damping parameter.
- Appendix E was verified and revised using user-supplied scans of Wieland pp.91-101. Equations (E1)-(E4) now consistently present Wieland's single-pole forms and literal source notation rather than mixing them with indexed multiple-pole or article-local notation; the following comparison introduces `g_W` to distinguish Wieland's signed `-9.8 m/s^2` from the paper's positive `g`. Source parameters, friction dimensions, inertia interpretation, and numerical-integration quotation were checked; and reference [5] now identifies the proceedings chapter “Evolving Controls for Unstable Systems” rather than the separate IEEE work.
- Appendix E equations (E1)-(E4) now reproduce Wieland's original `M`, `F`, `l`, and signed `g` notation. The following prose maps them to local `m_c`, `f`, `\hat l`, and `g_W=-g`; equations (E5)-(E6) use those local mappings. Appendix D likewise restores Barto's `l`, `F_t`, and time subscripts in (D1)-(D2), while Appendix C retains Cannon's source notation.
- Section 6 has been scanned for stale inertia notation; only a spelling fix was needed (`Adams-Bashforth`).
- Figure 1 SVG labels in `cart-pole-model.svg` and `cart-pole-model-master.svg` now use `G` for the centre of mass and `r` for the pivot-to-centre-of-mass distance.
- A final stale-reference scan across the staging draft and both SVG files found no old `Q`, `ℓ`, `7/3`, `3/7`, `(1+k)`, or old `k` inertia language; the only remaining `Q` character is inside a Google Books URL query string.
- The section 6 numerical integration CSVs, PNGs, table, and precision comparison have been regenerated with the corrected `4/3` recommended equations.
- The combined mass shorthand is now `m_t = m_c + m` for a single pole and `m_{\Sigma} = m_c + \sum_i m_i` for multiple poles. Do not reintroduce scalar `M`; reserve capital/bold `\mathbf{M}` for moments/torques.
- In article-local notation, reserve `\mu_c`/`\mu_p` for dimensionless Coulomb coefficients and `b_c`/`b_p` for damping. Barto and Wieland retain source-literal `\mu` symbols whose dimensions differ; keep that distinction explicit.

## Source Evidence

### Section 6 Simulation Source

- Supporting repository: `/mnt/d/home/projects/code/cartpole-physics/main`.
- C# solution: `/mnt/d/home/projects/code/cartpole-physics/main/src/CartPolePhysics.sln`.
- Single-pole double-precision equations: `src/CartPolePhysics/SinglePole/DoublePrecision/CartSinglePoleEquations.cs`.
- A parallel single-precision equation class and double-pole variants also exist; keep corresponding implementations mathematically consistent.
- Console experiment runner: `src/CartPoleConsole/Program.cs`; single-pole data recorder: `src/CartPoleConsole/SinglePole/DoublePrecision/CartSinglePoleSimulator.cs`.
- Existing CSVs, PNGs, and plotting scripts: `r-ggplot2/`.
- On 2026-07-12, the four C# equation classes were updated from the superseded `7/3` and `3/7` inertia factors to the staging article's corrected `3/4` and `4/3` uniform-pole equations. `CartPolePhysics.sln` then built successfully with zero warnings and zero errors.
- The source confirms that its length value `l = 1` is full physical pole length and calculates `l_hat = l / 2`; therefore use $L=1$ m and $r=\hat l=0.5$ m.
- Use the source-backed cart damping $b_c=0.1$ N s/m. The paper formerly said `0.01`, but all single/double and single-/multiple-pole C# defaults use `0.1`, repository history shows no prior `0.01` implementation, the artifact path does not override the default, and the website PNGs are byte-identical to the source-repository PNGs.
- Use pivot damping $b_p=0.001$ N m s/rad, cart mass $m_c=1$ kg, pole mass $m=0.1$ kg, initial angle $\theta=\pi/2$, zero initial cart position and cart/pole velocities, zero external force, and a 15-second duration.
- Preserve source-repository notation only where helpful internally; its `mu_c`/`mu_p` fields represent dimensional linear damping coefficients and correspond to article-local $b_c$/$b_p$, not dimensionless Coulomb coefficients.
- Existing plot scripts generate Figure 2 from Euler CSVs at timesteps `0.001`, `0.005`, and `0.01`, and Figure 3 from Euler versus RK4 at timestep `0.001`.
- On 2026-07-12, `CartPoleConsole` regenerated those six corrected CSVs. At exactly 15 seconds the Euler pole angles are `2.1217070831307066`, `4.829426343307865`, and `5.547432569165429` rad for timesteps `0.001`, `0.005`, and `0.01`; the corresponding RK4 values are `2.8896449083681524`, `2.8896448686982192`, and `2.8896444056696704` rad. All rows passed CSV shape/numeric validation and contained no NaN or infinity values.
- Figures 2-3 were regenerated from those CSVs using the repository's ggplot2 scripts. The scripts now resolve their own location instead of using a stale hard-coded working directory.
- Table 5 uses an RK4 reference with `tau=0.000005` seconds and final angle `2.8896449084416695` rad; halving the timestep from `0.00001` changed the reference by about `8.4e-13` rad. The accepted thresholds are: RK4, 168 steps (`tau=0.0892857143`, error `+0.0096937296`); RK2, 426 steps (`tau=0.0352112676`, error `-0.0097898068`); Euler, 1,203,384 steps (`tau=0.0000124648491`, error `-0.0099999965`). Each immediately coarser integer step count fails.
- At `tau=0.001` seconds, maximum absolute single-versus-double pole-angle differences over 15 seconds are `6.81344e-5` rad for Euler, `1.03984e-5` for RK2, and `4.11779e-5` for RK4. Signed final differences (single minus double) are `-5.63873e-5`, `4.14422e-6`, and `3.50004e-5` rad, respectively.
- Table 5 must be recalculated using the corrected equations. Define and record the reference trajectory, signed final-time error, absolute acceptance criterion $|e|<0.01$ rad at 15 seconds, and the search procedure used to identify each method's maximum accepted timestep.
- Rerun single- versus double-precision comparisons with the corrected equations. Quantify at least the maximum absolute pole-angle difference over 15 seconds and/or the difference at 15 seconds for specified method(s) and timestep(s). Replace section 6.4's provisional phrase “did not differ materially” with the measured result and comparison definition.

- Barto/Sutton/Anderson PDF downloaded to `memory/tmp/references/barto-sutton-anderson-83.pdf`; rendered pages in `memory/tmp/references/barto_pages/`. It is scanned, so text extraction failed.
- User supplied Cannon photo: `/mnt/c/Users/colin/Downloads/PXL_20260708_230436018.jpg`.
- Cannon photo verifies p.705 equations (22.50)-(22.56), including:
  - `J` is moment of inertia about the stick mass center.
  - `J = ml^2/3` for stick length `2l`.
  - Cannon equation (22.55) includes both `J\ddot\theta` and `ml(... + l\ddot\theta)`.
- Therefore Cannon's `J` is additional/body inertia about the centre of mass, not total pivot inertia.
- Cannon sign-error claims are not currently substantiated; do not reintroduce them without a dedicated sign-convention audit.
- Wieland primary-source scans are in `memory/tmp/references/wieland/proceeding_of_the_1990_Summer_school_pg091.png` through `pg101.png`.
- Wieland p.91 confirms the chapter title “Evolving Controls for Unstable Systems”; p.96 defines signed `g=-9.8 m/s^2` in the single-pole dynamics; p.100 gives single-pole equations (9)-(10), multiple-pole equations (11)-(12), and Table 1; p.101 gives Table 2 and the numerical-integration statement.
- Appendix E's primary source is the chapter in *Connectionist Models: Proceedings of the 1990 Summer School*, Morgan Kaufmann, 1991, pp.91-102. Wieland also published a similar IEEE work, but the proceedings chapter is the accessible source used for this comparison and is now reference [5].

## User Preferences

- Stay close to the original section headers, structure, and wording.
- Grammar/readability improvements are welcome.
- Avoid broad restructuring unless needed for correctness or genuine clarity.
- Use `r` consistently for pivot-to-centre-of-mass distance. Do not reuse `l` with multiple meanings.
- Source-specific variables should be mapped explicitly in comparison sections, e.g. Barto's `l` corresponds to this article's `r`.

## Next Steps

1. Review the regenerated Figures 2-3, Table 5 methodology/results, and quantified section 6.4 prose for publication approval. The artifact regeneration itself is complete.
2. Refresh the introduction and conclusion after the body is stable; the introduction overstates appendices C-E as highlighting errors, and the conclusion should summarize the `J`/total-pivot-inertia result.
3. Review Appendix D's statement that the rederivation reveals "discrepancies" plural; the principal equation discrepancy identified is signed gravity, while the inertia and friction discussions mainly clarify interpretation.
4. Consider whether Tables 2/3 should keep `\hat l` for the uniform-pole/recommended equations or map more explicitly to `r = \hat l`.
5. Decide whether shared Figure 1 SVG changes should remain in-place before replacing the published paper; the current shared SVG is consistent with the staging draft but not with the old published HTML.
6. Once the staging draft is stable, decide whether to replace `public/research/cart-pole/cart-pole-equations.html` with the revision draft.

## Resume Prompt

Resume the cart-pole equations revision and section 6 artifact regeneration. First read `/mnt/d/home/websites/sharpneat-web/cart-pole-derivation-review/memory/plans/cart-pole-equations-revision-handoff.md`, especially `Section 6 Simulation Source`, and the staging equations (56)-(57) in `public/research/cart-pole/cart-pole-equations-revision.html`. Then work in `/mnt/d/home/projects/code/cartpole-physics/main`: update the C# from the superseded `7/3`/`3/7` equations to the corrected `3/4`/`4/3` uniform-pole model, verify the implementation, and regenerate the section 6 CSVs, figures 2-3, table 5 results, and precision comparison using the recorded experiment specification. Return the finished website assets and quantified prose results to the website revision worktree.
