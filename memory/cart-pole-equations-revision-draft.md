# Cart-Pole Equations Revision Draft

Date started: 2026-07-08

Scope: Fresh working revision for `public/research/cart-pole/cart-pole-equations.html`.

Status: Draft notes for a future rewrite. This file is intentionally separate from the published article.

## Draft HTML

Created: `public/research/cart-pole/cart-pole-equations-revision.html`

Intent: This is a staging draft for a replacement of `public/research/cart-pole/cart-pole-equations.html` at the current URL, not a separate article that critiques the old one.

Current state:

- `cart-pole-equations-revision.html` is now a staging copy of the existing article, not a new outline.
- Includes a temporary draft notice, but the article body should read as the future replacement article.
- The user prefers staying close to the original section headers, structure, and wording.
- Grammar/readability improvements are welcome, but broader restructuring should be avoided unless needed to fix an error or clarify a genuinely confusing passage.
- Planned mathematical changes should be made as targeted edits to the original derivation flow, especially around the interpretation of `J` and the resulting `4/3` versus `7/3` factors.
- Table 1 in the revision draft now defines `G` as the pendulum/pole centre of mass, `L` as full physical pole length, `r` as pivot-to-pole-centre-of-mass distance, and `J` as body inertia about the pole centre of mass. It no longer defines `l` or `\hat l` as main symbols.
- Figure 1's SVG sources now label the pole centre of mass as `G` and the pivot-to-centre-of-mass distance as `r`.

Next derivation step:

Continue with the remaining publication-readiness prose checks and final replacement workflow. The numerical integration CSVs, Figures 2-3,
Table 5, and floating-point comparison have been regenerated from the corrected C# equations. The remaining focused checks are the introduction,
conclusion, Appendix D's use of “discrepancies” plural, and the final decision to replace the published HTML with the staging draft.

## Source-First Notation Audit

User direction: choose variables/constants after checking the reference papers directly, aiming to align with the sources as closely as possible without inheriting errors from the original article.

### Whole-paper symbol review (2026-07-12)

User direction: define one consistent article-local symbol set for the paper's own derivations, while retaining the literal source notation in source-labelled equations in Appendices C, D, and E.

Implemented resolution: the local notation now uses `r` (and `r_i`) everywhere for pivot-to-centre-of-mass distance. A uniform pole has `r=L/2`, but this is a specialization rather than the definition of `r`. Each source's `l` maps directly to local `r`; source `l` remains only in source-faithful equations and source descriptions in Appendices C-E.

User clarification: `r` is the distance from the pivot to the pole's actual centre of gravity wherever that point lies. It is not inherently the pole's half length. Thus the `r`-based general equations are also the appropriate starting point for a pole with a non-uniform mass distribution; only the uniform-pole specialization sets `r=L/2`. In the assumed uniform gravitational field, centre of gravity and centre of mass coincide; the paper may use “centre of mass” for the inertial derivation while noting this equivalence.

Other notation points to resolve during implementation:

- Resolved: retain `J` for body inertia about centre of mass `G`, and keep total pivot inertia explicit as `mr^2+J`; do not introduce `I_G` or `I_P`.
- Preserve source `F`, `F_t`, `M`, signed `g`, `\mu_c`, `\mu_p`, `\tilde F`, `\tilde m`, and time subscripts in source-labelled equations; introduce local symbols only after an explicit mapping boundary.
- Resolved: source-labelled equations in Appendices D-E retain Barto's and Wieland's literal `\mu_c` and `\mu_p`. At each mapping boundary the corresponding physical terms are defined as local signed quantities `F_f=-\mu_c\sgn(\dot x)` and `M_f=\mu_p\dot\theta`; locally rederived equations use `F_f` and `M_f`, avoiding mixed notation systems.
- Resolved: standardized vector/scalar typography. The complete inertial force `\mathbf{F}^i_G` and pivot reaction `\mathbf{N}_P` are vectors, while their Cartesian components use component notation. The planar signed moments `M_G`, `M_g`, `M_i`, and `M_f`, and horizontal friction force `F_f`, are scalars. General vector equations in Appendices A-B retain bold symbols.
- Resolved: standardized multiple-pole indexing as `m_i`, `r_i`, `J_i`, `\theta_i`, `M_{f,i}`, and `b_{p,i}`. Each pivot may have its own damping coefficient; identical pivots can assign the same numerical value to every `b_{p,i}`. The single-pole equations retain `b_p`.
- Resolved: section 6.3 now uses conventional timestep indices `n` and `n+1` instead of word-like `next` subscripts. Euler updates are written as `x_{n+1}=x_n+\tau\dot x_n` and corresponding indexed velocity and pole equations; pole `i` uses `\theta_{i,n}`. The prose states that every right-hand-side quantity is evaluated from the same saved timestep-`n` state.
- Resolved: Table 1 now defines `O` as the origin of the fixed Cartesian `xy` frame and states the positive axis directions, rather than treating the axes and point `O` as one compound table symbol. A separate row defines the cart's scalar horizontal coordinate `x`. Uppercase geometric-point labels are retained in subscripts throughout: `x_G`, `y_G`, `x_{G|P}`, `y_{G|P}`, and `\mathbf{F}^i_G`. Lowercase subscripts remain reserved for physical effects such as gravity (`M_g`) and friction (`F_f`, `M_f`).

Current source status:

- Barto, Sutton, Anderson 1983: downloaded primary PDF to `memory/tmp/references/barto-sutton-anderson-83.pdf`. It is scanned; `pdftotext` produced no text. Rendered pages to `memory/tmp/references/barto_pages/`. Visual inspection of the appendix confirms their cart-pole model uses `l = 0.5 m, half-pole length`, `m_c = 1.0 kg`, `m = 0.1 kg`, `g = -9.8 m/s^2`, `\mu_c`, `\mu_p`, and equations with the familiar `4/3` denominator factor.
- Cannon 1967: user supplied photo `/mnt/c/Users/colin/Downloads/PXL_20260708_230436018.jpg` of p.705, Sec. 22.4 "Unstable Mechanical System: Stick Balancer". This directly shows equations (22.50)-(22.56), including (22.53)-(22.55). It verifies that Cannon defines `J` as the moment of inertia of the stick about its mass center, with `J = ml^2/3` for a stick of length `2l`, and equation (22.55) uses `J\ddot\theta + ml(\ddot x\cos\theta + l\ddot\theta) - mgl\sin\theta = 0`. Therefore Cannon's `J` is additional/body inertia about the center of mass, not total pivot inertia.
- Cannon sign-error claims should be treated as unverified until a dedicated sign-convention audit is completed. The public draft should not claim Cannon has sign errors merely from the old article's annotations.
- Wieland 1991: the user supplied scans of pp.91-101 at `memory/tmp/references/wieland/`. Page 96 gives the single-pole linearized dynamics and explicitly defines signed `g=-9.8 m/s^2`; p.100 shows single-pole equations (9)-(10), multiple-pole equations (11)-(12), effective-force/effective-mass definitions, and Table 1; p.101 gives Table 2 and the numerical-integration methods. Appendix E has been revised to retain Wieland's literal single-pole notation before explicitly mapping `M`, `F`, `l`, and signed `g` into local notation. The integration quotation has also been verified and cleaned.
- Bibliographic correction for Wieland: the primary source used by Appendix E is Alexis P. Wieland, “Evolving Controls for Unstable Systems,” in *Connectionist Models: Proceedings of the 1990 Summer School*, eds. David S. Touretzky, Jeffrey L. Elman, Terrence J. Sejnowski, and Geoffrey E. Hinton, Morgan Kaufmann, 1991, pp. 91-102, ISBN 1-55860-156-2. Reference [5] now identifies this proceedings chapter instead of the separate IEEE work.

Notation implication from verified Barto source:

- The literature commonly uses lowercase `l` for half-pole length / pivot-to-centre-of-mass distance, not full physical pole length.
- Decision: use article-local `r` consistently for pivot-to-centre-of-mass distance. Do not mix meanings for `l`. In source comparison sections, map each paper's notation onto `r` explicitly, e.g. Barto's `l` corresponds to this article's `r`.

## Equation Review Progress

| Date | Reviewed Through | Next Equation | Status |
| --- | --- | --- | --- |
| 2026-07-08 | Table 1 | (1) | Table 1 reviewed. Uses `G` for pole/pendulum centre of mass, `r` for pivot-to-centre-of-mass distance, `L` for full physical pole length, and `J` for body inertia about centre of mass. Removed `l` and `\hat l` from main symbols; updated stale `Q` references to `G`/`PG`. |
| 2026-07-08 | (6) | (7) | Revised equations (1)-(6) to use centre of mass G at distance `r` from pivot P. Rewrote the note after (6) to distinguish centripetal acceleration from tangential acceleration. |
| 2026-07-08 | (9) | (10) | Revised section 3.2 to use absolute acceleration of point G. Equations (7)-(9) now use `G,r`; Cannon-style vector form is presented as mapped into this article's notation. |
| 2026-07-08 | (11) | (12) | Revised section 3.3 to use inertial force on point G. Equations (10)-(11) now use `F^i_g`, `x_g/y_g`, and `r`. |
| 2026-07-08 | (12) | (13) | Revised section 3.4 cart force balance to use point G, `F^i_g`, and `r`. Equation (12) remains the horizontal force balance with `r` replacing old `l`. |
| 2026-07-08 | (18) | (19) | Revised section 3.5 rotational force balance to use point G, `F^i_g`, `M_G`, and `r`. Equation (14) now explicitly contains the `mr^2\ddot\theta` contribution from centre-of-mass motion around P. Equation (16) now defines `J\ddot\theta` as additional body inertia about the centre of mass, not total pivot inertia. |
| 2026-07-09 | (18) | (19) | User supplied Cannon p.705 photo. Verified Cannon defines `J` as moment of inertia about the stick mass center. Removed unverified Cannon sign-error critique from inset after (14). Clarified equation (16) commentary: `J` is the pendulum/pole body's moment of inertia about centre of mass G, and total pivot inertia is `mr^2 + J`. |
| 2026-07-09 | (28) | (29) | Revised section 3.6 and 3.7. Equations (19)-(24) now use `r` and `mr^2 + J`. Equation (25) now defines the total pivot inertia factor `mr^2 + J = \alpha mr^2` rather than the ambiguous old `J = kml^2`. Equations (26)-(28) now use `\alpha`; point mass has `\alpha = 1`, uniform-pole hybrid has `\alpha = 4/3`. |
| 2026-07-09 | (59) | Section 6 / diagram pass | Revised section 3.8 friction equations, section 3.9 multiple-pole equations, section 4 hybrid model, and section 5 recommended equations. Downstream equations now use `r`, `r_i`, `\alpha`, and `\alpha_i`; uniform-pole hybrid and recommended equations now use `4/3` and `3/4` factors instead of the old `7/3` and `3/7` factors. |
| 2026-07-09 | Appendix E | Section 6 / diagram pass | Revised Appendix C to map Cannon's `J` to body inertia about the mass centre and removed the unsubstantiated Cannon sign-error claim. Revised appendices D and E so Barto/Wieland `4/3` factors are treated as consistent with the corrected uniform-pole hybrid interpretation rather than as problematic inertia terms. |
| 2026-07-09 | Section 6 and Figure 1 | Figure/data verification | Reviewed section 6 for stale notation; fixed the `Adams-Bashforth` typo. Updated `public/research/cart-pole/cart-pole-model.svg` and `cart-pole-model-master.svg` so Figure 1 labels the centre of mass as `G` and the pivot-to-centre-of-mass distance as `r`. Removed the temporary Table 1 note that said Figure 1 still used `Q`. |
| 2026-07-09 | Section 4 | Figure/data verification | Reframed section 4 from a "hybrid model" to a specialization of the general equations for a uniform pole. With `J` resolved as centre-of-mass body inertia, no separate hybrid approximation is required; section 4 now just substitutes `r = \hat l`, `J = (1/3)m\hat l^2`, and `\alpha = 4/3`. |
| 2026-07-09 | Section 3.8 friction | Figure/data verification | Revisited the friction treatment. Clarified that `\mu_c` and `\mu_p` are model-dependent friction parameters: dimensionless only in translational Coulomb friction, but dimensional damping coefficients in the recommended velocity-proportional equations. Fixed rotational friction wording so `\mathbf{M}_f` is a moment, not a force, and corrected `\sgn` notation in equations (35), (39), and (41). |
| 2026-07-09 | Appendix A | Figure/data verification | Rewrote the d'Alembert appendix to explain dynamic equilibrium as Newton's Second Law rearranged into `\sum \mathbf{F} + \mathbf{F}^i = 0`, using the same superscript `i` inertial-force notation as the main derivation. Added the corresponding moment form and a note reconciling `\mathbf{M}^i=-I\boldsymbol{\alpha}` with the article's clockwise-positive `\ddot\theta` scalar convention. |
| 2026-07-09 | Appendix B | Figure/data verification | Rewrote the torque appendix to define torque as `\boldsymbol{\tau}=\mathbf{r}\times\mathbf{F}`, clarify that only the force component perpendicular to the pivot radius contributes, and align the sign explanation with the article's anticlockwise-positive moment convention versus clockwise-positive angular variables. Replaced the old derivative-based lever discussion with a shorter static torque-balance explanation. |
| 2026-07-09 | Appendix C | Figure/data verification | Expanded the Cannon appendix to state Cannon's own nonlinear equations and small-angle equations using source-style tags `(Cannon 22.55)` and `(Cannon 22.56)`, then mapped them to the article's uniform-pole equations with local C-numbered derivations. The appendix now explicitly shows Cannon's pivot inertia as `J + ml^2 = 4/3 ml^2`, and derives equations (C3)-(C5) step by step from Cannon's nonlinear pair. |
| 2026-07-10 | Figure 1 / Appendix C | Figure/data verification | Clarified that Figure 1 is a centre-of-mass schematic: for an ideal point-mass pendulum G is the bob at the end of a massless rod, while for a uniform pole G is halfway along the physical pole and the full pole length is twice PG. Added matching Appendix C wording to distinguish Cannon's uniform stick of length `2l` from an ideal point-mass pendulum. |
| 2026-07-10 | Appendix D | Figure/data verification | Reviewed Appendix D against the scanned Barto source. Added explicit mappings from Barto's `l` and `F_t` to local `\hat l` and `f`; restored the full source parameter set; tightened the negative-`g` critique to the sign inconsistency demonstrated by the equation and angle convention; removed stale citation-count speculation; and identified `\mu_c` as a constant Coulomb friction force and `\mu_p` as a dimensional pivot damping coefficient. |
| 2026-07-10 | Appendices C-E titles | Figure/data verification | Renamed the three source-comparison appendices to `Comparison with the Equations of ...`, reflecting that each appendix maps, rederives, and evaluates source equations rather than merely reproducing them. |
| 2026-07-10 | Appendix E | Figure/data verification | Reviewed and revised Appendix E against user-supplied scans of Wieland pp.91-101. Restored consistent single-pole effective-force/effective-mass notation in (E1)-(E4); mapped source `M`, `F`, `l`, and signed `g` to local `m_c`, `f`, `\hat l`, and `g_W`; verified (E5)-(E6) against (28-F)/(27-F); restored Table 1 values; clarified dimensional friction parameters and the `4/3` pivot-inertia factor; verified and cleaned the integration quotation; and corrected reference [5] to the proceedings chapter “Evolving Controls for Unstable Systems.” |
| 2026-07-11 | Appendices C-E notation | Figure/data verification | Adopted a consistent source-notation boundary across the comparison appendices. Source-labelled equations retain Cannon's, Barto's, or Wieland's literal symbols; explicit mappings then introduce the paper's notation for local rederivations and comparisons. Appendix C already retained Cannon's notation and now states the rule explicitly. Appendix D restored Barto's `l`, `F_t`, and time subscripts in (D1)-(D2). Appendix E restored Wieland's `M`, `F`, `l`, and signed `g` in (E1)-(E4), while retaining `g_W` only after the mapping boundary. |
| 2026-07-11 | Equations (25)-(59), appendices C-E | Figure/data verification | Removed the model-specific `\alpha`/`\alpha_i` inertia parameters. General equations now retain the physically meaningful sums `mr^2+J` and `m_i r_i^2+J_i`; equation (25) states `J=0` for a point mass and `J=(1/3)mr^2` for a uniform pole. Recast (28), (28-F), and their uniform/recommended/multiple-pole descendants with positive denominators; the uniform-pole forms now expose the `3/4` coupling factors and match (C5) directly. |
| 2026-07-11 | Section 3.6 onward | Coherence pass | Removed the redundant former section 3.7 and equations (25)-(28). Equation (24) now carries the positive-denominator cart-acceleration form directly. The model-choice note and evaluation guidance now close section 3.6 and refer to (19)/(21) or (24)/(23). Friction is now section 3.7, multiple poles section 3.8, and the friction extensions are labelled (19-F), (21-F), (23-F), and (24-F). Downstream and appendix references were updated without renumbering later equations. |
| 2026-07-11 | Equations (24) and (24-F) | Coherence pass | Replaced compound fractions containing `(mr)^2/(mr^2+J)` with equivalent single-fraction forms. The revised equations retain the positive denominator and display the total pivot inertia `mr^2+J` directly; uniform-pole specialization still reduces to the simpler `3/4` form. |
| 2026-07-11 | Sections 3.7, 5, 6 and appendices D-E | Coherence pass | Separated friction notation by physical model. Local `\mu_c`/`\mu_p` are now dimensionless Coulomb coefficients; `b_c`/`b_p` are linear damping coefficients; Coulomb pivot moments include effective radius `r_p`; and the wheel-bearing example uses rotational damping `b_w`. Recommended equations and numerical parameter labels use `b_c`/`b_p`. Barto and Wieland retain source-literal `\mu` symbols with explicit dimensional distinctions from local notation. |
| 2026-07-11 | Equations (29)-(31) | Coherence pass | Redefined `N_c` as the conventional nonnegative upward normal reaction exerted by the track on the cart. Removed `|N_c|`, reversed equation (30) accordingly, and made the contact-domain condition explicit: if the calculated reaction reaches zero, the cart loses contact and the Coulomb-friction model ceases to apply. |
| 2026-07-12 | Whole-paper length notation | Symbol coherence pass | Removed local `\hat l`/`\hat l_i` notation. The paper's own equations now use `r`/`r_i` consistently for pivot-to-centre-of-mass distance, including uniform-pole and recommended equations. A uniform pole has `r=L/2`; non-uniform poles may have a different centre-of-mass location. Appendices C-E retain source `l` in source-style equations and map it explicitly to local `r`. |
| 2026-07-12 | Supporting C# equation classes | Symbol coherence pass | Updated the single- and double-pole equation classes, in both double and single precision, to use `r`/`r2` as pivot-to-centre-of-mass distances instead of accepting full `l`/`l2` values and deriving `l_hat`/`l2_hat`. Defaults changed from full lengths 1/0.1 m to the equivalent centre-of-mass distances 0.5/0.05 m, so numerical behaviour is unchanged. The solution builds with no warnings or errors, and paper-analysis results exactly match Table 5 and the recorded precision comparison. |
| 2026-07-12 | Supporting double-pole C# equation classes | Symbol coherence pass | Replaced the shared `b_p` field and constructor parameter with independent `b_p1` and `b_p2` coefficients in both double- and single-precision implementations. Both defaults remain 0.001 N m s/rad, preserving the previous identical-pivot model. The coefficients are applied separately in the cart-acceleration and two pole-acceleration equations. The solution builds with no warnings or errors. |
| 2026-07-12 | Appendix C notation transition | Symbol coherence pass | Added a Cannon-to-local mapping table covering `l -> r`, unchanged shared symbols, the local mass shorthand `m_t=m_c+m`, and the pivot-inertia expressions. Clarified that Cannon-labelled displays reproduce source notation, while local derivations (C1)-(C5) temporarily retain Cannon's symbols for direct comparison; `m_t` is explicitly marked as this paper's shorthand before the final `l -> r` conversion. |
| 2026-07-12 | Appendix D notation transition | Symbol coherence pass | Verified that Barto's appendix equations are unnumbered, then replaced local-looking source tags with descriptive `(Barto pole)` and `(Barto cart)` tags. Removed the listed gravity parameter from the equation block, combined the source parameter values and Barto-to-local mappings into one table, and renumbered the local rederivations as (D1)-(D2). The combined table flags the incompatible listed gravity sign; the prose states that the algebraic comparison uses local positive `g` and that time subscripts are omitted only because one instant is being evaluated. |
| 2026-07-12 | Appendix E notation transition | Symbol coherence pass | Verified Wieland's source numbers (9)-(10) and retagged the literal displays as `(Wieland 9)`, `(Wieland 10)`, `(Wieland force)`, and `(Wieland mass)`. Combined source values and Wieland-to-local mappings into one table, including `g_W=-g`, friction-term mappings, and the elimination of effective force/mass by substitution. Renumbered the local derivations from (E5)-(E6) to (E1)-(E2) and updated all references. |

## Aim

Rebuild the cart-pole derivation in a clearer and corrected form, with special care around rotational inertia terminology.

Audience assumption: the reader may be mathematically rusty and may not be familiar with rigid-body dynamics. The rewrite should therefore explain the physical meaning of each term before or alongside the algebra.

## Current Correction To Carry Forward

The published article currently lets `J` drift between two meanings:

1. Extra/body rotational inertia added as `J\ddot\theta`.
2. Total moment of inertia about the pivot.

Those are not the same quantity in the equations as written.

Equation (14) already contains the rotational inertia contribution from the center of mass moving around the pivot:

```tex
\mathbf{M}_q = ml(l\ddot\theta + \ddot x\cos\theta)
```

The first term expands to:

```tex
ml^2\ddot\theta
```

That is the inertia-like torque for a point mass at distance `l` from the pivot. Therefore, when equation (18) adds:

```tex
J\ddot\theta
```

`J` must be interpreted as extra/body moment of inertia about the mass center, not as the complete moment of inertia about the pivot.

Equivalently:

```tex
I_{pivot} = mr^2 + I_{cm}
```

In the current derivation:

```tex
mr^2\ddot\theta
```

is already present in equation (14), so:

```tex
J = I_{cm}
```

## Consequences

For an ideal point-mass pendulum:

```tex
J = 0
```

For a uniform pole modeled with its center of mass at distance `\hat l` from the pivot:

```tex
J = \frac{1}{3}m\hat l^2
```

because the full pole length is `2\hat l`, and the body inertia about the center of mass is:

```tex
I_{cm} = \frac{1}{12}m(2\hat l)^2 = \frac{1}{3}m\hat l^2
```

The total inertia about the pivot is then:

```tex
m\hat l^2 + \frac{1}{3}m\hat l^2 = \frac{4}{3}m\hat l^2
```

Therefore the hybrid equations should use the standard `4/3` factor, not `7/3`.

The revision draft now carries this correction through the main body and appendices C-E, but the original published article remains unchanged.

## Proposed Rewrite Strategy

### 1. Establish the physical model

Clearly distinguish these cases:

- Ideal point-mass pendulum: mass at the end of a massless rod.
- Uniform rigid pole: mass spread along the pole.
- Hybrid literature model: translational forces treated as if the pole mass is concentrated at its center of mass, with extra rigid-body inertia added separately.

Important wording:

```text
The center of mass can be treated as a point mass for translational motion, but an extended rigid body may also resist rotation about its own center of mass.
```

### 2. Derive center-of-mass acceleration

Keep the existing position, velocity, and acceleration derivation, but use the term "center of mass" where possible rather than only "point Q", because the same equations also apply to the hybrid model if `l` is read as the distance from pivot to center of mass.

Key conceptual distinction:

- Terms with `\dot\theta^2` are centripetal acceleration.
- Terms with `\ddot\theta` are tangential acceleration.
- The `\ddot x` term appears because the pivot itself accelerates horizontally with the cart.

### 3. Derive the cart force equation

Preserve the existing force balance, but explicitly state that it uses the acceleration of the pendulum mass center. For a rigid pole, this is the center of mass acceleration.

### 4. Derive the rotational equation

This is where the correction matters most.

Use language like:

```text
The torque caused by the inertial force at the mass center includes the `mr^2\ddot\theta` contribution associated with moving the mass center around the pivot. If the pendulum is an extended rigid body, there is an additional inertial torque due to the body's rotation about its own center of mass. We write that additional term as `J\ddot\theta`.
```

Then define:

```tex
J = I_{cm}
```

not:

```tex
J = I_{pivot}
```

### 5. Combine equations using a general `J`

The algebra can still use:

```tex
ml^2 + J
```

or, more generally:

```tex
mr^2 + J
```

where `r` is distance from pivot to center of mass.

This form is actually useful because it makes the physics explicit:

```tex
I_{pivot} = mr^2 + J
```

### 6. Apply model-specific inertia

Recommended organization:

- Point-mass model:

```tex
r = l,\quad J = 0,\quad I_{pivot} = ml^2
```

- Uniform-pole hybrid model:

```tex
r = \hat l,\quad J = \frac{1}{3}m\hat l^2,\quad I_{pivot} = \frac{4}{3}m\hat l^2
```

Avoid saying "substitute `k = 4/3` into `J = kml^2`", because that reintroduces the ambiguity. Retain `mr^2+J` explicitly in the
general equations. Substitute `J=0` for a point mass or `J=(1/3)mr^2` for a uniform pole only when specializing the equations.

### 7. Revisit appendices

Appendix C should be revisited carefully. The previous claim that Cannon's `4/3` should be corrected to `7/3` appears to follow from treating Cannon's `J` as total pivot inertia rather than additional body inertia.

Appendix statements about Barto et al. and Wieland inheriting a problematic inertia term should also be rechecked under this corrected interpretation.

## Open Questions

- Should the rewrite preserve Cannon's sign conventions exactly, or recast the derivation in a cleaner modern convention and then map back to Cannon?
- Should the revised article use `r` for center-of-mass distance throughout, reserving `l` for full pole length and `\hat l` for half length?
- Should the article explicitly derive the rigid-body inertia of a uniform rod about its center of mass, or cite it and explain only how the parallel-axis split works?
- The final recommended equations specialize to a uniform pole; the preceding general equations retain `J` and also support the point-mass case through `J=0`.
- The source-notation treatment in appendices C-E is resolved: source equations use source symbols, followed by explicit mappings into article-local notation.
- Resolved 2026-07-12: the section 6 CSVs, Figures 2-3, Table 5, and floating-point comparison were regenerated after correcting the recommended equations from the old `7/3` factors to `4/3`.
- The supporting simulation source and ggplot scripts are in `/mnt/d/home/projects/code/cartpole-physics/main`. The legacy single-pole equation
  class explicitly sets full pole length `l = 1` and derives `l_hat = l / 2`, confirming that Table 4's 1 m pole meant a full physical length
  of 1 m and that equations (56)-(57) were evaluated with $\hat l=0.5$ m. The source equation classes now use the corrected `4/3` and `3/4`
  factors and article-aligned `m_t`/`m_sigma` and `b_c`/`b_p` symbols. The artifact-generation path uses cart damping `b_c = 0.1`.
- Resolved 2026-07-12: Table 4's former $b_c=0.01$ N s/m conflicted with every single- and double-precision C# default, all of which use
  `0.1`; repository history found no earlier `0.01` source value or artifact-path override, and the website PNGs are byte-identical to those
  in the supporting repository. The user chose the source-backed $b_c=0.1$ N s/m for the corrected regenerated experiment, and Table 4 now
  states that value. This is an experiment parameter choice, not a physically mandated coefficient.

## Working Principle For The Revision

Every inertia term should answer one of two questions:

1. Is this resistance from the mass center moving around the pivot?
2. Is this extra resistance from an extended body rotating about its own center of mass?

If the answer is both, the derivation is probably double-counting.

## Coherence Audit (2026-07-11)

A fresh sequential reader's-pass found that the overall derivation story remains sound: physical model; centre-of-mass kinematics; inertial
forces; horizontal and rotational balances; coupled acceleration equations; friction and multiple-pole extensions; uniform-pole
specialization; recommended equations; numerical integration; source comparisons. The following narrative seams should be addressed in a
focused prose pass:

- Resolved 2026-07-11: removed the redundant former section 3.7 and equations (25)-(28); folded its useful model-choice and evaluation
  guidance into the end of section 3.6, with the positive-denominator form now carried by equation (24).
- Resolved 2026-07-11: section 3.7 now has a concise opening that introduces $F_f$/$\mathbf{M}_f$, identifies the two friction-model families,
  defines the distinct `\mu`/`b` notation, scopes out static friction, and states the sign check. The detailed Coulomb and bearing-damping
  explanations were moved into numbered cart-track and pivot subsections; the general microscopic-detail paragraph was removed.
- Resolved 2026-07-12: section 3.8 now explicitly returns from friction to the frictionless balances before deriving the multiple-pole
  equations. It identifies equation (10) as the inertial force at G, defines $\tilde F_i$ as each pole's inertial-force contribution after
  internal pivot interactions cancel, and no longer calls equation (10) the physical force applied by a pole to the cart.
- Resolved 2026-07-12: section 3.4 now describes equation (12) as the horizontal dynamic-equilibrium balance for the combined cart-pole
  system. It explains that the unknown pivot interaction is internal and cancels, and that gravity is absent because the balance is horizontal.
- Resolved 2026-07-12: section 4.1 retains only the short centre-of-mass and total-pivot inertia calculation needed to make the uniform-pole
  substitution self-contained; no further reduction was judged helpful.
- Resolved 2026-07-12: section 5 now introduces its uniform-pole inertia and linear-damping choices as explicit model assumptions used to
  produce one complete simulation equation set, rather than as a "recommendation for each option".
- Resolved 2026-07-12: section 6's corrected CSVs and Figures 2-3 were regenerated; Table 5 was recalculated against a converged RK4 reference;
  and section 6.4 now reports defined, measured single-versus-double precision differences.
- Resolved 2026-07-12: section 6.3 now defines an explicit Euler step using a saved start-of-timestep state. Both accelerations and all
  next-state values are calculated from that saved state, then the position and velocity updates are applied simultaneously.
- Resolved 2026-07-12: section 6's opening now describes the equations as state derivatives, numerical integration as a discrete
  approximation to continuous state evolution, and the resulting discrepancy as numerical error. It no longer uses the nonstandard
  "gradient projection error" terminology or treats numerical error as direct evidence about real-world fidelity.
- Resolved 2026-07-12: section 6.1 now explains the global-error scaling of Euler and RK4 without claiming a universal $10^6$ accuracy
  ratio. It notes the small-timestep qualification and dependence on the equations and method-specific error constants.
- Resolved 2026-07-12: section 6.3 no longer describes Runge-Kutta integration as an unspecified iterative algorithm. It now explains
  that these methods evaluate derivatives at intermediate trial states and combine those evaluations to obtain the next state.
- Resolved 2026-07-12: Table 4 now identifies the experiment pole measurement as full physical length $L=1$ m and states explicitly
  that the uniform-pole equations therefore use the centre-of-mass distance $r=\hat l=0.5$ m. This interpretation is confirmed by the
  supporting C# source, which sets full length to 1 m and derives its half-length parameter.
- Resolved 2026-07-12: section 6.2 now uses the physical term "damping" rather than "dampening" and describes the decreasing pole
  amplitude as the combined effect of coupled cart motion and the specified cart-track and pivot damping.
- Resolved 2026-07-12: section 6.2 now limits the three-timestep RK4 comparison to evidence of numerical convergence toward the
  differential-equation solution, rather than evidence of real-world physical fidelity. It states explicitly that the nearly coincident
  RK4 trajectories are omitted because a separate plot would add little value.
- Resolved 2026-07-12: the Figure 3 discussion now uses the converged RK4 trajectory as a numerical reference rather than calling it the
  "correct path". It acknowledges that RK4 remains approximate and limits the damping observation to the stated simulated system.
- Resolved 2026-07-12: Table 5 now distinguishes its absolute-error timestep-selection criterion from the signed errors displayed in its
  final column. The unexplained superscript `c` markers were removed; radians are identified conventionally as `rad` in the heading.
- Resolved 2026-07-12: section 6.2 now treats Table 5's timestep limits as specific to the stated model, duration, error measure, and
  tolerance. Its general recommendation is to prefer RK2/RK4 and establish a suitable timestep for other configurations through convergence
  testing rather than copying the table values directly.
- Resolved 2026-07-12: section 6.3 retains its general title but now distinguishes the timestep-based outer simulation loop shared by Euler,
  RK2, and RK4 from the method-specific work inside each step. The saved-state numbered procedure appears under an explicit "Euler Update"
  subheading, followed by a concise contrast with RK2/RK4 intermediate derivative evaluations.
- Resolved 2026-07-12: section 6.4 now distinguishes floating-point round-off from integration-method/timestep discretization error and
  reports the maximum absolute and signed final single-versus-double precision pole-angle differences for Euler, RK2, and RK4 at
  $\tau=0.001$ seconds over 15 seconds.

- Resolved 2026-07-12: the Figure 2 discussion now describes increasing swing amplitude as numerical energy introduced into the damped,
  unforced model. It no longer says that a physical conservation law is being violated or that the simulation is "in no way representative"
  of a real system, and the result is split into two shorter paragraphs.
- Appendix D says the rederivation reveals "discrepancies" plural, although the main identified equation discrepancy is the signed gravity;
  the inertia and friction subsections mostly explain consistency/model interpretation.
- Resolved 2026-07-11: Appendix E now explains that the `3/4` factor in (E5) and `4/3` factor in (E6) are algebraically related forms of
  the same uniform-pole inertia contribution.
- The introduction's promise that appendices C-E "highlight errors" now overstates their role; they primarily compare notation, modeling
  choices, and one apparent sign inconsistency. The conclusion should summarize the central `J`/total-pivot-inertia result rather than only
  offering a generic closing statement.
