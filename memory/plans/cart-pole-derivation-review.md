# Cart-Pole Equations Derivation Review

- Reviewed: 2026-07-13
- Reviewed page: `public/research/cart-pole/cart-pole-equations.html`
- Reviewed revision: `d07bb73` (`Refine cart-pole derivation and figure`)
- Status: In progress; F1-F6, F11, and F14 resolved on 2026-07-13

## Review Goal

Review the correctness, structure, and explanatory flow of the cart-pole paper, with particular attention to its pedagogical standard: a motivated reader who understands basic algebra, calculus, and trigonometry should be able to follow every derivation step without silently supplying missing mechanics knowledge or substantial intermediate algebra.

The initial review was documentation only. Resolution notes below now record changes made to the public paper as findings are worked through.

## Overall Assessment

The final equations are mathematically consistent with the physical model and sign conventions stated in the paper. Independent reconstruction and symbolic substitution found no incorrect final equation in the kinematics, single-pole dynamics, friction extensions, uniform-pole specialization, multiple-pole extension, or recommended equations.

The broad structure is also sound:

1. define the model and signs;
2. derive centre-of-mass kinematics;
3. form horizontal-force and moment balances;
4. solve the coupled equations;
5. add friction and multiple poles;
6. specialize to uniform poles;
7. collect recommended simulation equations; and
8. discuss numerical integration.

The main remaining weakness is not the correctness of the final formulas, but the gap between the paper's intended audience and the mechanics knowledge it sometimes assumes. The initial review also found uneven algebraic detail: a reader was guided line by line through differentiation, then expected to supply substantial later substitutions. The F3 and F4 revisions have now expanded the friction, multiple-pole, uniform-pole, and Wieland transitions; the remaining findings concern physical assumptions, topology, implementation guidance, and smaller narrative or editorial issues.

The initial review also found one formal derivation issue in section 3.6: the final acceleration equations were valid when the pole was horizontal, but both written elimination routes passed through intermediate equations that divided by `cos(theta)`. This was resolved on 2026-07-13 by retaining only the nonsingular cart-acceleration-first route in the main paper. The opposite elimination order is now derived once, in Appendix D, because Barto et al.'s source equation requires that form for direct comparison.

## Verification Performed

The following checks passed:

- Equations (1)-(8) follow from `x_(G|P) = r sin(theta)` and `y_(G|P) = r cos(theta)` with clockwise `theta` measured from upright.
- Equations (10)-(12) use the stated d'Alembert inertial-force convention consistently.
- The component expansion of the moment in (13)-(14), the gravity moment in (15), and the body inertial moment in (16) give the balance in (18) with the declared signs.
- Solving the balances in (12) and (18) independently reproduces the nonsingular pair (19)-(20).
- Adding signed `F_f` and `M_f` to the balances reproduces the friction pair (34)-(35).
- Substituting `J = (1/3)mr^2` reproduces the uniform-pole equations (44)-(47).
- Substituting each pole equation into the combined cart balance reproduces (41)-(43); a two-pole symbolic residual check reduced exactly to zero.
- Linearizing (47) for two dynamically identical equal-length poles and subtracting the two equations gives
  `delta-double-dot = (3g/4r) delta`; the cart acceleration cancels, confirming the unstable uncontrollable difference mode and its synchronized zero-difference exception.
- Substituting the adopted damping models reproduces the recommended equations (48)-(51).
- The algebra in Appendices C-E is consistent with the source equations as transcribed and with the paper's stated notation mappings.
- The explicit Euler update in section 6.3 uses one saved state and applies the new values simultaneously, as required.

The primary-source transcriptions and quotations in Appendices C-E were not independently re-read from every external publication during this pass. This review used the source-verification record already captured in `memory/sources.md` and checked the algebra beginning from the equations transcribed in the page.

## Findings Index

| ID | Priority | Finding | Status |
| --- | --- | --- | --- |
| F1 | P1 | Section 3.6 proves the final formulas through expressions that exclude `cos(theta)=0` | [x] Resolved 2026-07-13 |
| F2 | P1 | The moment derivation omits the component cross-product rule and gravity-moment steps | [x] Resolved 2026-07-13 |
| F3 | P1 | The friction-extended single-pole equations are stated rather than derived | [x] Resolved 2026-07-13 |
| F4 | P1 | Later specialization and multiple-pole friction steps are substantially compressed | [x] Resolved 2026-07-13 |
| F5 | P1 | The alternative solution route appears before the recommended route | [x] Resolved 2026-07-13 |
| F6 | P1 | The multiple-pole topology, index scope, and identical-length claim need clarification | [x] Resolved 2026-07-13 |
| F7 | P2 | The track and pivot reaction-force balances need explicit direction definitions | [ ] Open |
| F8 | P2 | The pivot-to-cart kinematic assumption is used without being written as an equation | [ ] Open |
| F9 | P2 | The recommended equations need a self-contained evaluation recipe and denominator check | [~] Partly resolved 2026-07-13 |
| F10 | P2 | The numerical “trajectory” discussion is broader than the endpoint-only error measure | [ ] Open |
| F11 | P2 | Calling torque a “rotational force” blurs two quantities with different units | [x] Resolved 2026-07-13 |
| F12 | P2 | Historical/source asides interrupt the main instructional path | [ ] Open |
| F13 | P3 | Equation (9)'s vector notation is nonstandard and its frame name changes from TR to RT | [ ] Open |
| F14 | P3 | Equation numbering jumps from (24) to (29), which looks like missing content | [x] Resolved 2026-07-13 |
| F15 | P3 | Several local prose, notation, accessibility, and dating issues remain | [ ] Open |
| F16 | P3 | Optional friction discussion sometimes overstates or weakly motivates approximations | [ ] Open |
| F17 | P2 | Angular variables are clockwise-positive while right-hand-rule moments are anticlockwise-positive | [ ] Deferred for a dedicated sign-convention pass |

Priority meanings:

- **P1:** address to meet the paper's correctness or intended-reader accessibility goal;
- **P2:** meaningful improvement to physical clarity, implementation flow, or claim precision;
- **P3:** editorial cleanup or optional-depth refinement.

## Detailed Findings

### F1 — Avoid dividing by `cos(theta)` in the main elimination

**Original location:** Former section 3.6 equations (20)-(24), before the 2026-07-13 renumbering.

Equations (20) and (22) are obtained by dividing by `cos(theta)`. The text correctly notes that these expressions are undefined for a horizontal pole. However, (21) is derived by equating (19) and (20), and (24) is derived by equating (22) and (23). As written, those derivations establish the final formulas only when `cos(theta)` is nonzero.

The final equations themselves are valid at `theta = +/-90 degrees`; this is a proof-domain issue, not an error in (21) or (24). It is avoidable because the original pair can be eliminated without dividing by `cos(theta)`.

With

```text
K = mr^2 + J
A = f + F_f + mr theta-dot^2 sin(theta)
B = mgr sin(theta) - M_f,
```

the friction-extended balances are

```text
m_tot x-double-dot + mr cos(theta) theta-double-dot = A
mr cos(theta) x-double-dot + K theta-double-dot = B.
```

Equation (23-F) follows from the second balance by dividing by `K`, which is positive for a physical pole. Substituting that expression directly into the first balance gives (24-F) without a division by `cos(theta)`. The alternative closed form (21-F) can likewise be obtained by substituting (19-F) into the second balance.

**Recommended change:** Make the nonsingular substitution the main derivation. Retain (20) and (22), if desired, only as optional algebraic rearrangements with their restricted domain stated. This would make the recommended route rigorous for the entire angle range and reduce the need for the warning after (24).

**Verification:** Evaluate the revised derivation at `theta = +/-90 degrees` and confirm it still produces finite acceleration values without an appeal to continuity.

**Outcome (2026-07-13):** Resolved. Section 3.6 now derives angular acceleration in terms of cart acceleration as (19), substitutes it directly into the horizontal balance, and derives cart acceleration as (20). The derivation divides only by `mr^2+J`, not by `cos(theta)`, and therefore includes horizontal-pole angles. The main paper no longer presents the two competing routes. Appendix C follows the same route; Appendix D derives the opposite elimination order once because Barto et al.'s pole equation is explicit in angular acceleration; Appendix E remains aligned with the cart-first form. Symbolic residual checks against both original balances reduced exactly to zero.

### F2 — Supply the missing moment mechanics

**Location:** Section 3.5, equations (13)-(16), and Appendices A-B.

The jump from the vector cross product in (13) to the long component expression assumes the two-dimensional rule

```text
(r cross F)_z = r_x F_y - r_y F_x.
```

Neither section 3.5 nor Appendix B currently states that rule. The reader also has to infer `r_x = r sin(theta)` and `r_y = r cos(theta)` at the point of substitution.

Equation (15) then gives the gravity moment directly, without applying the same rule:

```text
M_g = (r sin(theta))(-mg) - (r cos(theta))(0)
    = -mgr sin(theta).
```

That is a useful short derivation because it reinforces both the component rule and the sign convention. The text should also say that the pivot reaction is omitted from the moment balance because its line of action passes through P and therefore has zero lever arm.

Appendix A's statement `sum M = I alpha` “about a chosen axis” is too broad. That compact form applies about the centre of mass or a suitable fixed inertial axis; moments about an accelerating pivot require care. The main derivation is correct because it includes the centre-of-mass inertial force moment and the separate body inertial moment. Appendix A should say that explicitly rather than presenting the compact relation as universal for any chosen axis.

**Recommended change:** Add the 2D component identity, show the substitutions into (13), derive (15) in two lines, explain why the pivot reaction contributes no moment, and qualify the generic rotational statement in Appendix A.

**Outcome (2026-07-13):** Resolved. Section 3.5 now states $[\mathbf r\times\mathbf F]_z=r_xF_y-r_yF_x$, explains the physical meaning of its two products, identifies the radial-vector and inertial-force components before substitution, and includes the intermediate component line in (13)-(14). The gravity force is written as horizontal component zero and vertical component $-mg$, then substituted through the same rule to derive (15). The ideal pivot reaction is explicitly omitted because it is applied at P and therefore has zero moment about P; the later friction moment is distinguished from that reaction force. Appendix B repeats the planar component identity as part of its fuller cross-product explanation. Appendix A now limits the compact $\sum\mathbf M=J\boldsymbol\alpha$ form to planar moments about the centre of mass and explicitly includes both the centre-of-mass inertial-force moment and body inertial moment when balancing about accelerating pivot P. Symbolic residual checks for (14) and (15) reduced to zero, and the expanded derivation and appendices were rendered without overflow.

### F3 — Derive the friction-extended balances before listing four results

**Original location:** Former section 3.7.3 equations (19-F), (21-F), (23-F), and (24-F), before the 2026-07-13 rewrite.

The section says these are equations (19), (21), (23), and (24) “extended to incorporate friction terms”, then presents the results. It never displays the two balances from which the signs of `F_f` and `M_f` follow. This is where sign errors are most likely, especially because positive `theta` and positive moment use opposite rotational directions.

**Recommended change:** Begin with the two friction-extended balances:

```text
m_tot x-double-dot + mr cos(theta) theta-double-dot
    = f + F_f + mr theta-dot^2 sin(theta)

(mr^2 + J) theta-double-dot + mr cos(theta) x-double-dot
    = mgr sin(theta) - M_f.
```

Then derive the recommended pair (23-F) and (24-F) step by step using the nonsingular route from F1. Either derive (19-F)/(21-F) in a clearly secondary subsection or identify them as algebraically equivalent alternative results and show enough work to verify the friction signs.

**Outcome (2026-07-13):** Resolved. Section 3.7.3 now displays the two friction-extended balances, derives (34) from the moment balance, substitutes it into the horizontal balance, and expands and collects the algebra through (35). The unused alternative pair was removed. The section states the evaluation order explicitly, and symbolic substitution verified that (34)-(35) satisfy both balances.

### F4 — Expand the later “apply/substitute/repeat” transitions

**Locations:** Section 3.8 from (41) to (42)-(43); section 4.1; sections 4.2-4.3, equations (44)-(47); Appendix E, equation (E1).

Several later results are correct but not derived to the standard established in sections 3.1-3.6:

- The multiple-pole friction equations (42)-(43) follow after “include friction terms ... and repeat the same derivation steps”.
- The uniform-pole equations (44)-(47) follow after “apply the above substitutions”.
- The inertia identity jumps from `J=(1/12)mL^2` to `(1/3)mr^2` without showing `L=2r`, squaring `2r`, and cancelling `4/12`.
- Appendix E says that substituting Wieland's effective force and mass “and rearranging” gives (E1), but shows none of the substitution or collection.

These are reasonable compressions in a conventional technical paper, but conflict with this paper's stronger pedagogical goal.

**Recommended change:** Show one complete worked specialization for each new operation, then use a compact but explicit pattern for the remaining equations. At minimum:

1. expand `J=(1/12)m(2r)^2=(4/12)mr^2=(1/3)mr^2`;
2. show the denominator and numerator cancellations leading to (45), the uniform-pole cart equation;
3. show how `M_(f,i)` enters each pole balance and therefore the cart sum in (42); and
4. expand Wieland's `F-tilde` and `m-tilde` once before collecting the E1 numerator and denominator.

Equation (44) follows by substituting the uniform-pole inertia into the general relation; make the cancellation explicit rather than leaving it to “apply the above substitutions”.

**Outcome (2026-07-13):** Resolved. Section 3.8 now inserts $F_f$ and $M_{f,i}$ into the governing balances, derives the indexed angular-acceleration relation, substitutes it into the cart balance, expands the outer minus sign, and collects the terms through (42)-(43). Section 4.1 expands $L=2r$ through $J=\frac{1}{3}mr^2$ and then derives the total pivot inertia. Section 4.2 works the numerator and denominator factorizations and common-factor cancellation through (44)-(45); section 4.3 displays the indexed fraction cancellations through (46)-(47). Appendix E expands Wieland's effective force and mass in mapped notation before substituting them into (Wieland 9) to obtain (E1). Symbolic equivalence checks passed, and a rendered-page review found no clipping or MathJax layout failures.

### F5 — Put the recommended route before the alternative route

**Location:** Section 3.6.

The opening correctly says that (24) followed by (23) is recommended, but the reader must first traverse the “Angular Acceleration First (Alternative Route)” derivation and its singular intermediate (20). The main instructional path therefore begins with the route the paper does not recommend.

**Recommended change:** Present the cart-acceleration-first derivation first. Move the alternative route after it, reduce it to a clearly optional subsection, or move it to an appendix. If equation numbers are retained for historical stability, explain the resulting nonsequential presentation; otherwise renumber the section in one coordinated pass.

This change would also let section 3.6 introduce the nonsingular moment-relation-into-horizontal-balance derivation from F1 as the reader's first and only required path.

**Outcome (2026-07-13):** Resolved. Section 3.6 now contains only the recommended cart-acceleration-first route. The prose mentions that the simultaneous equations can be eliminated in the opposite order but sends the reader to Appendix D, where that form is required for the Barto comparison.

### F6 — Define the multiple-pole model and qualify the identical-length claim

**Location:** Section 3.8.

The equations assume `N` independent poles, each attached by its own pivot to the same cart. They are not the equations for a serial double pendulum or for poles connected to one another. The prose says only that another pendulum is “attached to the cart”, leaving the topology implicit.

The section also introduces pole indices without stating:

- that `i` runs from 1 through `N`;
- why all poles share the same cart acceleration but have separate angles and pivot moments.

The intended reader can be assumed to understand the summation operator. A summation primer or a two-term expansion such as
`sum_i m_i = m_1 + m_2` is therefore unnecessary; the paper only needs to define the index range used by its shorthand notation.

The claim that the two-pole task becomes “impossible” when the poles have identical lengths is also too absolute. With identical pole dynamics, the difference mode is uncontrollable and unstable, so arbitrary independent deviations cannot be stabilized by the one cart input. However, perfectly identical initial states remain on a symmetric trajectory and do not establish impossibility in every state. The intended controllability claim should be stated with that qualification, or omitted if it is not needed for the derivation.

**Recommended change:** Add a short physical description of the unjointed topology, state the index bounds, explain the shared cart coordinate and separate pole states, and qualify the controllability statement. Do not add a summation primer or two-pole expansion.

**Outcome (2026-07-13; figure added 2026-07-14):** Resolved. Section 3.8 now defines $N$ separate, unjointed poles attached directly to the same cart rather than to one another. It explains that each pivot coordinate is $x$ plus a fixed offset, so every pivot has acceleration $\ddot x$, while each pole has its own indexed state and parameters. Figure 2 shows the two-pole topology without repeating Figure 1's forces and coordinate frames. The section defines $i=1,\ldots,N$ with $\sum_i$ as shorthand for $\sum_{i=1}^{N}$; no summation tutorial or expanded two-pole example was added. The opening states the actual controllability limitation: dynamically identical equal-length poles have an uncontrollable near-upright difference mode, but identical angular states form a symmetric exception. This agrees with Wieland's qualified equal-length result on p.98 rather than claiming that every equal-length trajectory is impossible. Symbolic subtraction of the two linearized pole equations confirmed that $\ddot x$ cancels from the difference equation. The new SVG and its placement rendered without layout problems; the former numerical Figures 2 and 3 were renumbered Figures 3 and 4.

### F7 — Define reaction-force directions before using them

**Location:** Section 3.7.1 around (22), and section 3.7.2 around (28)-(30).

Equation (22) is correct, but it appears without the vertical dynamic-equilibrium line from which it follows. Displaying

```text
N_c - m_tot g + (F^i_G)_y = 0
```

would make the sign of the inertial term reproducible.

Equations (28)-(29) are consistent if `N_P` is defined as the force exerted by the pole on the cart at P:

```text
(N_P)_x = (F^i_G)_x
(N_P)_y = (F^i_G)_y - mg.
```

The reverse force exerted by the cart on the pole has the opposite components but the same magnitude. The current wording calls `N_P` a force “between” the bodies and does not select a direction, so a reader cannot tell why the signs are as displayed. Calling the entire pin reaction a “normal-force vector” is also potentially confusing; it is better described as the pivot reaction or bearing-load resultant, with the Coulomb moment explicitly identified as an effective bearing model.

**Recommended change:** Add the relevant free-body balance, define which body acts on which, note the equal-and-opposite alternative, and then take the magnitude.

### F8 — Write the pivot kinematic assumption explicitly

**Location:** Transition from section 3.1 to section 3.2.

Section 3.2 adds the cart acceleration `x-double-dot` to the relative acceleration of G. This uses the unstated relations

```text
x_P = x + constant,
x_P-double-dot = x-double-dot,
y_P = constant,
y_P-double-dot = 0.
```

They follow from P being rigidly fixed to a cart constrained to horizontal translation, but the equation should be stated before it is used. It would also make clear that any fixed horizontal or vertical offset between C and P does not affect the acceleration relation.

### F9 — Make section 5 a complete implementation handoff

**Location:** Sections 5.2 and 5.3.

The final equations are correct, but the section that readers are most likely to implement does not repeat the evaluation order next to them. A reader has to remember instructions from sections 3.6 and 3.8.

**Recommended change:** After (48)-(49), state “evaluate (48) first, then substitute its `x-double-dot` into (49)”. After (50)-(51), give the analogous two-step multiple-pole sequence. Also list the required current state and parameter values and state that all right-hand-side quantities come from the same instantaneous state.

It would be reassuring to show that the denominators cannot vanish for physical parameters. For the general single-pole equations,

```text
D = (mr^2+J)m_tot - (mr cos(theta))^2
  = mr^2[m_c + m sin^2(theta)] + J(m_c+m) > 0
```

when `m_c>0`, `m>0`, `r>0`, and `J>=0`. For the recommended uniform-pole equation, the denominator is at least `m_c + m/4`. This directly answers the concern raised by the singular intermediate equations in section 3.6.

The multiple-pole denominator is likewise positive for physical parameters because `(m_i r_i)^2/(m_i r_i^2+J_i) <= m_i`; a short bound can be supplied after (41), then restated for its uniform-pole specialization (46).

**Progress (2026-07-13):** The evaluation order is now stated immediately after both recommended pairs: evaluate (48) before (49), and evaluate the shared cart acceleration (50) before applying (51) to each pole. The required-state inventory and denominator proofs remain open, so this finding is only partly resolved.

### F10 — Keep numerical claims aligned with the measured error

**Location:** Section 6.2, especially the lead-in to table 5 and the interpretation after it.

Table 5 accurately describes its metric: pole-angle error at exactly 15 seconds. That single endpoint does not establish closeness of the entire trajectory; two paths can differ earlier and happen to agree at the final time. The surrounding prose asks when a method “deviate[s] significantly from the correct path” and recommends a convergence test based on whether “the result” changes, which is broader than the reported measurement.

**Recommended change:** Either narrow the prose consistently to final-angle accuracy, or add a trajectory-wide measure such as the maximum absolute angle error over sampled times, preferably together with cart position/velocity and pole angular velocity if full-state accuracy is the goal. “Numerically converged reference” should remain tied to a stated tolerance and metric.

The sentence saying Euler needs a timestep “approximately three orders of magnitude smaller” is imprecise for the displayed values: the factors are about 2,825 relative to RK2 and 7,163 relative to RK4, i.e. roughly three-and-a-half to nearly four orders of magnitude. “More than three orders of magnitude smaller” is accurate.

Also identify the RK2 variant as Heun's method, matching the accompanying implementation, rather than calling it only “the standard form”; several second-order Runge-Kutta schemes are standard.

### F11 — Prefer “moment” or “torque” over “rotational force”

**Location:** Table 1 and sections 3.5, 3.7.2, and 3.7.3.

A moment or torque is produced by a force and has units of N m; it is not itself a force, which has units of N. Calling it a “rotational force” may offer intuition, but it blurs a distinction the derivation later relies on for units and signs.

**Recommended change:** Use “moment (or torque)” consistently. If “rotational force” is retained as an informal analogy, state once that it is not a force in the dimensional sense.

**Outcome (2026-07-13):** Resolved. The main paper now uses “moment” as its primary term and introduces “torque” as the equivalent term. Every use of “rotational force” was removed. The first occurrence in table 1 defines a moment as the turning effect of a force about a point or axis. Section 3.5 then explains the concept with a door-hinge analogy, the magnitude $rF_\perp$, the zero-moment case for a force directed through the pivot, and the distinction between moment units (N m) and force units (N). Section 3.6 and the pivot-friction discussion use the same terminology. Appendix B retains the fuller conventional torque-vector treatment while connecting it to the paper's scalar moment convention.

### F12 — Move or label source-history detours in the main derivation

**Location:** The Cannon vector-notation block after (8), and the Cannon sign-convention block after (12).

Both blocks are relevant evidence, but neither is needed to follow the main derivation, and Appendix C already provides a detailed Cannon comparison. The first also introduces a new coordinate notation immediately before the paper returns to XY components. For a novice this creates another notation branch without a later payoff in the main route.

**Recommended change:** Move these blocks to Appendix C or label them conspicuously as optional source notes. Keep the main derivation focused on the notation it actually uses.

### F13 — Normalize equation (9)'s vector notation

**Location:** Section 3.2, equation (9) and its explanation.

Writing `1_x(x-double-dot)` makes a unit vector look like a function. The conventional form is scalar multiplication, for example

```text
a^G = x-double-dot 1_x + r theta-double-dot 1_T - r theta-dot^2 1_R.
```

The explanatory text also changes the name of the rotating frame from TR to RT.

**Recommended change:** If the optional block is retained, use conventional scalar-times-unit-vector notation and one consistent frame name.

### F14 — Resolve or explain the equation-number gap

**Original location:** Between former equations (24) and (29), before the 2026-07-13 renumbering.

The sequence skips (25)-(28). To a reader, this looks like missing equations or a publication error. Equations (34) and (40) are then displayed a second time in section 5, explicitly as repeats; that reuse is understandable, but it reinforces the need for a stated numbering policy.

**Recommended change:** Either renumber the main paper in one coordinated pass, or add an editorial note explaining that legacy numbers are intentionally preserved. If renumbering, update every prose reference and appendix comparison mechanically and verify the rendered tags.

**Outcome (2026-07-13):** Resolved. Removal of the unused alternative equations allowed the main equation tags to be renumbered continuously from (1) through (51), with the intentional repeated displays of adopted damping equations (26) and (32) retained in section 5. All prose and appendix references were updated and the rendered tags were checked.

### F15 — Apply a focused editorial and accessibility cleanup

**Locations and proposed corrections:**

- Section 3.2 says “RT coordinate frame” after defining TR.
- The paragraph before (25) begins “Noting that if ...”, which is a sentence fragment.
- Table 2 should state explicitly that `theta` is in radians, as Table 1 does.
- The prose after table 4 calls cart position and velocities “model parameters”; they are initial state variables.
- Figure 2's alternative text says it shows cart and pole trajectories, but the plot shows pole angle only.
- Figure 3's alternative text says “at several timestep sizes”, but the plot compares two methods at one timestep, 0.001 seconds.
- The signature says January 4, 2020 despite extensive 2026 revision work. Preserve the original date if desired, but add a visible revision date so readers can identify the version.
- Use one style for `theta-dot_i^2` throughout; both `dot(theta)^2_i` and `dot(theta_i)^2` forms currently appear.
- “Directionless magnitude” is understandable but slightly awkward; “positive magnitude” is more precise for `g`.

### F16 — Tighten optional friction modelling prose

**Location:** Sections 3.7.1-3.7.2.

Several optional passages are more speculative than the core derivation:

- The cart-only Coulomb magnitude in (25) is presented as potentially preferable during high-speed swinging, but pole motion can either increase or decrease the track reaction depending on angle and motion; `m_c g` is an ad hoc constant, not a generally improved high-speed approximation.
- The example `mg/2` pivot load in (31) is explicitly only a guess. It may be better presented as a tunable effective load rather than “the next best option”.
- The hundreds-or-thousands-of-rpm motor-engine comparison is remote from cart-pole operation and does not help choose `b_p`.
- Saying pivot friction may be ignored “so long as there is friction somewhere in the model” is not generally sufficient: cart and pivot damping dissipate different generalized motions, even though the motions are coupled.
- The discussion of Coulomb discontinuities says they impede “any numerical method”. Event-aware and nonsmooth methods can handle them; the practical point is that simple fixed-step smooth ODE integrators require extra care.

**Recommended change:** Retain the model alternatives, but label empirical constants as calibration choices, remove claims of general superiority, and keep the adopted linear-damping rationale tied to the intended simulation method.

### F17 — Reconsider the angular sign convention

**Location:** Figure 1 and table 1; the kinematic and dynamic derivations in sections 3.1-5.3; the source comparisons in Appendices C-E; and the supporting C# implementation and numerical artefacts.

The paper currently uses two different positive rotational directions:

- moment vectors follow the conventional right-hand rule, so a positive z-axis moment is anticlockwise; but
- $\theta$, $\dot\theta$, and $\ddot\theta$ are defined as positive clockwise.

This is mathematically consistent, and the right-hand-rule convention for moments is standard. The cognitive mismatch comes from the clockwise-positive angular coordinate: a physical anticlockwise angular acceleration has a positive z component but a negative scalar value of $\ddot\theta$ in the paper. Table 1 and Appendix B now make that deliberate difference explicit, but aligning the signs may ultimately be clearer for the intended reader.

The clockwise convention may have been inherited from Cannon, but that provenance has not yet been verified from the source record and should not be asserted publicly without checking it. Source compatibility is relevant, but source notation can instead be mapped explicitly in Appendix C if the main paper adopts a different convention.

**Provisional preferred direction:** Retain the standard right-hand rule for moment vectors and consider redefining $\theta$, $\dot\theta$, and $\ddot\theta$ as positive anticlockwise, so all rotational quantities share the same positive direction.

This is not a local wording change. It requires a dedicated rederivation and verification pass covering the geometry, rotating-frame directions, all differentiated kinematics, gravity and friction signs, single- and multiple-pole equations, source mappings, Figure 1, supporting code, initial conditions, plots, and numerical results. The transformed variables provide a useful equivalence check:

```text
theta_new            = -theta_old
theta-dot_new        = -theta-dot_old
theta-double-dot_new = -theta-double-dot_old.
```

Under this transformation, sine terms change sign while cosine terms do not. Every revised equation should reproduce the same physical motion after transforming the angular state, rather than being accepted from sign inspection alone.

**Recommended change:** Defer this choice until it can be handled as one coordinated sign-convention pass. At that point, verify the Cannon provenance, decide the main-paper convention explicitly, rederive rather than mechanically relabel the equations, regenerate affected numerical artefacts, and record the complete equivalence checks here.

## Suggested Work Order

Work in small passes so that mathematical and editorial changes remain reviewable:

1. **Mechanics foundations:** F7 and F8.
2. **Implementation and numerical claims:** the remainder of F9 and F10.
3. **Optional depth and notation:** F12, F13, and F16.
4. **Editorial integrity:** F15.
5. **Dedicated coordinate-convention decision:** F17, when a full rederivation and artefact-regeneration pass is desired.

For each pass:

- preserve the stated coordinate and sign conventions unless undertaking the dedicated F17 pass, in which case transform them systematically;
- check each changed equality symbolically or by substitution;
- test upright, horizontal, and downward pole configurations;
- check dimensions of every added force and moment term;
- verify all equation references after any numbering change;
- render the changed MathJax at desktop and narrow widths; and
- keep the supporting C# equations aligned with (48)-(51).

## Completion Criteria

This review can be closed when:

- every P1 finding is resolved or deliberately declined with rationale;
- the recommended single- and multiple-pole routes can be followed without using an expression that is undefined at a valid pole angle;
- every mechanics or vector rule beyond the assumed basic algebra, calculus, and trigonometry is introduced before use;
- friction, uniform-pole, and multiple-pole results show enough intermediate work to reproduce their signs and coefficients;
- section 5 is sufficient on its own as an implementation handoff after the reader has accepted the derivation; and
- all remaining P2/P3 decisions and verification results are recorded here.
