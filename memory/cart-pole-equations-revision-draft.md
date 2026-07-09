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
- The existing SVG diagram still labels the old ideal point mass as `Q`. Update the SVG later so the diagram labels the pole centre of mass as `G`, or otherwise clearly maps old `Q` to revised `G`.

Next derivation step:

Continue with publication-readiness checks for the numerical integration figures and final replacement workflow. The main derivation, friction extensions, multiple-pole equations, hybrid model, recommended equations, and appendices C-E have been revised to use `r`/`\alpha` and the corrected `4/3` inertia interpretation. Figure 1's SVG labels now use `G` and `r`.

## Source-First Notation Audit

User direction: choose variables/constants after checking the reference papers directly, aiming to align with the sources as closely as possible without inheriting errors from the original article.

Current source status:

- Barto, Sutton, Anderson 1983: downloaded primary PDF to `memory/tmp/references/barto-sutton-anderson-83.pdf`. It is scanned; `pdftotext` produced no text. Rendered pages to `memory/tmp/references/barto_pages/`. Visual inspection of the appendix confirms their cart-pole model uses `l = 0.5 m, half-pole length`, `m_c = 1.0 kg`, `m = 0.1 kg`, `g = -9.8 m/s^2`, `\mu_c`, `\mu_p`, and equations with the familiar `4/3` denominator factor.
- Cannon 1967: user supplied photo `/mnt/c/Users/colin/Downloads/PXL_20260708_230436018.jpg` of p.705, Sec. 22.4 "Unstable Mechanical System: Stick Balancer". This directly shows equations (22.50)-(22.56), including (22.53)-(22.55). It verifies that Cannon defines `J` as the moment of inertia of the stick about its mass center, with `J = ml^2/3` for a stick of length `2l`, and equation (22.55) uses `J\ddot\theta + ml(\ddot x\cos\theta + l\ddot\theta) - mgl\sin\theta = 0`. Therefore Cannon's `J` is additional/body inertia about the center of mass, not total pivot inertia.
- Cannon sign-error claims should be treated as unverified until a dedicated sign-convention audit is completed. The public draft should not claim Cannon has sign errors merely from the old article's annotations.
- Wieland 1991: Google Books metadata/preview downloaded to `memory/tmp/references/wieland-googlebooks.html`; direct search/page probe did not expose the relevant equations. Primary equations not yet independently verified.

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
| 2026-07-09 | Appendix C | Figure/data verification | Expanded the Cannon appendix to state Cannon's own nonlinear equations and small-angle equations using source-style tags `(Cannon 22.55)` and `(Cannon 22.56)`, then mapped them to the article's uniform-pole equations with local C-numbered derivations. The appendix now explicitly shows Cannon's pivot inertia as `J + ml^2 = 4/3 ml^2`. |
| 2026-07-10 | Figure 1 / Appendix C | Figure/data verification | Clarified that Figure 1 is a centre-of-mass schematic: for an ideal point-mass pendulum G is the bob at the end of a massless rod, while for a uniform pole G is halfway along the physical pole and the full pole length is twice PG. Added matching Appendix C wording to distinguish Cannon's uniform stick of length `2l` from an ideal point-mass pendulum. |

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

Avoid saying "substitute `k = 4/3` into `J = kml^2`", because that reintroduces the ambiguity. If a constant is useful, define it for the total pivot factor instead:

```tex
mr^2 + J = \alpha mr^2
```

Then:

```tex
\alpha = 1
```

for a point mass, and:

```tex
\alpha = \frac{4}{3}
```

for the uniform-pole hybrid model.

### 7. Revisit appendices

Appendix C should be revisited carefully. The previous claim that Cannon's `4/3` should be corrected to `7/3` appears to follow from treating Cannon's `J` as total pivot inertia rather than additional body inertia.

Appendix statements about Barto et al. and Wieland inheriting a problematic inertia term should also be rechecked under this corrected interpretation.

## Open Questions

- Should the rewrite preserve Cannon's sign conventions exactly, or recast the derivation in a cleaner modern convention and then map back to Cannon?
- Should the revised article use `r` for center-of-mass distance throughout, reserving `l` for full pole length and `\hat l` for half length?
- Should the article explicitly derive the rigid-body inertia of a uniform rod about its center of mass, or cite it and explain only how the parallel-axis split works?
- Should the final recommended equations support both point-mass and hybrid models via a parameter `\alpha`, or present separate final equations for each?
- The section 6 figures and table were not regenerated after changing the recommended equations from the old `7/3` factors to `4/3`. There are no local simulation scripts or data files next to the PNGs, so verify/regenerate those results before publication if exact numeric reproducibility matters.

## Working Principle For The Revision

Every inertia term should answer one of two questions:

1. Is this resistance from the mass center moving around the pivot?
2. Is this extra resistance from an extended body rotating about its own center of mass?

If the answer is both, the derivation is probably double-counting.
