# Cart-Pole Derivation Follow-Up Review

- Reviewed: 2026-07-20
- Reviewed page: `public/research/cart-pole/cart-pole-equations.html`
- Status: Open
- Preceding review: `cart-pole-derivation-review.md` (F1-F17 complete)

## Review Goal

Review the current sequence, correctness, and pedagogical completeness of the cart-pole derivation for a reader who understands basic algebra, calculus, and trigonometry but should not have to supply missing mechanics knowledge or substantial intermediate algebra.

This follow-up records issues found after completion of the earlier F1-F17 review. It does not supersede that review or reopen its resolved findings.

## Priority Meanings

- **P1:** Must address to meet the stated correctness or intended-reader accessibility requirement.
- **P2:** Important improvement to clarity, sequencing, or conceptual understanding.
- **P3:** Minor editorial or notation improvement; useful but not essential.

## Overall Assessment

The displayed kinematics, force and moment balances, elimination algebra, multiple-pole extension, uniform-pole specialization, and recommended equations are mathematically consistent with the stated model and sign conventions. No incorrect final equation was found in equations (1)-(51).

The review found one conditional correctness problem in the instructions for evaluating the friction equations. It also found that the main sequence still expects the intended reader to retrieve essential mechanics from a later appendix, imports the uniform-rod inertia formula without derivation, and gives optional friction models more prominence than the shortest route to the recommended equations.

## Verification Performed

- Independently reconstructed the coupled single-pole balances:

  ```text
  m_tot x-double-dot + mr cos(theta) theta-double-dot
      = f + F_f + mr theta-dot^2 sin(theta)

  mr cos(theta) x-double-dot + (mr^2+J) theta-double-dot
      = mgr sin(theta) - M_f.
  ```

- Symbolically verified the general friction equations (34)-(35) and uniform-pole equations (44)-(45); their residuals reduce to zero.
- Verified that the frictionless equations conserve mechanical energy, providing an independent check on their force and gravity signs.
- Traced the indexed substitutions in the multiple-pole equations and found their terms and signs consistent with the single-pole balances.
- Checked the algebra in Appendices C-E beginning from the source equations displayed in the page. The external publications were not independently re-read during this follow-up.

## Findings Index

| ID | Priority | Finding | Status |
| --- | --- | --- | --- |
| F18 | P1 | Restrict or qualify the explicit evaluation order for acceleration-dependent friction models | Resolved 2026-07-20 |
| F19 | P1 | Teach the essential d'Alembert mechanics before using it | Resolved 2026-07-20 |
| F20 | P1 | Derive the uniform-pole body inertia rather than importing it silently | Resolved 2026-07-21 |
| F21 | P2 | Move optional friction detail out of the main derivation path | Open |
| F22 | P2 | Label equation (12) consistently as a combined-system balance | Open |
| F23 | P3 | Define the sign function at first use | Open |

## Detailed Findings

### F18 - Restrict or qualify the explicit evaluation order for acceleration-dependent friction models

**Priority:** P1
**Location:** Sections 3.7.1-3.7.3, especially equations (23), (27), (30), (33), (34), and (35); the same qualification applies when generic friction terms are carried into the multiple-pole equations.

Section 3.7.3 says that `F_f` and `M_f` may be supplied by any of the preceding models, then instructs the reader to evaluate (35) for cart acceleration and substitute the result into (34). That is an explicit evaluation sequence only when the friction terms can be calculated from the current state without already knowing the accelerations.

The sequence works for the adopted linear damping models (26) and (32), the constant-load Coulomb approximations (24), (25), and (31), and the wheel-bearing model (26-R). It does not directly work for:

- equation (23), because `F_f` contains `theta-double-dot` through the dynamic track reaction;
- equations (27) and (30), because `M_f` depends on a pivot-reaction magnitude containing both accelerations; or
- equation (33), because its Coulomb component has the same acceleration dependence.

With those models, equations (34)-(35) remain valid algebraic relations, but they form an implicit, generally nonlinear and piecewise system that must be solved simultaneously with the friction definitions. The page already recognizes this coupling for equation (23), but does not carry the same qualification into the general evaluation instruction or apply it to the pivot-reaction model.

**Recommended change:** Restrict the simple evaluation sequence explicitly to acceleration-independent friction models. For the exact reaction-dependent models, state that the accelerations and friction loads must be solved together and that zero-velocity regime handling may also be required.

**Success criteria:**

- No statement implies that every friction model can be inserted into (35) and evaluated explicitly before (34).
- The adopted linear-damping path remains a simple cart-acceleration-first calculation.
- Both cart and pivot reaction-dependent Coulomb models are identified as implicit.

**Outcome (2026-07-20):** Resolved. Section 3.7.2 now states immediately after equation (30) that its pivot-reaction magnitude contains both unknown accelerations, so substituting it into (27) makes the Coulomb pivot moment implicit; the description of combined model (33) states that it inherits the same coupling. Section 3.7.3 now distinguishes algebraic compatibility with all listed friction models from explicit sequential evaluation. It limits the cart-first procedure to friction loads calculable from the current state and parameters, identifies the adopted linear models (26) and (32) as belonging to that class, and states that equations (23), (27), and (33) instead require a simultaneous, generally nonlinear and piecewise solve with the acceleration equations and applicable regime conditions. Section 3.8 applies the same qualification to equations (42)-(43) for multiple poles.

No equation was changed. Main equation numbering remains (1)-(51), including the deliberate repetitions of (26) and (32) in section 5; all 63 `align` environments and all paragraph tags remain balanced. A headless Chrome render produced 496 MathJax containers and no `mjx-merror` element.

### F19 - Teach the essential d'Alembert mechanics before using it

**Priority:** P1
**Location:** Sections 3.3-3.5 and Appendix A.

Section 3.3 adopts d'Alembert's principle, constructs the inertial-force components, and then uses them in the combined-system and moment balances. The explanation that an inertial force is the bookkeeping term `-m a`, that internal interaction forces cancel in a whole-system balance, and that an extended rigid body also contributes the inertial couple `-J alpha` does not appear in full until Appendix A after the conclusion.

A reader can follow the appendix link and return, but the linear reading sequence uses essential mechanics before teaching it. Calling Appendices A-B optional reinforces the impression that this material is supplementary even though the stated non-mechanics reader needs its central ideas.

The phrase "the pendulum or pole's mass at point G resists acceleration" should also be tightened. For a physical extended pole, its mass is distributed; `-m a_G` is the resultant translational inertial force represented at the centre of mass, not evidence that all mass is physically located there.

**Recommended change:** Move or summarize the indispensable parts of Appendix A immediately before equation (10): Newton's second law in dynamic-equilibrium form, the bookkeeping status of the inertial force, cancellation of internal pivot forces for the combined system, and the separate resultant force and inertial couple for a rigid body. The fuller appendix may remain for optional depth.

**Success criteria:**

- A first-time reader understands why `-m a_G` is introduced before seeing equations (10)-(11).
- The reader can distinguish an inertial bookkeeping force from a physical applied or contact force.
- The use of both `-m a_G` and `-J alpha` for an extended body is motivated before the moment balance.
- No essential premise is available only after the main derivation has ended.

**Outcome (2026-07-20):** Resolved with a navigation-and-signposting compromise that preserves the derivation's flow and keeps Appendix A intact. Section 3.3 now states the d'Alembert rule needed immediately: the inertial bookkeeping force is $\mathbf F^i_G=-m\mathbf a_G$, it is included so that the ordinary applied and inertial forces can be written as a zero sum, and it is not an additional physical force. Readers unfamiliar with the construction are explicitly directed to read Appendix A before continuing, where the translational and rotational forms used in sections 3.3-3.5 are explained. The following paragraph now describes $\mathbf F^i_G$ as the resultant translational inertial force represented at the centre of mass, avoiding the former implication that an extended pole's distributed mass is physically concentrated at G. The conclusion calls Appendices A-B supporting background and qualifies Appendix A as optional only for readers already familiar with the construction.

No Appendix A material or derivation section was relocated. Main equation numbering remains unchanged; all 63 `align` environments and all paragraph tags remain balanced, the section 3.3 link has exactly one Appendix A target, and a headless Chrome render produced no `mjx-merror` element.

### F20 - Derive the uniform-pole body inertia rather than importing it silently

**Priority:** P1
**Location:** Section 4.1.

Section 4.1 begins with the standard result

```text
J = (1/12)mL^2
```

and then carefully derives its forms in the paper's `r` notation. The conversion algebra is explicit, but the starting formula itself is mechanics knowledge not included in the assumed prerequisites.

**Recommended change:** Introduce moment of inertia as the mass-weighted squared distance from the rotation axis and derive the uniform-rod result using the reader's assumed calculus:

```text
lambda = m/L
dm = lambda ds
J = integral from -L/2 to L/2 of s^2 dm
  = (m/L) integral from -L/2 to L/2 of s^2 ds
  = (1/12)mL^2.
```

Then retain the existing substitution `L=2r` and addition of the `mr^2` offset contribution.

**Success criteria:**

- The reader is given an operational definition of body moment of inertia.
- The factor `1/12` is derived rather than asserted.
- The existing distinction between body inertia `J` and total pivot inertia `mr^2+J` remains explicit.

**Outcome (2026-07-21):** Resolved with a compact derivation in section 4.1. The uniform pole is now identified more precisely as a slender rod with mass distributed evenly along its length. A coordinate $s$ is measured from centre of mass G and a short element is assigned mass $dm=(m/L)ds$. The squared distance is explained without introducing further moment equations: a piece twice as far from G needs twice the tangential force for the same angular acceleration; because that force is perpendicular to the pole, its moment about G is force multiplied by distance, so the doubled moment arm supplies a second factor of two. Its rotational-inertia contribution is therefore $2\times2=2^2$ times as large. The displayed calculation then gives the mass-to-length substitution $\int s^2\,dm=\int_{-L/2}^{L/2}s^2(m/L)\,ds$ its own line and evaluates the endpoint terms explicitly to obtain $J=\frac{1}{12}mL^2$. The existing substitution $L=2r$ then gives $J=\frac{1}{3}mr^2$, after which the paper retains its separate derivation of total pivot inertia $mr^2+J=\frac{4}{3}mr^2$.

The integral coefficient was independently checked as $1/12$. Main equation numbering remains unchanged; the page now has 64 balanced `align` environments and 261 balanced paragraph pairs, and a headless Chrome render produced no `mjx-merror` element.

### F21 - Move optional friction detail out of the main derivation path

**Priority:** P2
**Location:** Section 3.7 relative to sections 3.8, 4, and 5.

After the frictionless single-pole equations are solved, the paper presents the full Coulomb cart model, dynamic track reaction, two constant-load approximations, linear damping, a wheel-bearing interpretation, the pivot reaction, Coulomb pivot friction, another constant-load approximation, linear pivot damping, and a combined model. Only equations (26) and (32) are adopted in the recommended equations.

This is valuable modelling material, but its position interrupts the shortest path from the core balances to the multiple-pole extension, uniform-pole specialization, and final simulation equations.

**Recommended change:** Keep a short main-path section that introduces generic signed `F_f` and `M_f`, derives their positions in the balances, and identifies the adopted linear models. Move the detailed optional Coulomb, reaction-load, bearing, and combined alternatives to a later optional section or appendix.

A clearer instructional sequence would be:

1. model, signs, and essential mechanics;
2. centre-of-mass kinematics;
3. frictionless single-pole balances and explicit solution;
4. multiple-pole extension;
5. uniform-pole specialization;
6. adopted damping and recommended equations;
7. optional friction models; and
8. historical source comparisons.

**Success criteria:**

- A reader can reach equations (48)-(51) without traversing friction models that are not used there.
- The optional models and their physical caveats are preserved.
- Generic friction signs are still derived rather than merely asserted.

### F22 - Label equation (12) consistently as a combined-system balance

**Priority:** P2
**Location:** Section 3.4 heading and the opening of section 3.6.

Section 3.4 is titled "Forces on the Cart", and section 3.6 later calls equation (12) the horizontal-force balance "for the cart". The derivation itself correctly describes and uses equation (12) as the horizontal dynamic-equilibrium balance for the combined cart-pole system. That is why the pole mass and pole inertial term appear while the internal pivot interaction cancels.

For a novice reader, the cart-only label conflicts with the actual system boundary and obscures the reason the pivot force is absent.

**Recommended change:** Rename the section to "Horizontal Balance for the Combined Cart-Pole System" and use the same description when equation (12) is recalled later.

**Success criteria:**

- Every description of equation (12) identifies the same system boundary.
- The cancellation of the internal pivot interaction follows naturally from that boundary.

### F23 - Define the sign function at first use

**Priority:** P3
**Location:** Equation (21) and the opening of section 3.7.1.

The symbol `sgn` first appears in the Coulomb cart-friction equation without definition. It is not guaranteed to be familiar to a reader whose stated prerequisites are basic algebra, calculus, and trigonometry. Its value at zero also matters because static friction is explicitly omitted.

**Recommended change:** Define

```text
sgn(v) = +1 for v > 0
       =  0 for v = 0
       = -1 for v < 0
```

and explain that the zero value belongs to this kinetic-friction-only mathematical model; it does not model the range of forces available during sticking.

**Success criteria:**

- Every value produced by `sgn` is defined before it is used.
- The definition is consistent with the paper's decision not to model static friction.

## Suggested Resolution Order

1. F22 - correct the combined-system heading and references.
2. F23 - define `sgn` at first use.
3. F21 - reorganize the optional friction material after the local content is stable.

After each change, verify equation references, MathJax rendering, print layout, and consistency with the mathematical and notation invariants in `../cart-pole-equations.md`.
