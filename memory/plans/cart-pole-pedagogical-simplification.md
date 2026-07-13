# Cart-Pole Pedagogical Simplification Plan

Created: 2026-07-12

## Goal

Improve the clarity and pacing of `public/research/cart-pole/cart-pole-equations.html` while preserving its defining purpose: a detailed derivation that a reader with basic algebra, but no postgraduate mathematics or physics background, can follow.

This is not a plan to make the paper conventionally terse. It is a plan to reduce avoidable repetition, distinguish the main learning path from optional depth, and make each displayed step look intentional and complete.

## Working Method

- Work through one pass at a time.
- Before each pass, inspect the relevant passage in its current form and agree the intended scope.
- Make only the changes belonging to that pass.
- Review the rendered prose and mathematics before marking the pass complete.
- Do not silently combine later passes with the active pass.
- Preserve all mathematical, notation, sign, inertia, and source-boundary invariants recorded in `../cart-pole-equations.md`.
- Prefer small reductions and clearer hierarchy over wholesale deletion.

## Improvement Passes

### 1. Complete or remove visually unfinished algebra lines

Status: Completed 2026-07-12

Review lines such as

```text
(m_c+m) x-double-dot =
```

and

```text
(mr^2+J) theta-double-dot =
```

which currently end with an empty right-hand side before the following division step.

Possible direction:

- Supply the right-hand expression, when repeating it improves continuity; or
- remove the incomplete intermediate line when it adds no explanatory value.

Success criteria:

- No displayed algebra line looks accidentally truncated.
- Every equality remains mathematically valid.
- Equation numbering and subsequent references remain unchanged unless a change is explicitly agreed.

Outcome:

- Completed all four empty right-hand sides rather than removing their lines.
- Each line now explicitly shows the result after factoring the acceleration term; the following line performs only the division.
- No equation numbers or references changed.

### 2. Consolidate repeated inertia explanations

Status: Completed 2026-07-12

Review the repeated explanations that:

- `mr^2` represents centre-of-mass motion about the pivot;
- `J` is body inertia about the centre of mass; and
- `mr^2+J` is total pivot inertia.

Possible direction:

- Retain the full explanation where the distinction is first derived.
- Retain short reminders at later points where they prevent a genuine misunderstanding.
- Remove or combine passages that merely repeat the conclusion without serving a new purpose.

Success criteria:

- A novice can still identify the meaning of all three inertia expressions.
- The important distinction is emphasized, not diluted.
- Later sections do not repeatedly re-explain the same point at full length.

Outcome:

- Retained the full first explanation beside equation (14), where the `mr^2` contribution is derived.
- Tightened the introduction and definition of the additional `J` term without removing the point-mass distinction or total-pivot-inertia result.
- Replaced full restatements in the section 3.6 and section 4 transitions with concise reminders of why `J` remains general and what the specialization does.
- Retained context-specific explanations for multiple poles, the uniform-pole calculation, the conclusion, and source comparisons.

### 3. Establish one primary single-pole solution route

Status: Completed 2026-07-12

Review section 3.6, which currently develops both acceleration-first routes and both explicit closed forms.

Possible direction:

- Present equations (24) followed by (23) as the primary evaluation route: calculate cart acceleration first, then pole angular acceleration.
- Clearly label the route through equations (21) and (19) as an alternative derivation or alternative evaluation order.
- Consider whether the intermediate expressions (20) and (22), which divide by `cos(theta)`, belong in the main flow, a subordinate note, or an appendix.

Success criteria:

- A first-time reader can immediately tell which route to follow.
- Algebraic completeness and the alternative route remain available.
- The warning about the horizontal-pole singularity of (20) and (22) remains explicit.
- Recommended implementation equations are unchanged unless separately justified.

Outcome:

- Added an upfront map of the two solution routes without moving or renumbering the derivations.
- Labelled equations (24) followed by (23) as the recommended cart-acceleration-first route.
- Labelled equations (21) followed by (19) as the alternative angular-acceleration-first route.
- Identified equations (20) and (22) as intermediate elimination expressions and retained the explicit warning that they divide by `cos(theta)`.
- Reworked the closing procedure box to lead with the recommended two-step sequence while retaining both valid alternatives.

### 4. Tighten repeated explanatory prose around kinematics and moments

Status: Completed 2026-07-12

Review sections 3.1-3.5 for nearby paragraphs that repeat an explanation already made by the equations or by an immediately preceding paragraph.

Possible direction:

- Preserve the explicit chain-rule, product-rule, centripetal, tangential, torque, and physical sign explanations.
- Combine only adjacent statements that make the same point.
- Retain physical sanity checks at upright, horizontal, clockwise, and rightward-force configurations.

Success criteria:

- The foundational derivation remains fully accessible to a basic-algebra reader.
- No prerequisite mathematical step becomes implicit.
- Physical sign and limiting-case checks remain prominent.

Outcome:

- Combined the adjacent introduction to the centre-of-mass coordinates and the adjacent interpretation of equations (5)-(6).
- Tightened the centripetal-component check without removing the vertical and horizontal limiting cases.
- Combined the moment/torque introduction and shortened the cross-product explanation while preserving operand order, the right-hand rule, and the upright-pole sign check.
- Rephrased the gravity sign check as one direct physical test.
- Retained every explicit differentiation step, the centripetal/tangential distinction, and all substantive physical checks.

### 5. Separate the adopted friction model from optional alternatives

Status: Completed 2026-07-12

Review section 3.7's treatment of full Coulomb friction, constant-normal approximations, wheel-bearing damping, pivot Coulomb friction, viscous damping, and combined models.

Possible direction:

- Preserve the derivation of the physically detailed Coulomb model and explain why it creates simultaneous coupling or discontinuities.
- Make the linear damping model adopted in section 5 visually and narratively prominent.
- Group secondary approximations in a clearly labelled optional-model subsection or box.

Success criteria:

- Dimensionless Coulomb coefficients and dimensional damping coefficients remain strictly distinguished.
- All force and moment signs and units remain correct.
- A reader can readily distinguish the recommended simulation model from alternatives.
- Useful modelling context is reorganized rather than indiscriminately removed.

Outcome:

- Added a route map identifying equations (34) and (40) as the adopted models before the detailed friction survey begins.
- Organized both cart-track and pivot friction into labelled Coulomb, optional approximation, adopted linear damping, and optional combined-model passages.
- Moved the wheel-bearing equation and explanation beside the adopted cart linear-damping discussion so that it no longer interrupts the Coulomb derivation.
- Relabelled the moved wheel-bearing equation from (29-R) to (34-R), reflecting that it is a rotational interpretation of linear damping rather than a variant of Coulomb equation (29); the old label had no references.
- Clarified that the generic friction-extended equations accept any stated model, while section 5 substitutes the adopted linear models.
- Preserved every friction equation, sign, unit, normal-force dependency, and modelling caveat.

### 6. Compress routine algebra in the multiple-pole extension

Status: Completed 2026-07-12

Review the substitution and collection of terms leading from equations (45) and (46) to equation (47).

Possible direction:

- Retain the conceptual extension from one pole to a sum over `N` poles.
- Retain enough intermediate algebra to show how angular accelerations are eliminated.
- Replace repeated routine collection steps with a short explanation only where the same operation was already demonstrated in the single-pole derivation.

Success criteria:

- A basic-algebra reader can still reproduce equation (47).
- The summation notation and indexed quantities remain clearly introduced.
- Equations (47)-(49) remain algebraically equivalent to the individual pole balances.

Outcome:

- Retained the direct substitution of equation (46), the expanded sums, the definition of `m_{tot}`, and the factored equation immediately before (47).
- Removed the very long intermediate line that displayed the `x`-acceleration summation moved to the left but not yet factored.
- Replaced that repeated algebraic state with a plain-language instruction to move the term, factor out `x` acceleration, and divide by its coefficient.
- Preserved equations (42)-(49), their numbering, and the complete conceptual extension from one pole to `N` poles.

### 7. Clarify the optional status and purpose of the appendices

Status: Completed 2026-07-12

Review the transition from the completed main derivation to Appendices A-E.

Possible direction:

- State clearly which appendices provide introductory background and which provide historical source comparisons.
- Tell readers that Appendices C-E are not required to implement the recommended equations.
- Preserve source-faithful notation and all explicit mappings.

Success criteria:

- Novice readers know when the main instructional route is complete.
- Readers interested in verification and historical comparison can still find the full evidence.
- No source equation, quotation, mapping, or comparison is altered merely for brevity.

Outcome:

- Marked the conclusion as the end of the main derivation and simulation guidance.
- Identified Appendices A-B as optional introductory background on d'Alembert's principle and torque.
- Identified Appendices C-E as optional historical verification that is not required to implement the recommended equations.
- Preserved every appendix equation, quotation, notation mapping, and comparison unchanged.

### 8. Final whole-paper pacing and accessibility review

Status: Completed 2026-07-12

Perform only after passes 1-7 have been individually considered and either completed or deliberately declined.

Review for:

- a clear main path through the derivation;
- transitions between new ideas and routine algebra;
- unexplained notation or skipped operations;
- accidental repetition introduced by earlier passes;
- equation-reference integrity;
- consistent use of “paper”, symbols, signs, units, and physical-model assumptions.

Success criteria:

- The paper remains intentionally detailed rather than conventionally terse.
- A reader with basic algebra can follow the primary path without consulting an external mechanics text.
- Optional depth is clearly signposted.
- All mathematical forms remain equivalent to the independently reviewed equations.

Outcome:

- Confirmed that the main derivation path, adopted friction models, optional alternatives, multiple-pole extension, and appendix boundary are clearly signposted.
- Standardized beginner-facing prose where compressed wording such as `w.r.t.`, `I.e. e.g.`, inconsistent pole ordinals, and `+-180°` impaired readability.
- Added descriptive alternative text to every content image and licence image.
- Corrected an unmatched paragraph in Appendix A and an unclosed table cell and row in reference [2]; paired structural tag counts now agree.
- Confirmed that every numeric equation reference resolves to a displayed equation tag, all five images have alternative text, and no obsolete `m_t` or `m_{Sigma}` shorthand remains in the paper.
- Re-ran symbolic checks of the single-pole balances, positive-denominator identity, and uniform-pole specialization; all passed.
- Retained the intentionally detailed differentiation, force, moment, friction, and source-comparison derivations.

## Verification for Every Pass

For each implemented pass:

1. Inspect the focused HTML diff while ignoring line-ending noise.
2. Check every changed equality and symbol locally.
3. Search for equation references affected by moved or removed material.
4. Render or otherwise inspect MathJax/HTML presentation when display structure changes.
5. Update this plan's status and record any deliberate decision to retain existing detail.
6. Update `../cart-pole-equations.md` if the completed pass establishes durable editing guidance.

## Status

All eight passes are complete. The next useful checkpoint is a Git review and commit of passes 4-8 and the unified `m_{\mathrm{tot}}` notation change.
