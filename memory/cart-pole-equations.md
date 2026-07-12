# Cart-Pole Equations Article

Last updated: 2026-07-12

## Current Status

- Canonical article: `public/research/cart-pole/cart-pole-equations.html`.
- The completed revision was promoted to the canonical filename on 2026-07-12; no separate staging HTML remains.
- Mathematical, notation, source-comparison, prose, Figure 1, and numerical-artifact reviews are complete.
- Supporting C# repository: `/mnt/d/home/projects/code/cartpole-physics/main`.
- The corrected C# solution builds with zero warnings and errors, and its analysis results match the article.

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
- Used `m_t=m_c+m` for single-pole combined mass and `m_Sigma=m_c+sum_i(m_i)` for multiple poles.
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

### Appendices and Prose

- Rewrote Appendix A's d'Alembert explanation and Appendix B's torque/sign-convention explanation to match the main derivation.
- Appendix C now retains Cannon-labelled source notation, maps it explicitly to local notation, and confirms Cannon's `J=ml^2/3` is centre-of-mass body inertia.
- Appendix D labels Barto's unnumbered equations `(Barto pole)` and `(Barto cart)`, combines source values with mappings, and reserves (D1)-(D2) for local derivations. It now presents broad structural agreement, isolates the listed gravity sign as the apparent discrepancy, and treats inertia/friction primarily as interpretation.
- Appendix E labels source displays `(Wieland 9)`, `(Wieland 10)`, `(Wieland force)`, and `(Wieland mass)`, combines values with mappings, and reserves (E1)-(E2) for local derivations.
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
