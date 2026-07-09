# Cart-Pole Equations Revision Handoff

Date: 2026-07-09

## Goal

Create a conservative replacement draft for `public/research/cart-pole/cart-pole-equations.html`, preserving the original article structure and wording where possible, while correcting the rotational inertia interpretation and associated equations.

## Active Draft

- Draft HTML: `public/research/cart-pole/cart-pole-equations-revision.html`
- Working notes: `memory/cart-pole-equations-revision-draft.md`
- Original article is still untouched.

## Current Progress

- Reviewed and revised through equation (59), appendices C-E comparison notes, section 6 light pass, and Figure 1 SVG labels.
- Next area to review: numerical-integration figure/table reproducibility, then final publication/replacement workflow.
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
- Section 3.7 now defines the total pivot inertia factor `mr^2 + J = \alpha mr^2`; point mass has `\alpha = 1`, uniform-pole hybrid has `\alpha = 4/3`.
- Section 3.8 friction equations and section 3.9 multiple-pole equations now use `r`, `r_i`, `\alpha`, and `\alpha_i`.
- Section 3.8 now distinguishes Coulomb friction parameters from velocity-proportional damping coefficients. In the recommended equations, `\mu_c` is a linear cart-track damping coefficient with units N s/m (newton-seconds per metre), and `\mu_p` is a linear pivot damping coefficient with units N m s/rad (newton-metre-seconds per radian).
- Section 4 hybrid equations and section 5 recommended equations now use the corrected `4/3` and `3/4` factors instead of the old `7/3` and `3/7` factors.
- Section 4 has been reframed from a "hybrid model" to a specialization of the general equations for a uniform pole. With `J` resolved as centre-of-mass body inertia, no separate hybrid approximation is required.
- Appendix C now treats Cannon's `J = ml^2/3` as body inertia about the mass centre, yielding total pivot inertia `4/3 ml^2`; the unsubstantiated Cannon sign-error claim was removed.
- Appendices D and E now describe Barto/Wieland `4/3` factors as consistent with the corrected uniform-pole hybrid model, rather than as problematic inertia terms.
- Section 6 has been scanned for stale inertia notation; only a spelling fix was needed (`Adams-Bashforth`).
- Figure 1 SVG labels in `cart-pole-model.svg` and `cart-pole-model-master.svg` now use `G` for the centre of mass and `r` for the pivot-to-centre-of-mass distance.
- A final stale-reference scan across the staging draft and both SVG files found no old `Q`, `ℓ`, `7/3`, `3/7`, `(1+k)`, or old `k` inertia language; the only remaining `Q` character is inside a Google Books URL query string.
- The section 6 numerical integration PNGs/table have not been regenerated with the corrected `4/3` recommended equations. No local simulation scripts or source data are present alongside the PNGs, so treat this as a publication-readiness check.
- The combined mass shorthand is now `m_t = m_c + m` for a single pole and `m_{\Sigma} = m_c + \sum_i m_i` for multiple poles. Do not reintroduce scalar `M`; reserve capital/bold `\mathbf{M}` for moments/torques.
- Do not describe all `\mu_c`/`\mu_p` terms as dimensionless coefficients of friction. Their meaning depends on the friction model; the recommended section 5 values are damping coefficients.

## Source Evidence

- Barto/Sutton/Anderson PDF downloaded to `memory/tmp/references/barto-sutton-anderson-83.pdf`; rendered pages in `memory/tmp/references/barto_pages/`. It is scanned, so text extraction failed.
- User supplied Cannon photo: `/mnt/c/Users/colin/Downloads/PXL_20260708_230436018.jpg`.
- Cannon photo verifies p.705 equations (22.50)-(22.56), including:
  - `J` is moment of inertia about the stick mass center.
  - `J = ml^2/3` for stick length `2l`.
  - Cannon equation (22.55) includes both `J\ddot\theta` and `ml(... + l\ddot\theta)`.
- Therefore Cannon's `J` is additional/body inertia about the centre of mass, not total pivot inertia.
- Cannon sign-error claims are not currently substantiated; do not reintroduce them without a dedicated sign-convention audit.

## User Preferences

- Stay close to the original section headers, structure, and wording.
- Grammar/readability improvements are welcome.
- Avoid broad restructuring unless needed for correctness or genuine clarity.
- Use `r` consistently for pivot-to-centre-of-mass distance. Do not reuse `l` with multiple meanings.
- Source-specific variables should be mapped explicitly in comparison sections, e.g. Barto's `l` corresponds to this article's `r`.

## Next Steps

1. Decide whether to regenerate or annotate section 6 figures/table after the recommended equations changed from old `7/3` factors to corrected `4/3` factors.
2. Consider whether Table 2/3 should keep `\hat l` for the hybrid/recommended equations or be mapped more explicitly to `r = \hat l`.
3. Decide whether shared Figure 1 SVG changes should remain in-place before replacing the published article; the current shared SVG is now consistent with the staging draft but not with the old published HTML.
4. Once the staging draft is stable, decide whether to replace `public/research/cart-pole/cart-pole-equations.html` with the revision draft.

## Resume Prompt

Resume the cart-pole equations revision in `/mnt/d/home/websites/sharpneat-web/cart-pole-derivation-review`. Read `memory/cart-pole-equations-revision-draft.md` and this handoff. Continue with numerical-integration figure/table verification and final publication-readiness checks for the staging draft.
