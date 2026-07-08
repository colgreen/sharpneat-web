# Cart-Pole Derivation Review

Date: 2026-07-08

Scope: `public/research/cart-pole/cart-pole-equations.html`

## Current Finding

The first numbered equation identified as erroneous is equation (25):

```tex
J = kml^2
```

The issue is not that a placeholder `J` is introduced. Equations (16) and (18) can be valid if `J` is interpreted as additional/body rotational inertia about the mass center. The error begins when equation (25) treats `J` as the pendulum's total moment of inertia about the pivot.

Equation (14) already includes the `ml^2 \ddot\theta` rotational inertia contribution from the point mass/center of mass translation:

```tex
\mathbf{M}_q = ml(l\ddot\theta + \ddot x\cos\theta)
```

Adding `J\ddot\theta` in equation (18) therefore means `J` should represent only additional inertia about the mass center, not the total pivot inertia.

## Consequences

- For the point-mass pendulum model, `J` should be `0`, not `ml^2`.
- For the uniform-pole hybrid model where `\hat l` is half the pole length and the center of mass is modeled at `\hat l`, the additional body inertia should be `J = (1/3)m\hat l^2`.
- This gives the standard total factor `4/3`, not `7/3`, in the hybrid equations.
- Equations (51)-(59), especially the recommended equations (56)-(59), likely need the `7/3` factors revised back to `4/3`.
- Appendix C's claim that Cannon's `4/3` should be corrected to `7/3` should be revisited.
- The related appendix statements that Barto et al. and Wieland adopted a problematic moment-of-inertia term should also be revisited.

## Review Status

No edits were made to `cart-pole-equations.html` during the review. The worktree already had many unrelated modified files, including the cart-pole article, so future sessions should inspect diffs carefully and preserve unrelated changes.

## Additional Notes

- The friction terms recommended as `F_f = -\mu_c \dot x` and `\mathbf{M}_f = \mu_p \dot\theta` are viscous damping forms. Their parameters have units, so describing `\mu_c` and `\mu_p` simply as coefficients of friction is dimensionally misleading.
- The introduction has a minor malformed HTML paragraph around the text for section 5, but this is not part of the derivation issue.

