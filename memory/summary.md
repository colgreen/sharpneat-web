# SharpNEAT Website Memory

This repository maintains the static website for SharpNEAT, a C#/.NET implementation of Neuroevolution of Augmenting Topologies (NEAT).

## Current Shape

- `public/` contains the published static website pages, CSS, release notes, research pages, data files, and assets.
- `archive/` contains older archived pages.
- `artwork/` contains logo and artwork source files.
- There is no package manifest or build system currently visible; edits are direct static file changes unless a future task establishes otherwise.
- `public/index.html` is the main entry point. It links to the SharpNEAT project page, roadmap, research citations, contact/licensing/FAQ pages, screenshots, file formats, research articles, release notes, and genetic art images.
- Shared styles live in `public/sharpneat.css` and `public/sharpneat-print.css`.
- Release notes are organized under `public/releases/sharpneat-*`, currently covering SharpNEAT 2.3.0 through 4.1.0.
- Research content is under `public/research/`, with top-level research pages plus subfolders for activation function review, cart-pole, efficacy sampling, integer neural net, and PTSP material.
- Generated or supporting research/release data includes CSV files and R scripts alongside the relevant HTML pages.
- The completed cart-pole article revision and its maintenance guidance are summarized in `memory/cart-pole-equations.md`. The canonical article is `public/research/cart-pole/cart-pole-equations.html`; mathematical, notation, Appendix A-E, prose, Figure 1, C# alignment, and numerical-artifact passes are complete.
- The eight-pass pedagogical simplification in `memory/plans/cart-pole-pedagogical-simplification.md` is complete. It improved hierarchy and pacing while retaining the basic-algebra audience and detailed derivations; that plan is retained as the historical record of the completed passes.
- A fresh review at revision `d07bb73` is recorded in `memory/plans/cart-pole-derivation-review.md`. It found no incorrect final equation and opened prioritized pedagogical work. On 2026-07-13, F1-F6, F11, and F14 were resolved: the main paper now has one nonsingular cart-acceleration-first route; the single- and multiple-pole friction forms are derived explicitly; the uniform-inertia substitutions and cancellations through (44)-(47) are shown; Wieland's effective force and mass are expanded before (E1); the multiple-pole section defines the unjointed topology and index range, qualifies the equal-length controllability claim, and now includes a focused two-pole topology diagram; the moment derivation supplies the planar cross-product rule and its component substitutions; Appendix A treats the accelerating pivot explicitly; Appendix C follows the main route; the alternative elimination is derived only in Appendix D for the Barto comparison; main equations are numbered continuously (1)-(51); and “moment” is distinguished dimensionally from force rather than described as a “rotational force”. On 2026-07-14, F7-F8 were resolved: the cart-to-pivot kinematics are written explicitly before absolute acceleration, the track reaction follows from a displayed vertical balance, and the pole-on-cart pivot reaction is defined before its components are derived. F9 was fully resolved on 2026-07-14: section 5 now inventories the instantaneous state, input, and parameters required to implement (48)-(51), and the general and uniform-pole denominators are proved positive for physical parameters. Smaller narrative and editorial findings remain open. F17 records a deferred, dedicated reconsideration of the clockwise-positive angular coordinate; the provisional preference is to retain right-hand-rule moments and consider making all angular variables anticlockwise-positive, but the current equations have not been changed. The intended reader may be assumed to understand basic calculus, trigonometry, and summation notation.

## How To Resume

1. Read `AGENTS.md`.
2. Read this file and `memory/map.md`.
3. Check `git status --short` before editing. In this shell, many tracked website files may appear modified because their index line endings are LF while the working tree uses CRLF; confirm with `git diff --ignore-space-at-eol` before treating them as content changes.
4. Read only the website files relevant to the task.
5. For cart-pole work, read `memory/cart-pole-equations.md`, relevant decisions/sources, and then the canonical article.
6. For the next cart-pole revision passes, read `memory/plans/cart-pole-derivation-review.md` and work through its findings in the suggested order.
7. Update memory when reusable context, decisions, constraints, or source evidence are discovered.
