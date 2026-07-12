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
- Current cart-pole derivation review notes are in `memory/cart-pole-derivation-review.md`.
- The cart-pole revision is now promoted to the canonical `public/research/cart-pole/cart-pole-equations.html`; the redundant staging HTML was removed on 2026-07-12. The mathematical, notation, Appendix A-E, prose, Figure 1, and numerical-artifact passes are complete. General equations retain `mr^2+J` explicitly; uniform-pole equations use the corrected `4/3` and `3/4` factors. Section 6 CSVs and Figures 2-3 were regenerated from corrected C# equations, Table 5 was recalculated against a converged RK4 reference, and the floating-point comparison is quantified. Historical working notes remain in `memory/cart-pole-equations-revision-draft.md`.

## How To Resume

1. Read `AGENTS.md`.
2. Read this file and `memory/map.md`.
3. Check `git status --short` before editing. In this shell, many tracked website files may appear modified because their index line endings are LF while the working tree uses CRLF; confirm with `git diff --ignore-space-at-eol` before treating them as content changes.
4. Read only the website files relevant to the task.
5. For cart-pole derivation work, read `memory/cart-pole-derivation-review.md` and `memory/cart-pole-equations-revision-draft.md` before editing `public/research/cart-pole/cart-pole-equations.html`.
6. Update memory when reusable context, decisions, constraints, or source evidence are discovered.

