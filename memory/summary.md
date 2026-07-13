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
- The eight-pass pedagogical simplification in `memory/plans/cart-pole-pedagogical-simplification.md` is complete. It improved hierarchy and pacing while retaining the basic-algebra audience and detailed derivations; the remaining checkpoint is to review and commit passes 4-8 and the unified `m_{\mathrm{tot}}` notation change.

## How To Resume

1. Read `AGENTS.md`.
2. Read this file and `memory/map.md`.
3. Check `git status --short` before editing. In this shell, many tracked website files may appear modified because their index line endings are LF while the working tree uses CRLF; confirm with `git diff --ignore-space-at-eol` before treating them as content changes.
4. Read only the website files relevant to the task.
5. For cart-pole work, read `memory/cart-pole-equations.md`, relevant decisions/sources, and then the canonical article.
6. Update memory when reusable context, decisions, constraints, or source evidence are discovered.

