# SharpNEAT Website Memory

This repository maintains the static website for SharpNEAT, a C#/.NET implementation of Neuroevolution of Augmenting Topologies (NEAT).

## Current Shape

- `public/` contains the published static website pages, CSS, release notes, research pages, data files, and assets.
- `archive/` contains older archived pages.
- `artwork/` contains logo and artwork source files.
- There is no package manifest or build system currently visible; edits are direct static file changes unless a future task establishes otherwise.

## How To Resume

1. Read `AGENTS.md`.
2. Read this file and `memory/map.md`.
3. Check `git status --short` before editing. In this shell, many tracked website files may appear modified because their index line endings are LF while the working tree uses CRLF; confirm with `git diff --ignore-space-at-eol` before treating them as content changes.
4. Read only the website files relevant to the task.
5. Update memory when reusable context, decisions, constraints, or source evidence are discovered.
