# Constraints

- Preserve the existing static website structure unless the user explicitly asks for a redesign, migration, or build-system change.
- Keep edits scoped to the requested task and avoid broad formatting churn in legacy HTML, CSS, CSV, R, or SVG files.
- Treat real pre-existing dirty worktree changes as user-owned. In this repo, line-ending-only status noise can appear for tracked website files; confirm with `git diff --ignore-space-at-eol` before treating it as content change.
- Prefer project-local memory updates in `memory/` for durable context discovered during work.
- Verify time-sensitive or externally dependent claims before updating public content or making recommendations.
