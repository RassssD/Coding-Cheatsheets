# Coding-Cheatsheets

Rasmus Duret's personal library of reusable helper functions and consistent
theming across languages. Point an agent at this repo to pull in a helper or
match Rasmus's visual style instead of language defaults (e.g. ggplot2's
`theme_grey()`, matplotlib defaults, Stata's default scheme).

## Structure

- `<Language>/Helper/` - standalone helper functions (one concern per file).
  Each file opens with a header comment: what it does, args, dependencies,
  and usually a runnable example at the bottom.
- `theming/<Language>/` - theme/style files (colour scales, plot themes,
  fonts) for that language.
- `theming/palette.yaml` - single source of truth for the shared palette and
  font. Per-language theme files mirror these values by hand (most of these
  languages can't load YAML at runtime), so if you edit a colour here,
  propagate it to every `theming/<Language>/theme_rasmus.*` file too.

Currently populated: `R`, `Stata`. Scaffolded but not yet filled in:
`Python`, `LaTeX`.

## Status

`theming/palette.yaml` and all `theming/<Language>/theme_rasmus.*` files are
**placeholders** (TODO hex codes) - the actual palette/font hasn't been
chosen yet. Don't treat current values in those files as real; check
`palette.yaml` for a TODO before relying on any theme file for actual colors.

## Conventions for integrating this into another project

- Prefer sourcing/importing these files directly (e.g. R `source()`, Python
  `import`, Stata `do`) over copy-pasting, so fixes here propagate.
- When generating plots/tables for Rasmus in R, Stata, Python, or LaTeX,
  apply the matching `theming/<Language>/theme_rasmus.*` file rather than a
  library's default style - once palette.yaml is no longer a placeholder.
- Helper functions here favor being self-contained (deps referenced via `::`
  or explicit imports, not attached) so they drop into another script cleanly.
