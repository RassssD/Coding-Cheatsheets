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
  propagate it to every per-language theme file too (R's is
  `theming/R/theme_rd.R`; Stata/Python/LaTeX still use the older
  `theme_rasmus.*` naming, pending their own build-out).

Currently populated: `R`, `Stata`. Scaffolded but not yet filled in:
`Python`, `LaTeX`.

## Status

`theming/R/theme_rd.R` is final and in use - font (Inter) and the
qualitative colour palette are real, mirrored in `theming/palette.yaml`.
Semantic roles in `palette.yaml` (accent/positive/negative/neutral_grey/
text) are still TODO, as are the Stata/Python/LaTeX theme files - don't
treat values in those as real. Check `palette.yaml` for a TODO before
relying on any theme file for actual colors.

## Conventions for integrating this into another project

- Prefer sourcing/importing these files directly (e.g. R `source()`, Python
  `import`, Stata `do`) over copy-pasting, so fixes here propagate.
- When generating plots for Rasmus in R, apply `theming/R/theme_rd.R`
  (`theme_rd()`, `theme_rd_pres()`/`theme_rd_art()` add-ons, `scale_*_rd()`)
  rather than a library's default style - see that file's header for usage.
  Stata/Python/LaTeX equivalents aren't built out yet.
- Helper functions here favor being self-contained (deps referenced via `::`
  or explicit imports, not attached) so they drop into another script cleanly.
