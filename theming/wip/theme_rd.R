#===============================================================================
# Rasmus Duret
# theme_rd(): ggplot2 base theme (structural: strips, gridlines, axes,
#   legend) - does most of the work, add to every plot.
# theme_rd_pres() / theme_rd_art(): thin *additive* theme() patches - stack
#   on top of theme_rd() for a presentation or an academic-article variant,
#   e.g. `p + theme_rd() + theme_rd_pres()`. Each only touches what actually
#   differs from the baseline. base_size/base_family still cascade correctly
#   through theme_rd()'s rel()-sized elements even though these are appended
#   afterwards, because ggplot2 resolves rel() at render time against the
#   fully merged theme, not at the point each piece was added.
#
# rd_make_ramp(): light -> dark tint/shade ramp from a single hex colour, in
#   HCL space (perceptually even steps) via colorspace::lighten/darken. This
#   is the "rebuild the ramp as a function of the colour" piece - swap
#   rd_colours$primary (or pass any other hex straight in) and every
#   sequential/diverging scale below follows.
# scale_colour_rd()/scale_fill_rd(): discrete qualitative scale.
# scale_colour_rd_seq()/scale_fill_rd_seq(): continuous scale from the
#   primary colour's ramp.
# scale_colour_rd_div()/scale_fill_rd_div(): diverging scale, contrast ramp
#   (reversed) -> white -> primary ramp.
#
# set_rd_geom_defaults(): opt-in - calls ggplot2::update_geom_defaults() so
#   geom_line()/geom_point()/etc. pick up the house line width, point size,
#   and alpha without typing them on every layer. Nothing runs on source();
#   call it once near the top of a script. Any explicit argument you still
#   pass to a geom afterwards (e.g. geom_line(linewidth = 2)) overrides it,
#   same as any other ggplot default.
#
# STATUS: rd_colours mirrors the brand/contrast/extra defaults settled on in
# the palette tester (theming/wip/palettetester.R). theming/palette.yaml
# itself is still a placeholder - once it's finalized, this should match it.
#
# Depends on: ggplot2, colorspace (both called via `::`, nothing attached).
# Optional: sysfonts + showtext, to auto-register Inter (see below) - if
# absent, ggplot just falls back to the system default font.
#
# Example:
#   source("theme_rd.R")
#   set_rd_geom_defaults("presentation")
#   ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#     geom_point() +
#     scale_colour_rd() +
#     theme_rd() + theme_rd_pres()
#===============================================================================

# ---- Font -----------------------------------------------------------------
# theme_rd() defaults to "Inter" everywhere (base, presentation, article).
# Best-effort auto-register it via sysfonts/showtext so it renders correctly
# out of the box, without any setup in the importing project. Silently does
# nothing if those packages aren't installed or there's no internet on first
# fetch (Google Fonts are cached locally after that) - ggplot just falls
# back to the system default font in that case, same as always.
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  try({
    if (!("Inter" %in% sysfonts::font_families())) {
      sysfonts::font_add_google("Inter", "Inter")
    }
    showtext::showtext_auto()
  }, silent = TRUE)
}

# ---- Brand colours ------------------------------------------------------
# Swap these and every scale/ramp below follows. Mirrors theming/palette.yaml.
rd_colours <- list(
  primary   = "#1E53A4",
  secondary = "#CAB06B",
  contrast  = "#E72922",
  extra     = "#006637"
)

# ---- Ramp builder --------------------------------------------------------
# n variants from light -> dark. n is forced odd so the colour you passed in
# sits exactly at the centre of its own ramp.
rd_make_ramp <- function(hex, n = 5, spread = 0.6) {
  if (n %% 2 == 0) n <- n + 1
  amounts <- seq(-spread, spread, length.out = n)
  vapply(amounts, function(a) {
    if (a < -1e-9)      colorspace::lighten(hex, -a, method = "relative")
    else if (a > 1e-9)  colorspace::darken(hex,   a, method = "relative")
    else                hex
  }, character(1))
}

# ---- Qualitative palette --------------------------------------------------
rd_qualitative <- c(rd_colours$primary, rd_colours$secondary, rd_colours$contrast, rd_colours$extra)

scale_colour_rd <- function(...) ggplot2::scale_colour_manual(values = rd_qualitative, ...)
scale_fill_rd   <- function(...) ggplot2::scale_fill_manual(values   = rd_qualitative, ...)

scale_colour_rd_seq <- function(...) ggplot2::scale_colour_gradientn(colours = rd_make_ramp(rd_colours$primary), ...)
scale_fill_rd_seq   <- function(...) ggplot2::scale_fill_gradientn(colours   = rd_make_ramp(rd_colours$primary), ...)

scale_colour_rd_div <- function(...) {
  ramp <- c(rev(rd_make_ramp(rd_colours$contrast)), "white", rd_make_ramp(rd_colours$primary))
  ggplot2::scale_colour_gradientn(colours = ramp, ...)
}
scale_fill_rd_div <- function(...) {
  ramp <- c(rev(rd_make_ramp(rd_colours$contrast)), "white", rd_make_ramp(rd_colours$primary))
  ggplot2::scale_fill_gradientn(colours = ramp, ...)
}

# ---- Base theme -----------------------------------------------------------
# Args: base_size (base font size), base_family (font family), grid
#   ("horizontal" = major horizontal gridlines only, "both" = major grid
#   both directions, "none" = no gridlines at all). No minor gridlines in
#   any case.
theme_rd <- function(base_size = 14, base_family = "Inter", grid = "horizontal") {
  th <- ggplot2::theme_light(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # facet strips: white fill, thin black bounding box
      strip.background = ggplot2::element_rect(fill = "white", colour = "black",
                                                 linewidth = 0.4),
      strip.text = ggplot2::element_text(colour = "grey20", face = "bold",
                                          size = ggplot2::rel(0.9), hjust = 0.5,
                                          margin = ggplot2::margin(t = 4, b = 4, l = 6, r = 6)),
      # drop the panel border, use real axis lines instead
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "grey40", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      panel.spacing = ggplot2::unit(1.2, "lines"),
      # ticks and axes
      axis.ticks = ggplot2::element_line(colour = "grey40", linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      axis.title = ggplot2::element_text(colour = "grey20", size = ggplot2::rel(0.95)),
      axis.text = ggplot2::element_text(colour = "grey35"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      # titles and legend
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15),
                                          margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(colour = "grey30", margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = "grey45", size = ggplot2::rel(0.8), hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(14, "pt"),
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(t = 0)
    )
  if (grid == "horizontal") th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (grid == "none")       th <- th + ggplot2::theme(panel.grid.major   = ggplot2::element_blank())
  th
}

# ---- Add-on variants -------------------------------------------------------
# Additive theme() patches - stack on top of theme_rd(), don't replace it:
# `p + theme_rd() + theme_rd_pres()`. Only the pieces that actually change
# for that use case are touched.
theme_rd_pres <- function(legend_key_pt = 18) {
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    legend.key.size  = ggplot2::unit(legend_key_pt, "pt")
  )
}

theme_rd_art <- function(base_size = 11, base_family = "Inter", legend_key_pt = 8) {
  ggplot2::theme(
    text            = ggplot2::element_text(size = base_size, family = base_family),
    legend.key.size = ggplot2::unit(legend_key_pt, "pt")
  )
}

# ---- Geom defaults ----------------------------------------------------------
# Opt-in: nothing runs on source(). Call once per script; any explicit geom
# argument you pass afterwards still overrides these, same as any other
# ggplot default. point_size values already fold in the same "point reads
# too big relative to the line" correction settled on in the palette tester
# (raw size * 0.6).
#
# geom_pointrange() note: `fatten` and `key_glyph` are call-time parameters,
# not aesthetics, so update_geom_defaults() can't touch them here - pass
# them directly wherever you use geom_pointrange(), e.g.
#   geom_pointrange(fatten = 1, key_glyph = "point")
# so its dot (and its legend key) render at the same size as a plain
# geom_point() using the same `size`. ggplot2's default fatten = 4 on
# geom_pointrange is what causes the mismatch if left alone.
rd_geom_presets <- list(
  default      = list(linewidth = 1.25, point_size = 2, alpha = 0.3),
  presentation = list(linewidth = 1.25, point_size = 2, alpha = 0.3),
  article      = list(linewidth = 1, point_size = 2, alpha = 0.5)
)

set_rd_geom_defaults <- function(preset = "default") {
  preset <- match.arg(preset, names(rd_geom_presets))
  p <- rd_geom_presets[[preset]]
  ggplot2::update_geom_defaults("line",       list(linewidth = p$linewidth))
  ggplot2::update_geom_defaults("point",      list(size = p$point_size))
  ggplot2::update_geom_defaults("pointrange", list(linewidth = p$linewidth, size = p$point_size))
  ggplot2::update_geom_defaults("ribbon",     list(alpha = p$alpha))
  ggplot2::update_geom_defaults("area",       list(alpha = p$alpha))
  ggplot2::update_geom_defaults("density",    list(alpha = p$alpha))
  invisible(p)
}


# Testing ======================================================================
regions <- c("Northeast","Midwest","South","West","Mountain","Pacific")
region_trends <- c(Northeast = 0.020, Midwest = 0.016, South = 0.028,
                   West = 0.030, Mountain = 0.024, Pacific = 0.027)
region_data <- expand.grid(year = 2000:2020, region = regions,
                           stringsAsFactors = FALSE) |>
  as_tibble() |>
  group_by(region) |>
  arrange(year, .by_group = TRUE) |>
  mutate(shock  = ifelse(year == 2008, -0.025, 0) + ifelse(year == 2020, -0.035, 0),
         growth = region_trends[region] + shock + rnorm(n(), 0, 0.010),
         y      = 100 * cumprod(1 + growth)) |>
  ungroup() |>
  select(year, region, y)

set_rd_geom_defaults()

region_data |> ggplot(aes(x=year, y=y, colour=region, group=region)) + 
  geom_line() + geom_point() + 
  theme_rd() + facet_wrap(.~region) + 
  labs(x="Year", y="Mean", colour="Region", title="Testing testing", subtitle="Woooo", caption="This is serious data")
  
