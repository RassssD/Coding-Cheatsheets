#===============================================================================
# Rasmus Duret
# theme_rasmus(): ggplot2 base theme (structural: strips, gridlines, axes,
# legend) - originally drafted as theme_facet() in quicktheme.txt (kept as an
# archival copy at the repo root), folded in here as the canonical version.
# scale_colour_rasmus()/scale_fill_rasmus(): discrete colour/fill scales using
# the shared qualitative palette from theming/palette.yaml.
#
# STATUS: the structural theme (theme_rasmus()) is real/in-use. The colour
# palette below is still a placeholder - theming/palette.yaml has not been
# finalized yet. Fill in palette.yaml first, then mirror the real values here.
#
# Depends on: ggplot2 (called via `::`, nothing attached).
#
# Args (theme_rasmus): base_size (base font size), base_family (font family -
#   mirrors font.primary in theming/palette.yaml once decided), grid ("y" =
#   horizontal gridlines only, "x" = vertical only, "none" = no major grid).
#===============================================================================

theme_rasmus <- function(base_size = 11, base_family = "", grid = "y") {
  th <- ggplot2::theme_light(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # facet strips: outlined box, no fill, dark readable text
      strip.background = ggplot2::element_rect(fill = NA, colour = "grey80",
                                                 linewidth = 0.4),
      strip.text = ggplot2::element_text(colour = "grey20", face = "bold",
                                          size = ggplot2::rel(0.9), hjust = 0,
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
      legend.background = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(t = 0)
    )
  if (grid == "y")    th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (grid == "x")    th <- th + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (grid == "none") th <- th + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  th
}

# Qualitative palette - mirrors colors.qualitative in theming/palette.yaml
rasmus_qualitative <- c("#TODO1", "#TODO2", "#TODO3", "#TODO4")

# Discrete colour/fill scales using the qualitative palette above
scale_colour_rasmus <- function(...) ggplot2::scale_colour_manual(values = rasmus_qualitative, ...)
scale_fill_rasmus   <- function(...) ggplot2::scale_fill_manual(values = rasmus_qualitative, ...)
