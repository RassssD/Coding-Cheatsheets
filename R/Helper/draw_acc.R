#===============================================================================
# Rasmus Duret
# draw_acc(): draws a ggplot at an exact physical size (mm) in a grid viewport,
# on a grey background - useful for checking how a plot will look at its
# actual export/print dimensions rather than the ad hoc device size RStudio uses.
# Depends on: ggplot2, grid (both called via `::`, nothing attached).
#
# Args: plot (a ggplot object), x_mm/y_mm (viewport width/height in mm).
#===============================================================================

draw_acc <- function(plot, x_mm = 100, y_mm = 100) {
  
  grid::grid.newpage()
  
  grid::rectGrob(gp = grid::gpar(fill = "gray")) |>
  grid::grid.draw()
  
  grid::viewport(width  = ggplot2::unit(x_mm, "mm"), 
                 height = ggplot2::unit(y_mm, "mm")) |>
    grid::pushViewport()
  
  ggplot2::ggplot_build(plot) |>
    ggplot2::ggplot_gtable() |>
    grid::grid.draw()
}
