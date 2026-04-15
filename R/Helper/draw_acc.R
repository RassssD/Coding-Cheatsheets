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
