#' @title theme_dviz_vgrid()
#'
#' @description vertical lines only - ggplot theme taken from https://github.com/clauswilke/dataviz/blob/master/R/themes.R
#' @export
# vertical grid lines only
theme_dviz_vgrid <- function(font_size = 14, font_family = "") {
  color = "grey90"
  line_size = 0.5

  # Starts with theme_cowplot and then modify some parts
  theme_cowplot(font_size = font_size, font_family = font_family) %+replace%
    theme(
      # make vertical grid lines
      panel.grid.major   = element_line(colour = color,
                                        size = line_size),
      panel.grid.major.y = element_blank(),

      # adjust axis tickmarks
      axis.ticks        = element_line(colour = color, size = line_size),

      # adjust y axis
      axis.line.y       = element_line(colour = color, size = line_size),
      # no x axis line
      axis.line.x       = element_blank()
    )
}
