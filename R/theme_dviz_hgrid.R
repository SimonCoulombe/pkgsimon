#' @title theme_dviz_hgrid()
#'
#' @description Horizontal lines only - ggplot theme taken from https://github.com/clauswilke/dataviz/blob/master/R/themes.R

theme_dviz_hgrid <- function(font_size = 14, font_family = "") {
  color = "grey90"
  line_size = 0.5

  # Starts with theme_cowplot and then modify some parts
  theme_cowplot(font_size = font_size, font_family = font_family) %+replace%
    theme(
      # make horizontal grid lines
      panel.grid.major   = element_line(colour = color,
                                        size = line_size),
      panel.grid.major.x = element_blank(),

      # adjust axis tickmarks
      axis.ticks        = element_line(colour = color, size = line_size),

      # adjust x axis
      axis.line.x       = element_line(colour = color, size = line_size),
      # no y axis line
      axis.line.y       = element_blank()
    )
}
