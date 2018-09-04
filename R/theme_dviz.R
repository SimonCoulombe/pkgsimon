#' @title theme_dviz()
#'
#' @description ggplot theme taken from https://github.com/clauswilke/dataviz/blob/master/R/themes.R

theme_dviz <- function(font_size = 14, font_family = ""){
  theme_cowplot(font_size = font_size, font_family = font_family)
}
