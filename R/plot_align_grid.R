#' @title plot_align_grid()
#'
#' @description Cette fonction prend n graphiques créés avec ggplot et les imprime un en dessous de l'autre, en alignant l'axe des x.
#' @examples plot_align_grid(plot1  + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) , plot2, heights= c(3,1))
#' @export

plot_align_grid <- function(... , heights = NULL) {
  grobs <- lapply(list(...), FUN=ggplot2::ggplotGrob)
  max_widths <- do.call(grid::unit.pmax, lapply(grobs,  function(x) { x$widths }))
  num_plots <- length(grobs)
  for (i in 1:num_plots) {
   grobs[[i]]$widths <- max_widths
 }
  gridExtra::grid.arrange(grobs=grobs , layout_matrix = matrix(c(1,num_plots)), heights = heights)
}
