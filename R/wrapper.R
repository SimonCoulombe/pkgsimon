#' @title wrapper()
#'
#' @description Cette fonction ajoute des line breaks pour avoir des titre qui fittent bien dans les ggplot..
#' @examples
#' wrapper(paste0("Chapitre 2, actu seulement , observé / moyenne ratio= ",round(worst_to_best_ratio,1)), 50))
wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}
