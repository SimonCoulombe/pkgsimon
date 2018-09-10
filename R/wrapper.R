#' @title wrapper()
#'
#' @description Cette fonction ajoute des line breaks pour avoir des titre qui fittent bien dans les ggplot..
#' @examples wrapper("pouet pouet", 50))
#' @export
wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}
