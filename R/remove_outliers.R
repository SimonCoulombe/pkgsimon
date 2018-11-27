#' @title remove_outliers()
#'
#' @description Cette fonction prend un vecteur x en entrée et en enlève les outliers ( x > Q3+ 1.5 IQR ou x < Q1 - 1.5IQR)
#' @param x le vecteur de valeurs
#' @param na.rm est-ce qu'on enlève les na dans le calcul des quantiles?
#' @export

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y[!is.na(y)]
}
