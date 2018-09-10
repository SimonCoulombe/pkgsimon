#' @title df_line45()
#'
#' @description Cette fonction crée un dataframe avec 2 observations pour créer une ligne à 45 degrés dans un ggplot
#' @examples +geom_line(data = df_line45(),aes(x=x, y=y), color = "lightgray")
#' @export

df_line45 <- function(label="label"){
  data.frame( x= c(-Inf, Inf), y = c(-Inf, Inf), label = factor(label))
}
