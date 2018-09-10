#' @title df_cutoff()
#'
#' @description Cette fonction crée un dataframe avec 2 observations pour créer une ligne horizontale dans un ggplot
#' @examples +geom_line(data= df_cutoff(100), aes(x=x, y=y), linetype = 3, color = "black")
#' @export

df_cutoff <- function(y, label="label"){
  data.frame( x= c(-Inf, Inf), y = y, label = factor(label))
}
