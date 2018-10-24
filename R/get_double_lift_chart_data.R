#' @title get_double_lift_chart_data()
#'
#' @description Cette fonction retourne un data.frame avec les colonnes nécessaires pour créer un beau double lift chart.
#' @param table data.frame à partir duquel on construit le lift
#' @param expo Exposition (utilisée pour créer des quantiles de la même taille.  Si NULL, l'exposition est égale pour toutes les observations) (Défault = NULL).
#' @param nb Nombre de quantiles crées (défaut = 10)
#' @param pred1 Prédiction du premier modèle.
#' @param pred2 Prédiction du second modèle .
#' @param obs Variable réponse observée (défaut = gperts)
#' @keywords double lift chart
#' @export



get_double_lift_chart_data <- function(table,  expo = NULL, nb = 10,
                                pred1= "pred1", pred2= "pred2",
                                obs = "obs") {

  result <- table
  # création de l'exposition ( = 1 si NULL)
  if (is.null(expo)){result$expo <- 1}
  else{result$expo <- result[[expo]]}

  # création des variables pour convertir un peu vachement les string qui peuvent prendre n'importe quelle valeur en une variable qui a toujours le même nom et donc
  # plus facile à programmer..
  result$pred1 <- result[[pred1]]
  result$pred2 <- result[[pred2]]
  result$sort_by <- result[[pred1]] / result[[pred2]]
  result$obs <- result[[obs]]

  cum_expo <- sum(result$expo)
  cum_obs <- sum(result$obs)
  mean_rate <- cum_obs / cum_expo

  breaks <- seq(0, cum_expo, length.out = nb + 1) %>% head(-1) %>% c(Inf) %>% unique

  data <- result  %>%
    arrange(sort_by) %>%
    mutate(cumExpo = cumsum(expo)) %>%
    mutate(quantile = cut(cumExpo, breaks = breaks, ordered_result = TRUE, include.lowest = TRUE) %>% as.numeric) %>%
    group_by(quantile) %>%
    summarise(pred1 = sum(pred1),
              pred2 = sum(pred2),
              obs = sum(obs),
              observé = round(  100*  sum(obs)/ sum(expo)/mean_rate,1),
              prédit1 = round(  100*  sum(pred1)/ sum(expo)/mean_rate,1),
              prédit2 = round(  100*  sum(pred2)/ sum(expo)/mean_rate,1),
              sumExpo = sum(expo),
              count = n())  %>%
    ungroup() %>%
    gather(key = type, value = lift_vs_nomodel, observé, prédit1, prédit2)
}
