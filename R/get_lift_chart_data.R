#' @title get_lift_chart_data()
#'
#' @description Cette fonction retourne un data.frame avec les colonnes nécessaires pour créer un beau lift chart.  On peut lifter à partir d'un autre modèle ou bien à partir de "no model".
#' @param table data.frame à partir duquel on construit le lift
#' @param sort_by Variable utilisée pour trier les observations.  utiliser un taux pour lifter contre "aucun modèle" et utiliser un ratio de prédictions pour lifter contre un autre modèle
#' @param expo Exposition (utilisée pour créer des quantiles de la même taille.  Si NULL, l'exposition est égale pour toutes les observations) (Défault = NULL).
#' @param nb Nombre de quantiles crées (défaut = 10)
#' @param pred Prédiction.  Utiliser la prédiction du modèle *de référence* pour une comparaison de modèles.  Utiliser la prédiction du modèle pour comparer contre "no model".
#' @param obs Variable réponse observée (défaut = gperts)
#' @keywords lift chart
#' @export
#' @examples
#' ## lift chart vs modèle actu
#' zz <- get_lift_chart_data(model_data  ,
#' expo= "nb_day",
#' sort_by = "ratio_algo3",
#' pred=  "pred",
#' obs = "gperts",
#' nb = 10)
#'  lift chart vs no model
#' zz <- get_lift_chart_data(model_data  %>%
#' mutate( rate = pred_algo3 / nb_day) ,
#' expo= "nb_day",
#' sort_by = "rate",
#' pred=  "pred_algo3",
#' obs = "gperts",
#' nb = 10)


get_lift_chart_data <- function(table, sort_by = "rate", expo = NULL, nb = 10,
                                pred= "pred",
                                obs = "obs") {

  result <- table
  # création de l'exposition ( = 1 si NULL)
  if (is.null(expo)){result$expo <- 1}
  else{result$expo <- result[[expo]]}

  # création des variables pour convertir un peu vachement les string qui peuvent prendre n'importe quelle valeur en une variable qui a toujours le même nom et donc
  # plus facile à programmer..
  result$sort_by <- result[[sort_by]]
  result$pred <- result[[pred]]
  result$obs <- result[[obs]]

  cum_expo <- sum(result$expo)
  cum_obs <- sum(result$obs)
  mean_rate <- cum_obs / cum_expo
  # if (is.null(expo)){ cum_expo <- dim(table)[1]}
  # else {cum_expo = sum( table %>% pull(!!expo))}
  print(cum_expo)

  breaks <- seq(0, cum_expo, length.out = nb + 1) %>% head(-1) %>% c(Inf) %>% unique

  data <- result  %>%
    arrange(sort_by) %>%
    mutate(cumExpo = cumsum(expo)) %>%
    mutate(quantile = cut(cumExpo, breaks = breaks, ordered_result = TRUE, include.lowest = TRUE) %>% as.numeric) %>%
    group_by(quantile) %>%
    summarise(pred = sum(pred),
              obs = sum(obs),
              observé = round(  100*  sum(obs)/ sum(expo)/mean_rate,1),
              prédit = round(  100*  sum(pred)/ sum(expo)/mean_rate,1),
              sumExpo = sum(expo),
              count = n())  %>%
    ungroup() %>%
    gather(key = type, value = lift_vs_nomodel, observé, prédit) %>%
    mutate(lift_vs_pred = obs/pred)
}
