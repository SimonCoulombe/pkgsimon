#' @title add_equal_weight_group()
#'
#' @description Cette fonction crée des groupe (quantiles) avec le nombre nombre total d'exposition.
#' @param table data.frame  source
#' @param sort_by Variable utilisée pour trier les observations.
#' @param expo Exposition (utilisée pour créer des quantiles de la même taille.  Si NULL, l'exposition est égale pour toutes les observations) (Défault = NULL).
#' @param nb Nombre de quantiles crées (défaut = 10)
#' @param group_variable_name Nom de la variable de groupes créée
#' @export


add_equal_weight_group <- function(table, sort_by, expo = NULL, group_variable_name = "groupe", nb = 10) {
  sort_by_var <- enquo(sort_by)
  groupe_variable_name_var <- enquo(group_variable_name)

  if (!(missing(expo))){ # https://stackoverflow.com/questions/48504942/testing-a-function-that-uses-enquo-for-a-null-parameter

    expo_var <- enquo(expo)

    total <- table %>% pull(!!expo_var) %>% sum
    br <- seq(0, total, length.out = nb + 1) %>% head(-1) %>% c(Inf) %>% unique
    table %>%
      arrange(!!sort_by_var) %>%
      mutate(cumExpo = cumsum(!!expo_var)) %>%
      mutate(!!group_variable_name := cut(cumExpo, breaks = br, ordered_result = TRUE, include.lowest = TRUE) %>% as.numeric) %>%
      select(-cumExpo)
  } else {
    total <- nrow(table)
    br <- seq(0, total, length.out = nb + 1) %>% head(-1) %>% c(Inf) %>% unique
    table %>%
      arrange(!!sort_by_var) %>%
      mutate(cumExpo = row_number()) %>%
      mutate(!!group_variable_name := cut(cumExpo, breaks = br, ordered_result = TRUE, include.lowest = TRUE) %>% as.numeric) %>%
      select(-cumExpo)
  }
}
