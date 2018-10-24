#' @title disloc()
#'
#' @description Cette fonction crée un tableau et une double lift chart
#' @param data data.frame  source
#' @param pred1 prediction of first model
#' @param pred1 prediction of second model
#' @param expo exposure
#' @param obs observed result
#' @param nb nombre de quantils créés
#' @param pred1_label Label pour la première prédiction dans le graphique
#' @param pred2_label Label pour la deuxième prédiction dans le graphique
#' @param y_label Label pour la valeur réalisée dans le graphique
#' @param y_format Fonction utilisée pour formater l'axe des y dans le graphique (par exemple percent_format() ou dollar_format() du package scales)
#' @export

disloc <- function(data, pred1, pred2, expo, obs, nb = 10, 
                   pred1_lab = "pred1", pred2_lab = "pred2",
                   y_label= "sinistralité",
                   y_format = percent_format()) {
  # obligé de mettre les variables dans un enquo pour pouvoir les utiliser dans dplyr
  pred1_var <- enquo(pred1)
  pred2_var <- enquo(pred2)
  expo_var <- enquo(expo)
  obs_var <- enquo(obs)
  
  
  pred1_name <- quo_name(pred1_var)
  pred2_name <- quo_name(pred2_var)
  obs_name <- quo_name(obs_var)
  
  
  
  # création de la comparaison entre les deux pred
  dd <- data %>% 
    mutate(ratio = !!pred1_var / !!pred2_var) %>% 
    filter(!!expo_var > 0) %>% 
    drop_na()
  
  # constitution des buckets de poids égaux
  dd <- dd %>% add_equal_weight_group(
    ratio = ratio,
    expo = !!expo_var, # était nb_day
    group_variable_name = "groupe",
    nb = nb
  )
  
  # comparaison sur ces buckets
  dd <- full_join(
    dd %>% group_by(groupe) %>%
      summarise(
        ratio_moyen = mean(ratio),
        ratio_min = min(ratio),
        ratio_max = max(ratio)
      ),
    dd %>% group_by(groupe) %>%
      summarise_at(
        fexpo(sum(.) / sum(!!expo_var)),
        .vars = vars(!!obs_var, !!pred1_var, !!pred2_var)
      ) %>%
      ungroup,
    by = "groupe"
  )
  
  # création des labels
  dd <- dd %>%
    mutate(labs = paste0("[", round(ratio_min, 2), ", ", round(ratio_max, 2), "]"))
  
  # graphe
  plotdata <-
    dd %>%
    gather(key, variable, !!obs_var, !!pred1_var, !!pred2_var) %>%
    ## Pas optimal mais je ne trouve pas mieux...
    mutate(key = case_when(
      key == obs_name ~ "réalisé",
      key == pred1_name ~ pred1_lab,
      key == pred2_name ~ pred2_lab
    )) %>%
    mutate(key = factor(key, levels = c("réalisé", pred1_lab, pred2_lab), ordered = TRUE))
  
  pl <- plotdata %>%
    ggplot(aes(groupe, variable, color = key, linetype = key)) +
    geom_line() + geom_point() +
    scale_color_manual(name = "", values = c("black", "dark green", "light green")) +
    scale_linetype_manual(name = "", values = c(3, 1, 1)) +
    scale_x_continuous(breaks = seq_along(dd$labs), labels = dd$labs) +
    xlab("ratio entre les prédictions") + ylab(y_label) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    scale_y_continuous(labels =  y_format())
  
  # écart au réalisé, pondéré
  ecart <- dd %>%
    mutate(poids = abs(1 - ratio_moyen)) %>%
    summarise_at(
      vars(!!pred1_var, !!pred2_var),
      fexpo(weighted.mean((. - !!obs_var)^2, w = poids) %>% sqrt())
    ) %>% summarise(ratio_distance = !!pred2_var / !!pred1_var) %>%
    as.numeric()
  
  list(
    graphe = pl,
    ecart = ecart,
    tableau = dd
  )
}