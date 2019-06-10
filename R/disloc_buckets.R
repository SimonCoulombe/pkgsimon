#' @title disloc_buckets()
#'
#' @description Cette fonction crée un tableau et une double lift chart en séparant sur des buckets de ratio pré-définis au lieu d'une exposition constante.
#' @param data data.frame  source
#' @param pred1 prediction of first model
#' @param pred1 prediction of second model
#' @param expo exposure
#' @param obs observed result
#' @param buckets limites des buckets
#' @param pred1_lab Label pour la première prédiction dans le graphique
#' @param pred2_lab Label pour la deuxième prédiction dans le graphique
#' @param y_label Label pour la valeur réalisée dans le graphique
#' @param x_label Label pour l'axe des x
#' @param y_format Fonction utilisée pour formater l'axe des y dans le graphique (par exemple percent_format() ou dollar_format() du package scales)
#' @param graphe_title une string qui donne le titre du modèle
#' @export

disloc_buckets <- function(data, pred1, pred2, expo, obs,
                           buckets = c(0, 0.825,0.875,0.925,0.975,1.025, 1.075, 1.125, 1.175, Inf),
                           pred1_lab = "pred1", pred2_lab = "pred2", actual_lab = "réalisé",
                           x_label = "ratio entre les prédictions",
                           y_label= "sinistralité",
                           y_format = percent_format(),
                           graphe_title = NULL) {

  #browser()
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
    drop_na() %>%
    mutate(labels = cut(ratio, breaks= buckets, ordered_result= TRUE))  %>%
    mutate(labels = as.factor(labels))


  # comparaison sur ces buckets
  dd <- full_join(
    dd %>% group_by(labels) %>%
      summarise(
        ratio_moyen = mean(ratio),
        ratio_min = min(ratio),
        ratio_max = max(ratio)
      ),
    dd %>% group_by(labels) %>%
      summarise_at(
        funs(sum(.) / sum(!!expo_var)),
        .vars = vars(!!obs_var, !!pred1_var, !!pred2_var)
      ) %>%
      ungroup,
    by = "labels"
  ) %>%
    full_join(
      dd %>% group_by(labels) %>%
        summarise(
          exposure = sum(!!expo_var)
        )) %>%
    mutate(groupe = row_number())

  # # création des labels
  # dd <- dd %>%
  #   mutate(labs = paste0("[", round(ratio_min, 2), ", ", round(ratio_max, 2), "]"))

  # graphe
  plotdata <-
    dd %>%
    gather(key, variable, !!obs_var, !!pred1_var, !!pred2_var) %>%
    ## Pas optimal mais je ne trouve pas mieux...
    mutate(key = case_when(
      key == obs_name ~ actual_lab,
      key == pred1_name ~ pred1_lab,
      key == pred2_name ~ pred2_lab
    )) %>%
    mutate(key = factor(key, levels = c(actual_lab, pred1_lab, pred2_lab), ordered = TRUE))


  if (is.null(graphe_title)  ){

    pl <- plotdata %>%
      ggplot(aes(labels, variable, color = key, linetype = key, group = key)) +
      geom_line() + geom_point() +
      pkgsimon::theme_dviz_grid() +
      scale_color_manual(name = "", values = c(cbbPalette)) +
      scale_linetype_manual(name = "", values = c(3, 1, 1)) +
      xlab(x_label) + ylab(y_label)+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      scale_y_continuous(labels =  y_format())
  } else {
    pl <- plotdata %>%
      ggplot(aes(labels, variable, color = key, linetype = key, group = key)) +
      geom_line() + geom_point() +
      pkgsimon::theme_dviz_grid() +
      scale_color_manual(name = "", values = c(cbbPalette)) +
      scale_linetype_manual(name = "", values = c(3, 1, 1)) +
      xlab(x_label) + ylab(y_label)+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      scale_y_continuous(labels =  y_format()) +
      ggtitle(graphe_title)
  }

  pl_expo <- dd %>%
    ggplot(aes(x= labels, y = exposure))+ geom_col() +
    pkgsimon::theme_dviz_grid() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    xlab("ratio entre les prédictions")




  # écart au réalisé, pondéré
  ecart <- dd %>%
    mutate(poids = abs(1 - ratio_moyen)) %>%
    summarise_at(
      vars(!!pred1_var, !!pred2_var),
      funs(weighted.mean((. - !!obs_var)^2, w = poids) %>% sqrt())
    ) %>% summarise(ratio_distance = !!pred2_var / !!pred1_var) %>%
    as.numeric()

  list(
    graphe = pl,
    graphe_expo = pl_expo,
    ecart = ecart,
    tableau = dd
  )
}
