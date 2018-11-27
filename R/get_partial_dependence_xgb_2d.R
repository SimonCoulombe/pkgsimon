#' @title get_partial_dependence_xgb_2d()
#'
#' @description Cette fonction calcule les dépendances partielles en 2D pour 2 variables d'un modèles xgboost après avoir enlevé les outliers.
#' @param xgb_model  le modèle xgboost qui va être prédit
#' @param feature_vars un vecteur caractère avec les noms des variables prédictives du modèle
#' @param pdp_var1 la premiere variable pour la pdp (caractère)
#' @param pdp_var2 la deuxième variable pour la pdp (caractère)
#' @param offset_var variable à mettre en offset, si nécessaire
#' @param model_data un data frame avec les observations à prédire
#' @param nquantiles le nombre de quantiles à calculer.
#' @examples
#' zzzz3 <- get_partial_dependence_xgb_2d(xgb_model = resBoost,
#'                                       feature_vars = c("brake_intensity", "number_of_brake", "number_of_accel", "distraction"),
#'                                       model_data = model_data,
#'                                       pdp_var1 = "braking_combined_roadtype0",
#'                                       pdp_var2 = "brake_intensity",
#'                                       offset_var = "pred_saison",
#'                                       nquantiles = 5)
#'  @export

get_partial_dependence_xgb_2d <- function(xgb_model, feature_vars, model_data, pdp_var1, pdp_var2, offset_var= NULL, nquantiles = 5) {
  message("Début du traitement\t\t", Sys.time())

  grille1 <- model_data[[pdp_var1]] %>%
    pkgsimon::remove_outliers() %>%
    quantile(probs = seq(0, 1, length.out = nquantiles), na.rm = TRUE) %>%
    as.numeric()

  grille2 <- model_data[[pdp_var2]] %>%
    pkgsimon::remove_outliers() %>%
    quantile(probs = seq(0, 1, length.out = nquantiles), na.rm = TRUE) %>%
    as.numeric()

  crossed <-  crossing(grille1, grille2)

  zz <- crossed %>% mutate( pouet = pmap(list(grille1, grille2),
                                         function(grille1, grille2){

                                           tt <- model_data
                                           tt[[pdp_var1]] <- grille1
                                           tt[[pdp_var2]] <- grille2
                                           ddd <- xgb.DMatrix(data = tt %>% select(!!feature_vars) %>% as.matrix,
                                                              missing = "NAN")

                                           if (!is.null(offset_var)) {
                                             setinfo(ddd, "base_margin",
                                                     tt %>% pull(offset_var) %>% log())
                                           } else{
                                             setinfo(ddd, "base_margin",
                                                     rep(1, nrow(tt)) %>% log() )
                                           }
                                           tt$predBoost <- predict(object = xgb_model, newdata = ddd)

                                           if (!is.null(offset_var)) {
                                             (sum(tt["predBoost"]) / sum(tt[offset_var])) %>% as_tibble %>% rename(ratio= value) %>%
                                               mutate(Variable1 = pdp_var1,
                                                      Variable2 = pdp_var2,
                                                      Mod1 = grille1,
                                                      Mod2 = grille2)
                                           } else{
                                             (sum(tt["predBoost"]) / nrow(tt)) %>% as_tibble %>% rename(ratio= value) %>%
                                               mutate(Variable1 = pdp_var1,
                                                      Variable2 = pdp_var2,
                                                      Mod1 = grille1,
                                                      Mod2 = grille2)
                                           }

                                         } # fin de fonction pmap
  ) # fin de pmap
  )  %>%  # fin de murate

    bind_cols(.,  .$pouet %>% bind_rows()) %>% # passe du coyote qui tousse parce que le pmap en haut retourne une list-column de tibble de 1 rangée
    select(-pouet, -grille1, -grille2)
  message("Fin du traitement\t\t", Sys.time())
  zz

} # fin function get_partial_dependence_2d
