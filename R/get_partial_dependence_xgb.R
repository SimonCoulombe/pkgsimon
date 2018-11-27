#' @title get_partial_dependence_xgb()
#'
#' @description Cette fonction calcule les dépendances partielles d'un modèles xgboost après avoir enlevé les outliers. défaut = toutes les variables.   elle retourne un data frame avec les valeurs (Mod) de la variable explicative (Variable)
#'
#' @param xgb_model  le modèle xgboost qui va être prédit
#' @param feature_vars un vecteur caractère avec les noms des variables prédictives du modèle
#' @param pdp_vars un vecteur caractère avec les noms des variables pour lesquelles on souhaite calculer la pdp
#' @param offset_var variable à mettre en offset, si nécessaire
#' @param model_data un data frame avec les observations à prédire
#' @param nquantiles le nombre de quantiles à calculer.
#' @examples
#' zzzz1 <- get_partial_dependence_xgb(xgb_model = resBoost,
#'                                     feature_vars = c("brake_intensity", "number_of_brake", "number_of_accel", "distraction"),
#'                                     model_data = model_data,
#'                                     pdp_vars = c("brake_intensity"),
#'                                     offset_var = "pred_saison",
#'                                     nquantiles = 11)
#' @export

get_partial_dependence_xgb <- function(xgb_model,pdp_vars = NULL,  feature_vars, offset_var = NULL,  model_data, nquantiles = 21) {
  message("Début du traitement\t\t", Sys.time())

  if (is.null(pdp_vars)){pdp_vars <- feature_vars} # si on ne spécifie pas de pdp_vars, alors on les fait toutes.
  retour <- pdp_vars %>%
    map_df(function(vv) {

      message("\t\t Traitement de ", vv, "\t\t", Sys.time())
      grille <- model_data[[vv]] %>%
        pkgsimon::remove_outliers() %>%  # enlever les gens qui sont en dehors de 1.5 inter quartile range

        quantile(probs = seq(0, 1, length.out = nquantiles), na.rm = TRUE) %>%
        as.numeric()

      res <- grille %>% map_df(
        function(xx) {
          tt <- model_data
          tt[[vv]] <- xx
          ddd <-
            xgb.DMatrix(data = tt %>% select(!!feature_vars) %>% as.matrix,
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
              mutate(Variable = vv, Mod = xx)
          } else{
            (sum(tt["predBoost"]) / nrow(tt)) %>% as_tibble %>% rename(ratio= value) %>%
              mutate(Variable = vv, Mod = xx)
          }


        } #fin fonction dansle  map_df de res
      ) # fin map_df de res
      message("Fin du traitement\t\t", Sys.time())
      res
    } # fin fonction anonyme dans mapdf
    )  # fin du mapdf
} # fin function get_partial_dependance

